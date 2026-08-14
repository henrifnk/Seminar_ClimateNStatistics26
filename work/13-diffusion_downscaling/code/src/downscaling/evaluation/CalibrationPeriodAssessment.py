"""
Evaluate the diffusion downscaling model on the held-out CERRA test period (2019-2020).

Computes: MAE, CRPS, power spectrum, spread-skill relationship, a spatial map of mean
ensemble spread, and where the model's downscaling correction (mean prediction − mean
coarse baseline) systematically lands spatially.

Usage:
    python -m downscaling.evaluation.CalibrationPeriodAssessment
    python -m downscaling.evaluation.CalibrationPeriodAssessment --checkpoint ./model/20.pt --stride 7 --n-clim 4 --cache-dir ./cache
"""
import argparse
import os
import random

import matplotlib.pyplot as plt
import numpy as np
import torch
import xarray as xr
from tqdm import tqdm

import wandb
from downscaling.DatasetAL import UpscaleDataset
from downscaling.evaluation.EnsembleInference import (
    DEVICE,
    VAR_COLORS,
    cache_tag,
    compute_cerra_grid,
    crps_ensemble,
    load_model,
    radial_power_spectrum,
    run_ensemble,
)
from downscaling.inference.Inference import CERRA_REF, OUTPUT_SIZE
from downscaling.paths import CALIBRATION_DIR


def _run_or_load_predictions(dataset_test, network, norm_res_mean, norm_res_std,
                              valid_mask, indices, n_clim, cache_dir, slug):
    """Run model inference or load from cache.

    Saves/loads pred_cal_{slug}.npz containing:
      all_samples (N, n_clim, 2, H, W) — raw ensemble samples (pre degC conversion)
      fines       (N, 2, H, W)         — CERRA ground truth
      coarses     (N, 2, H, W)         — coarse bilinear fields
    """
    pred_path = os.path.join(cache_dir, f"pred_cal_{slug}.npz") if cache_dir else None

    if pred_path and os.path.exists(pred_path):
        print(f"  Loading cached predictions from {pred_path}")
        d = np.load(pred_path, allow_pickle=True)
        return d["all_samples"], d["fines"], d["coarses"]

    all_samples_list, fines_list, coarses_list = [], [], []

    for day_idx in tqdm(indices, desc="Inference"):
        item = dataset_test[day_idx]
        inputs = item["inputs"].unsqueeze(0).to(DEVICE)
        coarse = item["coarse"].numpy().copy()
        fine   = item["fine"].numpy().copy()
        coarse[:, ~valid_mask] = np.nan
        fine[:, ~valid_mask]   = np.nan

        # The dataset already carries the labels in the training encoding — use them
        # directly rather than re-deriving from a date string.
        labels = torch.stack((item["doy"], item["hour"])).view(1, 2)

        samples = run_ensemble(network, inputs, coarse, norm_res_mean, norm_res_std,
                               valid_mask, n_clim, labels=labels)

        all_samples_list.append(samples)
        fines_list.append(fine)
        coarses_list.append(coarse)

    all_samples = np.stack(all_samples_list).astype(np.float32)  # (N, n_clim, 2, H, W)
    fines       = np.stack(fines_list).astype(np.float32)        # (N, 2, H, W)
    coarses     = np.stack(coarses_list).astype(np.float32)      # (N, 2, H, W)

    if pred_path:
        os.makedirs(cache_dir, exist_ok=True)
        np.savez_compressed(pred_path, all_samples=all_samples, fines=fines, coarses=coarses)
        print(f"  Saved predictions to {pred_path}")

    return all_samples, fines, coarses


def main(checkpoint, n_clim, stride, cache_dir, seed):
    random.seed(seed)
    np.random.seed(seed)
    os.makedirs(CALIBRATION_DIR, exist_ok=True)

    os.environ["WANDB_START_METHOD"] = "thread"
    wandb.init(
        project="climate-diffusion-downscaling",
        name="evaluation",
        config=dict(checkpoint=checkpoint, n_clim=n_clim,
                    stride=stride, seed=seed, period="2019-2021")
    )

    print("Loading normalization stats...")
    _stats = np.load("data/norm_stats.npz")
    norm_raw_mean = torch.from_numpy(_stats["norm_raw_mean"])
    norm_raw_std  = torch.from_numpy(_stats["norm_raw_std"])
    norm_res_mean = torch.from_numpy(_stats["norm_res_mean"])
    norm_res_std  = torch.from_numpy(_stats["norm_res_std"])

    print("Building test dataset...")
    dataset_test = UpscaleDataset(
        "data/reanalysis/", "data/reanalysis_coarsened/", "data/",
        year_start=2019, year_end=2021, constant_variables=["lsm", "orog"],
        normalize_rawdata_mean=norm_raw_mean,
        normalize_rawdata_std=norm_raw_std,
        normalize_residual_mean=norm_res_mean,
        normalize_residual_std=norm_res_std)

    # CERRA valid mask + lat/lon
    cerra_ds = xr.open_dataset(CERRA_REF, engine="netcdf4")
    valid_mask, fine_lat, fine_lon = compute_cerra_grid(cerra_ds)
    cerra_ds.close()

    print("Loading model...")
    network = load_model(checkpoint)

    indices = list(range(0, len(dataset_test), stride))
    slug = f"cal_2019_2021_s{stride}_n{n_clim}_{cache_tag(checkpoint)}"

    print(f"Evaluating {len(indices)} test days (stride={stride}), {n_clim} samples each...")

    all_samples, fines, coarses = _run_or_load_predictions(
        dataset_test, network, norm_res_mean, norm_res_std,
        valid_mask, indices, n_clim, cache_dir, slug)

    # K -> degC
    fines[:, 0]          -= 273.15
    coarses[:, 0]        -= 273.15
    all_samples[:, :, 0] -= 273.15

    mae_days      = []        # (n_days, 2)
    mae_base_days = []        # (n_days, 2) bilinear baseline
    crps_days     = []        # (n_days, 2)
    mae_map_sum    = np.zeros((2, *OUTPUT_SIZE))   # per-pixel MAE accumulator
    mae_map_count  = np.zeros(OUTPUT_SIZE)          # valid pixel count per location
    down_map_sum   = np.zeros((2, *OUTPUT_SIZE))   # per-pixel ensemble-mean accumulator
    coarse_map_sum = np.zeros((2, *OUTPUT_SIZE))   # per-pixel coarse-field accumulator
    spread_map_sum = np.zeros((2, *OUTPUT_SIZE))   # per-pixel ensemble-spread accumulator
    ps_coarse     = [[], []]  # [tas, pr] lists of 1D spectra
    ps_pred       = [[], []]
    ps_fine       = [[], []]
    spread_all    = [[], []]  # [tas, pr] flat lists of pixel-level spread values
    skill_all     = [[], []]  # corresponding pixel-level |error| values
    ranks              = [[], []]  # rank of obs in ensemble (0..n_clim)

    def log_progress(n_done):
        if not mae_days:
            return
        mae_mean_now      = np.mean(mae_days,      axis=0)
        mae_base_mean_now = np.mean(mae_base_days, axis=0)
        crps_mean_now     = np.mean(crps_days,     axis=0)

        # Finite-ensemble spread correction (Fortin et al. 2014, eq. 13): the sample
        # std of an N-member ensemble underestimates the true spread. Assumes ddof=0
        # (biased sample std, i.e. np.std/np.nanstd default, as used for `spread` below).
        SPREAD_CORRECTION = np.sqrt((n_clim + 1) / (n_clim - 1))

        LIGHT_VAR_COLORS = ["#F1948A", "#85C1E9"]  # light red / light blue tints of VAR_COLORS

        def _bar_data(v):
            """Bar chart labels/values/colors for full-field MAE & CRPS (all pixels/days)."""
            labels = ["Interpolation baseline -\nMAE (= CRPS)", "Diffusion model -\nCRPS"]
            vals   = [mae_base_mean_now[v], crps_mean_now[v]]
            colors = [LIGHT_VAR_COLORS[v], VAR_COLORS[v]]
            hatches = [None, None]
            return labels, vals, colors, hatches

        def _plot_bar_ax(ax, labels, vals, colors, hatches, title, unit, ylim_max=None):
            bars = ax.bar(labels, vals, color=colors, width=0.5)
            for bar, hatch in zip(bars, hatches):
                bar.set_hatch(hatch)
            ax.set_title(title); ax.set_ylabel(unit)
            ax.set_ylim(0, (ylim_max if ylim_max is not None else max(vals)) * 1.18)
            for bar, val in zip(bars, vals):
                ax.text(bar.get_x() + bar.get_width() / 2, val * 1.02,
                        f"{val:.3f}", ha="center", va="bottom", fontsize=9)
            ax.tick_params(axis="x", labelsize=7)
            ax.grid(True, axis="y", alpha=0.3)

        var_labels = [("Surface air temperature", "Temperature (°C)"), ("Precipitation", "Precipitation (kg/m²)")]

        # Power spectrum figure — shared y-axis per variable pair not forced
        # (tas and pr power magnitudes differ by orders of magnitude; share x-axis only)
        fig_ps, axs_ps = plt.subplots(1, 2, figsize=(12, 4), constrained_layout=True)
        for v, (name, _) in enumerate(var_labels):
            ax = axs_ps[v]
            ps_c = np.mean(ps_coarse[v], axis=0)
            ps_p = np.mean(ps_pred[v],   axis=0)
            ps_f = np.mean(ps_fine[v],   axis=0)
            k = np.arange(1, len(ps_c))
            scale_km = (256 * 3.4) / k
            ax.loglog(scale_km, ps_c[1:], label="Interpolated field", color="steelblue", lw=1.5)
            ax.loglog(scale_km, ps_p[1:], label="Diffusion model field", color="tomato", lw=1.5)
            ax.loglog(scale_km, ps_f[1:], label="True climate field", color="black", lw=1.5, ls="--")
            ax.set_title(f"Power spectrum  -  {name}")
            ax.set_xlabel("Spatial scale (km)"); ax.set_ylabel("Power")
            ax.invert_xaxis()
            ax.set_xlim(scale_km.max(), scale_km.min())
            ax.legend(fontsize=8); ax.grid(True, which="both", alpha=0.3)
        ps_path = os.path.abspath(f"{CALIBRATION_DIR}/1_power_spectrum.svg")
        fig_ps.savefig(ps_path, dpi=150, bbox_inches="tight")
        plt.close(fig_ps)

        # Spread-skill figure
        fig_ss, axs_ss = plt.subplots(1, 2, figsize=(12, 4), constrained_layout=True)
        for v, (name, _) in enumerate(var_labels):
            ax = axs_ss[v]
            sp = np.array(spread_all[v]); sk = np.array(skill_all[v])
            bin_edges = np.unique(np.quantile(sp, np.linspace(0, 1, 31)))
            bin_idx = np.clip(np.digitize(sp, bin_edges) - 1, 0, len(bin_edges) - 2)
            # RMS spread per bin (not the arithmetic mean) so both axes aggregate the
            # same way as bin_rmse, then apply the finite-ensemble correction.
            bin_spread = [SPREAD_CORRECTION * np.sqrt((sp[bin_idx == b] ** 2).mean())
                          for b in range(len(bin_edges) - 1) if (bin_idx == b).sum() > 0]
            bin_rmse   = [np.sqrt(sk[bin_idx == b].mean()) for b in range(len(bin_edges) - 1) if (bin_idx == b).sum() > 0]
            ax.scatter(bin_rmse, bin_spread, color=VAR_COLORS[v], s=40, zorder=3)
            diag = max(max(bin_rmse), max(bin_spread))
            ax.plot([0, diag], [0, diag], "k--", lw=1, label="Perfect calibration")
            # Aggregate spread-skill ratio over all pixel-day pairs (Mardani et al.):
            # 1.0 means the corrected ensemble variance matches the mean squared error.
            r_ss = np.sqrt(SPREAD_CORRECTION ** 2 * np.mean(sp ** 2) / np.mean(sk))
            var_short = ["tas", "pr"][v]
            print(f"  {var_short} spread-skill ratio: {r_ss:.2f}")
            ax.plot([], [], " ", label=f"mean ratio: {r_ss:.2f}")
            ax.set_xlabel("RMSE of ensemble mean"); ax.set_ylabel("Ensemble spread (standard deviation)")
            ax.set_title(f"Spread - skill  -  {name}")
            ax.legend(fontsize=8); ax.grid(True, alpha=0.3)
        ss_path = os.path.abspath(f"{CALIBRATION_DIR}/2_spread_skill.svg")
        fig_ss.savefig(ss_path, dpi=150, bbox_inches="tight")
        plt.close(fig_ss)

        # MAE/CRPS bar-chart figure (full-field, all pixels/days)
        fig_mc, axs_mc = plt.subplots(1, 2, figsize=(10, 4), constrained_layout=True)
        for v, (name, unit) in enumerate(var_labels):
            labels, vals, colors, hatches = _bar_data(v)
            _plot_bar_ax(axs_mc[v], labels, vals, colors, hatches, f"MAE & CRPS - {name}", unit)
        mc_path = os.path.abspath(f"{CALIBRATION_DIR}/3_mae_crps.svg")
        fig_mc.savefig(mc_path, dpi=150, bbox_inches="tight")
        plt.close(fig_mc)

        skill_now = 1 - mae_mean_now / np.where(mae_base_mean_now > 0, mae_base_mean_now, np.nan)
        wandb.log({
            "eval/mae_tas":          mae_mean_now[0],
            "eval/mae_pr":           mae_mean_now[1],
            "eval/mae_baseline_tas": mae_base_mean_now[0],
            "eval/mae_baseline_pr":  mae_base_mean_now[1],
            "eval/crps_tas":         crps_mean_now[0],
            "eval/crps_pr":          crps_mean_now[1],
            "eval/mae_skill_tas":    float(skill_now[0]),
            "eval/mae_skill_pr":     float(skill_now[1]),
        }, step=n_done)
        print(f"  wandb logged step={n_done}")

    for day_num, (_, samples, fine, coarse) in enumerate(
            tqdm(zip(indices, all_samples, fines, coarses), total=len(all_samples), desc="Metrics"),
            start=1):
        ens_mean = np.nanmean(samples, axis=0)   # (2, H, W)

        mae_days.append(np.array([
            np.nanmean(np.abs(ens_mean[0] - fine[0])),
            np.nanmean(np.abs(ens_mean[1] - fine[1]))
        ]))
        mae_base_days.append(np.array([
            np.nanmean(np.abs(coarse[0] - fine[0])),
            np.nanmean(np.abs(coarse[1] - fine[1]))
        ]))
        crps_days.append(crps_ensemble(fine, samples))

        for v in range(2):
            ps_coarse[v].append(radial_power_spectrum(coarse[v]))
            ps_pred[v].append(np.mean(
                [radial_power_spectrum(samples[s, v]) for s in range(n_clim)], axis=0))
            ps_fine[v].append(radial_power_spectrum(fine[v]))

        abs_err = np.abs(ens_mean - fine)           # (2, H, W)
        ok_map = valid_mask & ~np.isnan(abs_err[0]) & ~np.isnan(abs_err[1])
        mae_map_sum[:, ok_map]    += abs_err[:, ok_map]
        mae_map_count[ok_map]     += 1
        down_map_sum[:, ok_map]   += ens_mean[:, ok_map]
        coarse_map_sum[:, ok_map] += coarse[:, ok_map]

        spread = np.nanstd(samples, axis=0)       # (2, H, W)
        spread_map_sum[:, ok_map] += spread[:, ok_map]
        error_sq = (ens_mean - fine) ** 2         # (2, H, W) squared errors for RMSE
        for v in range(2):
            ok = valid_mask & ~np.isnan(error_sq[v]) & ~np.isnan(spread[v])
            spread_all[v].extend(spread[v][ok].tolist())
            skill_all[v].extend(error_sq[v][ok].tolist())

        for v in range(2):
            ok = valid_mask & ~np.isnan(fine[v])
            rank = (samples[:, v] < fine[v][np.newaxis]).sum(axis=0)
            ranks[v].extend(rank[ok].tolist())

    mae_mean      = np.mean(mae_days,      axis=0)
    mae_base_mean = np.mean(mae_base_days, axis=0)
    crps_mean     = np.mean(crps_days,     axis=0)

    print("\n── Results ─────────────────────────────────────────────────────────")
    for v, name in enumerate(["tas", "pr"]):
        print(f"  {name}:  Baseline MAE = {mae_base_mean[v]:.4f}   "
              f"Model MAE = {mae_mean[v]:.4f}   CRPS = {crps_mean[v]:.4f}")

    log_progress(len(indices))

    # Spatial MAE map
    import cartopy.crs as ccrs
    mae_map = np.full_like(mae_map_sum, np.nan)
    np.divide(mae_map_sum, mae_map_count, out=mae_map, where=mae_map_count > 0)
    mae_map[:, ~valid_mask] = np.nan
    extent = [float(np.nanmin(fine_lon)), float(np.nanmax(fine_lon)),
              float(np.nanmin(fine_lat)), float(np.nanmax(fine_lat))]
    vl_map = [("Surface air temperature", "MAE (°C)"),
              ("Precipitation", "MAE (kg/m²)")]
    fig_mae, axs_mae = plt.subplots(1, 2, figsize=(14, 5),
                                    subplot_kw={"projection": ccrs.PlateCarree()},
                                    constrained_layout=True)
    for v, (vname, clabel) in enumerate(vl_map):
        ax = axs_mae[v]
        ax.set_extent(extent, crs=ccrs.PlateCarree())
        ax.set_facecolor("lightgrey")
        ax.coastlines(linewidth=0.7)
        cmap_name = "Reds" if v == 0 else "Blues"
        cmap = plt.get_cmap(cmap_name).copy(); cmap.set_bad("lightgrey")
        im = ax.pcolormesh(fine_lon, fine_lat, mae_map[v],
                           cmap=cmap, shading="nearest",
                           transform=ccrs.PlateCarree(), rasterized=True)
        plt.colorbar(im, ax=ax, label=clabel, shrink=0.62)
        ax.set_title(vname)
    mae_map_path = os.path.abspath(f"{CALIBRATION_DIR}/4_mae_spatial.svg")
    fig_mae.savefig(mae_map_path, dpi=150, bbox_inches="tight")
    plt.close(fig_mae)
    print(f"  Saved {mae_map_path}")

    # Spatial ensemble-spread map: mean per-pixel ensemble std over the whole held-out
    # period, i.e. where the model is systematically uncertain.
    spread_map = np.full_like(spread_map_sum, np.nan)
    np.divide(spread_map_sum, mae_map_count, out=spread_map, where=mae_map_count > 0)
    spread_map[:, ~valid_mask] = np.nan
    vl_spread = [("Surface air temperature", "Ensemble std (°C)"),
                 ("Precipitation", "Ensemble std (kg/m²)")]
    fig_spread, axs_spread = plt.subplots(1, 2, figsize=(14, 5),
                                          subplot_kw={"projection": ccrs.PlateCarree()},
                                          constrained_layout=True)
    for v, (vname, clabel) in enumerate(vl_spread):
        ax = axs_spread[v]
        ax.set_extent(extent, crs=ccrs.PlateCarree())
        ax.set_facecolor("lightgrey")
        ax.coastlines(linewidth=0.7)
        cmap = plt.get_cmap("OrRd").copy(); cmap.set_bad("lightgrey")
        # True max, not a percentile — the whole point is showing where spread is highest.
        im = ax.pcolormesh(fine_lon, fine_lat, spread_map[v], vmin=0, vmax=float(np.nanmax(spread_map[v])),
                           cmap=cmap, shading="nearest", transform=ccrs.PlateCarree(), rasterized=True)
        plt.colorbar(im, ax=ax, label=clabel, shrink=0.62)
        ax.set_title(vname)
    spread_map_path = os.path.abspath(f"{CALIBRATION_DIR}/5_spread_spatial.svg")
    fig_spread.savefig(spread_map_path, dpi=150, bbox_inches="tight")
    plt.close(fig_spread)
    print(f"  Saved {spread_map_path}")

    # Downscaling correction map: mean(model prediction) − mean(coarse baseline) over
    # the whole held-out period, i.e. where the model systematically adds to, or
    # diverges from, the coarse input. Moved here from the projection-period
    # assessment (formerly GWL2-only there); here it's averaged over every test day.
    down_map   = np.full_like(down_map_sum, np.nan)
    coarse_map = np.full_like(coarse_map_sum, np.nan)
    np.divide(down_map_sum,   mae_map_count, out=down_map,   where=mae_map_count > 0)
    np.divide(coarse_map_sum, mae_map_count, out=coarse_map, where=mae_map_count > 0)

    corr_display_name = {"tas": "Temperature above surface", "pr": "Precipitation"}
    corr_log = {}
    fig_corr, axs_corr = plt.subplots(1, 2, figsize=(10, 4.5), constrained_layout=True,
                                      subplot_kw={"projection": ccrs.PlateCarree()})
    for v, (vname, unit) in enumerate([("tas", "°C"), ("pr", "kg/m²")]):
        diff = np.where(valid_mask, down_map[v] - coarse_map[v], np.nan)
        vabs = float(np.nanpercentile(np.abs(diff[valid_mask]), 98))
        ax = axs_corr[v]
        cmap = plt.get_cmap("RdBu_r" if v == 0 else "BrBG").copy(); cmap.set_bad("lightgrey")
        im = ax.pcolormesh(fine_lon, fine_lat, diff, vmin=-vabs, vmax=vabs, cmap=cmap,
                           transform=ccrs.PlateCarree(), shading="nearest", rasterized=True)
        ax.set_extent(extent, crs=ccrs.PlateCarree()); ax.coastlines(); ax.set_facecolor("lightgrey")
        ax.set_title(corr_display_name[vname], fontsize=9)
        fig_corr.colorbar(im, ax=ax, shrink=0.47, label=unit)

        mean_corr = float(np.nanmean(diff[valid_mask]))
        std_corr  = float(np.nanstd(diff[valid_mask]))
        corr_log[f"correction/{vname}_mean"] = mean_corr
        corr_log[f"correction/{vname}_std"]  = std_corr
        print(f"  {vname}: correction mean={mean_corr:+.3f} std={std_corr:.3f} {unit}")

    corr_path = os.path.abspath(f"{CALIBRATION_DIR}/6_downscaling_correction.svg")
    fig_corr.savefig(corr_path, dpi=150, bbox_inches="tight")
    plt.close(fig_corr)
    print(f"  Saved {corr_path}")
    wandb.log(corr_log)
    wandb.summary.update(corr_log)

    mae_skill = 1 - mae_mean / np.where(mae_base_mean > 0, mae_base_mean, np.nan)
    wandb.summary.update({
        "final/mae_tas":       float(mae_mean[0]),
        "final/mae_pr":        float(mae_mean[1]),
        "final/mae_skill_tas": float(mae_skill[0]),
        "final/mae_skill_pr":  float(mae_skill[1]),
        "final/crps_tas":      float(crps_mean[0]),
        "final/crps_pr":       float(crps_mean[1]),
    })

    vl = [("Surface air temperature", "Temperature (°C)"), ("Precipitation", "Precipitation (kg/m²)")]

    # Rank histogram
    fig_rh, axs_rh = plt.subplots(1, 2, figsize=(12, 4), constrained_layout=True)
    rh_counts = []
    bins_rh = n_clim + 1
    for v in range(2):
        cnt, _ = np.histogram(ranks[v], bins=bins_rh, range=(-0.5, bins_rh - 0.5))
        rh_counts.append(cnt / cnt.sum())
    rh_ymax = max(c.max() for c in rh_counts) * 1.15
    for v, (vname, _) in enumerate(vl):
        ax = axs_rh[v]
        ax.bar(range(bins_rh), rh_counts[v], color=VAR_COLORS[v], alpha=0.7, width=0.8)
        ax.axhline(1 / bins_rh, color="red", ls="--", lw=1.5, label="Uniform (calibrated)")
        ax.set_title(f"Rank histogram - {vname}")
        ax.set_xlabel("Rank"); ax.set_ylabel("Frequency")
        ax.set_ylim(0, rh_ymax)
        ax.legend(fontsize=8); ax.grid(True, axis="y", alpha=0.3)
    rh_path = os.path.abspath(f"{CALIBRATION_DIR}/7_rank_histogram.svg")
    fig_rh.savefig(rh_path, dpi=150, bbox_inches="tight")
    plt.close(fig_rh)

    outlier_frac = [((np.array(ranks[v]) == 0) | (np.array(ranks[v]) == n_clim)).mean()
                    for v in range(2)]
    wandb.summary.update({
        "final/rank_outlier_tas": float(outlier_frac[0]),
        "final/rank_outlier_pr":  float(outlier_frac[1]),
    })
    wandb.finish()

    np.savez(f"{CALIBRATION_DIR}/metrics.npz",
             mae=mae_mean, crps=crps_mean,
             ps_coarse_tas=np.mean(ps_coarse[0], axis=0),
             ps_pred_tas=np.mean(ps_pred[0],   axis=0),
             ps_fine_tas=np.mean(ps_fine[0],   axis=0),
             ps_coarse_pr=np.mean(ps_coarse[1], axis=0),
             ps_pred_pr=np.mean(ps_pred[1],   axis=0),
             ps_fine_pr=np.mean(ps_fine[1],   axis=0))
    print(f"Metrics saved to {CALIBRATION_DIR}/metrics.npz")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--checkpoint", default="./model/20.pt")
    parser.add_argument("--n-clim",     type=int, default=4,
                        help="ensemble members per day (default: 4)")
    parser.add_argument("--stride",     type=int, default=15,
                        help="use every Nth test date (default: 15)")
    parser.add_argument("--cache-dir",  default=None,
                        help="directory to cache/load predictions (skips inference if cache exists)")
    parser.add_argument("--skip-validation", action="store_true",
                        help="skip the calibration assessment (no-op, for CLI compatibility)")
    parser.add_argument("--seed",       type=int, default=42)
    args = parser.parse_args()

    if not args.skip_validation:
        main(args.checkpoint, args.n_clim, args.stride, args.cache_dir, args.seed)
