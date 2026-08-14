"""
Assessment of model behaviour on MBCn climate model data across GWL periods.

  Example date plot — one historical MBCn date, run through the model, shown as
            native coarse | bilinear interpolated | downscaled.
  Part 2 — Climate change signal: delta maps (proj - hist) for downscaled
            vs coarse; checks whether the model preserves the GCM signal.
  Part 3 — Distribution consistency: marginal histograms of downscaled output
            across historical / mid-century / end-century periods.

Parts 2 & 3 are drawn by ProjectionPlots.py, which writes every figure under
plots/projection/.

Usage:
    uv run python src/evaluation/ProjectionPeriodAssessment.py --checkpoint ./model/20.pt
    uv run python src/evaluation/ProjectionPeriodAssessment.py --checkpoint ./model/20.pt --n-clim 4 --stride 10 --cache-dir ./cache
"""
import argparse
import os
import sys

import matplotlib
import numpy as np
import torch
import torch.nn.functional as F
import xarray as xr

matplotlib.use("Agg")
from glob import glob
from pathlib import Path

import cartopy.crs as ccrs
import matplotlib.pyplot as plt
from scipy.interpolate import griddata as scipy_griddata
from tqdm import tqdm

import wandb

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

from DatasetAL import doy_hour_label
from evaluation.EnsembleInference import DEVICE, cache_tag, compute_cerra_grid, load_model, run_ensemble
from evaluation.ProjectionPlots import (
    GWL_PERIODS,
    OUT_DIR,
    _cache_slug,
    _plot_delta,
    _plot_distribution,
    _plot_period_means,
    _plot_quantile_delta,
    _plot_spread_persistence,
)
from inference.Inference import (
    CERRA_REF,
    DATADIR_PROJECTION,
    OUTPUT_SIZE,
    build_interpolator,
    find_mbcn_file,
    interpolate_to_cerra,
)

REANALYSIS        = "data/reanalysis"
REANALYSIS_COARSE = "data/reanalysis_coarsened"


# ── Setup ─────────────────────────────────────────────────────────────────────

def _load_grid(cerra_ref=None, cache_dir=None):
    """Load valid_mask, fine_lat, fine_lon — from cache if available, else from CERRA."""
    if cache_dir:
        grid_path = os.path.join(cache_dir, "grid.npz")
        if os.path.exists(grid_path):
            d = np.load(grid_path)
            result = dict(valid_mask=d["valid_mask"].astype(bool),
                          fine_lat=d["fine_lat"], fine_lon=d["fine_lon"])
            if "const_tensor" in d:
                result["const_tensor"] = torch.from_numpy(d["const_tensor"])
            return result
    close_after = cerra_ref is None
    if cerra_ref is None:
        cerra_ref = xr.open_dataset(CERRA_REF, engine="netcdf4")
    valid_mask, fine_lat, fine_lon = compute_cerra_grid(cerra_ref)
    if close_after:
        cerra_ref.close()
    grid = dict(valid_mask=valid_mask, fine_lat=fine_lat, fine_lon=fine_lon)
    if cache_dir:
        os.makedirs(cache_dir, exist_ok=True)
        np.savez(os.path.join(cache_dir, "grid.npz"), **grid)
        # const_tensor saved separately after load_setup builds it
    return grid


def _all_caches_present(args):
    """Return True if all prediction caches and grid cache exist so model loading can be skipped."""
    if not args.cache_dir:
        return False
    if not os.path.exists(os.path.join(args.cache_dir, "grid.npz")):
        return False
    s, n_clim = args.stride, args.n_clim
    tag = cache_tag(args.checkpoint)
    for label, start, end in GWL_PERIODS:
        full_slug = f"{_cache_slug(label)}_{start}_{end}_s{s}_n{n_clim}_{tag}"
        level1 = os.path.join(args.cache_dir, f"{full_slug}.npz")
        level2 = os.path.join(args.cache_dir, f"pred_{full_slug}.npz")
        if not (os.path.exists(level1) or os.path.exists(level2)):
            return False
    return True


def _build_interp(grid, cache_dir=None):
    """Build the MBCn -> fine-grid interpolator used by prep_mbcn_date: from CERRA when
    available, otherwise a direct Delaunay interpolator onto grid's fine_lat/fine_lon."""
    if os.path.exists(CERRA_REF):
        cerra_ref   = xr.open_dataset(CERRA_REF, engine="netcdf4")
        if not grid:
            grid = _load_grid(cerra_ref, cache_dir=cache_dir)
        mbcn_sample = xr.open_dataset(sorted(glob(f"{DATADIR_PROJECTION}/tas_mbcn_daily_*.nc"))[0], engine="netcdf4")
        interp = build_interpolator(mbcn_sample, cerra_ref)
        mbcn_sample.close(); cerra_ref.close()
    else:
        # No CERRA: build Delaunay interpolator directly to fine_lat/fine_lon from grid cache
        print("  WARNING: CERRA_REF not found — building interpolator to output grid directly")
        from scipy.spatial import Delaunay
        mbcn_sample = xr.open_dataset(sorted(glob(f"{DATADIR_PROJECTION}/tas_mbcn_daily_*.nc"))[0], engine="netcdf4")
        mbcn_lat = mbcn_sample["lat"].values; mbcn_lon = mbcn_sample["lon"].values
        mbcn_sample.close()
        fine_lat = grid["fine_lat"]; fine_lon = grid["fine_lon"]
        coarse_pts = np.column_stack([mbcn_lat.ravel(), mbcn_lon.ravel()])
        fine_pts   = np.column_stack([fine_lat.ravel(), fine_lon.ravel()])
        tri     = Delaunay(coarse_pts)
        simplex = tri.find_simplex(fine_pts)
        valid   = simplex >= 0
        T = tri.transform[simplex[valid]]
        b = np.einsum('...ij,...j->...i', T[:, :2, :2], fine_pts[valid] - T[:, 2, :])
        weights_interp = np.c_[b, 1 - b.sum(axis=-1)]
        idx = tri.simplices[simplex[valid]]
        interp = (valid, idx, weights_interp, fine_lat.shape)
    return interp


def load_setup(checkpoint, cache_dir=None):
    stats         = np.load("data/norm_stats.npz")
    norm_raw_mean = torch.from_numpy(stats["norm_raw_mean"])
    norm_raw_std  = torch.from_numpy(stats["norm_raw_std"])
    norm_res_mean = torch.from_numpy(stats["norm_res_mean"])
    norm_res_std  = torch.from_numpy(stats["norm_res_std"])

    network = load_model(checkpoint)

    # Try grid cache first; only open CERRA if cache is missing
    grid = _load_grid(cache_dir=cache_dir)
    interp = _build_interp(grid, cache_dir=cache_dir)

    static_path = "data/cerra_static_variables.nc"
    if os.path.exists(static_path):
        ds_c = xr.open_dataset(static_path, engine="netcdf4").squeeze("valid_time")
        const_lat = ds_c.latitude.values; const_lon = ds_c.longitude.values
        in_region = ((const_lat >= 42.2) & (const_lat <= 51.4) &
                     (const_lon >= 2.4)  & (const_lon <= 18.9))
        valid_rows = np.where(in_region.any(axis=1))[0]
        valid_cols = np.where(in_region.any(axis=0))[0]
        row_s = slice(int(valid_rows[0]), int(valid_rows[-1]) + 1)
        col_s = slice(int(valid_cols[0]), int(valid_cols[-1]) + 1)
        crop_lat = const_lat[row_s, col_s]
        const_list = []
        for vn in ["lsm", "orog"]:
            cv_np = ds_c[vn].values[row_s, col_s].astype(np.float32)
            if vn != "lsm":
                w = np.cos(np.radians(crop_lat)); wsum = w.sum()
                wmean = float((cv_np * w).sum() / wsum)
                wstd  = float(np.sqrt(((cv_np - wmean)**2 * w).sum() / wsum))
                cv_np = (cv_np - wmean) / wstd
            const_list.append(torch.from_numpy(cv_np).float())
        const_tensor = F.interpolate(torch.stack(const_list).unsqueeze(0),
                                     size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze(0)
        ds_c.close()
    elif "const_tensor" in grid:
        const_tensor = grid["const_tensor"]
        print("  Loaded const_tensor from grid cache")
    else:
        print("  WARNING: cerra_static_variables.nc not found — using zero constant fields")
        const_tensor = torch.zeros(2, *OUTPUT_SIZE)

    if cache_dir and os.path.exists(os.path.join(cache_dir, "grid.npz")):
        existing = dict(np.load(os.path.join(cache_dir, "grid.npz")))
        if "const_tensor" not in existing:
            existing["const_tensor"] = const_tensor.numpy()
            np.savez(os.path.join(cache_dir, "grid.npz"), **existing)

    grid.pop("const_tensor", None)
    return dict(**grid, network=network,
                norm_raw_mean=norm_raw_mean, norm_raw_std=norm_raw_std,
                norm_res_mean=norm_res_mean, norm_res_std=norm_res_std,
                const_tensor=const_tensor, interp=interp)


# ── Data helpers ──────────────────────────────────────────────────────────────

def get_period_dates(start_year, end_year):
    dates = []
    for path in sorted(glob(f"{DATADIR_PROJECTION}/tas_mbcn_daily_*.nc")):
        stem = Path(path).stem
        y0, y1 = map(int, stem.split("_")[-1].split("-"))
        if y1 < start_year or y0 > end_year:
            continue
        ds = xr.open_dataset(path, engine="netcdf4")
        for t in ds.time.values:
            d = str(t)[:10]
            if start_year <= int(d[:4]) <= end_year:
                dates.append(d)
        ds.close()
    return sorted(set(dates))


def prep_mbcn_date(date_str, setup):
    """Load MBCn for date_str ' (coarse (2,H,W), coarse_input tensor (1,4,H,W)). None on missing."""
    try:
        tas_ds = xr.open_dataset(find_mbcn_file("tas", date_str), engine="netcdf4")
        pr_ds  = xr.open_dataset(find_mbcn_file("pr",  date_str), engine="netcdf4")
    except FileNotFoundError:
        return None, None

    t_idx  = [str(t)[:10] for t in tas_ds.time.values].index(date_str)
    tas_np = tas_ds["tas"].isel(time=t_idx).values.astype(np.float32) - 273.15  # K -> degC
    pr_np  = pr_ds ["pr" ].isel(time=t_idx).values.astype(np.float32)
    tas_ds.close(); pr_ds.close()

    vi, ii, wi, fs = setup["interp"]
    def to_grid(arr):
        native = interpolate_to_cerra(arr, vi, ii, wi, fs)
        return F.interpolate(torch.from_numpy(native).unsqueeze(0).unsqueeze(0),
                             size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze().numpy()

    valid_mask = setup["valid_mask"]
    c_tas = to_grid(tas_np); c_pr = to_grid(pr_np)
    c_tas[~valid_mask] = np.nan; c_pr[~valid_mask] = np.nan
    coarse = np.stack([c_tas, c_pr])

    nrm, nrs = setup["norm_raw_mean"], setup["norm_raw_std"]
    stack = torch.stack([torch.from_numpy(c_tas).float(), torch.from_numpy(c_pr).float()])
    norm  = torch.nan_to_num((stack - nrm.view(2,1,1)) / nrs.view(2,1,1), nan=0.0)
    coarse_input = torch.cat([norm, setup["const_tensor"]], dim=0).unsqueeze(0).to(DEVICE)
    return coarse, coarse_input


# ── Example date plot ─────────────────────────────────────────────────────────

def _plot_example_date(setup, args):
    """2 rows (tas, pr) × 3 cols: native coarse | bilinear interpolated | downscaled."""
    if "network" not in setup:
        print("  Loading model for example date plot...")
        stats   = np.load("data/norm_stats.npz")
        network = load_model(args.checkpoint)

        static_path = "data/cerra_static_variables.nc"
        if os.path.exists(static_path):
            ds_c      = xr.open_dataset(static_path, engine="netcdf4").squeeze("valid_time")
            clat, clon = ds_c.latitude.values, ds_c.longitude.values
            in_r      = ((clat >= 42.2) & (clat <= 51.4) & (clon >= 2.4) & (clon <= 18.9))
            vr        = np.where(in_r.any(axis=1))[0]; vc = np.where(in_r.any(axis=0))[0]
            rs        = slice(int(vr[0]), int(vr[-1]) + 1); cs = slice(int(vc[0]), int(vc[-1]) + 1)
            crop_lat  = clat[rs, cs]
            const_list = []
            for vn in ["lsm", "orog"]:
                cv = ds_c[vn].values[rs, cs].astype(np.float32)
                if vn != "lsm":
                    w = np.cos(np.radians(crop_lat)); wsum = w.sum()
                    wmean = float((cv * w).sum() / wsum)
                    wstd  = float(np.sqrt(((cv - wmean)**2 * w).sum() / wsum))
                    cv    = (cv - wmean) / wstd
                const_list.append(torch.from_numpy(cv).float())
            const_tensor = F.interpolate(torch.stack(const_list).unsqueeze(0),
                                         size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze(0)
            ds_c.close()
        else:
            print("  WARNING: cerra_static_variables.nc not found — using zero constant fields")
            const_tensor = torch.zeros(2, *OUTPUT_SIZE)

        setup.update(dict(
            network=network, const_tensor=const_tensor,
            norm_raw_mean=torch.from_numpy(stats["norm_raw_mean"]),
            norm_raw_std =torch.from_numpy(stats["norm_raw_std"]),
            norm_res_mean=torch.from_numpy(stats["norm_res_mean"]),
            norm_res_std =torch.from_numpy(stats["norm_res_std"]),
        ))

    # prep_mbcn_date (used here and by _run_hottest_wettest) needs an MBCn -> fine-grid
    # interpolator too — load_setup() builds one, but the cache-only fast path in main()
    # skips load_setup() entirely, so it's missing whenever a cache hit short-circuits it.
    if "interp" not in setup:
        setup["interp"] = _build_interp(setup, cache_dir=getattr(args, "cache_dir", None))

    # Prefer a fixed example date; fall back to the first available date in range
    target_date = "2019-01-08"
    date_str = None
    try:
        find_mbcn_file("tas", target_date); find_mbcn_file("pr", target_date)
        date_str = target_date
    except FileNotFoundError:
        for d in get_period_dates(args.val_start, args.val_end):
            try:
                find_mbcn_file("tas", d); find_mbcn_file("pr", d)
                date_str = d; break
            except FileNotFoundError:
                continue
    if date_str is None:
        print("  No date available for example date plot")
        return
    print(f"  Example date: {date_str}")

    # Native coarse
    tas_ds     = xr.open_dataset(find_mbcn_file("tas", date_str), engine="netcdf4")
    pr_ds      = xr.open_dataset(find_mbcn_file("pr",  date_str), engine="netcdf4")
    t_idx      = [str(t)[:10] for t in tas_ds.time.values].index(date_str)
    tas_native = tas_ds["tas"].isel(time=t_idx).values.astype(np.float32) - 273.15
    pr_native  = pr_ds["pr"].isel(time=t_idx).values.astype(np.float32)
    native_lat = tas_ds["lat"].values
    native_lon = tas_ds["lon"].values
    tas_ds.close(); pr_ds.close()

    # Interpolate MBCn → OUTPUT_SIZE using scipy griddata (no CERRA reference needed)
    valid_mask = setup["valid_mask"]
    fine_lat   = setup["fine_lat"]
    fine_lon   = setup["fine_lon"]
    mbcn_pts   = np.column_stack([native_lat.ravel(), native_lon.ravel()])
    fine_pts   = np.column_stack([fine_lat.ravel(),   fine_lon.ravel()])

    # Clip the native coarse field to the reanalysis (CERRA) domain for display
    native_valid = scipy_griddata(fine_pts, valid_mask.ravel().astype(np.float32),
                                  mbcn_pts, method="nearest").reshape(native_lat.shape) > 0.5
    tas_native_clipped = np.where(native_valid, tas_native, np.nan)
    pr_native_clipped  = np.where(native_valid, pr_native,  np.nan)

    def interp_to_fine(arr):
        out = scipy_griddata(mbcn_pts, arr.ravel(), fine_pts, method="linear")
        out = out.reshape(OUTPUT_SIZE).astype(np.float32)
        out[~valid_mask] = np.nan
        return out

    c_tas  = interp_to_fine(tas_native)
    c_pr   = interp_to_fine(pr_native)
    coarse = np.stack([c_tas, c_pr])

    nrm, nrs = setup["norm_raw_mean"], setup["norm_raw_std"]
    stack  = torch.stack([torch.from_numpy(c_tas).float(), torch.from_numpy(c_pr).float()])
    norm   = torch.nan_to_num((stack - nrm.view(2,1,1)) / nrs.view(2,1,1), nan=0.0)
    coarse_input = torch.cat([norm, setup["const_tensor"]], dim=0).unsqueeze(0).to(DEVICE)

    downscaled = run_ensemble(setup["network"], coarse_input, coarse,
                              setup["norm_res_mean"], setup["norm_res_std"],
                              setup["valid_mask"], n=1,
                              labels=doy_hour_label(date_str))[0]  # (2, H, W)

    valid_mask = setup["valid_mask"]
    extent = [float(setup["fine_lon"].min()), float(setup["fine_lon"].max()),
              float(setup["fine_lat"].min()), float(setup["fine_lat"].max())]
    crs = ccrs.PlateCarree()

    # True width:height of the domain (see _plot_delta in ProjectionPlots.py for the same
    # fix) — without pinning each panel's box to it, GeoAxes pads whichever dimension is
    # short to preserve the geographic aspect ratio, which is what was showing up as a
    # wide gap between the tas and pr rows.
    panel_aspect = ((extent[1] - extent[0]) * np.cos(np.radians((extent[2] + extent[3]) / 2))
                     / (extent[3] - extent[2]))

    fig, axs = plt.subplots(2, 3, figsize=(15, 8), constrained_layout=True,
                            subplot_kw={"projection": crs})
    fig.patch.set_facecolor("white")

    field_titles = ["Native Coarse Field", "Bilinear Interpolated Field", "Downscaled Ensemble Mean"]
    for v, (vname, cmap, unit, row_name, native_field) in enumerate([
        ("tas", "RdBu_r", "°C",     "Temperature",  tas_native_clipped),
        ("pr",  "Blues",  "mm/day", "Precipitation", pr_native_clipped),
    ]):
        bilin = np.where(valid_mask, coarse[v],      np.nan)
        down  = np.where(valid_mask, downscaled[v],  np.nan)
        vmin  = min(float(np.nanpercentile(bilin, 2)),  float(np.nanpercentile(down, 2)))
        vmax  = max(float(np.nanpercentile(bilin, 98)), float(np.nanpercentile(down, 98)))
        cm = plt.get_cmap(cmap).copy(); cm.set_bad("lightgrey")

        for col, (data, lat, lon) in enumerate([
            (native_field, native_lat, native_lon),
            (bilin,        setup["fine_lat"], setup["fine_lon"]),
            (down,         setup["fine_lat"], setup["fine_lon"]),
        ]):
            ax = axs[v, col]
            im = ax.pcolormesh(lon, lat, data, vmin=vmin, vmax=vmax,
                               cmap=cm, transform=crs, shading="nearest", rasterized=True)
            ax.set_extent(extent, crs=crs)
            ax.set_box_aspect(1 / panel_aspect)
            ax.set_aspect("auto")
            ax.coastlines(); ax.set_facecolor("lightgrey")
            ax.set_xticks([]); ax.set_yticks([])
            ax.set_title(f"{row_name} - {field_titles[col]}", fontsize=9)
            if col == 2:
                # All three panels in a row share vmin/vmax/cmap, so one colorbar (on the
                # rightmost panel) covers the row instead of repeating it three times —
                # same layout as HottestWettestDayInference._plot_comparison.
                cbar = fig.colorbar(im, ax=ax, shrink=0.7, pad=0.03, label=unit)
                cbar.ax.tick_params(labelsize=7)

    out = os.path.abspath(f"{OUT_DIR}/1_example_date.svg")
    fig.savefig(out, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  Saved {out}")
    wandb.log({"example_date/date": date_str})


# ── Parts 2 & 3: Period accumulation ──────────────────────────────────────────

def _run_or_load_predictions(dates, setup, n_clim, cache_dir, slug, desc):
    """Run model inference for all dates, or load from a prediction cache.

    Saves/loads pred_{slug}.npz containing:
      dates     (N,)         — date strings
      ens_means (N, 2, H, W) — ensemble-mean downscaled fields
      coarses   (N, 2, H, W) — coarse MBCn fields
      spread_tas/spread_pr (N,) — per-date mean spread (only when n_clim > 1)

    Returns (valid_dates, ens_means, coarses, spread_records) or (None,...) if no data.
    """
    pred_path = os.path.join(cache_dir, f"pred_{slug}.npz") if cache_dir else None

    if pred_path and os.path.exists(pred_path):
        print(f"  Loading cached predictions from {pred_path}")
        d = np.load(pred_path, allow_pickle=True)
        valid_dates = d["dates"].tolist()
        ens_means   = d["ens_means"]   # (N, 2, H, W)
        coarses     = d["coarses"]     # (N, 2, H, W)
        if "spread_tas" in d and len(d["spread_tas"]) > 0:
            spread_records = list(zip(valid_dates,
                                      d["spread_tas"].tolist(),
                                      d["spread_pr"].tolist()))
        else:
            spread_records = []
        return valid_dates, ens_means, coarses, spread_records

    # --- run model ---
    valid_dates    = []
    ens_means_list = []
    coarses_list   = []
    spread_records = []
    valid_mask = setup["valid_mask"]

    for date_str in tqdm(dates, desc=desc):
        coarse, coarse_input = prep_mbcn_date(date_str, setup)
        if coarse is None:
            continue
        samples  = run_ensemble(setup["network"], coarse_input, coarse,
                                setup["norm_res_mean"], setup["norm_res_std"],
                                setup["valid_mask"], n_clim,
                                labels=doy_hour_label(date_str))
        ens_mean = np.nanmean(samples, axis=0)
        valid_dates.append(date_str)
        ens_means_list.append(ens_mean.astype(np.float32))
        coarses_list.append(coarse.astype(np.float32))
        if n_clim > 1:
            spread = np.nanstd(samples, axis=0)
            spread_records.append((
                date_str,
                float(np.nanmean(spread[0][valid_mask])),
                float(np.nanmean(spread[1][valid_mask])),
            ))

    if not valid_dates:
        return None, None, None, None

    ens_means = np.stack(ens_means_list)  # (N, 2, H, W)
    coarses   = np.stack(coarses_list)    # (N, 2, H, W)

    if pred_path:
        os.makedirs(cache_dir, exist_ok=True)
        save_kw = dict(dates=np.array(valid_dates),
                       ens_means=ens_means, coarses=coarses)
        if spread_records:
            save_kw["spread_tas"] = np.array([r[1] for r in spread_records], dtype=np.float32)
            save_kw["spread_pr"]  = np.array([r[2] for r in spread_records], dtype=np.float32)
        np.savez_compressed(pred_path, **save_kw)
        print(f"  Saved {len(valid_dates)} predictions -> {pred_path}")

    return valid_dates, ens_means, coarses, spread_records


def accumulate_period(dates, setup, n_clim, desc, log_prefix="", cache_dir=None, slug=""):
    """Load or run predictions, then aggregate into period statistics."""
    valid_dates, ens_means, coarses, spread_records = _run_or_load_predictions(
        dates, setup, n_clim, cache_dir, slug, desc)

    if valid_dates is None:
        return None

    valid_mask = setup["valid_mask"]
    vm = valid_mask[np.newaxis, np.newaxis]          # (1, 1, H, W) for broadcasting

    masked_means   = np.where(vm, ens_means, np.nan) # (N, 2, H, W)
    masked_coarses = np.where(vm, coarses,   np.nan) # (N, 2, H, W)

    mean_down   = np.nanmean(masked_means,   axis=0)  # (2, H, W)
    mean_coarse = np.nanmean(masked_coarses, axis=0)
    down_p99   = np.nanpercentile(masked_means,   99, axis=0)
    coarse_p99 = np.nanpercentile(masked_coarses, 99, axis=0)

    # Explicitly zero out anything outside valid_mask — nanpercentile/nanmean on all-NaN
    # columns can behave inconsistently across numpy versions
    for arr in (mean_down, mean_coarse, down_p99, coarse_p99):
        arr[:, ~valid_mask] = np.nan

    pixel_vals_down   = [ens_means[:, v][:, valid_mask].flatten().tolist() for v in range(2)]
    pixel_vals_coarse = [coarses[:, v][:, valid_mask].flatten().tolist()   for v in range(2)]

    if log_prefix:
        wandb.log({
            f"{log_prefix}/progress_mean_tas": float(np.nanmean(mean_down[0][valid_mask])),
            f"{log_prefix}/progress_mean_pr":  float(np.nanmean(mean_down[1][valid_mask])),
            f"{log_prefix}/progress_n_done":   len(valid_dates),
        })

    return (mean_down, mean_coarse, pixel_vals_down, pixel_vals_coarse,
            down_p99, coarse_p99, spread_records)


def save_period_result(result, path):
    """Save an accumulate_period result tuple to a .npz file."""
    mean_down, mean_coarse, pixel_vals_down, pixel_vals_coarse, down_p99, coarse_p99, spread_records = result
    spread_dates = np.array([r[0] for r in spread_records])
    spread_tas   = np.array([r[1] for r in spread_records], dtype=np.float32)
    spread_pr    = np.array([r[2] for r in spread_records], dtype=np.float32)
    np.savez(path,
             mean_down=mean_down,
             mean_coarse=mean_coarse,
             pixel_vals_down_0=np.array(pixel_vals_down[0], dtype=np.float32),
             pixel_vals_down_1=np.array(pixel_vals_down[1], dtype=np.float32),
             pixel_vals_coarse_0=np.array(pixel_vals_coarse[0], dtype=np.float32),
             pixel_vals_coarse_1=np.array(pixel_vals_coarse[1], dtype=np.float32),
             down_p99=down_p99,
             coarse_p99=coarse_p99,
             spread_dates=spread_dates,
             spread_tas=spread_tas,
             spread_pr=spread_pr)
    print(f"  Cached period result -> {path}")


def load_period_result(path):
    """Load a .npz file saved by save_period_result back into the result tuple."""
    d = np.load(path, allow_pickle=True)
    pixel_vals_down   = [d["pixel_vals_down_0"],   d["pixel_vals_down_1"]]
    pixel_vals_coarse = [d["pixel_vals_coarse_0"], d["pixel_vals_coarse_1"]]
    spread_records = list(zip(
        d["spread_dates"].tolist(),
        d["spread_tas"].tolist(),
        d["spread_pr"].tolist(),
    ))
    down_p99   = d["down_p99"]   if "down_p99"   in d else d["down_max"]
    coarse_p99 = d["coarse_p99"] if "coarse_p99" in d else d["coarse_max"]
    return (d["mean_down"], d["mean_coarse"],
            pixel_vals_down, pixel_vals_coarse,
            down_p99, coarse_p99,
            spread_records)


def _load_or_accumulate(cache_dir, label, dates, setup, n_clim, desc, log_prefix):
    """Run accumulate_period, using a cache file if available."""
    cache_path = os.path.join(cache_dir, f"{label}.npz") if cache_dir else None
    if cache_path and os.path.exists(cache_path):
        print(f"  Loading cached {desc} from {cache_path}")
        return load_period_result(cache_path)
    result = accumulate_period(dates, setup, n_clim, desc, log_prefix,
                               cache_dir=cache_dir, slug=label)
    if cache_path and result is not None:
        os.makedirs(cache_dir, exist_ok=True)
        save_period_result(result, cache_path)
    return result


def load_cerra_coarse_pixel_vals(dates, cache_dir=None, slug=""):
    """Load coarsened CERRA pixel values (native GCM resolution, no interpolation)."""
    cache_path = os.path.join(cache_dir, f"cerra_coarse_{slug}.npz") if cache_dir else None
    if cache_path and os.path.exists(cache_path):
        print(f"  Loading cached coarse CERRA pixel vals from {cache_path}")
        d = np.load(cache_path)
        return [d["tas"], d["pr"]]

    if not os.path.exists(REANALYSIS_COARSE):
        print("  Skipping coarse CERRA load — reanalysis_coarsened directory not found")
        return None

    rng_c = np.random.default_rng(0)
    pixels_per_day = 2000
    pixel_vals = [[], []]
    for date_str in tqdm(dates, desc="Loading coarse CERRA"):
        y, m = int(date_str[:4]), int(date_str[5:7])
        arrays = []
        for var in ["tas", "pr"]:
            path = os.path.join(REANALYSIS_COARSE,
                                f"CERRA_{var}_daily_{y}_{m:02d}_coarsened.nc")
            if not os.path.exists(path):
                arrays = None; break
            ds = xr.open_dataset(path, engine="netcdf4")
            times = [str(t)[:10] for t in ds.time.values]
            if date_str not in times:
                ds.close(); arrays = None; break
            arr = ds[var].isel(time=times.index(date_str)).values.astype(np.float32)
            ds.close()
            arrays.append(arr)
        if arrays is None:
            continue
        finite_mask = np.isfinite(arrays[0]) & np.isfinite(arrays[1])
        idx = np.where(finite_mask.ravel())[0]
        if len(idx) > pixels_per_day:
            idx = rng_c.choice(idx, size=pixels_per_day, replace=False)
        for v, arr in enumerate(arrays):
            pixel_vals[v].extend(arr.ravel()[idx].tolist())

    if cache_path and any(len(pixel_vals[v]) > 0 for v in range(2)):
        os.makedirs(cache_dir, exist_ok=True)
        np.savez_compressed(cache_path,
                            tas=np.array(pixel_vals[0], dtype=np.float32),
                            pr=np.array(pixel_vals[1], dtype=np.float32))
        print(f"  Saved coarse CERRA pixel vals -> {cache_path}")

    return [np.array(pixel_vals[0], dtype=np.float32),
            np.array(pixel_vals[1], dtype=np.float32)]


def load_mbcn_native_pixel_vals(dates, cache_dir=None, slug="", fine_lat=None, fine_lon=None, valid_mask=None):
    """Load MBCn pixel values at native 77×91 resolution for the given dates, restricted
    to the true CERRA valid domain (via nearest-neighbor lookup into valid_mask — the same
    method valid_mask itself was built with, so domain membership agrees with the coarse/
    interpolated side pixel-for-pixel) when fine_lat/fine_lon/valid_mask are given;
    falls back to a coarse lat/lon bounding box otherwise.
    """
    cache_path = os.path.join(cache_dir, f"mbcn_native_v2_{slug}.npz") if cache_dir else None
    if cache_path and os.path.exists(cache_path):
        print(f"  Loading cached native MBCn pixel vals from {cache_path}")
        d = np.load(cache_path)
        return [d["tas"], d["pr"]]

    fine_pts = None
    if fine_lat is not None and fine_lon is not None and valid_mask is not None:
        fine_pts = np.column_stack([fine_lat.ravel(), fine_lon.ravel()])

    LAT_MIN, LAT_MAX = 42.2, 51.4
    LON_MIN, LON_MAX =  2.4, 18.9

    rng_m = np.random.default_rng(1)
    pixels_per_day = 2000
    pixel_vals = [[], []]
    domain_mask = None  # built once from first file
    for date_str in tqdm(dates, desc="Loading native MBCn"):
        arrays = []
        for var in ["tas", "pr"]:
            try:
                path = find_mbcn_file(var, date_str)
            except FileNotFoundError:
                arrays = None; break
            ds = xr.open_dataset(path, engine="netcdf4")
            times = [str(t)[:10] for t in ds.time.values]
            if date_str not in times:
                ds.close(); arrays = None; break
            arr = ds[var].isel(time=times.index(date_str)).values.astype(np.float32)
            if domain_mask is None:
                lat = (ds["lat"].values if "lat" in ds else ds["latitude"].values)
                lon = (ds["lon"].values if "lon" in ds else ds["longitude"].values)
                if lat.ndim == 1 and lon.ndim == 1:
                    lon, lat = np.meshgrid(lon, lat)
                if fine_pts is not None:
                    native_pts  = np.column_stack([lat.ravel(), lon.ravel()])
                    domain_mask = (scipy_griddata(fine_pts, valid_mask.ravel().astype(np.float32),
                                                  native_pts, method="nearest") > 0.5).reshape(lat.shape)
                else:
                    domain_mask = ((lat >= LAT_MIN) & (lat <= LAT_MAX) &
                                   (lon >= LON_MIN) & (lon <= LON_MAX))
            ds.close()
            arrays.append(arr)
        if arrays is None:
            continue
        finite_mask = np.isfinite(arrays[0]) & np.isfinite(arrays[1]) & domain_mask
        idx = np.where(finite_mask.ravel())[0]
        if len(idx) > pixels_per_day:
            idx = rng_m.choice(idx, size=pixels_per_day, replace=False)
        for v, arr in enumerate(arrays):
            pixel_vals[v].extend(arr.ravel()[idx].tolist())

    tas_arr = np.array(pixel_vals[0], dtype=np.float32) - 273.15  # K → °C
    pr_arr  = np.array(pixel_vals[1], dtype=np.float32)
    result  = [tas_arr, pr_arr]
    if cache_path and any(len(r) > 0 for r in result):
        os.makedirs(cache_dir, exist_ok=True)
        np.savez_compressed(cache_path, tas=result[0], pr=result[1])
        print(f"  Saved native MBCn pixel vals -> {cache_path}")
    return result


def run_climate(setup, args):
    print(f"\n── Parts 2 & 3: Climatology (n_clim={args.n_clim}) ───────────────────")
    s, n = args.stride, args.n_clim
    cache = getattr(args, "cache_dir", None)
    # Only the sampler-output caches carry the tag; the pixel-value slugs below stay
    # plain, since native MBCn / coarse CERRA values don't depend on the model.
    tag = cache_tag(args.checkpoint)

    gwl_results = []
    for label, start, end in GWL_PERIODS:
        dates = get_period_dates(start, end)[::s]
        print(f"  {label} ({start}-{end}): {len(dates)} dates")
        slug = _cache_slug(label)
        result = _load_or_accumulate(cache, f"{slug}_{start}_{end}_s{s}_n{n}_{tag}",
                                     dates, setup, n, f"{label} ({start}-{end})", label)
        gwl_results.append((label, start, end, result))

    if all(r is None for _, _, _, r in gwl_results):
        print("  No MBCn data found for any GWL period — check year ranges.")
        return

    # Load CERRA pixel values for the historical (first) GWL period for distribution comparison
    hist_label, hist_start, hist_end, _ = gwl_results[0]
    hist_slug = f"{_cache_slug(hist_label)}_{hist_start}_{hist_end}_s{s}_n{n}"
    hist_dates = get_period_dates(hist_start, hist_end)[::s]
    cerra_coarse_pixel_vals = load_cerra_coarse_pixel_vals(hist_dates,
                                                           cache_dir=cache, slug=hist_slug)

    mbcn_native_per_gwl = []
    for label, start, end, _ in gwl_results:
        slug_gwl = f"{_cache_slug(label)}_{start}_{end}_s{s}_n{n}"
        dates_gwl = get_period_dates(start, end)[::s]
        mbcn_native_per_gwl.append(
            load_mbcn_native_pixel_vals(dates_gwl, cache_dir=cache, slug=slug_gwl,
                                        fine_lat=setup["fine_lat"], fine_lon=setup["fine_lon"],
                                        valid_mask=setup["valid_mask"]))

    show_native_coarse = getattr(args, "show_native_coarse", False)

    _plot_delta(gwl_results, setup)
    _plot_period_means(gwl_results, setup, mbcn_native_per_gwl, show_native_coarse)
    _plot_distribution(gwl_results, cerra_coarse_pixel_vals, mbcn_native_per_gwl)
    _plot_quantile_delta(gwl_results)
    _plot_spread_persistence(gwl_results)


def _run_hottest_wettest(setup, args):
    """Run the standalone hottest/wettest-day comparison (see HottestWettestDayInference.py)
    against this run's already-loaded setup, saving into the projection output dir as plot 7
    instead of that module's own output directory. Kept as a thin wrapper around its existing
    logic rather than merged into it — a deliberately separate, self-contained step.
    """
    # Deferred import: HottestWettestDayInference imports load_setup/prep_mbcn_date from this
    # module, so importing it back at module load time here would be circular.
    from inference.HottestWettestDayInference import (
        find_extreme_dates, _load_native_mbcn, _plot_comparison,
        PROJECTION_START_YEAR, PROJECTION_END_YEAR)

    if "interp" not in setup:
        # Only the full load_setup() path builds this; the cache-hit fast path
        # (_load_grid) and _plot_example_date's own patch-in both skip it, since
        # neither previously needed prep_mbcn_date's coarse->fine interpolation.
        print("  Building coarse->fine interpolator...")
        cerra_ref   = xr.open_dataset(CERRA_REF, engine="netcdf4")
        mbcn_sample = xr.open_dataset(sorted(glob(f"{DATADIR_PROJECTION}/tas_mbcn_daily_*.nc"))[0], engine="netcdf4")
        setup["interp"] = build_interpolator(mbcn_sample, cerra_ref)
        mbcn_sample.close(); cerra_ref.close()

    print("\n── Plot 7: Hottest & wettest day ──────────────────────────────────")
    (hot_date, hot_mean_tas), (wet_date, wet_mean_pr) = find_extreme_dates(
        PROJECTION_START_YEAR, PROJECTION_END_YEAR)
    print(f"  Highest mean temperature day: {hot_date}  (domain-mean coarse tas = {hot_mean_tas:.2f} °C)")
    print(f"  Highest precipitation mass day: {wet_date}  (domain-mean coarse pr  = {wet_mean_pr:.2f} mm/day)")

    for date_str, day_kind in [(hot_date, "Highest mean temperature day"),
                               (wet_date, "Highest precipitation mass day")]:
        coarse, coarse_input = prep_mbcn_date(date_str, setup)
        if coarse is None:
            print(f"  WARNING: {date_str} not found in MBCn data — skipping")
            continue
        samples = run_ensemble(setup["network"], coarse_input, coarse,
                               setup["norm_res_mean"], setup["norm_res_std"],
                               setup["valid_mask"], args.n_clim,
                               labels=doy_hour_label(date_str))
        downscaled = np.nanmean(samples, axis=0)
        native_tas, native_pr, native_lat, native_lon = _load_native_mbcn(date_str)
        _plot_comparison(date_str, day_kind, coarse, downscaled, native_tas, native_pr,
                         native_lat, native_lon, setup, args.n_clim, OUT_DIR,
                         filename_prefix="7_")


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--checkpoint",  default="./model/20.pt")
    parser.add_argument("--n-clim", type=int, default=1,
                        help="ensemble members for Parts 2&3 climatology (default 1)")
    parser.add_argument("--stride", type=int, default=15,
                        help="use every Nth date (default 15)")
    parser.add_argument("--val-start", type=int, default=2000,
                        help="start-of-range year for the example-date fallback search (needs CERRA)")
    parser.add_argument("--val-end",   type=int, default=2020,
                        help="end-of-range year for the example-date fallback search (needs CERRA)")
    parser.add_argument("--skip-validation", action="store_true",
                        help="no-op, kept for CLI compatibility with CalibrationPeriodAssessment")
    parser.add_argument("--cache-dir", default=None,
                        help="directory to cache/load period results (skips sampling if cache exists)")
    parser.add_argument("--show-native-coarse", action="store_true",
                        help="use the native-resolution (non-interpolated) MBCn coarse field as "
                             "plot 3's reference bar, instead of the bilinear-interpolated one")
    args = parser.parse_args()

    os.makedirs(OUT_DIR, exist_ok=True)

    wandb.init(
        project="climate-diffusion-downscaling",
        name="mbcn-assessment",
        config=dict(checkpoint=args.checkpoint, n_clim=args.n_clim,
                    stride=args.stride,
                    val=f"{args.val_start}-{args.val_end}",
                    gwl_periods={label: f"{s}-{e}" for label, s, e in GWL_PERIODS}))

    if _all_caches_present(args):
        print("All prediction caches found — skipping model load.")
        setup = _load_grid(cache_dir=args.cache_dir)
    else:
        print("Loading model and setup...")
        setup = load_setup(args.checkpoint, cache_dir=args.cache_dir)

    _plot_example_date(setup, args)

    run_climate(setup, args)
    _run_hottest_wettest(setup, args)
    wandb.finish()


if __name__ == "__main__":
    main()
