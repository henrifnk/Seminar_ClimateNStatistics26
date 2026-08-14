"""
Diffusion downscaling inference for a single date, from either coarse data source.

    cerra — coarsened CERRA reanalysis. Fine reanalysis exists for the same date, so the
            figure gets a ground-truth column: coarse | predicted | target.
    mbcn  — bias-corrected (MBCn) climate model output. No ground truth exists, so the
            figure shows the model's own grid instead: raw | interpolated | downscaled.

Both sources run the identical pipeline — interpolate onto the CERRA grid, normalize,
concatenate the static fields, sample the residual, add it back onto the coarse field —
so only the loading step and the figure's first/last column differ.

This module also holds the regridding helpers (build_interpolator, interpolate_to_cerra,
find_mbcn_file), the data paths and the normalization stats that the ensemble, evaluation
and diagnostic scripts import.

Usage:
    uv run python src/inference/Inference.py cerra 2019-01-05 [--borders]
    uv run python src/inference/Inference.py mbcn  2091-07-15 [--borders]
"""
import argparse
import os
import sys
from datetime import datetime
from glob import glob
from pathlib import Path

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.pyplot as plt
import numpy as np
import torch
import xarray as xr
from scipy.spatial import Delaunay

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

import wm2024model
from DatasetAL import doy_hour_label
from grids import OUTPUT_SIZE, compute_cerra_grid
from paths import plot_dir
from sampling import sample_once

DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'

# ── Configuration ──────────────────────────────────────────────────────────────
CHECKPOINT     = "./model/20.pt"
DATADIR_PROJECTION = "data/projection"
DATADIR_FINE   = "data/reanalysis"
DATADIR_COARSE = "data/reanalysis_coarsened"
CERRA_REF      = "data/reanalysis/tas/CERRA_tas_daily_2006_01.nc"
STATIC_FILE    = "data/cerra_static_variables.nc"

# Normalization stats — loaded from data/norm_stats.npz (run ComputeNormStats.py first)
_NORM_FILE = "data/norm_stats.npz"
if os.path.exists(_NORM_FILE):
    _stats = np.load(_NORM_FILE)
    NORM_RAW_MEAN = torch.from_numpy(_stats["norm_raw_mean"])
    NORM_RAW_STD  = torch.from_numpy(_stats["norm_raw_std"])
    NORM_RES_MEAN = torch.from_numpy(_stats["norm_res_mean"])
    NORM_RES_STD  = torch.from_numpy(_stats["norm_res_std"])
else:
    raise FileNotFoundError(
        f"Normalization stats not found at {_NORM_FILE}. "
        "Run: uv run python src/training/ComputeNormStats.py")
# ──────────────────────────────────────────────────────────────────────────────


# ── regridding helpers (imported by the ensemble/evaluation/diagnostic scripts) ─

def build_interpolator(coarse_ds, cerra_ds):
    coarse_lat = coarse_ds["lat"].values
    coarse_lon = coarse_ds["lon"].values
    fine_lat   = cerra_ds["lat"].values
    fine_lon   = cerra_ds["lon"].values

    coarse_pts = np.column_stack([coarse_lat.ravel(), coarse_lon.ravel()])
    fine_pts   = np.column_stack([fine_lat.ravel(),   fine_lon.ravel()])

    tri     = Delaunay(coarse_pts)
    simplex = tri.find_simplex(fine_pts)
    valid   = simplex >= 0

    T = tri.transform[simplex[valid]]
    b = np.einsum('...ij,...j->...i', T[:, :2, :2], fine_pts[valid] - T[:, 2, :])
    weights = np.c_[b, 1 - b.sum(axis=-1)]
    idx     = tri.simplices[simplex[valid]]

    return valid, idx, weights, fine_lat.shape


def interpolate_to_cerra(coarse_np, valid, idx, weights, fine_shape):
    result = np.full(valid.shape[0], np.nan, dtype=np.float32)
    result[valid] = np.einsum('ni,ni->n', coarse_np.ravel()[idx], weights)
    return result.reshape(fine_shape)


def find_mbcn_file(var, date_str):
    """Find the daily MBCn file containing date_str by parsing year ranges from filenames."""
    year = int(date_str[:4])
    for path in sorted(glob(f"{DATADIR_PROJECTION}/{var}_mbcn_daily_*.nc")):
        stem = Path(path).stem             # e.g. tas_mbcn_daily_2091-2095
        years_part = stem.split("_")[-1]   # e.g. 2091-2095
        start, end = map(int, years_part.split("-"))
        if start <= year <= end:
            return path
    raise FileNotFoundError(f"No MBCn {var} daily file found for {date_str} in {DATADIR_PROJECTION}")


def to_output_grid(arr, valid_mask):
    """Resample a field on the CERRA native grid down to OUTPUT_SIZE, NaN outside the domain."""
    out = torch.nn.functional.interpolate(
        torch.from_numpy(arr).float().unsqueeze(0).unsqueeze(0),
        size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze().numpy()
    out[~valid_mask] = np.nan
    return out


# ── coarse-field loaders — the only part that differs between the two sources ──

def load_cerra(date_str, cerra_ds, valid_mask):
    """Coarsened CERRA reanalysis for one date, plus the fine reanalysis as ground truth."""
    date = datetime.strptime(date_str, "%Y-%m-%d")
    year, month = date.year, date.month
    cp_tas = Path(DATADIR_COARSE) / f"CERRA_tas_daily_{year}_{month:02d}_coarsened.nc"
    cp_pr  = Path(DATADIR_COARSE) / f"CERRA_pr_daily_{year}_{month:02d}_coarsened.nc"
    fp_tas = Path(DATADIR_FINE) / "tas" / f"CERRA_tas_daily_{year}_{month:02d}.nc"
    fp_pr  = Path(DATADIR_FINE) / "pr"  / f"CERRA_pr_daily_{year}_{month:02d}.nc"
    for p in [cp_tas, cp_pr, fp_tas, fp_pr]:
        if not p.exists():
            raise FileNotFoundError(f"Missing: {p}")

    ds_ct = xr.open_dataset(cp_tas, engine="netcdf4")
    ds_cp = xr.open_dataset(cp_pr,  engine="netcdf4")
    dates = [str(t)[:10] for t in ds_ct.time.values]
    if date_str not in dates:
        raise ValueError(f"{date_str} not found. Available: {dates[0]} – {dates[-1]}")
    t = dates.index(date_str)

    native_tas = ds_ct["tas"].isel(time=t).values.astype(np.float32)
    native_pr  = ds_cp["pr"].isel(time=t).values.astype(np.float32)
    native_lat = ds_ct["lat"].values
    native_lon = ds_ct["lon"].values

    ds_ft = xr.open_dataset(fp_tas, engine="netcdf4")
    ds_fp = xr.open_dataset(fp_pr,  engine="netcdf4")
    fine_dates_t = [str(x)[:10] for x in ds_ft.time.values]
    fine_dates_p = [str(x)[:10] for x in ds_fp.time.values]
    fine_tas = ds_ft["tas"].isel(time=fine_dates_t.index(date_str)).values.astype(np.float32)
    fine_pr  = ds_fp["pr"].isel(time=fine_dates_p.index(date_str)).values.astype(np.float32)
    ds_ft.close(); ds_fp.close()

    print("Building geographic interpolator...")
    valid, idx, weights, fine_shape = build_interpolator(ds_ct, cerra_ds)
    ds_ct.close(); ds_cp.close()

    def prep(arr):
        return to_output_grid(interpolate_to_cerra(arr, valid, idx, weights, fine_shape), valid_mask)

    return {
        "coarse_tas": prep(native_tas), "coarse_pr": prep(native_pr),
        "native_tas": native_tas,       "native_pr": native_pr,
        "native_lat": native_lat,       "native_lon": native_lon,
        "truth_tas": to_output_grid(fine_tas, valid_mask),
        "truth_pr":  to_output_grid(fine_pr,  valid_mask),
    }


def load_mbcn(date_str, cerra_ds, valid_mask):
    """Bias-corrected climate model output for one date. No ground truth exists."""
    tas_file = find_mbcn_file("tas", date_str)
    pr_file  = find_mbcn_file("pr",  date_str)

    mbcn_tas = xr.open_dataset(tas_file, engine="netcdf4")
    mbcn_pr  = xr.open_dataset(pr_file,  engine="netcdf4")
    dates = [str(t)[:10] for t in mbcn_tas.time.values]
    if date_str not in dates:
        raise ValueError(f"{date_str} not found in MBCn file. Available: {dates[0]} – {dates[-1]}")
    t = dates.index(date_str)

    native_tas = mbcn_tas["tas"].isel(time=t).values.astype(np.float32) - 273.15  # K → °C
    native_pr  = mbcn_pr["pr"].isel(time=t).values.astype(np.float32)
    native_lat = mbcn_tas["lat"].values
    native_lon = mbcn_tas["lon"].values

    print("Building geographic interpolator...")
    valid, idx, weights, fine_shape = build_interpolator(mbcn_tas, cerra_ds)
    mbcn_tas.close(); mbcn_pr.close()

    def prep(arr):
        return to_output_grid(interpolate_to_cerra(arr, valid, idx, weights, fine_shape), valid_mask)

    return {
        "coarse_tas": prep(native_tas), "coarse_pr": prep(native_pr),
        "native_tas": native_tas,       "native_pr": native_pr,
        "native_lat": native_lat,       "native_lon": native_lon,
        "truth_tas": None,              "truth_pr": None,
    }


LOADERS = {"cerra": load_cerra, "mbcn": load_mbcn}


# ── shared pipeline ───────────────────────────────────────────────────────────

def load_static_constants():
    """Land-sea mask + orography, cropped to the Alpine Space and resampled to OUTPUT_SIZE.

    Returns a (2, H, W) tensor; orography is standardized with cos(latitude) area weights,
    the land-sea mask is left as-is (already 0-1).
    """
    ds_const = xr.open_dataset(STATIC_FILE, engine="netcdf4").squeeze("valid_time")
    const_lat = ds_const.latitude.values
    const_lon = ds_const.longitude.values
    in_region = ((const_lat >= 42.2) & (const_lat <= 51.4) &
                 (const_lon >= 2.4)  & (const_lon <= 18.9))
    valid_rows = np.where(in_region.any(axis=1))[0]
    valid_cols = np.where(in_region.any(axis=0))[0]
    row_s = slice(int(valid_rows[0]), int(valid_rows[-1]) + 1)
    col_s = slice(int(valid_cols[0]), int(valid_cols[-1]) + 1)
    crop_lat = const_lat[row_s, col_s]

    const_list = []
    for varname in ["lsm", "orog"]:
        cv_np = ds_const[varname].values[row_s, col_s].astype(np.float32)
        if varname != "lsm":
            w = np.cos(np.radians(crop_lat))
            wmean = float((cv_np * w).sum() / w.sum())
            wstd  = float(np.sqrt(((cv_np - wmean) ** 2 * w).sum() / w.sum()))
            cv_np = (cv_np - wmean) / wstd
        const_list.append(torch.from_numpy(cv_np).float())
    ds_const.close()

    return torch.nn.functional.interpolate(
        torch.stack(const_list, dim=0).unsqueeze(0),
        size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze(0)


def predict_residual(coarse_tas, coarse_pr, valid_mask, checkpoint=CHECKPOINT, labels=None):
    """Sample one residual field (2, H, W) in physical units, zeroed outside the domain.

    labels is the (1, 2) day-of-year/hour tensor for the date being downscaled; see
    sample_once() for the conditioning behaviour when it is omitted.
    """
    print("Loading constant variables...")
    const_tensor = load_static_constants()

    coarse_stack = torch.stack([torch.from_numpy(coarse_tas).float(),
                                torch.from_numpy(coarse_pr).float()], dim=0)
    coarse_norm = torch.nan_to_num(
        (coarse_stack - NORM_RAW_MEAN.view(2, 1, 1)) / NORM_RAW_STD.view(2, 1, 1), nan=0.0)
    coarse_input = torch.cat([coarse_norm, const_tensor], dim=0).unsqueeze(0).to(DEVICE)

    print(f"Loading model from {checkpoint} ...")
    network = wm2024model.EDMPrecond((256, 320), 6, 2, label_dim=2, model_channels=64)
    network.load_state_dict(torch.load(checkpoint, map_location=DEVICE))
    network.to(DEVICE)
    network.eval()

    print("Running diffusion sampling...")
    residual_norm = sample_once(network, coarse_input, labels=labels,
                                progress=True).squeeze().cpu().float()
    residual = (residual_norm * NORM_RES_STD.view(2, 1, 1) + NORM_RES_MEAN.view(2, 1, 1)).numpy()
    residual[:, ~valid_mask] = 0.0  # discard model output outside the reanalysis domain
    return residual


# ── plotting ──────────────────────────────────────────────────────────────────

def _panel(ax, lat, lon, data, title, vmin, vmax, cmap, extent, borders):
    cmap_obj = plt.get_cmap(cmap).copy()
    cmap_obj.set_bad("lightgrey")
    ax.set_title(title, fontsize=9)
    ax.set_extent(extent, crs=ccrs.PlateCarree())
    ax.set_facecolor("lightgrey")
    ax.coastlines()
    if borders:
        ax.add_feature(cfeature.BORDERS, linewidth=0.5, edgecolor="black")
    return ax.pcolormesh(lon, lat, data, vmin=vmin, vmax=vmax,
                         shading="nearest", transform=ccrs.PlateCarree(), cmap=cmap_obj,
                         rasterized=True)


def plot_comparison(columns, date_str, suptitle, outfile, extent, borders, ref_tas, ref_pr):
    """2x3 figure: one row per variable (tas, pr), one column per (title, tas, pr, lat, lon).

    ref_tas/ref_pr set the shared colour range, so every panel in a row is comparable.
    """
    fig, axs = plt.subplots(2, 3, figsize=(17, 8),
                            subplot_kw={"projection": ccrs.PlateCarree()},
                            constrained_layout=True)

    vmin_tas, vmax_tas = float(np.nanmin(ref_tas)), float(np.nanmax(ref_tas))
    vmin_pr,  vmax_pr  = 0.0, float(np.nanmax(ref_pr))

    for col, (title, tas, pr, lat, lon) in enumerate(columns):
        im_tas = _panel(axs[0, col], lat, lon, tas, f"{title}\n{date_str}",
                        vmin_tas, vmax_tas, "RdBu_r", extent, borders)
        im_pr  = _panel(axs[1, col], lat, lon, pr,  f"{title}\n{date_str}",
                        vmin_pr, vmax_pr, "Blues", extent, borders)

    fig.colorbar(im_tas, ax=axs[0, :], location="right", label="Temperature (°C)",
                 shrink=0.8, pad=0.02)
    fig.colorbar(im_pr, ax=axs[1, :], location="right", label="Precipitation (mm/day)",
                 shrink=0.8, pad=0.02)
    plt.suptitle(suptitle, fontsize=12)
    fig.savefig(outfile, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved to {outfile}")


def plot_residual(residual, valid_mask, fine_lat, fine_lon, date_str, outfile, extent, borders):
    """The residual the diffusion model actually outputs, before it is added back on."""
    fig, axs = plt.subplots(1, 2, figsize=(11, 5),
                            subplot_kw={"projection": ccrs.PlateCarree()},
                            constrained_layout=True)
    for ax, field, label, cmap, unit in [
        (axs[0], residual[0], "Temperature",   "RdBu_r", "°C"),
        (axs[1], residual[1], "Precipitation", "BrBG",   "mm/day"),
    ]:
        data = np.where(valid_mask, field, np.nan)
        vabs = float(np.nanpercentile(np.abs(data[valid_mask]), 98))
        im = _panel(ax, fine_lat, fine_lon, data, f"{label} residual\n{date_str}",
                    -vabs, vabs, cmap, extent, borders)
        fig.colorbar(im, ax=ax, shrink=0.8, pad=0.02, label=unit)

    plt.suptitle(f"Diffusion model residual — {date_str}", fontsize=12)
    fig.savefig(outfile, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved to {outfile}")


# ── main ──────────────────────────────────────────────────────────────────────

def main(source, date_str, borders=False, checkpoint=CHECKPOINT):
    outdir = plot_dir("inference")

    print(f"Loading {source} data for {date_str} ...")
    cerra_ds = xr.open_dataset(CERRA_REF, engine="netcdf4")
    valid_mask, fine_lat, fine_lon = compute_cerra_grid(cerra_ds)
    fields = LOADERS[source](date_str, cerra_ds, valid_mask)
    cerra_ds.close()

    coarse_tas, coarse_pr = fields["coarse_tas"], fields["coarse_pr"]
    residual = predict_residual(coarse_tas, coarse_pr, valid_mask, checkpoint,
                                labels=doy_hour_label(date_str))
    pred_tas = coarse_tas + residual[0]   # NaN outside the domain, inherited from coarse
    pred_pr  = coarse_pr  + residual[1]

    extent = [float(fine_lon[valid_mask].min()), float(fine_lon[valid_mask].max()),
              float(fine_lat[valid_mask].min()), float(fine_lat[valid_mask].max())]

    if source == "cerra":
        columns = [
            ("Coarse",        coarse_tas, coarse_pr, fine_lat, fine_lon),
            ("Predicted",     pred_tas,   pred_pr,   fine_lat, fine_lon),
            ("Fine (target)", fields["truth_tas"], fields["truth_pr"], fine_lat, fine_lon),
        ]
        # scale to the ground truth, so an under-dispersed prediction is visible as such
        ref_tas, ref_pr = fields["truth_tas"], fields["truth_pr"]
        suptitle = f"CERRA inference — {date_str}"
        stem = f"inference_cerra_{date_str}"
    else:
        columns = [
            ("MBCn (raw)",          fields["native_tas"], fields["native_pr"],
             fields["native_lat"], fields["native_lon"]),
            ("MBCn (interpolated)", coarse_tas, coarse_pr, fine_lat, fine_lon),
            ("Downscaled",          pred_tas,   pred_pr,   fine_lat, fine_lon),
        ]
        # no truth exists for a climate projection — scale to the coarse input instead
        ref_tas, ref_pr = coarse_tas, coarse_pr
        suptitle = f"MBCn → diffusion downscaling — {date_str}"
        stem = f"mbcn_downscaled_{date_str}"

    plot_comparison(columns, date_str, suptitle, f"{outdir}/{stem}.svg", extent, borders,
                    ref_tas, ref_pr)
    plot_residual(residual, valid_mask, fine_lat, fine_lon, date_str,
                  f"{outdir}/{source}_residual_{date_str}.svg", extent, borders)


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    ap.add_argument("source", choices=sorted(LOADERS), help="coarse data source")
    ap.add_argument("date", help="YYYY-MM-DD")
    ap.add_argument("--checkpoint", default=CHECKPOINT)
    ap.add_argument("-b", "--borders", action="store_true", help="draw country borders")
    args = ap.parse_args()

    main(args.source, args.date, borders=args.borders, checkpoint=args.checkpoint)
