"""
Scan the MBCn coarse climate-model record to find the day with the highest
domain-mean tas and the day with the highest domain-mean pr, run the
diffusion downscaling model for both dates, and plot native/coarse/downscaled
maps of the variable that made each day extreme (tas for the highest mean
temperature day, pr for the highest precipitation mass day).

Usage:
    uv run python code/inference/HottestWettestDayInference.py
    uv run python code/inference/HottestWettestDayInference.py --checkpoint ./model/20.pt --n 4
    uv run python code/inference/HottestWettestDayInference.py --start-year 2001 --end-year 2020

By default the search is restricted to the projection period (2001-2080, matching the
GWL_PERIODS windows in ProjectionPlots.py); pass --start-year/--end-year to override.
"""
import argparse
import os
import sys
from glob import glob
from pathlib import Path

import matplotlib
import numpy as np

matplotlib.use("Agg")
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import xarray as xr
from scipy.interpolate import griddata as scipy_griddata
from tqdm import tqdm

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

from DatasetAL import doy_hour_label
from evaluation.EnsembleInference import run_ensemble
from evaluation.ProjectionPeriodAssessment import load_setup, prep_mbcn_date
from grids import compute_cerra_grid
from inference.Inference import CERRA_REF, DATADIR_PROJECTION, find_mbcn_file
from paths import INFERENCE_DIR

OUT_DIR = str(INFERENCE_DIR)

# Projection period: matches the span covered by GWL_PERIODS in ProjectionPlots.py
# (PRESENT 2001-2020 through GWL4 2061-2080).
PROJECTION_START_YEAR = 2001
PROJECTION_END_YEAR = 2080


# ── Step 1: find the hottest and wettest day on record ────────────────────────

def _native_valid_mask():
    """Nearest-neighbor lookup of the CERRA valid domain onto the fixed MBCn native
    (rlat, rlon) grid that every projection file ships on."""
    cerra_ds = xr.open_dataset(CERRA_REF, engine="netcdf4")
    valid_mask, fine_lat, fine_lon = compute_cerra_grid(cerra_ds)
    cerra_ds.close()

    sample = xr.open_dataset(sorted(glob(f"{DATADIR_PROJECTION}/tas_mbcn_daily_*.nc"))[0], engine="netcdf4")
    native_lat, native_lon = sample["lat"].values, sample["lon"].values
    sample.close()

    mbcn_pts = np.column_stack([native_lat.ravel(), native_lon.ravel()])
    fine_pts = np.column_stack([fine_lat.ravel(), fine_lon.ravel()])
    native_valid = scipy_griddata(fine_pts, valid_mask.ravel().astype(np.float32),
                                  mbcn_pts, method="nearest").reshape(native_lat.shape) > 0.5
    return native_valid


def find_extreme_dates(start_year=None, end_year=None):
    """Scan every MBCn file and return ((hottest_date, mean_tas), (wettest_date, mean_pr)),
    ranked by domain-mean value on the native coarse (rlat, rlon) grid, restricted to the
    cells inside the CERRA valid domain (see _native_valid_mask)."""
    native_valid = _native_valid_mask()

    hottest = (None, -np.inf)
    tas_files = sorted(glob(f"{DATADIR_PROJECTION}/tas_mbcn_daily_*.nc"))
    for path in tqdm(tas_files, desc="Scanning MBCn tas for hottest day"):
        ds = xr.open_dataset(path, engine="netcdf4")
        dates = [str(t)[:10] for t in ds.time.values]
        vals = ds["tas"].values.astype(np.float64) - 273.15
        means = np.nanmean(np.where(native_valid[np.newaxis], vals, np.nan), axis=(1, 2))
        ds.close()
        for d, m in zip(dates, means):
            y = int(d[:4])
            if start_year and y < start_year:
                continue
            if end_year and y > end_year:
                continue
            if m > hottest[1]:
                hottest = (d, float(m))

    wettest = (None, -np.inf)
    pr_files = sorted(glob(f"{DATADIR_PROJECTION}/pr_mbcn_daily_*.nc"))
    for path in tqdm(pr_files, desc="Scanning MBCn pr for wettest day"):
        ds = xr.open_dataset(path, engine="netcdf4")
        dates = [str(t)[:10] for t in ds.time.values]
        vals = ds["pr"].values.astype(np.float64)
        means = np.nanmean(np.where(native_valid[np.newaxis], vals, np.nan), axis=(1, 2))
        ds.close()
        for d, m in zip(dates, means):
            y = int(d[:4])
            if start_year and y < start_year:
                continue
            if end_year and y > end_year:
                continue
            if m > wettest[1]:
                wettest = (d, float(m))

    return hottest, wettest


def _load_native_mbcn(date_str):
    """Load the MBCn field at its true native (~12km, 77x91) resolution, with its own
    (curvilinear) lat/lon — the resolution the coarse model actually ships at, before
    any interpolation onto the fine output grid."""
    tas_ds = xr.open_dataset(find_mbcn_file("tas", date_str), engine="netcdf4")
    pr_ds = xr.open_dataset(find_mbcn_file("pr", date_str), engine="netcdf4")
    t_idx = [str(t)[:10] for t in tas_ds.time.values].index(date_str)
    tas_native = tas_ds["tas"].isel(time=t_idx).values.astype(np.float32) - 273.15
    pr_native = pr_ds["pr"].isel(time=t_idx).values.astype(np.float32)
    native_lat = tas_ds["lat"].values
    native_lon = tas_ds["lon"].values
    tas_ds.close()
    pr_ds.close()
    return tas_native, pr_native, native_lat, native_lon


def _clip_native_to_domain(native_field, native_lat, native_lon, valid_mask, fine_lat, fine_lon):
    """Mask native-grid cells that fall outside the CERRA valid domain, so the native
    panel is visually comparable to the interpolated/downscaled ones. Map panel only —
    for stats, use _native_on_fine_grid_nearest instead (see its docstring)."""
    mbcn_pts = np.column_stack([native_lat.ravel(), native_lon.ravel()])
    fine_pts = np.column_stack([fine_lat.ravel(), fine_lon.ravel()])
    native_valid = scipy_griddata(fine_pts, valid_mask.ravel().astype(np.float32),
                                  mbcn_pts, method="nearest").reshape(native_lat.shape) > 0.5
    return np.where(native_valid, native_field, np.nan)


def _native_on_fine_grid_nearest(native_field, native_lat, native_lon, fine_lat, fine_lon):
    """Nearest-neighbor resample of the native field onto the fine CERRA grid — same
    footprint as the interpolated/downscaled fields, unlike _clip_native_to_domain's
    independent clip, which can drop the day's real extreme before it's measured."""
    native_pts = np.column_stack([native_lat.ravel(), native_lon.ravel()])
    fine_pts = np.column_stack([fine_lat.ravel(), fine_lon.ravel()])
    resampled = scipy_griddata(native_pts, native_field.ravel(), fine_pts, method="nearest")
    return resampled.reshape(fine_lat.shape)


# ── Step 2: plot coarse vs. downscaled ─────────────────────────────────────────


def _plot_comparison(date_str, day_kind, coarse, downscaled, native_tas, native_pr,
                      native_lat, native_lon, setup, n_ens, out_dir, filename_prefix=""):
    """1 row x 3 cols (native | coarse-interpolated | downscaled) map panels, for just
    the variable that made this day extreme: tas for the highest mean temperature day,
    pr for the highest precipitation mass day."""
    valid_mask = setup["valid_mask"]
    fine_lat, fine_lon = setup["fine_lat"], setup["fine_lon"]
    extent = [float(fine_lon[valid_mask].min()), float(fine_lon[valid_mask].max()),
              float(fine_lat[valid_mask].min()), float(fine_lat[valid_mask].max())]
    crs = ccrs.PlateCarree()

    var_by_day_kind = {
        "Highest mean temperature day": ("tas", "RdBu_r", "°C",     "Temperature",  native_tas),
        "Highest precipitation mass day": ("pr",  "Blues",  "mm/day", "Precipitation", native_pr),
    }
    v = 0 if day_kind == "Highest mean temperature day" else 1
    vname, cmap, unit, label, native_raw = var_by_day_kind[day_kind]

    fig, axs = plt.subplots(1, 3, figsize=(13, 3.6), constrained_layout=True,
                            subplot_kw={"projection": crs})
    fig.patch.set_facecolor("white")
    fig.suptitle(f"{day_kind} — {date_str}", fontsize=12)

    n_field = _clip_native_to_domain(native_raw, native_lat, native_lon, valid_mask, fine_lat, fine_lon)
    c_field = np.where(valid_mask, coarse[v], np.nan)
    d_field = np.where(valid_mask, downscaled[v], np.nan)
    all_three = np.concatenate([n_field.ravel(), c_field.ravel(), d_field.ravel()])
    vmin = float(np.nanpercentile(all_three, 2))
    vmax = float(np.nanmax(all_three))
    cm = plt.get_cmap(cmap).copy()
    cm.set_bad("lightgrey")

    for col, (data, title, lat_grid, lon_grid) in enumerate([
        (n_field, f"{label} - Coarse field", native_lat, native_lon),
        (c_field, f"{label} - Interpolated field", fine_lat, fine_lon),
        (d_field, f"{label} - Downscaled field (ensemble mean)", fine_lat, fine_lon),
    ]):
        ax = axs[col]
        im = ax.pcolormesh(lon_grid, lat_grid, data, vmin=vmin, vmax=vmax,
                            cmap=cm, transform=crs, shading="nearest", rasterized=True)
        ax.set_extent(extent, crs=crs)
        ax.coastlines()
        ax.set_facecolor("lightgrey")
        ax.set_xticks([])
        ax.set_yticks([])
        ax.set_title(title, fontsize=9)
        if col == 2:
            # All three panels share the same vmin/vmax/cmap, so one colorbar (on the
            # rightmost panel) covers all of them instead of repeating it three times.
            cbar = fig.colorbar(im, ax=ax, shrink=0.7, pad=0.03, label=unit)
            cbar.ax.tick_params(labelsize=7)

    os.makedirs(out_dir, exist_ok=True)
    slug = day_kind.lower().replace(" ", "_")
    out = f"{out_dir}/{filename_prefix}{slug}_{date_str}.svg"
    fig.savefig(out, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  Saved {out}")

    # Same footprint as c_field/d_field, not n_field (see _native_on_fine_grid_nearest).
    n_field_stats = np.where(
        valid_mask, _native_on_fine_grid_nearest(native_raw, native_lat, native_lon, fine_lat, fine_lon), np.nan)
    n_mean, c_mean, d_mean = (float(np.nanmean(f)) for f in (n_field_stats, c_field, d_field))
    n_max,  c_max,  d_max  = (float(np.nanmax(f))  for f in (n_field_stats, c_field, d_field))
    table_lines = [
        f"{day_kind} — {date_str} — {vname} ({unit})",
        "",
        f"{'':<6}{'native*':>10}{'interp.*':>10}{'downscaled':>12}{'d regrid':>10}{'d model':>10}",
        f"{'mean':<6}{n_mean:>10.2f}{c_mean:>10.2f}{d_mean:>12.2f}{c_mean - n_mean:>+10.2f}{d_mean - c_mean:>+10.2f}",
        f"{'max':<6}{n_max:>10.2f}{c_max:>10.2f}{d_max:>12.2f}{c_max - n_max:>+10.2f}{d_max - c_max:>+10.2f}",
        "",
        "*native = nearest-neighbor lookup of the true MBCn resolution (~12km) onto the fine grid  |  "
        "interp. = native field bilinearly resampled onto the fine output grid  |  "
        "d regrid = interp.-native (resampling effect)  |  d model = downscaled-interp. (learned residual)",
    ]
    table_out = f"{out_dir}/{filename_prefix}{slug}_{date_str}.txt"
    with open(table_out, "w", encoding="utf-8") as f:
        f.write("\n".join(table_lines) + "\n")
    print(f"  Saved {table_out}")
    print(f"  {vname}: native mean={n_mean:.2f}  interp mean={c_mean:.2f}  down mean={d_mean:.2f} {unit}")


# ── Main ────────────────────────────────────────────────────────────────────

def main(checkpoint, n_ens, start_year, end_year, cache_dir):
    os.makedirs(OUT_DIR, exist_ok=True)

    print("Finding hottest and wettest day in the MBCn record"
          + (f" ({start_year or '...'}-{end_year or '...'})" if start_year or end_year else "")
          + " ...")
    (hot_date, hot_mean_tas), (wet_date, wet_mean_pr) = find_extreme_dates(start_year, end_year)
    print(f"  Highest mean temperature day: {hot_date}  (domain-mean coarse tas = {hot_mean_tas:.2f} °C)")
    print(f"  Highest precipitation mass day: {wet_date}  (domain-mean coarse pr  = {wet_mean_pr:.2f} mm/day)")

    print("\nLoading model and grid setup...")
    setup = load_setup(checkpoint, cache_dir=cache_dir)

    for date_str, day_kind in [(hot_date, "Highest mean temperature day"),
                               (wet_date, "Highest precipitation mass day")]:
        print(f"\nRunning inference for {day_kind.lower()} ({date_str})...")
        coarse, coarse_input = prep_mbcn_date(date_str, setup)
        if coarse is None:
            print(f"  WARNING: {date_str} not found in MBCn data — skipping")
            continue
        samples = run_ensemble(setup["network"], coarse_input, coarse,
                                setup["norm_res_mean"], setup["norm_res_std"],
                                setup["valid_mask"], n_ens,
                                labels=doy_hour_label(date_str))
        downscaled = np.nanmean(samples, axis=0)
        native_tas, native_pr, native_lat, native_lon = _load_native_mbcn(date_str)
        _plot_comparison(date_str, day_kind, coarse, downscaled, native_tas, native_pr,
                         native_lat, native_lon, setup, n_ens, OUT_DIR)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--checkpoint", default="./model/20.pt")
    parser.add_argument("--n", type=int, default=4,
                         help="ensemble members for the downscaled mean (default 4)")
    parser.add_argument("--start-year", type=int, default=PROJECTION_START_YEAR,
                         help=f"restrict the search to dates >= this year (default: {PROJECTION_START_YEAR}, "
                              "the start of the projection period)")
    parser.add_argument("--end-year", type=int, default=PROJECTION_END_YEAR,
                         help=f"restrict the search to dates <= this year (default: {PROJECTION_END_YEAR}, "
                              "the end of the projection period)")
    parser.add_argument("--cache-dir", default=None,
                         help="directory to cache/load the grid setup (see ProjectionPeriodAssessment)")
    args = parser.parse_args()
    main(args.checkpoint, args.n, args.start_year, args.end_year, args.cache_dir)
