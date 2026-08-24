"""
Coarsen native CERRA reanalysis onto the MBCn 0.11° grid.

Builds a nearest-neighbour index from each MBCn cell to the CERRA grid once, then
averages a square window of CERRA pixels per MBCn cell. The result is the coarse half of
the training pairs — the model's job is to invert this step.

Reads  data/reanalysis/<var>/CERRA_<var>_daily_<YYYY>_<MM>.nc
Writes data/reanalysis_coarsened/CERRA_<var>_daily_<YYYY>_<MM>_coarsened.nc

Usage:
    uv run python src/data_prep/coarsen_cerra.py
"""
from pathlib import Path

import numpy as np
import xarray as xr
from pyproj import Transformer

# Resolved against the current working directory, i.e. run from the repo root —
# same convention as every other script here (aggregate_mbcn_daily.py, Inference.py, ...).
# Some users keep the data under `data/` while others have an extra nesting
# `data/data/` (observed in this repo). Resolve both locations where possible.
def _resolve_data_subpath(*parts: str) -> Path:
    p = Path("data").joinpath(*parts)
    if p.exists():
        return p
    p2 = Path("data").joinpath("data", *parts)
    if p2.exists():
        return p2
    return p

REANALYSIS_DIR = _resolve_data_subpath("reanalysis")
MBCN_REF       = _resolve_data_subpath("projection", "pr_mbcn_daily_1996-2000.nc")
OUTDIR         = _resolve_data_subpath("reanalysis_coarsened")


def build_cerra_to_mbcn_index(cerra_ds, mbcn_ds):
    """
    For each MBCn grid cell, compute the (row, col) index into the CERRA
    UTM grid by projecting MBCn lat/lon -> UTM-32N.
    Returns arrays of shape (nlat_mbcn, nlon_mbcn).
    """
    x0    = float(cerra_ds["x"].values[0])
    y0    = float(cerra_ds["y"].values[0])
    dx    = float(np.diff(cerra_ds["x"].values).mean())   # 500 m
    dy    = float(np.diff(cerra_ds["y"].values).mean())   # -500 m (y decreasing)

    # project MBCn lat/lon -> UTM-32N
    transformer = Transformer.from_crs("EPSG:4326", "EPSG:32632", always_xy=True)
    mbcn_lon = mbcn_ds["lon"].values   # (rlat, rlon)
    mbcn_lat = mbcn_ds["lat"].values

    utm_x, utm_y = transformer.transform(mbcn_lon, mbcn_lat)

    # nearest CERRA pixel index
    col_idx = np.round((utm_x - x0) / dx).astype(int)
    row_idx = np.round((utm_y - y0) / dy).astype(int)

    return row_idx, col_idx


def coarsen_snapshot(data_2d, row_idx, col_idx, half_win):
    """
    Average a (2*half_win+1)² window of CERRA pixels for each MBCn cell.
    data_2d: (ny_cerra, nx_cerra) — may contain NaN.
    """
    nlat_mbcn, nlon_mbcn = row_idx.shape
    out = np.full((nlat_mbcn, nlon_mbcn), np.nan, dtype=np.float32)

    ny, nx = data_2d.shape
    for i in range(nlat_mbcn):
        for j in range(nlon_mbcn):
            r, c = row_idx[i, j], col_idx[i, j]
            r0, r1 = max(0, r - half_win), min(ny, r + half_win + 1)
            c0, c1 = max(0, c - half_win), min(nx, c + half_win + 1)
            if r0 >= r1 or c0 >= c1:
                continue
            window = data_2d[r0:r1, c0:c1]
            valid  = window[~np.isnan(window)]
            if len(valid) > 0:
                out[i, j] = valid.mean()
    return out


def coarsen_file(cerra_path: Path, row_idx, col_idx, half_win: int,
                 mbcn_ds, outdir: Path) -> None:
    print(f"\n[{cerra_path.name}]")
    ds = xr.open_dataset(cerra_path)
    varname = "pr" if "pr" in cerra_path.stem.lower() else "tas"
    data = ds[varname].values   # (time, ny, nx)
    n_time = data.shape[0]

    out_data = np.full((n_time, *row_idx.shape), np.nan, dtype=np.float32)
    for t in range(n_time):
        if t % 5 == 0:
            print(f"  timestep {t+1}/{n_time} ...")
        out_data[t] = coarsen_snapshot(data[t], row_idx, col_idx, half_win)

    ds_out = xr.Dataset(
        {varname: (["time", "rlat", "rlon"], out_data)},
        coords={
            "time": ds["time"].values,
            "rlat": mbcn_ds["rlat"],
            "rlon": mbcn_ds["rlon"],
            "lat":  mbcn_ds["lat"],
            "lon":  mbcn_ds["lon"],
        },
    )
    ds_out[varname].attrs.update(ds[varname].attrs)
    ds_out[varname].attrs["coarsened_from"] = "CERRA 500m -> MBCn 0.11° grid"
    ds_out[varname].attrs["coarsening_method"] = f"spatial mean over {2*half_win+1}×{2*half_win+1} window"

    stem = cerra_path.stem.replace(" ", "_").replace("copy", "").strip("_")
    outfile = outdir / f"{stem}_coarsened.nc"
    print(f"  Writing -> {outfile.name}")
    ds_out.to_netcdf(outfile)
    print(f"  Done. Shape: {out_data.shape}")
    ds.close()


def main():
    OUTDIR.mkdir(parents=True, exist_ok=True)

    mbcn_ds = xr.open_dataset(MBCN_REF)

    # Collect all CERRA files from pr/ and tas/ subfolders
    cerra_files = sorted(
        list((REANALYSIS_DIR / "pr").glob("CERRA_pr_daily_*.nc")) +
        list((REANALYSIS_DIR / "tas").glob("CERRA_tas_daily_*.nc"))
    )
    print(f"Found {len(cerra_files)} CERRA files to coarsen.")

    # If there are no CERRA files, print a helpful message and exit.
    if len(cerra_files) == 0:
        print("No CERRA files found. Place CERRA_pr_daily_*.nc and/or CERRA_tas_daily_*.nc"
              " under the 'data/reanalysis/pr' and 'data/reanalysis/tas' folders respectively.")
        return

    # Load one CERRA file to build index
    cerra_ref = xr.open_dataset(cerra_files[0])
    print("Building CERRA to MBCn projection index ...")
    row_idx, col_idx = build_cerra_to_mbcn_index(cerra_ref, mbcn_ds)

    # Window: 12km / 500m = 24 pixels -> half_win = 12
    half_win = 12
    print(f"Averaging window: {2*half_win+1}×{2*half_win+1} CERRA pixels per MBCn cell")

    # Check how many MBCn cells fall within the CERRA grid
    first_varname = "pr" if "pr" in cerra_files[0].stem else "tas"
    ny, nx = cerra_ref[first_varname].shape[1:]
    in_bounds = ((row_idx >= 0) & (row_idx < ny) &
                 (col_idx >= 0) & (col_idx < nx))
    print(f"MBCn cells within CERRA domain: {in_bounds.sum()} / {in_bounds.size} "
          f"({100*in_bounds.mean():.1f}%)")
    cerra_ref.close()

    for fpath in cerra_files:
        coarsen_file(fpath, row_idx, col_idx, half_win, mbcn_ds, OUTDIR)

    print("\nAll files coarsened.")


if __name__ == "__main__":
    main()
