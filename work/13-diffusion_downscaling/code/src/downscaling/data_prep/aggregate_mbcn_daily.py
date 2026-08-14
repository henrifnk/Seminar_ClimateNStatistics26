"""
Aggregate 3-hourly MBCn files to daily resolution.

tas: daily mean
pr:  daily sum  (to match CERRA kg/m2/day totals)

Reads from data/projection/*_mbcn_YYYY-YYYY.nc (no "daily" in name).
Writes to  data/projection/*_mbcn_daily_YYYY-YYYY.nc.

Usage:
    python -m downscaling.data_prep.aggregate_mbcn_daily
"""
from pathlib import Path

import numpy as np
import xarray as xr

DATADIR = Path("data/projection")
STEPS_PER_DAY = 8  # 3-hourly


def aggregate_file(infile: Path) -> None:
    print(f"\n[{infile.name}]")
    outfile = infile.parent / infile.name.replace("mbcn_", "mbcn_daily_")
    if outfile.exists():
        print(f"  Already exists, skipping: {outfile.name}")
        return

    ds = xr.open_dataset(infile, engine="netcdf4")
    varname = "pr" if "pr" in infile.stem else "tas"
    data = ds[varname].values  # (n_time, rlat, rlon)
    n_time, nlat, nlon = data.shape

    if n_time % STEPS_PER_DAY != 0:
        print(f"  WARNING: {n_time} timesteps not divisible by {STEPS_PER_DAY}, skipping.")
        ds.close()
        return

    n_days = n_time // STEPS_PER_DAY
    print(f"  {n_time} 3-hourly steps -> {n_days} daily values ...")

    reshaped = data.reshape(n_days, STEPS_PER_DAY, nlat, nlon)
    daily = reshaped.sum(axis=1) if varname == "pr" else reshaped.mean(axis=1)

    time_daily = ds.time.values[::STEPS_PER_DAY]

    ds_daily = xr.Dataset(
        {varname: (["time", "rlat", "rlon"], daily)},
        coords={
            "time": time_daily,
            "rlat": ds.rlat,
            "rlon": ds.rlon,
            "lat":  ds.lat,
            "lon":  ds.lon,
        },
    )
    ds_daily[varname].attrs.update(ds[varname].attrs)
    ds_daily[varname].attrs["aggregation"] = "3-hourly to daily"
    ds_daily[varname].attrs["aggregation_method"] = "daily sum" if varname == "pr" else "daily mean"
    ds_daily.attrs.update(ds.attrs)
    ds.close()

    print(f"  Writing -> {outfile.name}")
    ds_daily.to_netcdf(outfile)
    units = ds_daily[varname].attrs.get("units", "")
    print(f"  Done. Shape: {ds_daily[varname].shape}  mean: {np.nanmean(daily):.4f} {units}")


def main() -> None:
    infiles = sorted(f for f in DATADIR.glob("*_mbcn_*.nc") if "daily" not in f.name)
    print(f"Found {len(infiles)} non-daily MBCn files to process:")
    for f in infiles:
        print(f"  {f.name}")

    for infile in infiles:
        aggregate_file(infile)

    print("\nAll done.")


if __name__ == "__main__":
    main()
