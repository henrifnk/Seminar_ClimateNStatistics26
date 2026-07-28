"""Generate a tiny synthetic processed-data fixture for smoke-testing the
training pipeline without the real ~327 MB dataset.

Matches the schema DroughtDataset expects (dynamic_grid.nc, static_grid.nc,
global_scalars.nc, normalization.json) on a much smaller spatial grid (8x8
instead of 44x98), but keeps the real 1971-01..2024-12 time span so the
default split years (val_from_year=2005, test_from_year=2015) produce
non-empty train/val/test splits exactly like the real dataset -- no config
overrides needed beyond pointing data.processed_dir at the fixture.

Usage:
    python code/make_fixture.py [output_dir]   # defaults to a fresh temp dir; prints the path
"""

import json
import sys
from pathlib import Path

import numpy as np
import xarray as xr

# Mirrors dataset.py's _MED_GROUPS source basins.
_MED_BASINS = [
    "Alboran_Sea", "Balearic_Sea", "Ligurian_Sea", "Gulf_of_Lion", "Tyrrhenian_Sea", "Sicilian_Sea",
    "Adriatic_Sea", "Ionian_Sea", "Lybian_Sea", "Aegean_Sea", "Levantine_Basin", "Black_Sea",
]
_DYNAMIC_VARS = ["spei", "wb", "pr", "ps", "tas"]

H, W = 8, 8      # tiny grid (real grid is 44x98)
N_MONTHS = 648   # 1971-01 .. 2024-12, matching the real time span
TRAIN_CUTOFF_YEAR = 2005  # matches configs/data/default.yaml val_from_year


def make_fixture(out_dir: Path | str, seed: int = 0) -> Path:
    rng = np.random.default_rng(seed)
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    times = (np.array(["1971-01"], dtype="datetime64[M]") + np.arange(N_MONTHS)).astype("datetime64[ns]")
    rlat = np.arange(H, dtype=np.float32)
    rlon = np.arange(W, dtype=np.float32)
    years = times.astype("datetime64[Y]").astype(int) + 1970
    is_train = years < TRAIN_CUTOFF_YEAR

    # -- static grid: mask + orography -----------------------------------------
    mask = np.ones((H, W), dtype=np.int8)
    mask[0, :] = 0  # a few invalid cells, like the real ocean/non-Alpine mask
    orogf = rng.uniform(0, 2000, size=(H, W)).astype(np.float32)
    xr.Dataset(
        {"orogf": (("rlat", "rlon"), orogf), "mask": (("rlat", "rlon"), mask)},
        coords={"rlat": rlat, "rlon": rlon},
    ).to_netcdf(out_dir / "static_grid.nc")

    # -- dynamic grid -----------------------------------------------------------
    dyn_arrays = {}
    for var in _DYNAMIC_VARS:
        arr = rng.normal(0, 1, size=(N_MONTHS, H, W)).astype(np.float32)
        arr[:, mask == 0] = np.nan
        dyn_arrays[var] = arr
    ds_dyn = xr.Dataset(
        {var: (("time", "rlat", "rlon"), arr) for var, arr in dyn_arrays.items()},
        coords={"time": times, "rlat": rlat, "rlon": rlon},
    )
    ds_dyn.to_netcdf(out_dir / "dynamic_grid.nc")

    # -- global scalars -----------------------------------------------------------
    months = (np.arange(N_MONTHS) % 12) + 1
    glob_arrays = {
        "nao_index": rng.normal(0, 1, N_MONTHS).astype(np.float32),
        "month_sin": np.sin(2 * np.pi * months / 12).astype(np.float32),
        "month_cos": np.cos(2 * np.pi * months / 12).astype(np.float32),
    }
    for basin in _MED_BASINS:
        glob_arrays[basin] = rng.normal(15, 3, N_MONTHS).astype(np.float32)
    ds_glob = xr.Dataset(
        {name: (("time",), arr) for name, arr in glob_arrays.items()},
        coords={"time": times},
    )
    ds_glob.to_netcdf(out_dir / "global_scalars.nc")

    # -- normalization stats (training period only, matching preprocessing.py) --
    mask_bool = mask.astype(bool)
    stats: dict = {}
    for var, arr in dyn_arrays.items():
        train_valid = arr[is_train][:, mask_bool]
        stats[var] = {"mean": float(np.nanmean(train_valid)), "std": float(max(np.nanstd(train_valid), 1e-8))}
    glob_stats = {}
    for name, arr in glob_arrays.items():
        train_vals = arr[is_train]
        glob_stats[name] = {"mean": float(train_vals.mean()), "std": float(max(train_vals.std(), 1e-8))}
    stats["_global"] = glob_stats
    with open(out_dir / "normalization.json", "w") as f:
        json.dump(stats, f, indent=2)

    return out_dir


if __name__ == "__main__":
    import tempfile

    if len(sys.argv) > 1:
        target = Path(sys.argv[1])
    else:
        target = Path(tempfile.mkdtemp(prefix="drought_fixture_"))
    make_fixture(target)
    print(target)
