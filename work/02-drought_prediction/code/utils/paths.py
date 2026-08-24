"""Shared data-root resolution for every entry point (train.py, eval_baselines.py,
reports/trend_baseline.py, dataset.py, visualization.py, ...).

The processed- and raw-data roots are each read from exactly one configurable
location -- an environment variable, falling back to a project-relative
default -- never a hard-coded absolute path and never dependent on the
process's current working directory. This module is imported by scripts
launched both from the GitBook repo root and from this chapter folder
directly, so PROJECT_ROOT is always computed from __file__, not cwd.
"""

import os
from pathlib import Path

# code/utils/paths.py -> code/ -> work/02-drought_prediction/
PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent

DROUGHT_DATA_DIR_ENV = "DROUGHT_DATA_DIR"
DROUGHT_RAW_DIR_ENV = "DROUGHT_RAW_DIR"

_REQUIRED_PROCESSED_FILES = ("dynamic_grid.nc", "static_grid.nc", "global_scalars.nc", "normalization.json")
_REQUIRED_DYNAMIC_VARS = ("spei", "wb", "pr", "ps", "tas")
_REQUIRED_STATIC_VARS = ("orogf", "mask")


def project_root() -> Path:
    return PROJECT_ROOT


def raw_data_dir() -> Path:
    """Raw NetCDF input root consumed by preprocessing.py / visualization.py.

    Override with the DROUGHT_RAW_DIR environment variable.
    """
    return Path(os.environ.get(DROUGHT_RAW_DIR_ENV, PROJECT_ROOT / "data" / "raw"))


def processed_data_dir() -> Path:
    """Preprocessed-data root consumed by DroughtDataset (and produced by
    preprocessing.py). Override with the DROUGHT_DATA_DIR environment variable.
    """
    return Path(os.environ.get(DROUGHT_DATA_DIR_ENV, PROJECT_ROOT / "data" / "processed"))


def require_raw_dir(raw_dir: Path | str) -> Path:
    """Fail early with an actionable message if the raw-data root is missing."""
    raw_dir = Path(raw_dir)
    if not raw_dir.is_dir():
        raise FileNotFoundError(
            f"Raw-data directory not found: {raw_dir}\n"
            f"Set {DROUGHT_RAW_DIR_ENV} to the directory containing the raw NetCDF inputs "
            f"(al/, atlantic/, mediteranean/, topography/)."
        )
    return raw_dir


def require_processed_dir(processed_dir: Path | str) -> Path:
    """Fail early with an actionable message if the processed-data root or any
    of its expected files are missing.
    """
    processed_dir = Path(processed_dir)
    if not processed_dir.is_dir():
        raise FileNotFoundError(
            f"Processed-data directory not found: {processed_dir}\n"
            f"Set {DROUGHT_DATA_DIR_ENV} to the directory containing "
            f"{', '.join(_REQUIRED_PROCESSED_FILES)}, or run `invoke preprocess`."
        )
    missing = [f for f in _REQUIRED_PROCESSED_FILES if not (processed_dir / f).exists()]
    if missing:
        raise FileNotFoundError(
            f"Missing expected file(s) in {processed_dir}: {', '.join(missing)}\n"
            f"Set {DROUGHT_DATA_DIR_ENV} to the correct processed-data directory, "
            f"or run `invoke preprocess`."
        )
    return processed_dir


def check_processed_schema(processed_dir: Path | str) -> None:
    """Lightweight schema/shape check across the processed files: expected
    variable names present, and the static grid's (rlat, rlon) matches the
    dynamic grid's. Raises AssertionError with a specific message on the
    first mismatch found. Cheap -- opens each file without loading data.
    """
    import xarray as xr

    processed_dir = Path(processed_dir)

    with xr.open_dataset(processed_dir / "dynamic_grid.nc") as ds_dyn:
        missing = [v for v in _REQUIRED_DYNAMIC_VARS if v not in ds_dyn.data_vars]
        assert not missing, f"dynamic_grid.nc missing variable(s): {missing}"
        assert "time" in ds_dyn.dims, "dynamic_grid.nc missing a 'time' dimension"
        dyn_rlat, dyn_rlon = ds_dyn.sizes.get("rlat"), ds_dyn.sizes.get("rlon")
        assert dyn_rlat and dyn_rlon, "dynamic_grid.nc missing rlat/rlon dimensions"

    with xr.open_dataset(processed_dir / "static_grid.nc") as ds_static:
        missing = [v for v in _REQUIRED_STATIC_VARS if v not in ds_static.data_vars]
        assert not missing, f"static_grid.nc missing variable(s): {missing}"
        static_rlat, static_rlon = ds_static.sizes.get("rlat"), ds_static.sizes.get("rlon")
        assert (static_rlat, static_rlon) == (dyn_rlat, dyn_rlon), (
            f"static_grid.nc grid ({static_rlat}, {static_rlon}) does not match "
            f"dynamic_grid.nc grid ({dyn_rlat}, {dyn_rlon})"
        )

    import json

    with open(processed_dir / "normalization.json") as f:
        norm = json.load(f)
    missing = [v for v in _REQUIRED_DYNAMIC_VARS if v not in norm]
    assert not missing, f"normalization.json missing stats for: {missing}"


def validate_processed_dir(processed_dir: Path | str) -> Path:
    """require_processed_dir() + check_processed_schema() in one call."""
    processed_dir = require_processed_dir(processed_dir)
    check_processed_schema(processed_dir)
    return processed_dir
