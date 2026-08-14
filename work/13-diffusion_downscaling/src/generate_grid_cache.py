"""Write cache/grid.npz from the reanalysis reference file.

The CERRA valid-domain mask and lat/lon grid are derived from a single reanalysis file
(Inference.CERRA_REF). On a machine that only has MBCn projection data that file is
absent, so run this once where the reanalysis lives and copy cache/grid.npz across —
diag_common and ProjectionPeriodAssessment fall back to it automatically.

Usage:
    uv run python src/generate_grid_cache.py [--cache-dir ./cache]
"""
import argparse
import os

import numpy as np
import xarray as xr

from grids import compute_cerra_grid
from inference.Inference import CERRA_REF


def main(cache_dir="./cache"):
    cerra_ref = xr.open_dataset(CERRA_REF, engine="netcdf4")
    valid_mask, fine_lat, fine_lon = compute_cerra_grid(cerra_ref)
    cerra_ref.close()

    os.makedirs(cache_dir, exist_ok=True)
    out = os.path.join(cache_dir, "grid.npz")
    np.savez(out, valid_mask=valid_mask, fine_lat=fine_lat, fine_lon=fine_lon)
    print(f"Saved {out}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--cache-dir", default="./cache")
    main(parser.parse_args().cache_dir)
