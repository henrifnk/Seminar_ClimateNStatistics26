"""
Compute normalization statistics from the training dataset and save to data/norm_stats.npz.

TrainDiffusion.py writes the same file as a side effect of training; this script is the
standalone entry point for when you only need the stats (e.g. to run inference against an
existing checkpoint). Keep the dataset arguments in sync with TrainDiffusion.main().

Usage:
    uv run python code/training/ComputeNormStats.py
"""
import sys
from pathlib import Path

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

from DatasetAL import UpscaleDataset, save_norm_stats


def main():
    print("Building training dataset and computing normalization stats...")
    dataset_train = UpscaleDataset(
        "data/reanalysis/", "data/reanalysis_coarsened/", "data/",
        year_start=2000, year_end=2017, constant_variables=["lsm", "orog"])

    save_norm_stats(dataset_train)
    print(f"  raw_mean: {dataset_train._norm_raw_mean.numpy()}")
    print(f"  raw_std:  {dataset_train._norm_raw_std.numpy()}")
    print(f"  res_mean: {dataset_train._norm_res_mean.numpy()}")
    print(f"  res_std:  {dataset_train._norm_res_std.numpy()}")


if __name__ == "__main__":
    main()
