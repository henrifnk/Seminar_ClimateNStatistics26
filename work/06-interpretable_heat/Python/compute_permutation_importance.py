"""
Compute permutation feature importance (dynamic + static features) for a
trained water_temp_lstm model and save the results as a CSV file.

This script performs ONLY the computation step. Plotting is done separately
(see the accompanying R / ggplot script).
"""

import os
os.environ["TF_ENABLE_ONEDNN_OPTS"] = "0"
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

from pathlib import Path

import numpy as np
import pandas as pd

from water_temp_lstm.interpretability.loading import load_cfg, load_model, load_test_datasets, build_loader
from water_temp_lstm.interpretability.permutation import permute_feature_temporally, permute_static_feature, compute_importance
from water_temp_lstm.interpretability.inference import run_inference
from water_temp_lstm.interpretability.metrics import nse


# ======================================================================
# Configuration
# ======================================================================
RUN_DIR = Path("runs/water_temp_lstm_3006_234701")
EPOCH = 22
BATCH_SIZE = 256
N_PERMUTATION_REPEATS = 5

# Directory where the resulting CSV is stored.
DIR_RESULTS = Path("data/results")
RESULTS_CSV_NAME = "permutation_importance.csv"


# ======================================================================
# Functions
# ======================================================================
def load_model_and_data(run_dir: Path, epoch: int, batch_size: int):
    """Load config, model, test datasets, and build the data loader."""
    cfg = load_cfg(run_dir)
    tester = load_model(cfg, run_dir, epoch=epoch)
    model = tester.model

    datasets = load_test_datasets(cfg, tester)
    loader = build_loader(datasets, batch_size)

    return cfg, model, loader


def compute_baseline_nse(loader, model):
    """Run inference and compute the baseline NSE (only last time step,
    since predict_last_n=1)."""
    baseline_preds, observations = run_inference(loader, model)

    baseline_preds_last = baseline_preds[:, -1, :]
    observations_last = observations[:, -1, :]

    baseline_nse = nse(observations_last, baseline_preds_last)
    return baseline_nse, observations_last


def compute_dynamic_importance(cfg, loader, model, baseline_nse, observations_last, n_repeats):
    """Compute permutation importance for dynamic (time-varying) features."""
    return compute_importance(
        cfg.dynamic_inputs,
        permute_feature_temporally,
        loader, model,
        baseline_nse,
        observations_last,
        n_repeats,
    )


def compute_static_importance(cfg, loader, model, baseline_nse, observations_last, n_repeats):
    """Compute permutation importance for static (time-invariant) features."""
    return compute_importance(
        cfg.static_attributes,
        lambda batch, feature_name, seed: permute_static_feature(
            batch, cfg.static_attributes.index(feature_name), seed
        ),
        loader, model,
        baseline_nse,
        observations_last,
        n_repeats,
    )


def print_importances(importances: dict, label: str = "") -> None:
    """Print mean and standard deviation of importance values per feature."""
    print(f"\n{label}")
    for feature_name, values in importances.items():
        print(f"{feature_name}: mean={np.mean(values):.4f}, sd={np.std(values):.4f}")


def importances_to_dataframe(importances: dict, feature_type: str) -> pd.DataFrame:
    """Convert an importance dict {feature: [values]} into a tidy DataFrame."""
    rows = [
        {
            "feature": feature,
            "feature_type": feature_type,
            "mean_importance": np.mean(values),
            "sd": np.std(values),
        }
        for feature, values in importances.items()
    ]
    return pd.DataFrame(rows)


def save_results(df_dynamic: pd.DataFrame, df_static: pd.DataFrame, out_dir: Path, filename: str) -> Path:
    """Combine dynamic + static results and save them as a single CSV."""
    out_dir.mkdir(parents=True, exist_ok=True)
    df_all = pd.concat([df_dynamic, df_static], ignore_index=True)
    out_path = out_dir / filename
    df_all.to_csv(out_path, index=False)
    return out_path


# ======================================================================
# Main
# ======================================================================
def main():
    cfg, model, loader = load_model_and_data(RUN_DIR, EPOCH, BATCH_SIZE)

    baseline_nse, observations_last = compute_baseline_nse(loader, model)
    print("Baseline NSE:", baseline_nse)

    importances_dynamic = compute_dynamic_importance(
        cfg, loader, model, baseline_nse, observations_last, N_PERMUTATION_REPEATS
    )
    importances_static = compute_static_importance(
        cfg, loader, model, baseline_nse, observations_last, N_PERMUTATION_REPEATS
    )

    print_importances(importances_dynamic, "Dynamic Features")
    print_importances(importances_static, "Static Features")

    df_dynamic = importances_to_dataframe(importances_dynamic, "dynamic")
    df_static = importances_to_dataframe(importances_static, "static")

    out_path = save_results(df_dynamic, df_static, DIR_RESULTS, RESULTS_CSV_NAME)
    print(f"\nSaved CSV to {out_path}")


if __name__ == "__main__":
    main()