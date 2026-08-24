"""
Combines all 'validation_metrics.csv' files in the per-epoch subfolders (model_epoch001, model_epoch002, ...) 
of the model's validation directory into a single table, adding an 'epoch' column.
The result is saved as 'validation_metrics_table.csv' in the validation directory.
"""

import re
import sys
from pathlib import Path

import pandas as pd

VALIDATION_DIR = Path("runs/water_temp_lstm_3006_234701/validation")

EPOCH_DIR_PATTERN = re.compile(r"model_epoch(\d+)$")
METRICS_FILENAME = "validation_metrics.csv"
OUTPUT_FILENAME = "validation_metrics_table.csv"


def find_epoch_metric_files(validation_dir: Path) -> list[tuple[int, Path]]:
    """Find all validation_metrics.csv files together with their epoch number."""
    epoch_files = []
    for entry in sorted(validation_dir.iterdir()):
        if not entry.is_dir():
            continue
        match = EPOCH_DIR_PATTERN.match(entry.name)
        if not match:
            continue
        metrics_file = entry / METRICS_FILENAME
        if metrics_file.exists():
            epoch_files.append((int(match.group(1)), metrics_file))
        else:
            print(f"Warning: no {METRICS_FILENAME} found in {entry}", file=sys.stderr)
    return sorted(epoch_files, key=lambda item: item[0])


def build_combined_table(epoch_files: list[tuple[int, Path]]) -> pd.DataFrame:
    """Read each metrics file, tag it with its epoch, and concatenate."""
    frames = []
    for epoch, file_path in epoch_files:
        df = pd.read_csv(file_path)
        df.insert(0, "epoch", epoch)
        frames.append(df)

    if not frames:
        raise ValueError("No validation_metrics.csv files were found.")

    return pd.concat(frames, ignore_index=True)


def main() -> None:
    validation_dir = VALIDATION_DIR
    if not validation_dir.is_dir():
        raise NotADirectoryError(f"Not a valid directory: {validation_dir}")

    epoch_files = find_epoch_metric_files(validation_dir)
    if not epoch_files:
        raise FileNotFoundError(
            f"No model_epochXXX/{METRICS_FILENAME} files found under {validation_dir}"
        )

    combined_df = build_combined_table(epoch_files)

    output_path = validation_dir / OUTPUT_FILENAME
    combined_df.to_csv(output_path, index=False)

    print(f"Combined {len(epoch_files)} epoch files into {output_path}")
    print(f"Resulting table shape: {combined_df.shape}")


if __name__ == "__main__":
    main()

