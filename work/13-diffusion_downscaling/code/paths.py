"""
Central output locations.

Every figure any script in this repo renders lands somewhere under `plots/`, which is
gitignored. Paths are relative to the current working directory, i.e. run from the repo
root — the same convention the data paths use.

Layout:
    plots/
    ├── training/      per-epoch sample grids from TrainDiffusion
    ├── calibration/   CalibrationPeriodAssessment (held-out CERRA metrics)
    ├── projection/    ProjectionPeriodAssessment / ProjectionPlots (GWL climate deltas)
    │   └── 2_delta/   per-period delta panels
    ├── inference/     single-date downscaling figures
    └── diagnostics/   reserved for future forward-/reverse-process diagnostic scripts;
                        no script currently writes here
"""
from pathlib import Path

PLOTS_DIR = Path("plots")

TRAINING_DIR    = PLOTS_DIR / "training"
CALIBRATION_DIR = PLOTS_DIR / "calibration"
PROJECTION_DIR  = PLOTS_DIR / "projection"
INFERENCE_DIR   = PLOTS_DIR / "inference"
DIAGNOSTICS_DIR = PLOTS_DIR / "diagnostics"


def plot_dir(*parts) -> Path:
    """Return a directory under plots/, creating it (and any parents) if needed.

    plot_dir("diagnostics", f"forward_process_{date}") -> plots/diagnostics/forward_process_2019-01-05
    """
    p = PLOTS_DIR.joinpath(*(str(x) for x in parts))
    p.mkdir(parents=True, exist_ok=True)
    return p
