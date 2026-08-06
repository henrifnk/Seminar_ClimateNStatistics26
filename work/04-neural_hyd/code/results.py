from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path


def run_step(python_exe: str, script_name: str, cwd: Path) -> None:
    cmd = [python_exe, script_name]
    print(f"[results] running: {' '.join(cmd)}")
    subprocess.run(cmd, cwd=cwd, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description="Build and sync result CSV files.")
    parser.add_argument("--python-exe", default=sys.executable, help="Python executable used for core scripts.")
    args = parser.parse_args()

    code_dir = Path(__file__).resolve().parent
    core_dir = code_dir / "90_core" / "rq3_finetune_full"
    out_dir = code_dir.parent / "results"
    out_dir.mkdir(parents=True, exist_ok=True)

    if not core_dir.exists():
        raise FileNotFoundError(f"Core directory not found: {core_dir}")

    run_step(args.python_exe, "eval_global_true_by_group.py", core_dir)
    run_step(args.python_exe, "gen_three_way_comparison.py", core_dir)

    required = [
        "results_folder_groups_summary_with_global_true_ea.csv",
        "results_folder_groups_all_basins_ea.csv",
        "results_folder_05_local_vs_finetune_ea.csv",
        "results_folder_05_global_eval_true_ea.csv",
        "results_folder_groups_summary_with_global_true.csv",
        "results_folder_groups_all_basins.csv",
        "results_folder_01_local_vs_finetune.csv",
        "results_folder_01_global_eval_true.csv",
    ]

    for name in required:
        src = core_dir / name
        dst = out_dir / name
        if src.exists():
            shutil.copy2(src, dst)
            print(f"[results] copied: {name}")
        else:
            print(f"[results] missing: {name}")

    print(f"[results] done, output dir: {out_dir}")


if __name__ == "__main__":
    main()
