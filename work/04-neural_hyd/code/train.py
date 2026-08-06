from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


def run_step(python_exe: str, script_name: str, cwd: Path) -> None:
    cmd = [python_exe, script_name]
    print(f"[train] running: {' '.join(cmd)}")
    subprocess.run(cmd, cwd=cwd, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description="Train EA-LSTM pipeline (pretrain + group finetune).")
    parser.add_argument("--python-exe", default=sys.executable, help="Python executable used for core scripts.")
    args = parser.parse_args()

    core_dir = Path(__file__).resolve().parent / "90_core" / "rq3_finetune_full"
    if not core_dir.exists():
        raise FileNotFoundError(f"Core directory not found: {core_dir}")

    print(f"[train] core directory: {core_dir}")
    run_step(args.python_exe, "run_pretrain_ea.py", core_dir)
    run_step(args.python_exe, "run_folder_group_compare_ea.py", core_dir)
    print("[train] done")


if __name__ == "__main__":
    main()
