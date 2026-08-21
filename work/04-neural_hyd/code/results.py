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


def has_neuralhydrology(python_exe: str) -> bool:
    probe = [python_exe, "-c", "import neuralhydrology"]
    result = subprocess.run(probe, capture_output=True, text=True)
    return result.returncode == 0


def resolve_python_exe(requested: str | None, code_dir: Path) -> str:
    if requested:
        return requested

    candidates: list[Path] = []
    candidates.append(Path(sys.executable))

    try:
        github_root = code_dir.parents[3]
        candidates.append(github_root / "climate-change" / ".venv" / "Scripts" / "python.exe")
    except IndexError:
        pass

    for cand in candidates:
        if cand.exists() and has_neuralhydrology(str(cand)):
            return str(cand)

    tried = "\n".join(str(c) for c in candidates)
    raise RuntimeError(
        "Cannot find a Python interpreter with 'neuralhydrology' installed. "
        "Use --python-exe to specify one explicitly. Tried:\n"
        f"{tried}"
    )


def main() -> None:
    parser = argparse.ArgumentParser(description="Build and sync result CSV files.")
    parser.add_argument("--python-exe", default=None, help="Python executable used for core scripts.")
    args = parser.parse_args()

    code_dir = Path(__file__).resolve().parent
    python_exe = resolve_python_exe(args.python_exe, code_dir)
    core_dir = code_dir / "90_core" / "rq3_finetune_full"
    out_dir = code_dir.parent / "results"
    out_dir.mkdir(parents=True, exist_ok=True)

    if not core_dir.exists():
        raise FileNotFoundError(f"Core directory not found: {core_dir}")

    print(f"[results] using python: {python_exe}")
    run_step(python_exe, "eval_global_true_by_group.py", core_dir)
    run_step(python_exe, "gen_three_way_comparison.py", core_dir)

    # Some core scripts write outputs to the climate-change repo path.
    source_dirs = [core_dir]
    try:
        github_root = code_dir.parents[3]
        source_dirs.append(github_root / "climate-change" / "NeuralHydrology" / "rq3_finetune")
    except IndexError:
        pass

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
        dst = out_dir / name
        found_src = None
        for src_dir in source_dirs:
            candidate = src_dir / name
            if candidate.exists():
                found_src = candidate
                break

        if found_src is not None:
            shutil.copy2(found_src, dst)
            print(f"[results] copied: {name} (from {found_src.parent})")
        else:
            print(f"[results] missing: {name}")

    print(f"[results] done, output dir: {out_dir}")


if __name__ == "__main__":
    main()
