from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


def run_step(python_exe: str, script_name: str, cwd: Path) -> None:
    cmd = [python_exe, script_name]
    print(f"[train] running: {' '.join(cmd)}")
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
    parser = argparse.ArgumentParser(description="Train EA-LSTM pipeline (pretrain + group finetune).")
    parser.add_argument("--python-exe", default=None, help="Python executable used for core scripts.")
    args = parser.parse_args()

    code_dir = Path(__file__).resolve().parent
    python_exe = resolve_python_exe(args.python_exe, code_dir)
    core_dir = code_dir / "90_core" / "rq3_finetune_full"
    if not core_dir.exists():
        raise FileNotFoundError(f"Core directory not found: {core_dir}")

    print(f"[train] core directory: {core_dir}")
    print(f"[train] using python: {python_exe}")
    run_step(python_exe, "run_pretrain_ea.py", core_dir)
    run_step(python_exe, "run_folder_group_compare_ea.py", core_dir)
    print("[train] done")


if __name__ == "__main__":
    main()
