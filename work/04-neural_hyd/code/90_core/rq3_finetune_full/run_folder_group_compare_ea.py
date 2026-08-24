# -*- coding: utf-8 -*-
"""
run_folder_group_compare_ea.py — Regional local vs finetune comparison for 18 groups (EA-LSTM)

Pipeline for each group:
  1. Generate scratch_folder_XX_ea.yml and finetune_folder_XX_ea.yml from templates
  2. Train local model from scratch on group XX basins
  3. Finetune pretrained global model on group XX basins
  4. Evaluate both and compare results

Usage:
    python run_folder_group_compare_ea.py                    # Run all groups
    python run_folder_group_compare_ea.py --start-group 05   # Resume from group 05
    python run_folder_group_compare_ea.py --force-rerun      # Ignore existing results
"""

from pathlib import Path
import argparse
import json
import re
import pandas as pd

from neuralhydrology.nh_run import start_run, finetune, eval_run

WORK = Path(r"C:/GitHub/climate-change/NeuralHydrology/rq3_finetune")
BASE_RUNS_ROOT = Path(r"C:/GitHub/climate-change/runs")
LOCAL_RUNS_ROOT = BASE_RUNS_ROOT
DAYMET_ROOT = Path(r"C:/GitHub/climate-change/NeuralHydrology/data/CAMELS_US/basin_mean_forcing/daymet")
STATE_FILE = WORK / "folder_groups_checkpoint_ea.json"
SEED = 1001

LOCAL_TEMPLATE = WORK / "scratch_region01_same_params_ea.yml"
FT_TEMPLATE = WORK / "finetune_region01_same_params_ea.yml"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run folder-group local vs finetune comparison (EA-LSTM) with resume support."
    )
    parser.add_argument(
        "--start-group",
        type=str,
        default=None,
        help="Start from this group id (e.g., 04). Groups before it are skipped.",
    )
    parser.add_argument(
        "--force-rerun",
        action="store_true",
        help="Do not skip groups even if result CSV already exists.",
    )
    return parser.parse_args()


def newest(prefix: str, root: Path) -> Path:
    cands = sorted(
        [p for p in root.glob(f"{prefix}*") if p.is_dir()], key=lambda p: p.stat().st_mtime
    )
    if not cands:
        raise FileNotFoundError(f"No runs matching {prefix} in {root}")
    return cands[-1]


def write_cfg_with_updates(base_cfg: Path, out_cfg: Path, updates: dict):
    lines = base_cfg.read_text(encoding="utf-8").splitlines()
    written = {k: False for k in updates}
    out_lines = []

    for line in lines:
        replaced = False
        for key, value in updates.items():
            if line.startswith(f"{key}:"):
                out_lines.append(f"{key}: {value}")
                written[key] = True
                replaced = True
                break
        if not replaced:
            out_lines.append(line)

    for key, value in updates.items():
        if not written[key]:
            out_lines.append(f"{key}: {value}")

    out_cfg.write_text("\n".join(out_lines).rstrip() + "\n", encoding="utf-8")


def load_metrics(run_dir: Path) -> pd.DataFrame:
    test_epochs = sorted((run_dir / "test").glob("model_epoch*"))
    if not test_epochs:
        raise FileNotFoundError(f"No test epochs in {run_dir}")
    csv_path = test_epochs[-1] / "test_metrics.csv"
    return pd.read_csv(csv_path, dtype={"basin": str})[["basin", "NSE", "KGE"]]


def load_state(path: Path) -> dict:
    if not path.exists():
        return {"completed_groups": [], "current_group": None, "status": "idle", "history": []}
    return json.loads(path.read_text(encoding="utf-8"))


def save_state(path: Path, state: dict):
    path.write_text(json.dumps(state, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")


def get_completed_groups_from_result_files() -> set[str]:
    done = set()
    for csv_path in WORK.glob("results_folder_??_local_vs_finetune_ea.csv"):
        m = re.match(r"results_folder_(\d{2})_local_vs_finetune_ea\.csv", csv_path.name)
        if m:
            done.add(m.group(1))
    return done


def build_summary_and_all_basins_outputs() -> tuple[Path, Path | None]:
    detail_paths = sorted(WORK.glob("results_folder_??_local_vs_finetune_ea.csv"))

    summary_rows = []
    all_detail_rows = []

    for detail_path in detail_paths:
        m = re.match(r"results_folder_(\d{2})_local_vs_finetune_ea\.csv", detail_path.name)
        if not m:
            continue
        group = m.group(1)
        merged = pd.read_csv(detail_path, dtype={"basin": str})
        if merged.empty:
            continue

        summary_rows.append(
            {
                "group": group,
                "n_basins": int(len(merged)),
                "seed": int(merged["seed"].iloc[0]) if "seed" in merged.columns else SEED,
                "local_nse_mean": float(merged["local_nse"].mean()),
                "ft_nse_mean": float(merged["ft_nse"].mean()),
                "delta_nse_mean": float(merged["delta_nse_ft_minus_local"].mean()),
                "local_kge_mean": float(merged["local_kge"].mean()),
                "ft_kge_mean": float(merged["ft_kge"].mean()),
                "delta_kge_mean": float(merged["delta_kge_ft_minus_local"].mean()),
                "nse_improved_count": int((merged["delta_nse_ft_minus_local"] > 0).sum()),
                "kge_improved_count": int((merged["delta_kge_ft_minus_local"] > 0).sum()),
                "detail_csv": str(detail_path),
            }
        )
        all_detail_rows.append(merged)

    summary_df = pd.DataFrame(summary_rows).sort_values("group") if summary_rows else pd.DataFrame()
    summary_out = WORK / "results_folder_groups_summary_ea.csv"
    summary_df.to_csv(summary_out, index=False)

    if all_detail_rows:
        all_detail = pd.concat(all_detail_rows, ignore_index=True)
        all_out = WORK / "results_folder_groups_all_basins_ea.csv"
        all_detail.to_csv(all_out, index=False)
    else:
        all_out = None

    return summary_out, all_out


args = parse_args()

# Find the latest EA-LSTM base global run (if exists) or cudalstm as fallback
try:
    base_global = newest("rq3_base_global_ea", BASE_RUNS_ROOT)
    print(f"Using EA-LSTM base global run: {base_global}")
except FileNotFoundError:
    print("Warning: No EA-LSTM base run found. Make sure to run pretrain_ea.yml first.")
    print("Attempting to use cudalstm base run as fallback...")
    try:
        base_global = newest("rq3_base_global", BASE_RUNS_ROOT)
        print(f"Using cudalstm base run (fallback): {base_global}")
    except FileNotFoundError:
        raise FileNotFoundError(
            "No base global run found. Please run pretrain_ea.yml or pretrain.yml first."
        )

folders = sorted([p for p in DAYMET_ROOT.iterdir() if p.is_dir()])
basin_list_dir = WORK / "basin_lists_by_folder"
basin_list_dir.mkdir(exist_ok=True)

state = load_state(STATE_FILE)
completed_groups = set(state.get("completed_groups", [])) | get_completed_groups_from_result_files()

if args.start_group is not None:
    print(f"Start-group requested: {args.start_group}")

print(f"Resume checkpoint file: {STATE_FILE}")
print(f"Already completed groups: {sorted(completed_groups)}")

for folder in folders:
    group = folder.name

    if args.start_group is not None and group < args.start_group:
        print(f"Skip group {group}: before start-group {args.start_group}")
        continue

    detail_out = WORK / f"results_folder_{group}_local_vs_finetune_ea.csv"
    if not args.force_rerun and (group in completed_groups or detail_out.exists()):
        print(f"Skip group {group}: already completed")
        completed_groups.add(group)
        state["completed_groups"] = sorted(completed_groups)
        state["current_group"] = None
        state["status"] = "running"
        save_state(STATE_FILE, state)
        continue

    print(f"\n{'=' * 80}")
    print(f"GROUP {group} (EA-LSTM)")
    print(f"{'=' * 80}")

    basins = []
    for f in folder.glob("*_lump_cida_forcing_leap.txt"):
        m = re.match(r"^(\d{8})_lump_cida_forcing_leap\.txt$", f.name)
        if m:
            basins.append(m.group(1))
    basins = sorted(set(basins))

    if not basins:
        print(f"Skip group {group}: no basin files")
        continue

    basin_file = basin_list_dir / f"basins_{group}.txt"
    basin_file.write_text("\n".join(basins) + "\n", encoding="utf-8")

    local_cfg = WORK / f"scratch_folder_{group}_ea.yml"
    ft_cfg = WORK / f"finetune_folder_{group}_ea.yml"

    write_cfg_with_updates(
        LOCAL_TEMPLATE,
        local_cfg,
        {
            "experiment_name": f"rq3_scratch_folder_{group}_ea",
            "train_basin_file": basin_file.as_posix(),
            "validation_basin_file": basin_file.as_posix(),
            "test_basin_file": basin_file.as_posix(),
            "seed": SEED,
        },
    )

    write_cfg_with_updates(
        FT_TEMPLATE,
        ft_cfg,
        {
            "experiment_name": f"rq3_finetuned_folder_{group}_ea",
            "train_basin_file": basin_file.as_posix(),
            "validation_basin_file": basin_file.as_posix(),
            "test_basin_file": basin_file.as_posix(),
            "seed": SEED,
            "base_run_dir": base_global.as_posix(),
        },
    )

    state["current_group"] = group
    state["status"] = "running"
    save_state(STATE_FILE, state)

    try:
        print(f"\n[1/{len(basins)}] Training local model...")
        start_run(local_cfg)
        local_run = newest(f"rq3_scratch_folder_{group}_ea", LOCAL_RUNS_ROOT)
        print(f"✓ Local run: {local_run.name}")

        print(f"\n[2/{len(basins)}] Finetuning on group {group}...")
        finetune(ft_cfg)
        ft_run = newest(f"rq3_finetuned_folder_{group}_ea", LOCAL_RUNS_ROOT)
        print(f"✓ Finetune run: {ft_run.name}")

        print(f"\n[3/{len(basins)}] Evaluating...")
        eval_run(local_run, period="test")
        eval_run(ft_run, period="test")

        local_df = load_metrics(local_run).rename(
            columns={"NSE": "local_nse", "KGE": "local_kge"}
        )
        ft_df = load_metrics(ft_run).rename(columns={"NSE": "ft_nse", "KGE": "ft_kge"})

        merged = local_df.merge(ft_df, on="basin", how="inner")
        merged["group"] = group
        merged["seed"] = SEED
        merged["delta_nse_ft_minus_local"] = merged["ft_nse"] - merged["local_nse"]
        merged["delta_kge_ft_minus_local"] = merged["ft_kge"] - merged["local_kge"]

        merged.to_csv(detail_out, index=False)

        completed_groups.add(group)
        state["completed_groups"] = sorted(completed_groups)
        state["current_group"] = None
        state["status"] = "running"
        state.setdefault("history", []).append(
            {
                "group": group,
                "n_basins": int(len(merged)),
                "detail_csv": str(detail_out),
                "local_run": str(local_run),
                "ft_run": str(ft_run),
                "delta_nse_mean": float(merged["delta_nse_ft_minus_local"].mean()),
                "delta_kge_mean": float(merged["delta_kge_ft_minus_local"].mean()),
            }
        )
        save_state(STATE_FILE, state)

        delta_nse = merged["delta_nse_ft_minus_local"].mean()
        delta_kge = merged["delta_kge_ft_minus_local"].mean()
        sign_nse = "+" if delta_nse > 0 else ""
        sign_kge = "+" if delta_kge > 0 else ""
        print(
            f"\n✓ Group {group} done | {len(merged):2d} basins | "
            f"Δ NSE: {sign_nse}{delta_nse:.4f} | Δ KGE: {sign_kge}{delta_kge:.4f}"
        )

    except Exception as e:
        state["status"] = "failed"
        state["current_group"] = group
        state["completed_groups"] = sorted(completed_groups)
        save_state(STATE_FILE, state)
        print(f"\n✗ Group {group} FAILED: {e}")
        raise

# Build summary
summary_out, all_out = build_summary_and_all_basins_outputs()
state["status"] = "idle"
state["current_group"] = None
state["completed_groups"] = sorted(completed_groups)
save_state(STATE_FILE, state)

print(f"\n{'=' * 80}")
print("SUMMARY: EA-LSTM FOLDER GROUPS")
print(f"{'=' * 80}")

summary_df = pd.read_csv(summary_out) if summary_out.exists() else pd.DataFrame()
if not summary_df.empty:
    print(
        summary_df[
            ["group", "n_basins", "local_nse_mean", "ft_nse_mean", "delta_nse_mean"]
        ].to_string(index=False)
    )
    print(f"\nWeighted mean Δ NSE: {(summary_df['delta_nse_mean'] * summary_df['n_basins']).sum() / summary_df['n_basins'].sum():.6f}")
    print(f"Weighted mean Δ KGE: {(summary_df['delta_kge_mean'] * summary_df['n_basins']).sum() / summary_df['n_basins'].sum():.6f}")
    print(f"Groups where FT beats Local (NSE): {(summary_df['delta_nse_mean'] > 0).sum()} / {len(summary_df)}")

print(f"\n📊 Saved summary:     {summary_out}")
if all_out is not None:
    print(f"📊 Saved all basins:  {all_out}")
print(f"📋 Checkpoint file:   {STATE_FILE}")
print("\n✅ EA-LSTM folder group comparison completed!")
