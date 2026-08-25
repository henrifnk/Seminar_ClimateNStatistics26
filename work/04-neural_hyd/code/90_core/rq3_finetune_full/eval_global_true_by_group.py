from __future__ import annotations

import re
from pathlib import Path

import pandas as pd
from neuralhydrology.nh_run import eval_run

WORK = Path(r"C:/GitHub/climate-change/NeuralHydrology/rq3_finetune")
BASE_RUN = Path(r"C:/GitHub/climate-change/runs/rq3_base_global_3006_154109")
CONFIG_PATH = BASE_RUN / "config.yml"
BASIN_DIR = WORK / "basin_lists_by_folder"
SUMMARY_PATH = WORK / "results_folder_groups_summary.csv"
OUT_GROUP_CSV = WORK / "results_folder_groups_global_eval_true.csv"
OUT_MERGED_CSV = WORK / "results_folder_groups_summary_with_global_true.csv"


def set_test_basin_file(config_text: str, basin_file: Path) -> str:
    # Match the two-line YAML style in NeuralHydrology run configs:
    # test_basin_file:\n  C:/path/to/file.txt
    pattern = re.compile(r"(^test_basin_file:\s*\n\s+.*$)", re.MULTILINE)
    replacement = f"test_basin_file:\n  {basin_file.as_posix()}"
    new_text, n = pattern.subn(replacement, config_text, count=1)
    if n != 1:
        raise RuntimeError("Could not uniquely update 'test_basin_file' in config.yml")
    return new_text


def read_test_metrics(epoch: int = 3) -> pd.DataFrame:
    metrics_csv = BASE_RUN / "test" / f"model_epoch{epoch:03d}" / "test_metrics.csv"
    if not metrics_csv.exists():
        raise FileNotFoundError(f"Missing metrics file: {metrics_csv}")
    return pd.read_csv(metrics_csv, dtype={"basin": str})


def main() -> None:
    print("=== True Global Evaluation By Folder Group ===")
    print(f"Base run: {BASE_RUN}")

    groups = [f"{i:02d}" for i in range(1, 19)]
    original_cfg = CONFIG_PATH.read_text(encoding="utf-8")

    rows = []
    try:
        for group in groups:
            basin_file = BASIN_DIR / f"basins_{group}.txt"
            if not basin_file.exists():
                raise FileNotFoundError(f"Missing basin list for group {group}: {basin_file}")

            basin_ids = {
                line.strip() for line in basin_file.read_text(encoding="utf-8").splitlines() if line.strip()
            }
            if not basin_ids:
                raise RuntimeError(f"Group {group} basin list is empty: {basin_file}")

            print(f"\n[{group}] evaluating {len(basin_ids)} basins...")

            updated_cfg = set_test_basin_file(original_cfg, basin_file)
            CONFIG_PATH.write_text(updated_cfg, encoding="utf-8")

            # Evaluate the fixed global model on this group's test basin file.
            eval_run(BASE_RUN, period="test", epoch=3)

            df = read_test_metrics(epoch=3)
            df = df[df["basin"].isin(basin_ids)].copy()
            if df.empty:
                raise RuntimeError(f"Group {group}: no matching basins found in test_metrics.csv")

            per_group_out = WORK / f"results_folder_{group}_global_eval_true.csv"
            df.to_csv(per_group_out, index=False)

            nse_mean = float(df["NSE"].mean())
            kge_mean = float(df["KGE"].mean())
            print(f"[{group}] NSE={nse_mean:.6f}, KGE={kge_mean:.6f}, basins={len(df)}")

            rows.append(
                {
                    "group": group,
                    "n_basins_global_eval": int(len(df)),
                    "global_nse_mean": nse_mean,
                    "global_kge_mean": kge_mean,
                    "global_detail_csv": str(per_group_out),
                }
            )

    finally:
        # Always restore the base run config no matter success/failure.
        CONFIG_PATH.write_text(original_cfg, encoding="utf-8")
        print("\nRestored original base run config.yml")

    global_df = pd.DataFrame(rows).sort_values("group")
    global_df.to_csv(OUT_GROUP_CSV, index=False)
    print(f"Saved true global per-group summary: {OUT_GROUP_CSV}")

    summary = pd.read_csv(SUMMARY_PATH, dtype={"group": str})
    summary["group"] = summary["group"].str.zfill(2)

    merged = summary.merge(global_df, on="group", how="left")
    merged["delta_global_vs_local_nse"] = merged["global_nse_mean"] - merged["local_nse_mean"]
    merged["delta_ft_vs_global_nse"] = merged["ft_nse_mean"] - merged["global_nse_mean"]
    merged["delta_ft_vs_local_nse"] = merged["ft_nse_mean"] - merged["local_nse_mean"]

    merged.to_csv(OUT_MERGED_CSV, index=False)
    print(f"Saved merged 3-way summary: {OUT_MERGED_CSV}")

    print("\n=== Quick Check (NSE means) ===")
    print(
        merged[
            [
                "group",
                "n_basins",
                "local_nse_mean",
                "global_nse_mean",
                "ft_nse_mean",
                "delta_ft_vs_global_nse",
            ]
        ].to_string(index=False)
    )


if __name__ == "__main__":
    main()
