from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


LMU_BLUE = "#00558C"
LMU_LIGHT = "#6AB0DE"
GREY50 = "#888888"
GREEN_OK = "#388E3C"
AMBER = "#F57F17"


def must_read(results_dir: Path, filename: str) -> pd.DataFrame:
    path = results_dir / filename
    if not path.exists():
        raise FileNotFoundError(f"Missing required result file: {path}")
    return pd.read_csv(path)


def weighted_mean(values: pd.Series, weights: pd.Series) -> float:
    return float(np.average(values.to_numpy(dtype=float), weights=weights.to_numpy(dtype=float)))


def save_fig(fig: plt.Figure, fig_dir: Path, filename: str) -> None:
    out = fig_dir / filename
    fig.tight_layout()
    fig.savefig(out, dpi=300, facecolor="white")
    plt.close(fig)
    print(f"saved: {out}")


def ecdf(values: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    x = np.sort(values)
    y = np.arange(1, len(x) + 1) / len(x)
    return x, y


def main() -> None:
    code_dir = Path(__file__).resolve().parent
    project_dir = code_dir.parent
    results_dir = project_dir / "results"
    fig_dir = project_dir / "figures"
    fig_dir.mkdir(parents=True, exist_ok=True)

    plt.style.use("default")
    plt.rcParams.update(
        {
            "font.size": 11,
            "axes.titlesize": 12,
            "axes.labelsize": 11,
            "legend.frameon": False,
            "axes.spines.top": False,
            "axes.spines.right": False,
        }
    )

    rq3_group = must_read(results_dir, "results_folder_groups_summary_with_global_true_ea.csv")
    rq3_all = must_read(results_dir, "results_folder_groups_all_basins_ea.csv")
    g05_local_ft = must_read(results_dir, "results_folder_05_local_vs_finetune_ea.csv")
    g05_global = must_read(results_dir, "results_folder_05_global_eval_true_ea.csv")

    rq3_group_cuda = must_read(results_dir, "results_folder_groups_summary_with_global_true.csv")
    rq3_all_cuda = must_read(results_dir, "results_folder_groups_all_basins.csv")
    g01_local_ft_cuda = must_read(results_dir, "results_folder_01_local_vs_finetune.csv")
    g01_global_cuda = must_read(results_dir, "results_folder_01_global_eval_true.csv")

    rq3_group["group"] = rq3_group["group"].astype(int).map(lambda v: f"{v:02d}")
    rq3_all["group"] = rq3_all["group"].astype(int).map(lambda v: f"{v:02d}")
    g05_local_ft["basin"] = g05_local_ft["basin"].astype(int).map(lambda v: f"{v:08d}")
    g05_global["basin"] = g05_global["basin"].astype(int).map(lambda v: f"{v:08d}")
    g05_merged = g05_local_ft.merge(g05_global[["basin", "NSE", "KGE"]], on="basin", how="left")

    rq3_group_cuda["group"] = rq3_group_cuda["group"].astype(int).map(lambda v: f"{v:02d}")
    rq3_all_cuda["group"] = rq3_all_cuda["group"].astype(int).map(lambda v: f"{v:02d}")
    g01_local_ft_cuda["basin"] = g01_local_ft_cuda["basin"].astype(int).map(lambda v: f"{v:08d}")
    g01_global_cuda["basin"] = g01_global_cuda["basin"].astype(int).map(lambda v: f"{v:08d}")
    g01_merged_cuda = g01_local_ft_cuda.merge(
        g01_global_cuda[["basin", "NSE", "KGE"]], on="basin", how="left"
    )

    overall_local = weighted_mean(rq3_group["local_nse_mean"], rq3_group["n_basins"])
    overall_global = weighted_mean(rq3_group["global_nse_mean"], rq3_group["n_basins"])
    overall_ft = weighted_mean(rq3_group["ft_nse_mean"], rq3_group["n_basins"])

    overall_local_kge = weighted_mean(rq3_group["local_kge_mean"], rq3_group["n_basins"])
    overall_global_kge = weighted_mean(rq3_group["global_kge_mean"], rq3_group["n_basins"])
    overall_ft_kge = weighted_mean(rq3_group["ft_kge_mean"], rq3_group["n_basins"])

    overall_local_cuda = weighted_mean(rq3_group_cuda["local_nse_mean"], rq3_group_cuda["n_basins"])
    overall_global_cuda = weighted_mean(rq3_group_cuda["global_nse_mean"], rq3_group_cuda["n_basins"])
    overall_ft_cuda = weighted_mean(rq3_group_cuda["ft_nse_mean"], rq3_group_cuda["n_basins"])

    cov_df = rq3_group[["group", "n_basins"]].sort_values("n_basins", ascending=False)
    fig, ax = plt.subplots(figsize=(9.8, 3.8))
    ax.bar(cov_df["group"], cov_df["n_basins"], color=LMU_BLUE, width=0.68)
    for i, v in enumerate(cov_df["n_basins"].tolist()):
        ax.text(i, v + 2, str(int(v)), ha="center", va="bottom", fontsize=9)
    ax.set_xlabel("Geographic Group")
    ax.set_ylabel("Number of basins")
    ax.set_title("531 basins total across 18 Daymet-based groups")
    save_fig(fig, fig_dir, "01_coverage_bars.png")

    fig, ax = plt.subplots(figsize=(6.4, 2.6))
    vals = rq3_group["n_basins"].to_numpy(dtype=float)
    bins = np.arange(vals.min() - 7.5, vals.max() + 15, 15)
    ax.hist(vals, bins=bins, color=LMU_LIGHT, edgecolor="white")
    ax.axvline(np.median(vals), linestyle="--", color=GREY50, linewidth=1)
    ax.set_xlabel("Basins per group")
    ax.set_ylabel("Frequency")
    save_fig(fig, fig_dir, "02_group_size_distribution.png")

    models = ["Local", "Global", "Fine-tune"]
    nse_vals = [overall_local, overall_global, overall_ft]
    colors = [AMBER, LMU_LIGHT, LMU_BLUE]
    fig, ax = plt.subplots(figsize=(8.2, 3.3))
    bars = ax.bar(models, nse_vals, color=colors, width=0.58)
    for bar, v in zip(bars, nse_vals):
        ax.text(bar.get_x() + bar.get_width() / 2, v + 0.005, f"{v:.3f}", ha="center", fontweight="bold")
    ax.set_ylabel("Weighted mean NSE")
    save_fig(fig, fig_dir, "03_overall_weighted_nse.png")

    kge_vals = [overall_local_kge, overall_global_kge, overall_ft_kge]
    fig, ax = plt.subplots(figsize=(8.2, 3.3))
    bars = ax.bar(models, kge_vals, color=colors, width=0.58)
    for bar, v in zip(bars, kge_vals):
        ax.text(bar.get_x() + bar.get_width() / 2, v + 0.005, f"{v:.3f}", ha="center", fontweight="bold")
    ax.set_ylabel("Weighted mean KGE")
    save_fig(fig, fig_dir, "04_overall_weighted_kge.png")

    groups = sorted(rq3_group["group"].unique(), key=lambda x: int(x))
    idx = np.arange(len(groups))
    width = 0.24
    local_map = rq3_group.set_index("group")["local_nse_mean"].reindex(groups)
    global_map = rq3_group.set_index("group")["global_nse_mean"].reindex(groups)
    ft_map = rq3_group.set_index("group")["ft_nse_mean"].reindex(groups)
    fig, ax = plt.subplots(figsize=(10, 4.8))
    ax.bar(idx - width, local_map, width, label="Local", color=AMBER)
    ax.bar(idx, global_map, width, label="Global", color=LMU_LIGHT)
    ax.bar(idx + width, ft_map, width, label="Fine-tune", color=LMU_BLUE)
    ax.set_xticks(idx)
    ax.set_xticklabels(groups)
    ax.set_ylim(-0.12, 0.92)
    ax.set_xlabel("Group (numeric order)")
    ax.set_ylabel("Mean NSE")
    ax.legend(loc="lower center", bbox_to_anchor=(0.5, -0.25), ncols=3)
    save_fig(fig, fig_dir, "05_group_three_way_nse.png")

    delta_local = rq3_group.set_index("group")["delta_ft_vs_local_nse"].reindex(groups)
    delta_global = rq3_group.set_index("group")["delta_ft_vs_global_nse"].reindex(groups)
    fig, ax = plt.subplots(figsize=(9.8, 4.3))
    ax.bar(idx - width / 2, delta_local, width, label="FT - Local", color=LMU_BLUE)
    ax.bar(idx + width / 2, delta_global, width, label="FT - Global", color=GREEN_OK)
    ax.axhline(0, linestyle="--", color=GREY50, linewidth=1)
    ax.set_xticks(idx)
    ax.set_xticklabels(groups)
    ax.set_xlabel("Group")
    ax.set_ylabel("Delta NSE")
    ax.legend(loc="lower center", bbox_to_anchor=(0.5, -0.25), ncols=2)
    save_fig(fig, fig_dir, "06_group_delta_nse.png")

    dk_local = rq3_group["ft_kge_mean"] - rq3_group["local_kge_mean"]
    dk_global = rq3_group["ft_kge_mean"] - rq3_group["global_kge_mean"]
    dk_local = pd.Series(dk_local.to_numpy(), index=rq3_group["group"]).reindex(groups)
    dk_global = pd.Series(dk_global.to_numpy(), index=rq3_group["group"]).reindex(groups)
    fig, ax = plt.subplots(figsize=(9.6, 4.1))
    ax.bar(idx - width / 2, dk_local, width, label="FT - Local", color=LMU_BLUE)
    ax.bar(idx + width / 2, dk_global, width, label="FT - Global", color=GREEN_OK)
    ax.axhline(0, linestyle="--", color=GREY50, linewidth=1)
    ax.set_xticks(idx)
    ax.set_xticklabels(groups)
    ax.set_xlabel("Group")
    ax.set_ylabel("Delta KGE")
    ax.legend(loc="lower center", bbox_to_anchor=(0.5, -0.25), ncols=2)
    save_fig(fig, fig_dir, "07_group_delta_kge.png")

    fig, ax = plt.subplots(figsize=(8.9, 3.6))
    sizes = rq3_group["n_basins"].to_numpy(dtype=float)
    s = 30 + 2.0 * (sizes - sizes.min())
    ax.scatter(rq3_group["local_nse_mean"], rq3_group["ft_nse_mean"], s=s, c=LMU_BLUE, alpha=0.75)
    lims = [
        min(rq3_group["local_nse_mean"].min(), rq3_group["ft_nse_mean"].min()),
        max(rq3_group["local_nse_mean"].max(), rq3_group["ft_nse_mean"].max()),
    ]
    ax.plot(lims, lims, linestyle="--", color=GREY50)
    ax.set_xlabel("Local mean NSE")
    ax.set_ylabel("Fine-tune mean NSE")
    save_fig(fig, fig_dir, "08_local_vs_finetune_scatter.png")

    fig, ax = plt.subplots(figsize=(9.8, 4.2))
    local_vals = rq3_all["local_nse"].dropna().to_numpy(dtype=float)
    ft_vals = rq3_all["ft_nse"].dropna().to_numpy(dtype=float)
    bp = ax.boxplot([local_vals, ft_vals], labels=["Local", "Fine-tune"], patch_artist=True, widths=0.55)
    bp["boxes"][0].set_facecolor(AMBER)
    bp["boxes"][1].set_facecolor(LMU_BLUE)
    jitter_x_local = np.random.default_rng(42).normal(1, 0.04, len(local_vals))
    jitter_x_ft = np.random.default_rng(43).normal(2, 0.04, len(ft_vals))
    ax.scatter(jitter_x_local, local_vals, s=8, c=GREY50, alpha=0.15)
    ax.scatter(jitter_x_ft, ft_vals, s=8, c=GREY50, alpha=0.15)
    ax.set_ylabel("Basin-level NSE")
    ax.set_title("Distribution shifts upward after fine-tuning")
    save_fig(fig, fig_dir, "09_basin_boxplot_nse.png")

    fig, ax = plt.subplots(figsize=(9.6, 4.0))
    x_l, y_l = ecdf(local_vals)
    x_f, y_f = ecdf(ft_vals)
    ax.plot(x_l, y_l, color=AMBER, linewidth=1.8, label="Local")
    ax.plot(x_f, y_f, color=LMU_BLUE, linewidth=1.8, label="Fine-tune")
    ax.set_xlabel("NSE")
    ax.set_ylabel("Empirical CDF")
    ax.set_title("Fine-tune curve dominates across quantiles")
    ax.legend(loc="lower right")
    save_fig(fig, fig_dir, "10_basin_ecdf_nse.png")

    fig, ax = plt.subplots(figsize=(8.9, 3.4))
    x = rq3_group["local_nse_mean"].to_numpy(dtype=float)
    y = rq3_group["delta_ft_vs_local_nse"].to_numpy(dtype=float)
    ax.scatter(x, y, s=40, c=GREEN_OK)
    m, b = np.polyfit(x, y, 1)
    xx = np.linspace(x.min(), x.max(), 100)
    ax.plot(xx, m * xx + b, color=GREY50)
    ax.set_xlabel("Local mean NSE")
    ax.set_ylabel("Delta NSE (FT - Local)")
    save_fig(fig, fig_dir, "11_local_quality_vs_transfer_gain.png")

    labels = ["Local", "Global", "Fine-tune"]
    arch_vals = {
        "CudaLSTM": [overall_local_cuda, overall_global_cuda, overall_ft_cuda],
        "EA-LSTM": [overall_local, overall_global, overall_ft],
    }
    idx = np.arange(len(labels))
    width = 0.33
    fig, ax = plt.subplots(figsize=(10.0, 3.3))
    bars1 = ax.bar(idx - width / 2, arch_vals["CudaLSTM"], width, label="CudaLSTM", color=GREY50)
    bars2 = ax.bar(idx + width / 2, arch_vals["EA-LSTM"], width, label="EA-LSTM", color=LMU_BLUE)
    for bars in (bars1, bars2):
        for bar in bars:
            v = bar.get_height()
            ax.text(bar.get_x() + bar.get_width() / 2, v + 0.005, f"{v:.3f}", ha="center", fontsize=9)
    ax.set_xticks(idx)
    ax.set_xticklabels(labels)
    ax.set_ylabel("Weighted mean NSE")
    ax.legend(loc="lower center", bbox_to_anchor=(0.5, -0.32), ncols=2)
    save_fig(fig, fig_dir, "12_architecture_comparison_nse.png")

    g05_plot = g05_merged[["basin", "local_nse", "NSE", "ft_nse"]].copy()
    g05_plot.columns = ["basin", "Local", "Global", "Fine-tune"]
    g05_plot = g05_plot.sort_values("Fine-tune", ascending=False)
    basins = g05_plot["basin"].tolist()
    idx = np.arange(len(basins))
    width = 0.27
    fig, ax = plt.subplots(figsize=(10.4, 4.2))
    ax.bar(idx - width, g05_plot["Local"], width, label="Local", color=AMBER)
    ax.bar(idx, g05_plot["Global"], width, label="Global", color=GREY50)
    ax.bar(idx + width, g05_plot["Fine-tune"], width, label="Fine-tune", color=LMU_BLUE)
    ax.set_xticks(idx)
    ax.set_xticklabels(basins, rotation=75, ha="right", fontsize=8)
    ax.set_xlabel("Basin (sorted by Fine-tune NSE)")
    ax.set_ylabel("NSE")
    ax.set_title("Per-basin performance in Group 05")
    ax.legend(loc="lower center", bbox_to_anchor=(0.5, -0.35), ncols=3)
    save_fig(fig, fig_dir, "13_group05_basin_comparison.png")

    mask = rq3_all[["ft_nse", "global_nse"]].notna().all(axis=1)
    ea_basin_total = int(mask.sum())
    ea_basin_wins = int((rq3_all.loc[mask, "ft_nse"] > rq3_all.loc[mask, "global_nse"]).sum())
    ea_basin_losses = ea_basin_total - ea_basin_wins
    ea_win_rate = (ea_basin_wins / ea_basin_total) if ea_basin_total > 0 else 0.0
    fig, ax = plt.subplots(figsize=(9.6, 4.1))
    outcomes = ["FT > Global", "FT < Global"]
    counts = [ea_basin_wins, ea_basin_losses]
    bars = ax.bar(outcomes, counts, color=[LMU_BLUE, GREY50], width=0.6)
    for bar, v in zip(bars, counts):
        ax.text(bar.get_x() + bar.get_width() / 2, v + max(1, 0.02 * max(counts)), f"{v}", ha="center")
    ax.set_ylabel("Number of basins")
    ax.set_title(
        f"EA-LSTM basin-level win rate: {ea_basin_wins}/{ea_basin_total} = {100 * ea_win_rate:.2f}%"
    )
    save_fig(fig, fig_dir, "14_ea_basin_winrate_vs_global.png")

    print(f"done: all figures are in {fig_dir}")


if __name__ == "__main__":
    main()
