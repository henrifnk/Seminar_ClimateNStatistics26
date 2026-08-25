"""Redraw slide-22 figure as a classic point + confidence-interval plot.

Geometry (interval bounds and point estimates) was recovered from the original
rendered figure; pixel -> year calibration uses the x-axis ticks
(x = 52 px -> 2000, x = 1830 px -> 2100).
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

# (lo_px, est_px, hi_px, kind) recovered from the original figure
BANDS = [
    (1307, 1801, 1830, "forecast"),
    (1190, 1659, 1830, "forecast"),
    (1069, 1487, 1830, "forecast"),
    (859, 1397, 1830, "forecast"),
    (880, 1287, 1693, "forecast"),
    (890, 1229, 1567, "forecast"),
    (743, 1125, 1507, "forecast"),
    (710, 1017, 1322, "forecast"),
    (608, 896, 1182, "forecast"),
    (526, 803, 1078, "forecast"),
    (525, 756, 985, "forecast"),
    (431, 701, 970, "forecast"),
    (449, 674, 898, "forecast"),
    (380, 583, 786, "forecast"),
    (332, 537, 742, "forecast"),
    (306, 456, 605, "forecast"),
    (284, 435, 585, "forecast"),
    (205, 357, 508, "observed"),
    (184, 299, 413, "observed"),
]

X0, Y0, X1, Y1 = 52.0, 2000.0, 1830.0, 2100.0
px2yr = lambda x: Y0 + (x - X0) * (Y1 - Y0) / (X1 - X0)

COL = {"forecast": "#C4699F", "observed": "#1F6F8B"}
rows = [(px2yr(lo), px2yr(est), px2yr(hi), k) for lo, est, hi, k in BANDS]
rows.sort(key=lambda r: r[1])                      # earliest at the bottom

fig, ax = plt.subplots(figsize=(9.415, 4.0), dpi=200)

for i, (lo, est, hi, kind) in enumerate(rows):
    c = COL[kind]
    ax.errorbar(est, i, xerr=[[est - lo], [hi - est]], fmt="o",
                color=c, ecolor=c, elinewidth=1.6, capsize=3.5,
                capthick=1.6, markersize=5.5, markeredgecolor="white",
                markeredgewidth=0.6, zorder=3)

ax.axvline(2020, color="0.5", lw=1.1, ls="--", zorder=1)

ax.set_xlim(2000, 2100)
ax.set_ylim(-1.2, len(rows) + 0.2)
ax.set_xticks(range(2000, 2101, 20))
ax.set_yticks([])
ax.tick_params(axis="x", labelsize=11, length=4)
ax.set_xlabel("Emergence year  ( |SNR| $\\geq$ 1 )", fontsize=11, labelpad=6)
for s in ("top", "right", "left"):
    ax.spines[s].set_visible(False)
ax.spines["bottom"].set_color("black")

ax.set_title("Each emergence year carries a wide error bar ($\\pm$10–20 yr)",
             fontsize=11.5, fontweight="bold", loc="left", pad=10)

handles = [
    Line2D([], [], color=COL["observed"], marker="o", markersize=5.5,
           markeredgecolor="white", lw=1.6, label="Observed river ($\\leq$2020)"),
    Line2D([], [], color=COL["forecast"], marker="o", markersize=5.5,
           markeredgecolor="white", lw=1.6, label="Forecast river (extrapolated)"),
]
ax.legend(handles=handles, loc="lower right", frameon=False, fontsize=9.5,
          handlelength=2.2, borderaxespad=0.4)

fig.tight_layout()
fig.savefig("slide/images/emergence_year_uncertainty.png", dpi=200,
            facecolor="white")
