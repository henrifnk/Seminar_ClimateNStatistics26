#!/usr/bin/env python3
# =============================================================================
# Publication-style restyling of the five Results figures, without recomputing
# any analysis. Reads stl_snr_forecast_results.csv and
# seasonal_snr_emergence_results.csv, writes to figures/.
#
# Style: Okabe-Ito colourblind-safe palette, direct line labelling (no legend
# boxes), top/right spines removed, y-grid only, honest step curves for
# cumulative counts, observed vs forecast periods visually separated.
# =============================================================================
import csv
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

YEAR_MIN, NOW, YEAR_MAX = 1981, 2020, 2100
REGIONS = ["Jura","Black Forest","Plateau","Alps North","Alps West","Alps South","Alps East"]
SEASONS = ["DJF","MAM","JJA","SON"]
SEASON_LABEL = {"DJF":"DJF (winter)","MAM":"MAM (spring)","JJA":"JJA (summer)","SON":"SON (autumn)"}

# Okabe-Ito
OI = {"blue":"#0072B2","orange":"#E69F00","green":"#009E73","vermillion":"#D55E00",
      "purple":"#CC79A7","sky":"#56B4E9","yellow":"#F0E442","black":"#000000"}
# Region palette matched to the slide-9 bio-geographic map (ColorBrewer BrBG,
# colourblind-safe teal<->brown diverging). Teal side = Jura/Black Forest/Plateau,
# brown side = the Alps. Alps West gets a distinct gold since the map paints
# North and West the same brown.
REGION_C = {"Jura":"#7FCDC1", "Black Forest":"#35978F", "Plateau":"#01665E",
            "Alps South":"#DFC27E", "Alps West":"#E8A33D",
            "Alps North":"#BF812D", "Alps East":"#8C510A"}
SEASON_C = {"DJF":OI["blue"], "MAM":OI["green"], "JJA":OI["orange"], "SON":OI["purple"]}
UP_C, DN_C = OI["vermillion"], OI["blue"]

plt.rcParams.update({
    "figure.dpi": 100, "savefig.dpi": 200,
    "axes.spines.top": False, "axes.spines.right": False,
    "axes.linewidth": 0.8, "axes.edgecolor": "#444444",
    "xtick.color": "#444444", "ytick.color": "#444444",
    "text.color": "#222222", "axes.labelcolor": "#222222",
    "font.size": 13,
})

def despine_grid(ax):
    ax.grid(axis="y", alpha=0.28, lw=0.6)
    ax.set_axisbelow(True)

def spread_labels(vals, min_gap):
    """Return adjusted y positions so labels don't overlap (input order kept)."""
    idx = np.argsort(vals)[::-1]
    pos = np.array(vals, dtype=float)
    for a, b in zip(idx[:-1], idx[1:]):
        if pos[a] - pos[b] < min_gap:
            pos[b] = pos[a] - min_gap
    return pos

def cumulative_extent_plot(groups, colors, fname, min_gap=3.4):
    """groups: list of (label, n_total, sorted emergence years array)."""
    years = np.arange(YEAR_MIN, YEAR_MAX + 1)
    fig, ax = plt.subplots(figsize=(11.2, 5.6))
    ax.axvspan(YEAR_MIN, NOW, color="0.5", alpha=0.09, zorder=0)
    ends = []
    for label, tot, em in groups:
        frac = np.array([100 * np.sum(em <= t) / tot for t in years])
        ax.plot(years, frac, drawstyle="steps-post", color=colors[label], lw=2.4)
        ends.append(frac[-1])
    ypos = spread_labels(ends, min_gap)
    for (label, tot, _), y in zip(groups, ypos):
        ax.annotate(f"{label}  (n={tot})", xy=(YEAR_MAX + 1.2, y),
                    va="center", ha="left", fontsize=11.5,
                    color=colors[label], fontweight="bold",
                    annotation_clip=False)
    ax.axvline(NOW, color="0.35", lw=1.0, ls=(0, (4, 3)))
    ax.text(NOW - 1.5, 97, "observed", ha="right", va="top", fontsize=10.5, color="0.35")
    ax.text(NOW + 1.5, 97, "Theil–Sen forecast", ha="left", va="top",
            fontsize=10.5, color="0.35")
    ax.set_xlim(YEAR_MIN, YEAR_MAX); ax.set_ylim(0, 100)
    ax.set_xticks([1981, 2000, 2020, 2040, 2060, 2080, 2100])
    ax.set_xlabel("Year")
    ax.set_ylabel("Rivers with emerged signal (%)   ($|\\mathrm{SNR}| \\geq 1$)")
    despine_grid(ax)
    fig.subplots_adjust(left=0.075, right=0.80, top=0.97, bottom=0.115)
    fig.savefig(fname); plt.close(fig); print("saved", fname)

# ---------------------------------------------------------------- data ------
annual = [r for r in csv.DictReader(open("results/stl_snr_forecast_results.csv"))
          if r["insufficient"] != "True"]
seas_all = [r for r in csv.DictReader(open("results/seasonal_snr_emergence_results.csv"))
            if r["insufficient"] != "True"]
seas_em = [r for r in seas_all if r["emerge_type"] in ("observed", "forecast")]

# ---- FIG 1: emerged extent by region (annual STL route) ---------------------
groups = []
for reg in REGIONS:
    sub = [r for r in annual if r["region"] == reg]
    em = np.array(sorted(int(float(r["snr1_year"])) for r in sub
                         if r["snr1_type"] in ("observed", "forecast") and r["snr1_year"]))
    groups.append((reg, len(sub), em))
cumulative_extent_plot(groups, REGION_C, "figures/emergence_extent_by_region.png")

# ---- FIG 2: emerged extent by season ----------------------------------------
groups = []
for s in SEASONS:
    sub = [r for r in seas_all if r["season"] == s]
    em = np.array(sorted(int(float(r["emerge_year"])) for r in sub
                         if r["emerge_type"] in ("observed", "forecast") and r["emerge_year"]))
    groups.append((SEASON_LABEL[s], len(sub), em))
cumulative_extent_plot(groups, {SEASON_LABEL[s]: SEASON_C[s] for s in SEASONS},
                       "figures/seasonal_emergence_extent_curves.png", min_gap=4.2)

# ---- FIG 3: season x region heatmap -----------------------------------------
mat = np.full((len(SEASONS), len(REGIONS)), np.nan)
cnt = np.zeros_like(mat, dtype=int)
for i, s in enumerate(SEASONS):
    for j, reg in enumerate(REGIONS):
        sub = [r for r in seas_all if r["season"] == s and r["region"] == reg]
        if not sub: continue
        em = sum(1 for r in sub if r["emerge_type"] in ("observed", "forecast"))
        mat[i, j] = 100 * em / len(sub); cnt[i, j] = len(sub)

fig, ax = plt.subplots(figsize=(11, 4.9))
im = ax.imshow(mat, cmap="YlGnBu", vmin=0, vmax=100, aspect="auto")
# white cell separators
ax.set_xticks(np.arange(-0.5, len(REGIONS)), minor=True)
ax.set_yticks(np.arange(-0.5, len(SEASONS)), minor=True)
ax.grid(which="minor", color="white", lw=2.2)
ax.tick_params(which="minor", length=0)
ax.set_xticks(range(len(REGIONS)))
ax.set_xticklabels(REGIONS, fontsize=13)
ax.set_yticks(range(len(SEASONS)))
ax.set_yticklabels([SEASON_LABEL[s].replace(" (", "\n(") for s in SEASONS], fontsize=13)
for i in range(len(SEASONS)):
    for j in range(len(REGIONS)):
        if np.isnan(mat[i, j]): continue
        dark = mat[i, j] > 55
        ax.text(j, i, f"{mat[i, j]:.0f}%", ha="center", va="center",
                fontsize=15, fontweight="bold", color="white" if dark else "#1a3a5c")
for sp in ax.spines.values(): sp.set_visible(False)
cb = fig.colorbar(im, ax=ax, fraction=0.035, pad=0.015)
cb.set_label("Rivers emerged by 2100 (%)", fontsize=12)
cb.ax.tick_params(labelsize=11)
cb.outline.set_visible(False)
fig.tight_layout()
fig.savefig("figures/seasonal_emergence_heatmap.png"); plt.close(fig)
print("saved figures/seasonal_emergence_heatmap.png")

# ---- FIG 4: direction per season (horizontal, same style as FIG 5 by-region) -
# denominator = ALL analysable rivers of the season; bars = % of that season's
# rivers that emerge by 2100 (|SNR|>=1) with rising (increase) / falling (decrease)
# direction. Top Increase/Decrease legend, orange right / blue left, like FIG 5.
tot = [max(sum(1 for r in seas_all if r["season"] == s), 1) for s in SEASONS]
up = [100 * sum(1 for r in seas_em if r["season"] == s and r["direction"] == "increase") / tot[i]
      for i, s in enumerate(SEASONS)]
dn = [100 * sum(1 for r in seas_em if r["season"] == s and r["direction"] == "decrease") / tot[i]
      for i, s in enumerate(SEASONS)]
x = np.arange(len(SEASONS))
fig, ax = plt.subplots(figsize=(9.6, 5.0))
ax.bar(x, up, 0.56, color=UP_C, edgecolor="white", lw=0.8)
ax.bar(x, [-d for d in dn], 0.56, color=DN_C, edgecolor="white", lw=0.8)
for xi, (u, d) in enumerate(zip(up, dn)):
    ax.text(xi, u + 1.5, f"{u:.0f}", ha="center", va="bottom", fontsize=12,
            fontweight="bold", color=UP_C)
    ax.text(xi, -d - 1.5, f"{d:.0f}", ha="center", va="top", fontsize=12,
            fontweight="bold", color=DN_C)
ax.axhline(0, color="#444444", lw=0.9)
ax.set_xticks(x); ax.set_xticklabels([SEASON_LABEL[s] for s in SEASONS], fontsize=13)
ax.set_ylim(-80, 52)
ax.set_yticks([-75, -50, -25, 0, 25, 50]); ax.set_yticklabels([75, 50, 25, 0, 25, 50], fontsize=11)
ax.set_ylabel("% of season's rivers   ($|\\mathrm{SNR}| \\geq 1$)", fontsize=12)
ax.grid(axis="y", alpha=0.22, lw=0.6); ax.set_axisbelow(True)
ax.spines["top"].set_visible(False); ax.spines["right"].set_visible(False)
ax.spines["bottom"].set_visible(False); ax.tick_params(axis="x", length=0, pad=8)
handles = [plt.Rectangle((0, 0), 1, 1, color=UP_C),
           plt.Rectangle((0, 0), 1, 1, color=DN_C)]
fig.legend(handles, ["Increase", "Decrease"], loc="upper center",
           bbox_to_anchor=(0.5, 1.02), fontsize=12, frameon=False, ncol=2)
fig.tight_layout(rect=[0, 0, 1, 0.94])
fig.savefig("figures/seasonal_direction_bars.png"); plt.close(fig)
print("saved figures/seasonal_direction_bars.png")

# ---- FIG 5: direction by region, 4 panels -----------------------------------
fig, axes = plt.subplots(1, len(SEASONS), figsize=(12.4, 5.2), sharey=True)
yreg = np.arange(len(REGIONS))
for k, s in enumerate(SEASONS):
    ax = axes[k]
    tot = [max(sum(1 for r in seas_all if r["season"] == s and r["region"] == reg), 1)
           for reg in REGIONS]
    ups = [100 * sum(1 for r in seas_em if r["season"] == s and r["region"] == reg
                     and r["direction"] == "increase") / tot[i]
           for i, reg in enumerate(REGIONS)]
    dns = [100 * sum(1 for r in seas_em if r["season"] == s and r["region"] == reg
                     and r["direction"] == "decrease") / tot[i]
           for i, reg in enumerate(REGIONS)]
    ax.barh(yreg, ups, 0.62, color=UP_C, edgecolor="white", lw=0.6)
    ax.barh(yreg, [-d for d in dns], 0.62, color=DN_C, edgecolor="white", lw=0.6)
    for i, (u, d) in enumerate(zip(ups, dns)):
        if u > 85: ax.text(u - 5, i, f"{u:.0f}", va="center", ha="right",
                           fontsize=9.5, color="white")
        elif u > 0: ax.text(u + 4, i, f"{u:.0f}", va="center", ha="left",
                            fontsize=9.5, color="#555555")
        if d > 85: ax.text(-d + 5, i, f"{d:.0f}", va="center", ha="left",
                           fontsize=9.5, color="white")
        elif d > 0: ax.text(-d - 4, i, f"{d:.0f}", va="center", ha="right",
                            fontsize=9.5, color="#555555")
    ax.axvline(0, color="#444444", lw=0.9)
    ax.set_title(SEASON_LABEL[s], fontsize=13.5, pad=8)
    ax.set_yticks(yreg)
    if k == 0: ax.set_yticklabels(REGIONS, fontsize=12.5)
    ax.set_xlim(-112, 112)
    ax.set_xticks([-100, -50, 0, 50, 100])
    ax.set_xticklabels(["100", "50", "0", "50", "100"], fontsize=10.5)
    ax.set_xlabel("% of region's rivers", fontsize=11)
    ax.grid(axis="x", alpha=0.22, lw=0.6); ax.set_axisbelow(True)
    ax.tick_params(axis="y", length=0)
    ax.spines["left"].set_visible(False)
    ax.invert_yaxis()
handles = [plt.Rectangle((0, 0), 1, 1, color=UP_C),
           plt.Rectangle((0, 0), 1, 1, color=DN_C)]
fig.legend(handles, ["Increase", "Decrease"], loc="upper right",
           bbox_to_anchor=(0.995, 1.02), fontsize=12, frameon=False, ncol=2)
fig.tight_layout(rect=[0, 0, 1, 0.94])
fig.savefig("figures/seasonal_direction_by_region.png"); plt.close(fig)
print("saved figures/seasonal_direction_by_region.png")
