#!/usr/bin/env python3
# Replot the two seasonal figures (heatmap + extent curves) from
# seasonal_snr_emergence_results.csv without recomputing the analysis.
# Identical plotting code to seasonal_snr_emergence.py, sigma threshold notation.
import csv, math
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

RESULTS = "results/seasonal_snr_emergence_results.csv"
FIG_HEAT = "figures/seasonal_emergence_heatmap.png"
FIG_CURVE = "figures/seasonal_emergence_extent_curves.png"
YEAR_START, YEAR_END, FORECAST_HORIZON = 1981, 2020, 2100
SEASONS = ["DJF","MAM","JJA","SON"]
REGIONS = ["Jura","Black Forest","Plateau","Alps North","Alps West","Alps South","Alps East"]
SEASON_COLORS = {"DJF":"#0072B2","MAM":"#009E73","JJA":"#E69F00","SON":"#CC79A7"}

usable = [r for r in csv.DictReader(open(RESULTS)) if r["insufficient"] != "True"]

# ---- FIGURE A: season x region heatmap of % emerged (by 2100) ----
mat = np.full((len(SEASONS), len(REGIONS)), np.nan)
cnt = np.zeros((len(SEASONS), len(REGIONS)), dtype=int)
for i, s in enumerate(SEASONS):
    for j, reg in enumerate(REGIONS):
        sub = [r for r in usable if r["season"] == s and r["region"] == reg]
        if not sub: continue
        em = sum(1 for r in sub if r["emerge_type"] in ("observed","forecast"))
        mat[i,j] = 100*em/len(sub); cnt[i,j] = len(sub)
SEASON_YLABEL = {"DJF":"DJF\n(winter)","MAM":"MAM\n(spring)","JJA":"JJA\n(summer)","SON":"SON\n(autumn)"}
fig, ax = plt.subplots(figsize=(11,5.0))
im = ax.imshow(mat, cmap="YlOrRd", vmin=0, vmax=100, aspect="auto")
ax.set_xticks(range(len(REGIONS))); ax.set_xticklabels(REGIONS, rotation=20, ha="right", fontsize=15)
ax.set_yticks(range(len(SEASONS))); ax.set_yticklabels([SEASON_YLABEL[s] for s in SEASONS], fontsize=15)
for i in range(len(SEASONS)):
    for j in range(len(REGIONS)):
        if not np.isnan(mat[i,j]):
            ax.text(j, i, f"{mat[i,j]:.0f}%", ha="center", va="center",
                    fontsize=16, fontweight="bold", color="black" if mat[i,j] < 60 else "white")
cb = fig.colorbar(im, ax=ax); cb.set_label("Rivers emerged by 2100 (%)", fontsize=14)
cb.ax.tick_params(labelsize=12)
fig.tight_layout(); fig.savefig(FIG_HEAT, dpi=160); print("saved", FIG_HEAT)

# ---- FIGURE B: national emerged extent over time, one line per season ----
years = np.arange(YEAR_START, FORECAST_HORIZON+1)
fig, ax = plt.subplots(figsize=(11,6))
for s in SEASONS:
    sub = [r for r in usable if r["season"] == s]
    tot = len(sub)
    em = np.array(sorted(int(float(r["emerge_year"])) for r in sub
                         if r["emerge_type"] in ("observed","forecast") and r["emerge_year"] != ""))
    frac = np.array([100*np.sum(em <= t)/tot for t in years]) if tot > 0 else np.zeros(len(years))
    ax.plot(years, frac, color=SEASON_COLORS[s], lw=3.2, label=f"{s} (n={tot})")
ax.axvline(YEAR_END, color="grey", lw=1.2, linestyle="--")
ax.set_xlabel("Year", fontsize=15); ax.set_ylabel("Rivers with emerged signal (%)  (|SNR| ≥ 1)", fontsize=15)
ax.tick_params(labelsize=13)
ax.set_xlim(YEAR_START, FORECAST_HORIZON); ax.set_ylim(0,100); ax.grid(alpha=0.2)
ax.legend(fontsize=13, loc="upper left")
fig.tight_layout(); fig.savefig(FIG_CURVE, dpi=160); print("saved", FIG_CURVE)
