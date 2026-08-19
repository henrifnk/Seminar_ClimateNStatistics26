#!/usr/bin/env python3
# =============================================================================
# Emerged extent over time, by bio-geographic region (CAMELS-CH, obs).
# For each region: cumulative fraction of rivers whose SNR signal has emerged
# (|SNR| >= 1, either direction) by year t, from 1981 to 2100.
# Follows Collins (2021) "emerged extent as a function of time".
# Uses stl_snr_forecast_results.csv (STL-based explicit SNR, SNR=1 threshold).
# =============================================================================
import csv, math
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

RESULTS = "results/stl_snr_forecast_results.csv"
OUT_FIG = "figures/emergence_extent_by_region.png"
OUT_CSV = "results/emergence_extent_by_region.csv"
YEAR_MIN, YEAR_MAX, NOW = 1981, 2100, 2020
REGIONS = ["Jura","Black Forest","Plateau","Alps North","Alps West","Alps South","Alps East"]
COLORS = {"Jura":"#1b9e77","Black Forest":"#0b3d2e","Plateau":"#7fc8b8",
          "Alps North":"#d4a017","Alps West":"#e0c068","Alps South":"#c77f3a","Alps East":"#5c3a1a"}

def main():
    rows=[r for r in csv.DictReader(open(RESULTS)) if r["insufficient"]!="True"]
    years=np.arange(YEAR_MIN, YEAR_MAX+1)

    # per region: total usable rivers, and each river's emergence year (SNR>=1, any direction)
    region_total={reg:0 for reg in REGIONS}
    region_emyears={reg:[] for reg in REGIONS}
    for r in rows:
        reg=r["region"]
        if reg not in region_total: continue
        region_total[reg]+=1
        ey=r["snr1_year"]
        if r["snr1_type"] in ("observed","forecast") and ey not in ("",None):
            region_emyears[reg].append(int(float(ey)))

    # cumulative fraction emerged by each year
    extent={}
    for reg in REGIONS:
        tot=region_total[reg]
        em=np.array(sorted(region_emyears[reg]))
        frac=np.array([ (np.sum(em<=t)/tot*100 if tot>0 else np.nan) for t in years ])
        extent[reg]=frac

    # ---- plot ----
    fig,ax=plt.subplots(figsize=(11,6))
    for reg in REGIONS:
        ax.plot(years, extent[reg], color=COLORS[reg], lw=3.2,
                label=f"{reg} (n={region_total[reg]})")
    ax.axvline(NOW, color="grey", lw=1.0, linestyle="--")
    ax.set_xlabel("Year", fontsize=15)
    ax.set_ylabel("Rivers with emerged signal (%)  (|SNR| ≥ 1)", fontsize=15)
    ax.tick_params(labelsize=13)
    ax.set_xlim(YEAR_MIN, YEAR_MAX); ax.set_ylim(0, 100)
    ax.grid(alpha=0.2)
    ax.legend(fontsize=13, loc="upper left", framealpha=0.9)
    fig.tight_layout()
    fig.savefig(OUT_FIG, dpi=160)
    print("saved", OUT_FIG)

    # ---- csv ----
    with open(OUT_CSV,"w",newline="") as f:
        w=csv.writer(f)
        w.writerow(["year"]+REGIONS)
        for i,t in enumerate(years):
            w.writerow([t]+[f"{extent[reg][i]:.1f}" for reg in REGIONS])
    print("saved", OUT_CSV)

    # ---- summary: extent at 2020, 2050, 2100 ----
    print("\nEmerged extent (% of region's rivers) at key years:")
    print(f"{'region':12} {'2020':>7} {'2050':>7} {'2100':>7}  n")
    for reg in REGIONS:
        f=extent[reg]
        def at(y): return f[np.where(years==y)[0][0]]
        print(f"{reg:12} {at(2020):6.0f}% {at(2050):6.0f}% {at(2100):6.0f}%  {region_total[reg]}")

if __name__=="__main__":
    main()
