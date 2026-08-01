#!/usr/bin/env python3
# =============================================================================
# Direction of seasonal streamflow signals (increase vs decrease) for CAMELS-CH.
# Uses seasonal_snr_emergence_results.csv (Theil-Sen trend + SNR>=1 emergence).
#
# Figure A: grouped bar chart - per season, count of rivers with an EMERGED
#           signal that is increasing vs decreasing.
# Figure B: season x region diverging stacked bars showing the share of
#           increasing (red, up) vs decreasing (blue, down) emerged rivers.
# =============================================================================
import csv, math
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

RESULTS="results/seasonal_snr_emergence_results.csv"
FIG_A="figures/seasonal_direction_bars.png"
FIG_B="figures/seasonal_direction_by_region.png"
SEASONS=["DJF","MAM","JJA","SON"]
REGIONS=["Jura","Black Forest","Plateau","Alps North","Alps West","Alps South","Alps East"]
SEASON_LABEL={"DJF":"DJF (winter)","MAM":"MAM (spring)","JJA":"JJA (summer)","SON":"SON (autumn)"}
UP_C="#c0392b"; DN_C="#2c6fbf"   # red=increase, blue=decrease

def load():
    rows=[r for r in csv.DictReader(open(RESULTS)) if r["insufficient"]!="True"]
    return rows, [r for r in rows if r["emerge_type"] in ("observed","forecast")]

def main():
    allrows,em=load()

    # ---- FIGURE A: grouped bars per season (up vs down counts) ----
    up=[sum(1 for r in em if r["season"]==s and r["direction"]=="increase") for s in SEASONS]
    dn=[sum(1 for r in em if r["season"]==s and r["direction"]=="decrease") for s in SEASONS]
    x=np.arange(len(SEASONS)); w=0.38
    fig,ax=plt.subplots(figsize=(9,5.5))
    b1=ax.bar(x-w/2,up,w,color=UP_C,label="Increase")
    b2=ax.bar(x+w/2,dn,w,color=DN_C,label="Decrease")
    for bars in (b1,b2):
        for b in bars:
            ax.text(b.get_x()+b.get_width()/2,b.get_height()+1,f"{int(b.get_height())}",
                    ha="center",va="bottom",fontsize=12)
    ax.set_xticks(x); ax.set_xticklabels([SEASON_LABEL[s] for s in SEASONS],fontsize=13)
    ax.tick_params(axis="y",labelsize=13)
    ax.set_ylabel("Rivers with emerged signal (|SNR| ≥ 1)",fontsize=15)
    ax.legend(fontsize=13); ax.grid(alpha=0.2,axis="y")
    fig.tight_layout(); fig.savefig(FIG_A,dpi=160); print("saved",FIG_A)

    # ---- FIGURE B: season x region diverging bars (up above 0, down below) ----
    fig,axes=plt.subplots(1,len(SEASONS),figsize=(12,5.4),sharey=True)
    yreg=np.arange(len(REGIONS))
    for k,s in enumerate(SEASONS):
        ax=axes[k]
        tot=[max(sum(1 for r in allrows if r["season"]==s and r["region"]==reg),1) for reg in REGIONS]
        ups=[100*sum(1 for r in em if r["season"]==s and r["region"]==reg and r["direction"]=="increase")/tot[i] for i,reg in enumerate(REGIONS)]
        dns=[100*sum(1 for r in em if r["season"]==s and r["region"]==reg and r["direction"]=="decrease")/tot[i] for i,reg in enumerate(REGIONS)]
        ax.barh(yreg, ups, color=UP_C, label="increase")
        ax.barh(yreg, [-d for d in dns], color=DN_C, label="decrease")
        for i,(u,d) in enumerate(zip(ups,dns)):
            if u>88: ax.text(u-4, i, f"{u:.0f}", va="center", ha="right", fontsize=10, color="white")
            elif u>0: ax.text(u+4, i, f"{u:.0f}", va="center", ha="left", fontsize=10)
            if d>88: ax.text(-d+4, i, f"{d:.0f}", va="center", ha="left", fontsize=10, color="white")
            elif d>0: ax.text(-d-4, i, f"{d:.0f}", va="center", ha="right", fontsize=10)
        ax.axvline(0,color="black",lw=0.6)
        ax.set_title(SEASON_LABEL[s],fontsize=15,fontweight="bold")
        ax.set_yticks(yreg)
        if k==0: ax.set_yticklabels(REGIONS,fontsize=13)
        ax.set_xlim(-112,112)
        ax.set_xticks([-100,-50,0,50,100]); ax.set_xticklabels(["100","50","0","50","100"],fontsize=11)
        ax.set_xlabel("% of rivers",fontsize=12)
        ax.grid(alpha=0.15,axis="x")
        ax.invert_yaxis()
    handles=[plt.Rectangle((0,0),1,1,color=UP_C),plt.Rectangle((0,0),1,1,color=DN_C)]
    fig.legend(handles,["Increase","Decrease"],
               loc="upper right",bbox_to_anchor=(0.99,1.0),fontsize=12,frameon=False)
    fig.tight_layout(rect=[0,0,1,0.88]); fig.savefig(FIG_B,dpi=160); print("saved",FIG_B)

    # summary
    print("\n各季节显现signal方向:")
    print(f"{'season':6} {'上升':>5} {'下降':>5} {'下降占比':>8}")
    for i,s in enumerate(SEASONS):
        tot=up[i]+dn[i]
        print(f"{s:6} {up[i]:5} {dn[i]:5} {100*dn[i]/max(tot,1):7.0f}%")

if __name__=="__main__":
    main()
