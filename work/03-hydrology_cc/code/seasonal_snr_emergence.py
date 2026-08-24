#!/usr/bin/env python3
# =============================================================================
# Per-SEASON trend + signal-to-noise emergence for CAMELS-CH observed flow.
#
# For each gauge x season (DJF/MAM/JJA/SON):
#   1. Seasonal mean of specific discharge -> one value per year (1981-2020).
#   2. SIGNAL = Theil-Sen trend of that seasonal series.
#   3. NOISE  = SD of the DETRENDED residuals over the reference period
#               (1981-2000): residual = value - (Theil-Sen fit). This removes
#               the trend so noise = pure inter-annual variability.
#   4. SNR(t) = (trend(t) - mean trend over reference) / noise.
#      Emergence = first year |SNR| >= 1 (persistent to end), observed if <=2020
#      else forecast (extrapolated to 2100).
#
# Two figures:
#   A) season x region heatmap of % rivers emerged (eventually, by 2100).
#   B) national emerged-extent-over-time curve, one line per season (sentinel).
#
# Observed data only. Uses gauge_regions.csv for the 7 regions.
# =============================================================================
import csv, os, glob, math
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy.stats import theilslopes, norm

INPUT_DIR="data/camels_ch/timeseries/observation_based"
DATE_COL="date"; DISCHARGE_COL="discharge_spec(mm/d)"
REGION_FILE="data/gauge_regions.csv"
YEAR_START,YEAR_END=1981,2020
REF_START,REF_END=1981,2000
SNR_K=1.0
MAX_MISSING_FRAC=0.05
MIN_VALID_YEARS=20
FORECAST_HORIZON=2100
SEASONS=["DJF","MAM","JJA","SON"]
REGIONS=["Jura","Black Forest","Plateau","Alps North","Alps West","Alps South","Alps East"]
SEASON_COLORS={"DJF":"#0072B2","MAM":"#009E73","JJA":"#E69F00","SON":"#CC79A7"}  # colorblind-safe (Okabe-Ito), no red

RESULTS_OUT="results/seasonal_snr_emergence_results.csv"
FIG_HEAT="figures/seasonal_emergence_heatmap.png"
FIG_CURVE="figures/seasonal_emergence_extent_curves.png"

def gid_of(p): return os.path.splitext(os.path.basename(p))[0].split("_")[-1]
def dim(y,m):
    if m==2: return 29 if (y%4==0 and(y%100!=0 or y%400==0)) else 28
    return [31,28,31,30,31,30,31,31,30,31,30,31][m-1]
def season_dates(y,s):
    if s=="DJF": spec=[(y-1,12),(y,1),(y,2)]
    elif s=="MAM": spec=[(y,3),(y,4),(y,5)]
    elif s=="JJA": spec=[(y,6),(y,7),(y,8)]
    else: spec=[(y,9),(y,10),(y,11)]
    out=[]
    for yy,mm in spec:
        for dd in range(1,dim(yy,mm)+1): out.append(f"{yy:04d}-{mm:02d}-{dd:02d}")
    return out
def read_daily(p):
    out={}
    with open(p,encoding="latin-1") as f:
        r=csv.reader(f); h=next(r); di=h.index(DATE_COL); qi=h.index(DISCHARGE_COL)
        for row in r:
            v=row[qi]
            try: out[row[di]]=float(v) if v not in("","NaN","nan","NA") else math.nan
            except: out[row[di]]=math.nan
    return out
def seasonal_series(daily,s):
    yrs=[]; vals=[]
    for y in range(YEAR_START,YEAR_END+1):
        ds=season_dates(y,s); arr=np.array([daily.get(d,math.nan) for d in ds])
        nval=np.sum(~np.isnan(arr))
        v=float(np.nanmean(arr)) if (len(arr)>0 and (1-nval/len(arr))<=MAX_MISSING_FRAC) else math.nan
        yrs.append(y); vals.append(v)
    return np.array(yrs,dtype=float),np.array(vals)

def mann_kendall_p(y):
    n=len(y)
    if n<4: return math.nan
    s=0
    for i in range(n-1): s+=np.sum(np.sign(y[i+1:]-y[i]))
    var=n*(n-1)*(2*n+5)/18.0
    if var<=0: return math.nan
    z=(s-1)/math.sqrt(var) if s>0 else ((s+1)/math.sqrt(var) if s<0 else 0)
    return 2*(1-norm.cdf(abs(z)))
def first_cross(years,snr,k):
    beyond=np.abs(snr)>=k; n=len(years)
    for i in range(n):
        if beyond[i] and np.all(beyond[i:]): return int(years[i])
    return None

def analyze(gid,region,daily):
    res={}
    for s in SEASONS:
        yrs,vals=seasonal_series(daily,s)
        ok=~np.isnan(vals); nval=int(ok.sum())
        row=dict(gauge_id=gid,region=region,season=s,n_valid=nval,
                 noise_sd=math.nan,trend_per_decade=math.nan,mk_pvalue=math.nan,
                 direction="none",emerge_year=math.nan,emerge_type="none",insufficient=True)
        if nval<MIN_VALID_YEARS:
            res[s]=(row,None); continue
        fy=yrs[ok]; fv=vals[ok]
        slope,intercept=theilslopes(fv,fy)[:2]
        # detrended residuals; noise = SD of residuals in reference period
        resid=fv-(intercept+slope*fy)
        refm=(fy>=REF_START)&(fy<=REF_END)
        if refm.sum()<10: res[s]=(row,None); continue
        noise=float(np.std(resid[refm],ddof=1))
        if not np.isfinite(noise) or noise<=0: res[s]=(row,None); continue
        ref_trend_mean=float(np.mean((intercept+slope*fy)[refm]))
        p=mann_kendall_p(fv)
        row.update(noise_sd=noise,trend_per_decade=slope*10,mk_pvalue=p,
                   direction=("increase" if slope>0 else "decrease" if slope<0 else "flat"),
                   insufficient=False)
        # SNR projection to 2100
        yrs_full=np.arange(YEAR_START,FORECAST_HORIZON+1)
        snr=(intercept+slope*yrs_full-ref_trend_mean)/noise
        yr=first_cross(yrs_full,snr,SNR_K)
        if yr is None: row["emerge_type"]="none"
        else:
            row["emerge_year"]=yr
            row["emerge_type"]="observed" if yr<=YEAR_END else ("forecast" if yr<=FORECAST_HORIZON else "beyond")
        res[s]=(row,dict(yrs_full=yrs_full,snr=snr))
    return res

def main():
    regions={}
    for r in csv.DictReader(open(REGION_FILE)): regions[str(r["gauge_id"])]=r["region"]
    files=sorted(glob.glob(os.path.join(INPUT_DIR,"*.csv")))
    all_rows=[]
    for p in files:
        gid=gid_of(p); daily=read_daily(p)
        if not any(not math.isnan(v) for v in daily.values()): continue
        res=analyze(gid,regions.get(gid,"NA"),daily)
        for s in SEASONS: all_rows.append(res[s][0])

    fields=["gauge_id","region","season","n_valid","noise_sd","trend_per_decade",
            "mk_pvalue","direction","emerge_year","emerge_type","insufficient"]
    with open(RESULTS_OUT,"w",newline="") as f:
        w=csv.DictWriter(f,fieldnames=fields); w.writeheader()
        for r in all_rows:
            w.writerow({k:("" if (isinstance(r.get(k),float) and math.isnan(r.get(k))) else r.get(k,"")) for k in fields})

    usable=[r for r in all_rows if not r["insufficient"]]
    print(f"可分析 gauge-season: {len(usable)}")
    from collections import Counter
    for s in SEASONS:
        sub=[r for r in usable if r["season"]==s]
        em=[r for r in sub if r["emerge_type"] in ("observed","forecast")]
        print(f"  {s}: 可分析 {len(sub)}, 显现 {len(em)} ({100*len(em)/max(len(sub),1):.0f}%)")

    # ---- FIGURE A: season x region heatmap of % emerged (by 2100) ----
    mat=np.full((len(SEASONS),len(REGIONS)),np.nan)
    cnt=np.zeros((len(SEASONS),len(REGIONS)),dtype=int)
    for i,s in enumerate(SEASONS):
        for j,reg in enumerate(REGIONS):
            sub=[r for r in usable if r["season"]==s and r["region"]==reg]
            if not sub: continue
            em=sum(1 for r in sub if r["emerge_type"] in ("observed","forecast"))
            mat[i,j]=100*em/len(sub); cnt[i,j]=len(sub)
    fig,ax=plt.subplots(figsize=(11,4.6))
    im=ax.imshow(mat,cmap="YlOrRd",vmin=0,vmax=100,aspect="auto")
    ax.set_xticks(range(len(REGIONS))); ax.set_xticklabels(REGIONS,rotation=20,ha="right",fontsize=9)
    ax.set_yticks(range(len(SEASONS))); ax.set_yticklabels(SEASONS,fontsize=10)
    for i in range(len(SEASONS)):
        for j in range(len(REGIONS)):
            if not np.isnan(mat[i,j]):
                ax.text(j,i,f"{mat[i,j]:.0f}%\n(n={cnt[i,j]})",ha="center",va="center",
                        fontsize=7,color="black" if mat[i,j]<60 else "white")
    cb=fig.colorbar(im,ax=ax); cb.set_label("Rivers emerged by 2100 (%)")
    ax.set_title("Seasonal signal emergence by region (≥1σ, either direction)\n"
                 "CAMELS-CH observed · Theil-Sen trend · noise=detrended residual SD",
                 fontsize=12,fontweight="bold",loc="left")
    fig.tight_layout(); fig.savefig(FIG_HEAT,dpi=160); print("saved",FIG_HEAT)

    # ---- FIGURE B: national emerged extent over time, one line per season ----
    years=np.arange(YEAR_START,FORECAST_HORIZON+1)
    fig,ax=plt.subplots(figsize=(11,6))
    for s in SEASONS:
        sub=[r for r in usable if r["season"]==s]
        tot=len(sub)
        em=sorted(int(r["emerge_year"]) for r in sub
                  if r["emerge_type"] in ("observed","forecast") and not (isinstance(r["emerge_year"],float) and math.isnan(r["emerge_year"])))
        em=np.array(em)
        frac=np.array([100*np.sum(em<=t)/tot for t in years]) if tot>0 else np.zeros(len(years))
        ax.plot(years,frac,color=SEASON_COLORS[s],lw=2.4,label=f"{s} (n={tot})")
    ax.axvline(YEAR_END,color="grey",lw=1.0,linestyle="--")
    ax.set_xlabel("Year"); ax.set_ylabel("Rivers with emerged signal (%)  (≥1σ)")
    ax.set_title("National emerged extent over time, by season",
                 fontsize=12,fontweight="bold",loc="left")
    ax.set_xlim(YEAR_START,FORECAST_HORIZON); ax.set_ylim(0,100); ax.grid(alpha=0.2)
    ax.legend(fontsize=10,loc="upper left")
    fig.tight_layout(); fig.savefig(FIG_CURVE,dpi=160); print("saved",FIG_CURVE)

    # season summary at key years
    print("\n各季节全国显现比例:")
    print(f"{'season':6} {'2020':>6} {'2050':>6} {'2100':>6}")
    for s in SEASONS:
        sub=[r for r in usable if r["season"]==s]; tot=len(sub)
        em=np.array(sorted(int(r["emerge_year"]) for r in sub
                    if r["emerge_type"] in ("observed","forecast") and not(isinstance(r["emerge_year"],float) and math.isnan(r["emerge_year"]))))
        def at(y): return 100*np.sum(em<=y)/tot if tot>0 else 0
        print(f"{s:6} {at(2020):5.0f}% {at(2050):5.0f}% {at(2100):5.0f}%")

if __name__=="__main__":
    main()
