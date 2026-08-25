#!/usr/bin/env python3
# =============================================================================
# STL-based explicit Signal-to-Noise Time of Emergence for CAMELS-CH observed flow.
#
# Pipeline (one gauge):
#   1. Daily specific discharge -> QUARTERLY means (4 per year, DJF/MAM/JJA/SON),
#      placed on a complete 1981Q1..2020Q4 grid (160 slots). Small gaps are
#      linearly interpolated; gauges with too many missing quarters are skipped.
#   2. STL decomposition (period=4): series = TREND + SEASONAL + RESIDUAL.
#        - SIGNAL = STL trend component (long-term change, seasonal cycle removed)
#        - NOISE  = SD of STL residual over the reference period (1981-2000)
#                   (pure random noise, trend & season already removed)
#   3. EXPLICIT SNR time series:
#        SNR(t) = ( trend(t) - mean(trend over reference period) ) / NOISE
#      Emergence (two stringency levels, after Hawkins & Sutton / IPCC):
#        - first year |SNR| >= 1   (1-sigma)
#        - first year |SNR| >= 2   (2-sigma)
#   4. FORECAST: fit Theil-Sen to the STL trend component, extrapolate it,
#      convert to projected SNR, and find the future year SNR first reaches
#      1 and 2 (observed if within record, forecast if future, else none).
#
# Observed data only (observation_based / *_obs). Simulation data never used.
# Rivers sampled 3 per bio-geographic region (essd-15-5755-2023 Fig.4), seed=42.
# =============================================================================
import csv, os, glob, math, random
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from scipy.stats import theilslopes, norm
from statsmodels.tsa.seasonal import STL

INPUT_DIR     = "data/camels_ch/timeseries/observation_based"
DATE_COL      = "date"
DISCHARGE_COL = "discharge_spec(mm/d)"
REGION_FILE   = "data/gauge_regions.csv"

YEAR_START, YEAR_END = 1981, 2020
REF_START, REF_END   = 1981, 2000
PERIOD = 4                       # quarterly -> seasonal period = 4
MAX_MISSING_FRAC_Q = 0.05        # per-quarter daily missing tolerance
MAX_MISSING_QUARTERS = 0.20      # skip gauge if >20% of 160 quarters missing
FORECAST_HORIZON = 2100
SEED = 42
PER_REGION = 3
SEASON_OF_Q = {1:"DJF",2:"MAM",3:"JJA",4:"SON"}
REGIONS=["Jura","Black Forest","Plateau","Alps North","Alps West","Alps South","Alps East"]

RESULTS_OUT = "results/stl_snr_forecast_results.csv"
SERIES_OUT  = "results/stl_snr_forecast_series.csv"
SAMPLED_OUT = "results/stl_snr_sampled_rivers.csv"
OUTDIR = "figures"
os.makedirs(OUTDIR, exist_ok=True)
PINK="#cf6fb0"; MARK="#b0259a"; BLACK="#222222"; TREND_C="#1f6f8b"

def gauge_id(path):
    return os.path.splitext(os.path.basename(path))[0].split("_")[-1]

def days_in_month(y,m):
    if m==2: return 29 if (y%4==0 and (y%100!=0 or y%400==0)) else 28
    return [31,28,31,30,31,30,31,31,30,31,30,31][m-1]

def quarter_dates(year, q):
    # Q1=DJF(Dec prev..Feb), Q2=MAM, Q3=JJA, Q4=SON
    if q==1: spec=[(year-1,12),(year,1),(year,2)]
    elif q==2: spec=[(year,3),(year,4),(year,5)]
    elif q==3: spec=[(year,6),(year,7),(year,8)]
    else: spec=[(year,9),(year,10),(year,11)]
    out=[]
    for yy,mm in spec:
        for dd in range(1,days_in_month(yy,mm)+1):
            out.append(f"{yy:04d}-{mm:02d}-{dd:02d}")
    return out

def read_daily(path):
    out={}
    with open(path, encoding="latin-1") as f:
        r=csv.reader(f); hdr=next(r)
        di=hdr.index(DATE_COL); qi=hdr.index(DISCHARGE_COL)
        for row in r:
            v=row[qi]
            try: out[row[di]]=float(v) if v not in ("","NaN","nan","NA") else math.nan
            except: out[row[di]]=math.nan
    return out

def quarterly_grid(daily):
    """Return (x_dec, values, mask_valid) on complete 1981Q1..2020Q4 grid."""
    xs=[]; vals=[]; yq=[]
    for y in range(YEAR_START, YEAR_END+1):
        for q in range(1,5):
            ds=quarter_dates(y,q)
            arr=np.array([daily.get(d,math.nan) for d in ds],dtype=float)
            n_exp=len(arr); n_val=np.sum(~np.isnan(arr))
            v = float(np.nanmean(arr)) if (n_exp>0 and (1-n_val/n_exp)<=MAX_MISSING_FRAC_Q) else math.nan
            xs.append(y + (q-1)/4.0 + 1/8.0)   # quarter center as decimal year
            vals.append(v); yq.append((y,q))
    return np.array(xs), np.array(vals), yq

def interp_gaps(vals):
    """Linear-interpolate interior NaNs; leave leading/trailing NaN as NaN."""
    v=vals.copy(); n=len(v)
    idx=np.where(~np.isnan(v))[0]
    if len(idx)==0: return v, 1.0
    miss_frac=np.mean(np.isnan(v))
    # interpolate only inside the observed span
    lo,hi=idx[0],idx[-1]
    xi=np.arange(n)
    good=~np.isnan(v)
    v[lo:hi+1]=np.interp(xi[lo:hi+1], xi[good], v[good])
    return v, miss_frac

def mann_kendall_p(y):
    n=len(y)
    if n<4: return math.nan
    s=0
    for i in range(n-1): s+=np.sum(np.sign(y[i+1:]-y[i]))
    var=n*(n-1)*(2*n+5)/18.0
    if var<=0: return math.nan
    z=(s-1)/math.sqrt(var) if s>0 else ((s+1)/math.sqrt(var) if s<0 else 0)
    return 2*(1-norm.cdf(abs(z)))

def first_cross_year(years, snr, k):
    """First year |snr|>=k AND stays beyond for the rest (persistence)."""
    beyond = np.abs(snr) >= k
    n=len(years)
    for i in range(n):
        if beyond[i] and np.all(beyond[i:]):
            return int(years[i]), ("up" if snr[i]>0 else "down")
    return None, None

def analyze(gid, region, daily):
    out=dict(gauge_id=gid, region=region,
             n_quarters_valid=0, noise_sd=math.nan, ref_trend_mean=math.nan,
             trend_slope_per_decade=math.nan, mk_pvalue=math.nan, direction="none",
             snr_max_abs=math.nan,
             snr1_year=math.nan, snr1_type="none",
             snr2_year=math.nan, snr2_type="none",
             insufficient=True)
    xs, vals, yq = quarterly_grid(daily)
    n_valid=int(np.sum(~np.isnan(vals)))
    out["n_quarters_valid"]=n_valid
    filled, miss = interp_gaps(vals)
    if np.any(np.isnan(filled)) :
        # trim to the longest fully-observed-then-filled span
        good=~np.isnan(filled)
        if good.sum() < (1-MAX_MISSING_QUARTERS)*len(filled):
            return out, None
        lo=np.where(good)[0][0]; hi=np.where(good)[0][-1]
        xs=xs[lo:hi+1]; filled=filled[lo:hi+1]; yq=yq[lo:hi+1]
    if len(filled) < 0.5*160 or miss > MAX_MISSING_QUARTERS:
        return out, None

    # ---- STL decomposition (period = 4 quarters) ----
    try:
        stl = STL(filled, period=PERIOD, robust=True).fit()
    except Exception:
        return out, None
    trend = stl.trend
    resid = stl.resid
    seasonal = stl.seasonal

    yrs_q = np.array([y for (y,q) in yq], dtype=float)
    refmask = (yrs_q>=REF_START)&(yrs_q<=REF_END)
    if refmask.sum() < 20: return out, None

    noise = float(np.std(resid[refmask], ddof=1))
    if not np.isfinite(noise) or noise<=0: return out, None
    ref_trend_mean = float(np.mean(trend[refmask]))

    # explicit SNR time series (quarterly) and yearly aggregation for reporting
    snr_q = (trend - ref_trend_mean)/noise

    # Theil-Sen on the STL trend component (vs decimal year) for forecast
    slope, intercept = theilslopes(trend, xs)[:2]
    p = mann_kendall_p(trend[::PERIOD] if len(trend)>=PERIOD else trend)  # MK on ~yearly trend pts

    out.update(noise_sd=noise, ref_trend_mean=ref_trend_mean,
               trend_slope_per_decade=slope*10, mk_pvalue=p,
               direction=("increase" if slope>0 else "decrease" if slope<0 else "flat"),
               snr_max_abs=float(np.max(np.abs(snr_q))), insufficient=False)

    # ---- build a yearly SNR curve from observed + forecast trend ----
    yrs_full = np.arange(YEAR_START, FORECAST_HORIZON+1)
    trend_proj = intercept + slope*(yrs_full+0.5)      # projected trend at year center
    snr_proj = (trend_proj - ref_trend_mean)/noise

    for k, key in [(1,"snr1"), (2,"snr2")]:
        yr, dirn = first_cross_year(yrs_full, snr_proj, k)
        if yr is None:
            out[f"{key}_year"]=math.nan; out[f"{key}_type"]="none"
        else:
            out[f"{key}_year"]=yr
            out[f"{key}_type"]= "observed" if yr<=YEAR_END else ("forecast" if yr<=FORECAST_HORIZON else "beyond")

    series=dict(xs=xs, observed=filled, trend=trend, seasonal=seasonal, resid=resid,
                snr_q=snr_q, yrs_full=yrs_full, snr_proj=snr_proj,
                slope=slope, intercept=intercept, noise=noise,
                ref_trend_mean=ref_trend_mean)
    return out, series

def main():
    regions={}
    if os.path.exists(REGION_FILE):
        for r in csv.DictReader(open(REGION_FILE)):
            regions[str(r["gauge_id"])]=r["region"]
    files=sorted(glob.glob(os.path.join(INPUT_DIR,"*.csv")))
    results={}; series_cache={}
    for path in files:
        gid=gauge_id(path)
        daily=read_daily(path)
        if not any(not math.isnan(v) for v in daily.values()): continue
        res, ser = analyze(gid, regions.get(gid,"NA"), daily)
        results[gid]=res
        if ser is not None: series_cache[gid]=ser

    fields=["gauge_id","region","n_quarters_valid","noise_sd","ref_trend_mean",
            "trend_slope_per_decade","mk_pvalue","direction","snr_max_abs",
            "snr1_year","snr1_type","snr2_year","snr2_type","insufficient"]
    with open(RESULTS_OUT,"w",newline="") as f:
        w=csv.DictWriter(f,fieldnames=fields); w.writeheader()
        for gid,r in results.items():
            w.writerow({k:("" if (isinstance(r.get(k),float) and math.isnan(r.get(k))) else r.get(k,"")) for k in fields})

    usable=[r for r in results.values() if not r["insufficient"]]
    from collections import Counter
    print(f"Usable gauges (STL ok): {len(usable)}")
    print("SNR>=1 type:", dict(Counter(r["snr1_type"] for r in usable)))
    print("SNR>=2 type:", dict(Counter(r["snr2_type"] for r in usable)))
    f1=[r["snr1_year"] for r in usable if r["snr1_type"]=="forecast"]
    if f1: print(f"SNR>=1 forecast years: n={len(f1)} min={min(f1)} median={int(np.median(f1))} max={max(f1)}")

    # sample 3 per region (prefer those with SNR>=1 emergence)
    rng=random.Random(SEED); chosen={}
    for reg in REGIONS:
        gl=[g for g,r in results.items() if r["region"]==reg and not r["insufficient"]]
        # prefer rising-SNR emergence cases (signal increasing, crosses +1/+2),
        # then any emergence, then any usable gauge - reproducible with fixed seed
        emg_up=[g for g in gl if results[g]["snr1_type"] in ("observed","forecast")
                and results[g]["direction"]=="increase"]
        emg_any=[g for g in gl if results[g]["snr1_type"] in ("observed","forecast")]
        rng.shuffle(emg_up); rng.shuffle(emg_any); rng.shuffle(gl)
        pick=emg_up[:PER_REGION]
        for g in emg_any+gl:
            if len(pick)>=PER_REGION: break
            if g not in pick: pick.append(g)
        chosen[reg]=pick

    def draw(ax, gid):
        r=results[gid]; s=series_cache[gid]
        # show explicit SNR curve (observed solid teal + forecast dotted) with
        # the two stringency lines SNR=1 and SNR=2
        yrs=s["yrs_full"]; snr=s["snr_proj"]
        obs_mask = yrs<=YEAR_END
        x_max=YEAR_END+10
        for k,key in [(1,"snr1"),(2,"snr2")]:
            yy=r[f"{key}_year"]
            if not (isinstance(yy,float) and math.isnan(yy)) and r[f"{key}_type"]=="forecast":
                x_max=max(x_max, int(yy)+6)
        x_max=min(x_max, FORECAST_HORIZON)
        ax.axhline(0,color="#bbbbbb",lw=0.5)
        ax.axhline(1,color=PINK,lw=1.2,linestyle=(0,(5,3)))
        ax.axhline(2,color="#9c2b8a",lw=1.2,linestyle=(0,(2,2)))
        ax.axhline(-1,color=PINK,lw=0.8,alpha=0.4,linestyle=(0,(5,3)))
        ax.axhline(-2,color="#9c2b8a",lw=0.8,alpha=0.4,linestyle=(0,(2,2)))
        ax.plot(yrs[obs_mask], snr[obs_mask], color=TREND_C, lw=2.0)
        ax.plot(yrs[~obs_mask], snr[~obs_mask], color=TREND_C, lw=2.0, linestyle=(0,(2,2)))
        ax.axvline(YEAR_END,color="grey",lw=0.5,alpha=0.5)
        # mark emergence points ON the line actually crossed (+k if rising, -k if falling)
        notes=[]
        for k,key,col in [(1,"snr1",MARK),(2,"snr2","#9c2b8a")]:
            yy=r[f"{key}_year"]
            if not (isinstance(yy,float) and math.isnan(yy)):
                # SNR value at that year decides which line (+k or -k) was crossed
                snr_at = s["intercept"]+s["slope"]*(yy+0.5)
                snr_at = (snr_at - s["ref_trend_mean"])/s["noise"]
                yline = k if snr_at>=0 else -k
                sgn = "≥" if snr_at>=0 else "≤−"
                ax.scatter([yy],[yline],s=45,color=col,edgecolor="white",lw=0.7,zorder=6)
                ax.annotate(f"{int(yy)}",(yy,yline),textcoords="offset points",xytext=(2,5),
                            fontsize=7,fontweight="bold",color=col)
                notes.append(f"|SNR|≥{k}:{int(yy)}({r[f'{key}_type'][:4]})")
        if not notes: notes=["no |SNR|≥1"]
        p=r["mk_pvalue"]
        sig="p<0.05" if (not math.isnan(p) and p<0.05) else f"p={p:.2f}"
        ax.set_title(gid,fontsize=9,loc="left")
        ax.text(0.98,0.04," ".join(notes)+f" · {sig}",transform=ax.transAxes,ha="right",va="bottom",
                fontsize=6.5,color="#444",bbox=dict(boxstyle="round,pad=0.2",fc="white",ec="none",alpha=0.7))
        vis=snr[yrs<=x_max]
        lo=min(-2.6, np.min(vis)*1.1); hi=max(3.0, np.max(vis)*1.1)
        ax.set_xlim(YEAR_START,x_max); ax.set_ylim(lo, hi)
        ax.grid(alpha=0.15); ax.tick_params(labelsize=7)

    fig,axes=plt.subplots(7,3,figsize=(15,22))
    for i,reg in enumerate(REGIONS):
        for j in range(PER_REGION):
            ax=axes[i][j]
            if j>=len(chosen[reg]): ax.axis("off"); continue
            draw(ax,chosen[reg][j])
            if j==0: ax.set_ylabel(f"{reg}\nSignal-to-noise ratio",fontsize=9)
    handles=[Line2D([0],[0],color=TREND_C,lw=2.0,label="SNR = (STL trend − ref mean)/noise (solid=obs, dotted=forecast)"),
             Line2D([0],[0],color=PINK,lw=1.2,linestyle=(0,(5,3)),label="SNR = 1 (1σ emergence)"),
             Line2D([0],[0],color="#9c2b8a",lw=1.2,linestyle=(0,(2,2)),label="SNR = 2 (2σ emergence)"),
             Line2D([0],[0],marker="o",color="w",markerfacecolor=MARK,markersize=8,label="First year SNR crosses threshold")]
    fig.legend(handles=handles,loc="upper center",ncol=2,fontsize=10,frameon=False,bbox_to_anchor=(0.5,0.999))
    fig.suptitle("STL-based explicit signal-to-noise emergence, CAMELS-CH\n"
                 "signal=STL trend · noise=STL residual SD · 3 rivers/region (seed=42) · obs only",
                 fontsize=14,y=1.012)
    fig.tight_layout(rect=[0,0,1,0.985])
    out=os.path.join(OUTDIR,"stl_snr_3perregion.png")
    fig.savefig(out,dpi=150,bbox_inches="tight"); plt.close(fig); print("saved",out)

    # example: clearest forecast SNR>=1 near mid-century, significant
    best=None
    for gid,r in results.items():
        if r["snr1_type"]!="forecast" or math.isnan(r["mk_pvalue"]): continue
        if r["direction"]!="increase": continue   # pick a clearly RISING SNR example
        sc=(r["mk_pvalue"], abs(r["snr1_year"]-2050))
        if best is None or sc<best[0]: best=(sc,gid)
    if best:
        gid=best[1]
        # rich example: 2 panels (STL components + SNR curve)
        s=series_cache[gid]; r=results[gid]
        fig,(a1,a2)=plt.subplots(2,1,figsize=(10,8),height_ratios=[1.1,1])
        a1.plot(s["xs"],s["observed"],color="#999",lw=0.7,label="Quarterly mean (obs)")
        a1.plot(s["xs"],s["trend"],color=TREND_C,lw=2.0,label="STL trend (signal)")
        a1.plot(s["xs"],s["trend"]+s["seasonal"],color=PINK,lw=0.6,alpha=0.6,label="trend+seasonal")
        a1.set_title(f"{gid} — STL decomposition",fontsize=11,loc="left")
        a1.set_ylabel("Quarterly Q (mm/d)"); a1.legend(fontsize=8,frameon=False); a1.grid(alpha=0.15)
        draw(a2,gid); a2.set_xlabel("Year"); a2.set_ylabel("Signal-to-noise ratio")
        a2.legend(handles=handles,loc="upper left",fontsize=7,frameon=False)
        a2.set_title(f"{gid} — explicit SNR & emergence",fontsize=11,loc="left")
        fig.tight_layout()
        out2=os.path.join(OUTDIR,"stl_snr_example.png")
        fig.savefig(out2,dpi=160); plt.close(fig); print("saved",out2,"(",gid,")")

    with open(SAMPLED_OUT,"w",newline="") as f:
        w=csv.writer(f); w.writerow(["region","gauge_id","snr1_year","snr1_type","snr2_year","snr2_type","trend_per_decade","mk_pvalue"])
        for reg in REGIONS:
            for gid in chosen[reg]:
                r=results[gid]
                w.writerow([reg,gid,
                            "" if (isinstance(r["snr1_year"],float) and math.isnan(r["snr1_year"])) else int(r["snr1_year"]),
                            r["snr1_type"],
                            "" if (isinstance(r["snr2_year"],float) and math.isnan(r["snr2_year"])) else int(r["snr2_year"]),
                            r["snr2_type"], r["trend_slope_per_decade"], r["mk_pvalue"]])
    print("saved",SAMPLED_OUT)

if __name__=="__main__":
    main()
