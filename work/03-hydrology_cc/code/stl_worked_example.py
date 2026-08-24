#!/usr/bin/env python3
# =============================================================================
# Worked example for the slides: gauge 2219, STL + signal-to-noise, two figures.
#   Figure 1  stl_example_decomposition.png : the four STL steps
#             (observed quarterly series -> trend -> seasonal -> remainder)
#   Figure 2  stl_example_snr.png           : the explicit SNR curve with the
#             1-sigma / 2-sigma emergence levels and crossing years
# Threshold notation: sigma only (1σ / 2σ), consistent with the slides.
# =============================================================================
import csv, math, os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy.stats import theilslopes, norm
from statsmodels.tsa.seasonal import STL

GID = "2219"
INPUT = f"data/camels_ch/timeseries/observation_based/CAMELS_CH_obs_based_{GID}.csv"
DATE_COL = "date"; DISCHARGE_COL = "discharge_spec(mm/d)"
YEAR_START, YEAR_END = 1981, 2020
REF_START, REF_END = 1981, 2000
PERIOD = 4
FORECAST_HORIZON = 2100
OUTDIR = "figures"
TEAL = "#1f6f8b"; PINK = "#cf6fb0"; DARK = "#9c2b8a"; MARK = "#b0259a"; GREY = "#999999"

def days_in_month(y, m):
    if m == 2: return 29 if (y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)) else 28
    return [31,28,31,30,31,30,31,31,30,31,30,31][m-1]

def quarter_dates(year, q):
    if q == 1: spec = [(year-1,12),(year,1),(year,2)]
    elif q == 2: spec = [(year,3),(year,4),(year,5)]
    elif q == 3: spec = [(year,6),(year,7),(year,8)]
    else: spec = [(year,9),(year,10),(year,11)]
    return [f"{yy:04d}-{mm:02d}-{dd:02d}" for yy,mm in spec
            for dd in range(1, days_in_month(yy,mm)+1)]

def mann_kendall_p(y):
    n = len(y); s = 0
    for i in range(n-1): s += np.sum(np.sign(y[i+1:]-y[i]))
    var = n*(n-1)*(2*n+5)/18.0
    z = (s-1)/math.sqrt(var) if s > 0 else ((s+1)/math.sqrt(var) if s < 0 else 0)
    return 2*(1-norm.cdf(abs(z)))

# ---- 1. daily -> quarterly means on the 1981Q1..2020Q4 grid -----------------
daily = {}
with open(INPUT, encoding="latin-1") as f:
    r = csv.reader(f); hdr = next(r)
    di, qi = hdr.index(DATE_COL), hdr.index(DISCHARGE_COL)
    for row in r:
        v = row[qi]
        try: daily[row[di]] = float(v) if v not in ("","NaN","nan","NA") else math.nan
        except Exception: daily[row[di]] = math.nan

xs, vals = [], []
for y in range(YEAR_START, YEAR_END+1):
    for q in range(1,5):
        ds = quarter_dates(y,q)
        arr = np.array([daily.get(d, math.nan) for d in ds])
        nval = np.sum(~np.isnan(arr))
        v = float(np.nanmean(arr)) if (1-nval/len(arr)) <= 0.05 else math.nan
        xs.append(y + (q-1)/4.0 + 1/8.0); vals.append(v)
xs = np.array(xs); vals = np.array(vals)
good = ~np.isnan(vals)
vals[~good] = np.interp(xs[~good], xs[good], vals[good])

# ---- 2. STL (period = 4, robust) --------------------------------------------
stl = STL(vals, period=PERIOD, robust=True).fit()
trend, seasonal, resid = stl.trend, stl.seasonal, stl.resid

refmask = (xs >= REF_START) & (xs < REF_END+1)
sigma_N = float(np.std(resid[refmask], ddof=1))          # noise = ref-period SD of remainder
ref_mean = float(np.mean(trend[refmask]))                # reference level of the signal

# ---- 3. SNR + Theil-Sen forecast --------------------------------------------
slope, intercept = theilslopes(trend, xs)[:2]
p_mk = mann_kendall_p(trend[::PERIOD])
yrs_full = np.arange(YEAR_START, FORECAST_HORIZON+1)
snr_proj = (intercept + slope*(yrs_full+0.5) - ref_mean) / sigma_N
snr_obs_q = (trend - ref_mean) / sigma_N

def first_cross(k):
    beyond = np.abs(snr_proj) >= k
    for i in range(len(yrs_full)):
        if beyond[i] and np.all(beyond[i:]): return int(yrs_full[i])
    return None
y1, y2 = first_cross(1), first_cross(2)

print(f"gauge {GID}:  sigma_N={sigma_N:.3f} mm/d   ref_mean={ref_mean:.2f} mm/d")
print(f"Theil-Sen slope = {slope*10:+.3f} mm/d per decade   MK p = {p_mk:.3f}")
print(f"SNR(2020) = {snr_proj[yrs_full==2020][0]:+.2f}   1-sigma: {y1}   2-sigma: {y2}")

os.makedirs(OUTDIR, exist_ok=True)

# ---- Figure 0: OVERLAY (obs / trend / trend+seasonal) -----------------------
# Large, clean single-axes plot in the style of the old example's top panel.
fig, ax = plt.subplots(figsize=(12.5, 5.4))
ax.plot(xs, vals, color=GREY, lw=0.9, label="Quarterly mean (obs)")
ax.plot(xs, trend + seasonal, color=PINK, lw=0.8, alpha=0.75, label="trend + seasonal")
ax.plot(xs, trend, color=TEAL, lw=3.2, label="STL trend (signal)")
ax.set_ylabel("Quarterly Q (mm/d)", fontsize=15)
ax.set_xlabel("Year", fontsize=15)
ax.tick_params(labelsize=13)
ax.grid(alpha=0.15)
ax.legend(fontsize=13, loc="upper left", framealpha=0.9)
ax.set_xlim(YEAR_START, YEAR_END+1.5)
ax.set_xticks([1981, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020])
fig.tight_layout()
fig.savefig(f"{OUTDIR}/stl_example_overlay.png", dpi=200)
plt.close(fig)
print("saved", f"{OUTDIR}/stl_example_overlay.png")

# ---- Figure 1: the four STL steps -------------------------------------------
fig, axes = plt.subplots(4, 1, figsize=(9.6, 7.6), sharex=True)
panels = [
    (vals,     GREY, "1. Observed quarterly mean flow  $Q_t$", 0.9),
    (trend,    TEAL, "2. STL trend  $T_t$  =  the SIGNAL", 2.2),
    (seasonal, PINK, "3. STL seasonal  $S_t$  (removed)", 0.9),
    (resid,    "#666666", "4. STL remainder  $R_t$  =  the NOISE", 0.9),
]
for ax, (y, c, title, lw) in zip(axes, panels):
    ax.plot(xs, y, color=c, lw=lw)
    ax.set_title(title, fontsize=11, loc="left", fontweight="bold")
    ax.grid(alpha=0.15); ax.tick_params(labelsize=9)
axes[0].set_ylabel("mm/d", fontsize=9)
# annotate the additive model on panel 1
axes[0].text(0.99, 0.92, r"$Q_t = T_t + S_t + R_t$", transform=axes[0].transAxes,
             ha="right", va="top", fontsize=11,
             bbox=dict(boxstyle="round,pad=0.25", fc="white", ec="#cccccc"))
# reference period + sigma_N on the remainder panel
axes[3].axvspan(REF_START, REF_END+1, color=PINK, alpha=0.12)
axes[3].axhline(+sigma_N, color=DARK, lw=1.0, ls=(0,(5,3)))
axes[3].axhline(-sigma_N, color=DARK, lw=1.0, ls=(0,(5,3)))
axes[3].text(REF_START+0.5, axes[3].get_ylim()[1]*0.75,
             "reference period 1981–2000", fontsize=9, color=DARK)
axes[3].text(2004, axes[3].get_ylim()[1]*0.75,
             r"$\pm\sigma_N$ = SD of remainder over reference period"
             f" = {sigma_N:.2f} mm/d", fontsize=9, color=DARK, va="center")
axes[3].set_xlabel("Year", fontsize=10)
fig.suptitle(f"Gauge {GID} — STL decomposition of quarterly flow (period = 4, robust)",
             fontsize=13, fontweight="bold")
fig.tight_layout(rect=[0, 0, 1, 0.97])
fig.savefig(f"{OUTDIR}/stl_example_decomposition.png", dpi=160)
plt.close(fig)
print("saved", f"{OUTDIR}/stl_example_decomposition.png")

# ---- Figure 2: SNR curve + 1σ / 2σ emergence --------------------------------
fig, ax = plt.subplots(figsize=(12.5, 5.6))
obs = yrs_full <= YEAR_END
x_max = min(max(YEAR_END+10, (y2 or y1 or YEAR_END)+8), FORECAST_HORIZON)
ax.axvspan(REF_START, REF_END, color=PINK, alpha=0.10)
ax.text(1990.5, -2.25, "reference period 1981–2000", fontsize=12, color=DARK, ha="center")
ax.axhline(0, color="#bbbbbb", lw=0.6)
for k, c, ls, lab in [(1, PINK, (0,(5,3)), "SNR(t) = 1"), (2, DARK, (0,(2,2)), "SNR(t) = 2")]:
    ax.axhline(+k, color=c, lw=1.8, ls=ls, label=lab)
    ax.axhline(-k, color=c, lw=1.0, ls=ls, alpha=0.35)
ax.plot(yrs_full[obs], snr_proj[obs], color=TEAL, lw=3.2, label="SNR, observed record")
ax.plot(yrs_full[~obs], snr_proj[~obs], color=TEAL, lw=3.2, ls=(0,(2,2)),
        label="SNR, Theil-Sen extrapolation")
ax.axvline(YEAR_END, color="grey", lw=1.0, alpha=0.6)
for k, yy, c in [(1, y1, MARK), (2, y2, DARK)]:
    if yy:
        ax.scatter([yy], [k], s=110, color=c, edgecolor="white", lw=1.2, zorder=6)
        ax.annotate(f"{yy}", (yy, k), textcoords="offset points",
                    xytext=(8, -20), fontsize=14, fontweight="bold", color=c)
ax.set_xlim(YEAR_START, x_max); ax.set_ylim(-2.4, 3.0)
ax.set_xticks([1981] + list(range(2000, int(x_max)+1, 20)))
ax.set_xlabel("Year", fontsize=15)
ax.set_ylabel("SNR(t)", fontsize=15)
ax.tick_params(labelsize=13)
ax.grid(alpha=0.15); ax.legend(fontsize=13, loc="upper left", frameon=False)
fig.tight_layout()
fig.savefig(f"{OUTDIR}/stl_example_snr.png", dpi=200)
plt.close(fig)
print("saved", f"{OUTDIR}/stl_example_snr.png")
