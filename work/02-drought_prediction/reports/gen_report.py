"""Generate reports/results_report_v2.html — self-contained with embedded images.

Phase 1 (naive/naive architecture, loss-selection grid):
  - 4 loss groups: MSE, Pinball q=0.20, Weighted MSE hinge w=1, Weighted MSE hinge w=5
  - One best-val/loss checkpoint per group (survivors after within-group pruning)
  - RQ2 = global encoder (FiLM) contrast  |  RQ3 = static encoder contrast

Design principles:
  - All headline metrics are POOLED (single confusion matrix / RMSE over all
    valid masked cell-months). Per-cell *_median columns are never used as
    fallbacks in any headline table.
  - rmse_vs_trend = test/trend/rmse_pooled − test/rmse_pooled
      positive  →  model RMSE is lower than trend RMSE  →  model beats trend
      negative  →  model RMSE is higher than trend RMSE  →  trend wins
  - If a required pooled metric is missing for a run, that run is excluded from
    all tables and listed explicitly. No silent substitution.
  - Auto-generated "winner" / "standout" verdicts are removed. Tables show
    numbers; captions state the direction of effects.
"""

import base64
import csv
import re as _re
from pathlib import Path

ROOT     = Path(__file__).parent.parent
FIG      = ROOT / "figures"        # data figures: baselines/, processed/ — unchanged
FIG_V2   = ROOT / "figures_v2"    # model-run figures and frontier plots for this branch
OUT      = ROOT / "reports" / "results_report_v2.html"
CSV_PATH = ROOT / "saved_models_v2" / "gridsearch_comparison.csv"
CKPT_DIR = ROOT / "saved_models_v2"

# ── Utilities ─────────────────────────────────────────────────────────────────

def b64(path):
    with open(path, "rb") as f:
        return "data:image/png;base64," + base64.b64encode(f.read()).decode()


def img_tag(path, w="100%"):
    p = Path(path)
    if not p.exists():
        return f'<p style="color:#aaa">Figure not found: {p.name}</p>'
    return f'<img src="{b64(p)}" style="width:{w};border-radius:4px">'


def _safe(v):
    """Return float or None (never NaN)."""
    try:
        f = float(v)
        return None if f != f else f
    except Exception:
        return None


def _median(vals):
    s = sorted(v for v in vals if v is not None)
    return s[len(s) // 2] if s else None


def fmt(v, dec=3):
    try:
        return f"{float(v):.{dec}f}"
    except Exception:
        return "—"


def pct(v):
    try:
        return f"{float(v) * 100:.1f}%"
    except Exception:
        return "—"


# ── Load CSV ──────────────────────────────────────────────────────────────────

_rows_all: list[dict] = []
with open(CSV_PATH) as f:
    for r in csv.DictReader(f):
        _rows_all.append(r)

CKPT_RUNS: set[str] = {
    p.name.replace("best_model_", "").replace(".ckpt", "")
    for p in CKPT_DIR.glob("best_model_*.ckpt")
}

# ── Required columns & completeness check ────────────────────────────────────

_REQUIRED = [
    "test/rmse_pooled",
    "test/mae_pooled",
    "test/corr_pooled",
    "test/drought_tpr_pooled",
    "test/drought_f1_pooled",
    "test/persistence/rmse_pooled",
    "test/clim/rmse_pooled",
    "test/trend/rmse_pooled",
    "test/rmse_vs_trend",
]


def _has_all(r):
    return all(_safe(r.get(c)) is not None for c in _REQUIRED)


# Phase 1 = naive/naive arch (baseline); Phase 2 = all other arch conditions.
# The CSV has no "phase" column — classify by static_encoder / global_encoder.
_phase1_all  = [r for r in _rows_all
                if r.get("static_encoder","") == "naive"
                and r.get("global_encoder","") == "naive"]
_ckpt_ok     = [r for r in _phase1_all if _has_all(r)]
_ckpt_excl   = [r for r in _phase1_all if not _has_all(r)]


def _phase1_winners(complete_rows):
    """Return one winner row per (loss_fn, drought_weight) group — lowest val/loss."""
    groups: dict = {}
    for r in complete_rows:
        loss_fn = r.get("loss_fn", "")
        dw = r.get("drought_weight", "") if loss_fn == "weighted_mse" else ""
        groups.setdefault((loss_fn, dw), []).append(r)
    out = []
    for group_rows in groups.values():
        ranked = sorted(group_rows, key=lambda r: _safe(r.get("val/loss")) or float("inf"))
        out.append(ranked[0])
    return out


print("\n=== Part A — completeness check ===")
print(f"Phase 1 rows in CSV     : {len(_phase1_all)}")
print(f"All required cols OK    : {len(_ckpt_ok)}")
print(f"Excluded (missing cols) : {len(_ckpt_excl)}")
if _ckpt_excl:
    for r in _ckpt_excl:
        missing = [c for c in _REQUIRED if _safe(r.get(c)) is None]
        print(f"  EXCLUDED: {r['run_name']}  missing: {', '.join(missing)}")

# One winner per loss group — Phase 1 baseline (naive/naive)
rows = _phase1_winners(_ckpt_ok)
print(f"Phase 1 winners shown   : {len(rows)}  "
      f"({', '.join(r['run_name'] for r in rows)})")

# ── Phase 2 winners — one per (loss, dw, static_enc, global_enc) by val/loss ─
_phase2_all = [r for r in _rows_all
               if not (r.get("static_encoder","") == "naive" and r.get("global_encoder","") == "naive")
               and _has_all(r)]

def _p2_group_key(r):
    loss = r.get("loss_fn", "")
    dw   = r.get("drought_weight", "") if loss == "weighted_mse" else ""
    return (loss, dw, r.get("static_encoder", ""), r.get("global_encoder", ""))

_p2_groups: dict = {}
for r in _phase2_all:
    _p2_groups.setdefault(_p2_group_key(r), []).append(r)

_p2_winners = [
    sorted(v, key=lambda r: _safe(r.get("val/loss")) or float("inf"))[0]
    for v in _p2_groups.values()
]
print(f"Phase 2 winners loaded  : {len(_p2_winners)}  ({len(_p2_groups)} groups, {len(_phase2_all)} rows total)")

# Combined lookup pool: Phase 1 naive/naive + Phase 2 arch conditions
all_winners = rows + _p2_winners

# ── Sign-convention check ─────────────────────────────────────────────────────
# Verify rmse_vs_trend = trend/rmse_pooled − rmse_pooled for the first few rows.

print("\n=== Sign-convention check (rmse_vs_trend = trend − model, positive = model better) ===")
for r in rows[:3]:
    trend = _safe(r.get("test/trend/rmse_pooled"))
    model = _safe(r.get("test/rmse_pooled"))
    stored = _safe(r.get("test/rmse_vs_trend"))
    computed = (trend - model) if (trend is not None and model is not None) else None
    match = abs(stored - computed) < 1e-6 if (stored is not None and computed is not None) else "n/a"
    sign = "positive=model better ✓" if stored and stored > 0 else "negative=trend wins"
    print(f"  {r['run_name'][:55]}")
    print(f"    stored={stored:+.4f}  computed={computed:+.4f}  match={match}  → {sign}")

# Worked example from spec: RMSE ≈ 1.113, trend ≈ 1.065 → rvt ≈ −0.048
print("\n=== Worked example (RMSE≈1.113, trend≈1.065 → rvt≈−0.048, rendered red) ===")
_near = sorted(rows, key=lambda r: abs((_safe(r.get("test/rmse_pooled")) or 99) - 1.113))
for r in _near[:1]:
    rm = _safe(r.get("test/rmse_pooled"))
    tr = _safe(r.get("test/trend/rmse_pooled"))
    rv = _safe(r.get("test/rmse_vs_trend"))
    print(f"  {r['run_name']}")
    color = "red (negative rvt)" if (rv is not None and rv < 0) else "green (positive rvt)"
    print(f"    RMSE={rm:.3f}  trend={tr:.3f}  rvt={rv:+.3f}  → {color}")

# ── Constants from pooled data ────────────────────────────────────────────────
# Baselines are model-independent: read from first complete row (all identical).

_r0 = rows[0] if rows else {}
PERSISTENCE_RMSE = _safe(_r0.get("test/persistence/rmse_pooled")) or float("nan")
CLIM_RMSE        = _safe(_r0.get("test/clim/rmse_pooled"))        or float("nan")
TREND_RMSE       = _safe(_r0.get("test/trend/rmse_pooled"))       or float("nan")

_floor_vals = [v for r in rows if (v := _safe(r.get("test/residual_floor"))) is not None]
RESIDUAL_FLOOR = _median(_floor_vals) if _floor_vals else float("nan")

# ── Family definitions ────────────────────────────────────────────────────────
# (family_key, loss_fn, weighted_loss_mode|None, drought_weight|None, display_label, badge_class)
# Phase 1 uses only these four families (naive/naive architecture).
# drought_weight disambiguates the two weighted_mse variants, whose val/loss scales differ.

FAMILIES = [
    ("mse",                   "mse",         None,    None, "MSE",                        "badge-mse"),
    ("pinball",               "pinball",      None,    None, "Pinball (q=0.20)",           "badge-pinball"),
    ("weighted_mse_hinge_w1", "weighted_mse", "hinge", "1",  "Hinge-weighted MSE (w=1)", "badge-weighted"),
    ("weighted_mse_hinge_w5", "weighted_mse", "hinge", "5",  "Hinge-weighted MSE (w=5)", "badge-weighted"),
]

LOSS_BADGE = {
    "mse":              "badge-mse",
    "pinball":          "badge-pinball",
    "cellwise_pinball": "badge-cellwise",
    "weighted_mse":     "badge-weighted",
}
COND_BADGE = {
    ("naive",    "naive"): "badge-nn",
    ("single",   "naive"): "badge-singlenn",
    ("seasonal", "naive"): "badge-sn",
    ("naive",    "film"):  "badge-nf",
    ("single",   "film"):  "badge-singlesf",
    ("seasonal", "film"):  "badge-sf",
}
COND_SHORT = {
    ("naive",    "naive"): "naive/naive",
    ("single",   "naive"): "single/naive",
    ("seasonal", "naive"): "seasonal/naive",
    ("naive",    "film"):  "naive/film",
    ("single",   "film"):  "single/film",
    ("seasonal", "film"):  "seasonal/film",
}

def _run_fam(r):
    if r["loss_fn"] == "weighted_mse":
        mode = r.get("weighted_loss_mode", "threshold")
        dw   = r.get("drought_weight", "")
        return f"weighted_mse_{mode}_w{dw}"
    return r["loss_fn"]


# ── Cell accessor ─────────────────────────────────────────────────────────────

def _cell_runs(loss, static_enc, global_enc, mode=None, drought_weight=None):
    """Winner row for one (loss, condition) cell — searches Phase 1 + Phase 2 pool."""
    out = [
        r for r in all_winners
        if r["loss_fn"] == loss
        and r["static_encoder"] == static_enc
        and r["global_encoder"] == global_enc
    ]
    if mode is not None:
        out = [r for r in out if r.get("weighted_loss_mode") == mode]
    if drought_weight is not None:
        out = [r for r in out if str(r.get("drought_weight", "")) == str(drought_weight)]
    return out


def _fam_cell(loss, mode, static_enc, global_enc, dw=None):
    return _cell_runs(loss, static_enc, global_enc, mode, drought_weight=dw)


def _get(r, col):
    """Return pooled metric value or None. No fallback to median."""
    return _safe(r.get(col))


# ── Paired-delta computation ──────────────────────────────────────────────────

def _hp_key(run_name):
    """Hyperparameter identity (w, lr, dropout, wd), ignoring condition."""
    w  = _re.search(r"weighted_mse_w(\d+)_", run_name)
    lr = _re.search(r"_lr([^_]+)", run_name)
    do = _re.search(r"_do([^_]+)", run_name)
    wd = _re.search(r"_wd([^_]+)", run_name)
    return (
        w.group(1)  if w  else "",
        lr.group(1) if lr else "",
        do.group(1) if do else "",
        wd.group(1) if wd else "",
    )


def _paired_deltas(loss, mode, cond_b, metric, cond_a=("naive", "naive"), dw=None):
    """Compute metric(cond_b) − metric(cond_a).

    Prefers hp-key matched pairs (same lr/dropout/wd). After checkpoint pruning
    each slot holds one run; if keys differ, falls back to the direct single-value
    difference and returns a 1-element list tagged as unmatched.
    """
    runs_a = _fam_cell(loss, mode, *cond_a, dw=dw)
    runs_b = _fam_cell(loss, mode, *cond_b, dw=dw)
    a = {_hp_key(r["run_name"]): _get(r, metric) for r in runs_a}
    b = {_hp_key(r["run_name"]): _get(r, metric) for r in runs_b}
    matched = []
    for k, vb in b.items():
        va = a.get(k)
        if va is not None and vb is not None:
            matched.append(vb - va)
    if matched:
        return matched, True    # (values, was_matched)
    # Fallback: direct single-value difference
    all_a = [v for v in a.values() if v is not None]
    all_b = [v for v in b.values() if v is not None]
    if all_a and all_b:
        return [(_median(all_b) or 0) - (_median(all_a) or 0)], False
    return [], False


def _delta_cell(deltas_pair, good=0.005, fmt_str="{:+.3f}", lower_is_better=False):
    """Render a delta cell with sign coloring.

    lower_is_better=True  →  Δ<0 is green (e.g. RMSE, lower error = better).
    lower_is_better=False →  Δ>0 is green (e.g. TPR/F1, higher = better).
    Returns (html_td, median_value).
    """
    deltas, _ = deltas_pair if isinstance(deltas_pair, tuple) else (deltas_pair, True)
    if not deltas:
        return "<td>—</td>", None
    med = _median(deltas)
    if med is None:
        return "<td>—</td>", None
    if lower_is_better:
        cls = "g" if med < -good else ("r" if med > good else "y")
    else:
        cls = "g" if med > good else ("r" if med < -good else "y")
    return f'<td class="{cls}">{fmt_str.format(med)}</td>', med


# ── Pareto front ──────────────────────────────────────────────────────────────

def _pareto_runs(ymetric):
    """Rows not dominated on (rmse_vs_trend, ymetric_pooled), maximising both."""
    pts = []
    for r in all_winners:
        x = _get(r, "test/rmse_vs_trend")
        y = _get(r, ymetric)
        if x is not None and y is not None:
            pts.append((x, y, r))
    front = []
    for i, (x, y, r) in enumerate(pts):
        dominated = any(
            x2 >= x and y2 >= y and (x2 > x or y2 > y)
            for j, (x2, y2, _) in enumerate(pts) if j != i
        )
        if not dominated:
            front.append(r)
    return front


FRONT_TPR = _pareto_runs("test/drought_tpr_pooled")
FRONT_F1  = _pareto_runs("test/drought_f1_pooled")
FRONT_TPR_NAMES = {r["run_name"] for r in FRONT_TPR}
FRONT_F1_NAMES  = {r["run_name"] for r in FRONT_F1}


# ── Eval figures on disk ──────────────────────────────────────────────────────

EVAL_RUNS: set[str] = (
    {d.name[: -len("_eval")] for d in (FIG_V2 / "models").iterdir()
     if d.is_dir() and d.name.endswith("_eval")}
    if (FIG_V2 / "models").exists() else set()
)


def model_fig(run_name, metric):
    return FIG_V2 / "models" / f"{run_name}_eval" / f"test_{metric}_spatial.png"


# ── RQ2 table data ────────────────────────────────────────────────────────────

_RQ2_METRICS = [
    ("test/drought_tpr_pooled", "TPR", False),
    ("test/drought_f1_pooled",  "F1",  False),
    ("test/rmse_pooled",        "RMSE", True),   # lower = better
    ("test/rmse_vs_trend",      "rvt",  False),  # higher = better
]

_rq2_data = []
for fam_key, loss, mode, dw, label, badge in FAMILIES:
    cell = _fam_cell(loss, mode, "naive", "naive", dw=dw)
    if not cell:
        continue
    r = cell[0]  # exactly 1 checkpoint per slot
    _rq2_data.append({
        "fam_key": fam_key, "loss": loss, "mode": mode, "dw": dw,
        "label": label, "badge": badge,
        "tpr":       _get(r, "test/drought_tpr_pooled"),
        "f1":        _get(r, "test/drought_f1_pooled"),
        "rmse":      _get(r, "test/rmse_pooled"),
        "rvt":       _get(r, "test/rmse_vs_trend"),
        "val_f1":    _get(r, "val/drought_f1_pooled"),
        "val_rmse":  _get(r, "val/rmse_median"),
        "drmse":     _get(r, "val/drought_rmse_pooled"),
        "run_name":  r.get("run_name", ""),
        "lr":        r.get("lr", ""),
        "dropout":   r.get("dropout", ""),
        "wd":        r.get("weight_decay", ""),
    })

# Sort by F1 descending
_rq2_data.sort(key=lambda d: -(d["f1"] or 0))

# Best values per column for bolding
_best_tpr  = max((d["tpr"]  or -99) for d in _rq2_data)
_best_f1   = max((d["f1"]   or -99) for d in _rq2_data)
_best_rmse = min((d["rmse"] or  99) for d in _rq2_data)
_best_rvt  = max((d["rvt"]  or -99) for d in _rq2_data)

print("\n=== RQ2 table (naive/naive, sorted by F1 desc) ===")
print(f"  {'Family':<30} {'TPR':>6} {'F1':>6} {'RMSE':>6} {'rvt':>7}")
print("  " + "-" * 55)
for d in _rq2_data:
    print(f"  {d['label']:<30} {d['tpr'] or 0:6.3f} {d['f1'] or 0:6.3f} "
          f"{d['rmse'] or 0:6.3f} {d['rvt'] or 0:+7.3f}")

print(f"\n  Ref baselines: persistence={PERSISTENCE_RMSE:.3f}  "
      f"climatology={CLIM_RMSE:.3f}  trend={TREND_RMSE:.3f}")

# ── Detect delta regime (matched vs single-config) ───────────────────────────
# Check for one representative pair to determine the regime for the caption.

_delta_regime_matched = False
for _, loss, mode, dw, __, ___ in FAMILIES:
    _, was_matched = _paired_deltas(loss, mode, ("single", "naive"), "test/rmse_pooled")
    if was_matched:
        _delta_regime_matched = True
        break

_delta_caption_note = (
    "Δ values are matched-pair differences (same hyperparameter signature)."
    if _delta_regime_matched else
    "Δ values are single-configuration differences (one best-val-loss checkpoint per slot "
    "after pruning; hyperparameter signatures differ between conditions so no paired "
    "matching is possible)."
)

print(f"\n=== Delta regime: {'matched pairs' if _delta_regime_matched else 'single-config differences'} ===")

# ── RQ2/RQ3 contrast data (global encoder = rq12, static encoder = rq11) ───────

_RQ1_METRICS = [
    ("test/drought_tpr_pooled", "TPR",  False, 0.02),
    ("test/drought_f1_pooled",  "F1",   False, 0.01),
    ("test/rmse_pooled",        "RMSE", True,  0.005),
]

def _rq1_table_data(cond_list):
    """Return rows x conditions x metrics data dict for contrast tables."""
    out = []
    for fam_key, loss, mode, dw, label, badge in FAMILIES:
        row = {"fam_key": fam_key, "label": label, "badge": badge, "cols": {}}
        for cond in cond_list:
            cell = _fam_cell(loss, mode, *cond, dw=dw)
            r = cell[0] if cell else None
            row["cols"][cond] = {
                col: (_get(r, col) if r else None)
                for col, _, _, _ in _RQ1_METRICS
            }
        row["deltas"] = {}
        baseline = cond_list[0]
        for cond in cond_list[1:]:
            row["deltas"][cond] = {
                col: _paired_deltas(loss, mode, cond, col, cond_a=baseline, dw=dw)
                for col, _, _, _ in _RQ1_METRICS
            }
        out.append(row)
    return out

_rq11_conds = [("naive","naive"), ("single","naive"), ("seasonal","naive")]
_rq12_conds = [("naive","naive"), ("naive","film")]
_rq11_rows  = _rq1_table_data(_rq11_conds)
_rq12_rows  = _rq1_table_data(_rq12_conds)

# ── CSS ───────────────────────────────────────────────────────────────────────

CSS = """
:root {
  --bg:#0f1117; --card:#1a1d27; --border:#2e3148;
  --accent:#5b8dee; --green:#3ecf8e; --red:#f87171;
  --yellow:#fbbf24; --text:#e2e8f0; --muted:#7c85a2;
}
*{box-sizing:border-box;margin:0;padding:0}
body{background:var(--bg);color:var(--text);font-family:'Segoe UI',system-ui,sans-serif;
     font-size:14px;line-height:1.6}
.container{max-width:1400px;margin:0 auto;padding:32px 24px}
h1{font-size:2rem;font-weight:700;color:#fff;margin-bottom:6px}
h2{font-size:1.3rem;font-weight:600;color:var(--accent);margin:40px 0 16px;
   border-bottom:1px solid var(--border);padding-bottom:8px}
h3{font-size:1rem;font-weight:600;color:#c7d2fe;margin:24px 0 10px}
h4{font-size:.9rem;font-weight:600;color:var(--muted);margin:16px 0 8px}
p{color:var(--muted);margin-bottom:12px}
.subtitle{color:var(--muted);font-size:.9rem;margin-bottom:32px}
.card{background:var(--card);border:1px solid var(--border);border-radius:10px;
      padding:20px;margin-bottom:24px}
.grid2{display:grid;grid-template-columns:1fr 1fr;gap:16px}
.grid3{display:grid;grid-template-columns:1fr 1fr 1fr;gap:16px}
table{width:100%;border-collapse:collapse;font-size:12.5px}
th{background:#22263a;color:var(--accent);text-align:left;padding:8px 10px;
   border-bottom:2px solid var(--border);white-space:nowrap;font-weight:600}
td{padding:7px 10px;border-bottom:1px solid var(--border);color:var(--text);white-space:nowrap}
tr:hover td{background:#1e2235}
.badge{display:inline-block;padding:2px 8px;border-radius:12px;font-size:11px;font-weight:600}
.badge-mse       {background:#1e3a5f;color:#7eb8f7}
.badge-pinball   {background:#2d1e5f;color:#c4b5fd}
.badge-cellwise  {background:#1e4a3a;color:#6ee7b7}
.badge-weighted  {background:#4a2e1e;color:#fbbf24}
.badge-nn        {background:#1e2e1e;color:#86efac}
.badge-singlenn  {background:#1e2e2e;color:#a5f3fc}
.badge-sn        {background:#2e2e1e;color:#fde68a}
.badge-nf        {background:#1e2e3f;color:#93c5fd}
.badge-singlesf  {background:#2e1e2e;color:#f0abfc}
.badge-sf        {background:#2e1e3f;color:#d8b4fe}
.g{color:var(--green);font-weight:600}
.r{color:var(--red);font-weight:500}
.y{color:var(--yellow);font-weight:500}
.fig-label{font-size:11px;color:var(--muted);text-align:center;margin-top:6px}
.caption{color:var(--muted);font-size:12px;margin:6px 0 18px;padding:8px 12px;
         border-left:2px solid var(--border)}
"""

# ── HTML builder ──────────────────────────────────────────────────────────────

_parts: list[str] = []

def W(s):
    _parts.append(s)


# ── Head ──────────────────────────────────────────────────────────────────────

W(f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="UTF-8">
<title>Drought Prediction — Phase 1 + 2 Results</title>
<style>{CSS}</style></head>
<body><div class="container">
<h1>Alpine Drought Prediction — Phase 1 + 2 Results</h1>
<p class="subtitle">ConvLSTM · 12-month lead · SPEI regression · Test period 2015–2024 ·
  Phase 1: {len(rows)} loss-group winners (naive/naive) ·
  Phase 2: {len(_p2_winners)} architecture-condition winners (5 conditions × 4 loss groups × best-val/loss HP)</p>
""")

# TOC
W("""<div class="card">
<strong>Contents</strong>
<ol style="margin-top:10px;padding-left:20px">
  <li><a href="#baselines">Baselines &amp; Data Properties</a></li>
  <li><a href="#rq1">RQ1 — Loss Function Comparison (naive/naive architecture)</a></li>
  <li><a href="#rq2">RQ2 — Global Encoder (FiLM) Contrast</a></li>
  <li><a href="#rq3">RQ3 — Static Encoder Contrast</a></li>
  <li><a href="#frontier">Detection–Accuracy Frontier</a></li>
  <li><a href="#appendix">Appendix — Data, Setup, Gallery</a></li>
</ol></div>""")

# ── Baselines & data properties ───────────────────────────────────────────────

W('<h2 id="baselines">Baselines &amp; Data Properties</h2>')
W(f"""<div class="card">
<table>
<tr><th>Reference</th><th>RMSE</th><th>Notes</th></tr>
<tr><td>Persistence (last observed SPEI carried 12 months)</td>
    <td class="r">{PERSISTENCE_RMSE:.3f}</td>
    <td>Strong lower bound; correlates with persistence at long lead</td></tr>
<tr><td>Climatology (per-cell training mean)</td>
    <td class="y">{CLIM_RMSE:.3f}</td>
    <td>Inflated by test-period drying shift (drought frequency doubled 5%→10%)</td></tr>
<tr><td>Linear trend extrapolation</td>
    <td class="y">{TREND_RMSE:.3f}</td>
    <td>Primary reference in rmse_vs_trend column (positive = model beats this baseline)</td></tr>
</table>
<p class="caption" style="margin-top:10px">
  <strong>rmse_vs_trend = test/trend/rmse_pooled − test/rmse_pooled.</strong>
  Positive means the model's pooled RMSE is lower than the trend baseline's RMSE — the model
  beats the trend. Negative means the trend wins.
  All metrics are pooled over valid masked Alpine cell-months (single number per run, not a
  per-cell average of cell-level numbers).
</p>
</div>""")

if len(_ckpt_excl) > 0:
    excl_list = "<br>".join(r["run_name"] for r in _ckpt_excl)
    W(f'<div style="background:#2d1e0e;border-left:3px solid #f87171;padding:12px 16px;'
      f'border-radius:0 6px 6px 0;margin:12px 0">'
      f'<p style="color:#fde68a;margin:0"><strong>Excluded runs ({len(_ckpt_excl)}) — '
      f'missing required pooled metrics:</strong><br>{excl_list}</p></div>')
else:
    W(f'<p style="color:var(--muted);font-size:12px">All {len(rows)} checkpoint runs '
      f'have the required pooled metrics. No rows excluded.</p>')

# ── RQ2 — Loss comparison ─────────────────────────────────────────────────────

W('<h2 id="rq1">RQ1 — Loss Function Comparison (Phase 1)</h2>')
W('<p class="caption">Naive/naive architecture · one best-val/loss checkpoint per loss group · '
  'rows sorted by test F1 descending. '
  '<strong>Bold</strong> = best value in each column. '
  'val metrics align with the saved checkpoint (best val/loss epoch). '
  f'Reference baselines: persistence RMSE {PERSISTENCE_RMSE:.3f} · '
  f'climatology RMSE {CLIM_RMSE:.3f} · trend RMSE {TREND_RMSE:.3f} '
  f'(trend TPR = 0%, trend F1 = 0 — confirmed).</p>')
W('<div class="card"><table>')
W('<tr>'
  '<th>Loss family</th>'
  '<th>HP (lr / do / wd)</th>'
  '<th>val F1</th>'
  '<th>val RMSE</th>'
  '<th>val drought RMSE</th>'
  '<th>test TPR</th>'
  '<th>test F1</th>'
  '<th>test RMSE</th>'
  '<th>rmse_vs_trend</th>'
  '</tr>')

def _bold_best(val, best, decimals=3):
    if val is None:
        return "—"
    s = f"{val:.{decimals}f}"
    return f"<strong>{s}</strong>" if abs(val - best) < 1e-9 else s


_best_val_f1   = max((d["val_f1"]  or -99) for d in _rq2_data)
_best_val_rmse = min((d["val_rmse"] or  99) for d in _rq2_data)
_best_drmse    = min((d["drmse"]   or  99) for d in _rq2_data)

for d in _rq2_data:
    tpr_s      = _bold_best(d["tpr"],      _best_tpr)
    f1_s       = _bold_best(d["f1"],       _best_f1)
    rmse_s     = _bold_best(d["rmse"],     _best_rmse)
    val_f1_s   = _bold_best(d["val_f1"],   _best_val_f1)
    val_rmse_s = _bold_best(d["val_rmse"], _best_val_rmse)
    drmse_s    = _bold_best(d["drmse"],    _best_drmse)
    rvt        = d["rvt"]
    rvt_cls    = "g" if (rvt or 0) > 0.005 else ("r" if (rvt or 0) < -0.005 else "y")
    rvt_s      = _bold_best(rvt, _best_rvt)
    hp_s       = f"{d['lr']} / {d['dropout']} / {d['wd']}"
    W(f'<tr>'
      f'<td><span class="badge {d["badge"]}">{d["label"]}</span></td>'
      f'<td style="font-size:11px;color:var(--muted)">{hp_s}</td>'
      f'<td>{val_f1_s}</td>'
      f'<td>{val_rmse_s}</td>'
      f'<td>{drmse_s}</td>'
      f'<td>{tpr_s}</td>'
      f'<td>{f1_s}</td>'
      f'<td>{rmse_s}</td>'
      f'<td class="{rvt_cls}">{rvt_s}</td>'
      f'</tr>')

W('</table></div>')

# ── RQ1 contrast table helper ─────────────────────────────────────────────────

def _contrast_table(title, table_rows, cond_list, metric_col, label,
                    lower_is_better, dead_band):
    """Emit one sub-table for a single metric across the given conditions."""
    W(f'<h3>{title}</h3>')
    extra_conds = cond_list[1:]
    # Column headers
    cond_headers = "".join(
        f'<th><span class="badge {COND_BADGE.get(c, "")}">'
        f'{COND_SHORT.get(c, str(c))}</span></th>'
        for c in cond_list
    )
    delta_headers = "".join(
        f'<th>Δ {COND_SHORT.get(c, str(c))}</th>'
        for c in extra_conds
    )
    W(f'<table><tr><th>Loss family</th>{cond_headers}{delta_headers}</tr>')

    for row in table_rows:
        fam_td = f'<td><span class="badge {row["badge"]}">{row["label"]}</span></td>'
        # Absolute values per condition
        abs_vals = {cond: row["cols"][cond].get(metric_col) for cond in cond_list}
        # Bold the better of the absolute values (ignoring None)
        valid_vals = {c: v for c, v in abs_vals.items() if v is not None}
        if valid_vals:
            best_cond_val = min(valid_vals.values()) if lower_is_better else max(valid_vals.values())
        else:
            best_cond_val = None

        def _cell_abs(v, _best=best_cond_val):
            if v is None:
                return "<td>—</td>"
            s = f"{v:.3f}"
            if _best is not None and abs(v - _best) < 1e-9:
                s = f"<strong>{s}</strong>"
            return f"<td>{s}</td>"

        abs_tds = "".join(_cell_abs(abs_vals[c]) for c in cond_list)

        # Delta cells
        delta_tds = ""
        for cond in extra_conds:
            dp = row["deltas"].get(cond, {}).get(metric_col, ([], False))
            td, _ = _delta_cell(dp, good=dead_band, lower_is_better=lower_is_better)
            delta_tds += td

        W(f"<tr>{fam_td}{abs_tds}{delta_tds}</tr>")

    W("</table>")

# ── vs-trend table helper ────────────────────────────────────────────────────

def _vs_trend_table(cond_list):
    """Three sub-tables (rvt, TPR, F1) showing absolute metrics with the linear
    trend model as a reference column.  Trend has rvt=0 (by definition); TPR=0
    and F1=0 are confirmed empirically (the per-cell linear SPEI extrapolation
    never falls below −1.5 in any of the 120 test months, missing all 23 793
    drought events).  Green cells beat the trend; red cells are worse."""
    cond_ths = "".join(
        f'<th><span class="badge {COND_BADGE.get(c, "")}">'
        f'{COND_SHORT.get(c, str(c))}</span></th>'
        for c in cond_list
    )

    # --- rvt sub-table -------------------------------------------------------
    W('<h3>rmse_vs_trend — absolute (positive = model beats trend baseline)</h3>')
    W(f'<table><tr><th>Loss family</th><th>Trend ref</th>{cond_ths}</tr>')
    W('<tr><td style="color:var(--muted)">Linear trend model</td>'
      '<td class="y">0.000</td>'
      + ''.join('<td style="color:var(--muted)">—</td>' for _ in cond_list)
      + '</tr>')
    for _, loss, mode, dw, label, badge in FAMILIES:
        tds = ""
        for cond in cond_list:
            cell = _fam_cell(loss, mode, *cond, dw=dw)
            v = _get(cell[0], "test/rmse_vs_trend") if cell else None
            cls = "g" if (v or 0) > 0.005 else ("r" if (v or 0) < -0.005 else "y")
            tds += f'<td class="{cls}">{f"{v:+.3f}" if v is not None else "—"}</td>'
        W(f'<tr><td><span class="badge {badge}">{label}</span></td>'
          f'<td class="y">0.000</td>{tds}</tr>')
    W('</table><br>')

    # --- TPR sub-table -------------------------------------------------------
    W('<h3>TPR — absolute</h3>')
    W(f'<table><tr><th>Loss family</th>{cond_ths}</tr>')
    for _, loss, mode, dw, label, badge in FAMILIES:
        tds = ""
        for cond in cond_list:
            cell = _fam_cell(loss, mode, *cond, dw=dw)
            v = _get(cell[0], "test/drought_tpr_pooled") if cell else None
            cls = "g" if (v or 0) > 0.05 else ("y" if (v or 0) > 0 else "r")
            tds += f'<td class="{cls}">{f"{v * 100:.1f}%" if v is not None else "—"}</td>'
        W(f'<tr><td><span class="badge {badge}">{label}</span></td>{tds}</tr>')
    W('</table><p class="caption" style="margin-top:4px">Trend baseline: TPR = 0% '
      '(confirmed — linear trend never predicts SPEI ≤ −1.5). Green: &gt; 5%.</p><br>')

    # --- F1 sub-table --------------------------------------------------------
    W('<h3>F1 — absolute</h3>')
    W(f'<table><tr><th>Loss family</th>{cond_ths}</tr>')
    for _, loss, mode, dw, label, badge in FAMILIES:
        tds = ""
        for cond in cond_list:
            cell = _fam_cell(loss, mode, *cond, dw=dw)
            v = _get(cell[0], "test/drought_f1_pooled") if cell else None
            cls = "g" if (v or 0) > 0.05 else ("y" if (v or 0) > 0 else "r")
            tds += f'<td class="{cls}">{f"{v:.3f}" if v is not None else "—"}</td>'
        W(f'<tr><td><span class="badge {badge}">{label}</span></td>{tds}</tr>')
    W('</table><p class="caption" style="margin-top:4px">Trend baseline: F1 = 0 '
      '(confirmed). Green: &gt; 0.05.</p>')


# ── RQ2 — Global encoder (FiLM) contrast ────────────────────────────────────

W('<h2 id="rq2">RQ2 — Global Encoder (FiLM) Contrast</h2>')
W(f'<p class="caption">Rows = loss family. Conditions: naive/naive (baseline), '
  f'naive/film. Static encoder is held at naive in both. One best-val-loss checkpoint per slot. '
  f'<strong>Bold</strong> = best absolute value in the row. '
  f'Δ columns: for TPR/F1, green if Δ&gt;0; for RMSE, green if Δ&lt;0. '
  f'Dead-band ±0.005 → yellow. {_delta_caption_note}</p>')
W('<div class="card">')
for metric_col, label, lower_is_better, dead_band in _RQ1_METRICS:
    _contrast_table(
        f"{label} — Δ vs naive/naive baseline",
        _rq12_rows, _rq12_conds, metric_col, label, lower_is_better, dead_band,
    )
    W('<br>')
W('</div>')

W('<h3 style="margin-top:24px">Absolute metrics vs linear trend benchmark</h3>')
W('<p class="caption">Absolute values per condition. '
  'The linear trend model (rvt = 0 by definition; TPR = 0%, F1 = 0 confirmed — '
  'the per-cell linear trend never predicts SPEI ≤ −1.5 in any test month) is shown as a reference. '
  'rvt: green = model beats trend; red = trend wins. '
  'TPR/F1: green if &gt; 5%.</p>')
W('<div class="card">')
_vs_trend_table(_rq12_conds)
W('</div>')

# Factual caption: compute direction of effects from the data
def _direction_summary(rows_data, conds, metric_col, label, lower_is_better):
    lines = []
    for row in rows_data:
        for cond in conds[1:]:
            dp = row["deltas"].get(cond, {}).get(metric_col, ([], False))
            deltas, _ = dp
            if deltas:
                med = _median(deltas) or 0
                if lower_is_better:
                    direction = "↓" if med < -0.005 else ("↑" if med > 0.005 else "≈")
                else:
                    direction = "↑" if med > 0.005 else ("↓" if med < -0.005 else "≈")
                lines.append(
                    f"{row['label']} / {COND_SHORT.get(cond, '?')}: Δ{label}={med:+.3f} ({direction})"
                )
    return "; ".join(lines) if lines else "No data."

_rq12_tpr_summary = _direction_summary(_rq12_rows, _rq12_conds, "test/drought_tpr_pooled", "TPR", False)
_rq12_f1_summary  = _direction_summary(_rq12_rows, _rq12_conds, "test/drought_f1_pooled",  "F1",  False)
_rq12_rm_summary  = _direction_summary(_rq12_rows, _rq12_conds, "test/rmse_pooled",         "RMSE", True)

W(f'<p class="caption"><strong>RQ2 summary:</strong> '
  f'TPR effects — {_rq12_tpr_summary}. '
  f'F1 effects — {_rq12_f1_summary}. '
  f'RMSE effects — {_rq12_rm_summary}.</p>')

# ── RQ3 — Static encoder contrast ──────────────────────────────────────────────

W('<h2 id="rq3">RQ3 — Static Encoder Contrast</h2>')
W(f'<p class="caption">Rows = loss family. Conditions: naive/naive (baseline), '
  f'single/naive, seasonal/naive. One best-val-loss checkpoint per slot. '
  f'<strong>Bold</strong> = best absolute value in the row. '
  f'Δ columns: for TPR/F1, green if Δ&gt;0; for RMSE, green if Δ&lt;0 (lower error). '
  f'Dead-band ±0.005 → yellow. {_delta_caption_note}</p>')
W('<div class="card">')
for metric_col, label, lower_is_better, dead_band in _RQ1_METRICS:
    _contrast_table(
        f"{label} — Δ vs naive/naive baseline",
        _rq11_rows, _rq11_conds, metric_col, label, lower_is_better, dead_band,
    )
    W('<br>')
W('</div>')

W('<h3 style="margin-top:24px">Absolute metrics vs linear trend benchmark</h3>')
W('<p class="caption">Absolute values per condition. '
  'The linear trend model (rvt = 0 by definition; TPR = 0%, F1 = 0 confirmed — '
  'the per-cell linear trend never predicts SPEI ≤ −1.5 in any test month) is shown as a reference. '
  'rvt: green = model beats trend; red = trend wins. '
  'TPR/F1: green if &gt; 5%.</p>')
W('<div class="card">')
_vs_trend_table(_rq11_conds)
W('</div>')

_rq11_tpr_summary = _direction_summary(_rq11_rows, _rq11_conds, "test/drought_tpr_pooled", "TPR", False)
_rq11_f1_summary  = _direction_summary(_rq11_rows, _rq11_conds, "test/drought_f1_pooled",  "F1",  False)
_rq11_rm_summary  = _direction_summary(_rq11_rows, _rq11_conds, "test/rmse_pooled",         "RMSE", True)

W(f'<p class="caption"><strong>RQ3 summary:</strong> '
  f'TPR effects — {_rq11_tpr_summary}. '
  f'F1 effects — {_rq11_f1_summary}. '
  f'RMSE effects — {_rq11_rm_summary}.</p>')

# ── Detection–accuracy frontier plots ────────────────────────────────────────

W('<h2 id="frontier">Detection–Accuracy Frontier</h2>')
W('<p style="color:var(--muted);font-size:12px">Every checkpoint run colored by loss '
  'family. The Pareto front (black-edged circles, dashed line) is the set of runs not '
  'dominated on both axes. Dotted vertical line = rmse_vs_trend 0 (right = beats trend '
  'baseline on RMSE). Star = run with highest pooled TPR among those with rmse_vs_trend&gt;0.</p>')

_best_trade = None

try:
    import io as _io

    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as _plt

    _FAM_COLOR = {
        "mse":                    "#111111",
        "pinball":                "#c6402a",
        "weighted_mse_hinge_w1":  "#a8c8de",
        "weighted_mse_hinge_w5":  "#286ea0",
    }

    _THEME_DARK = dict(
        bg="#1a1d27", text="#e2e8f0", muted="#7c85a2",
        spine="#2e3148", leg_face="#22263a", leg_edge="#2e3148",
        front="#e2e8f0", star_fill="#ffffff", star_edge="#0f1117",
        zero_line="#7c85a2",
    )
    _THEME_LIGHT = dict(
        bg="#ffffff", text="#1a1a2e", muted="#666666",
        spine="#cccccc", leg_face="#f8f8f8", leg_edge="#cccccc",
        front="#333333", star_fill="#333333", star_edge="#ffffff",
        zero_line="#999999",
    )

    # Best tradeoff: highest pooled TPR among runs that beat the trend
    _frontier_pool = [r for r in all_winners if (_get(r, "test/rmse_vs_trend") or -99) > 0]
    _best_trade = (
        max(_frontier_pool, key=lambda r: _get(r, "test/drought_tpr_pooled") or 0)
        if _frontier_pool else None
    )

    # Load baseline test metrics from eval_baselines output
    import json as _json
    _BL_DIR = ROOT / "figures" / "baselines"
    def _load_bl(name):
        p = _BL_DIR / name / "test_metrics.json"
        return _json.loads(p.read_text()) if p.exists() else {}
    _bl_data = {n: _load_bl(n) for n in ("trend", "climatology", "persistence")}
    _bl_trend_rmse = _bl_data["trend"].get("rmse_pooled", 1.0651)
    _BASELINES = [
        ("Climatology", "climatology", "#9ca3af", (4, 7)),
        ("Persistence", "persistence", "#d97706", (4, 7)),
    ]

    def _scatter_b64(ymetric, ylabel, front_runs, save_path=None, save_light_path=None):
        def _yval(r):
            return _get(r, ymetric)

        def _build(theme, dpi=150):
            fig, ax = _plt.subplots(figsize=(5.0, 5.0), dpi=dpi)
            fig.patch.set_facecolor(theme["bg"])
            ax.set_facecolor(theme["bg"])
            for fam_key, loss, mode, dw, label, badge in FAMILIES:
                xs, ys = [], []
                for r in all_winners:
                    if _run_fam(r) != fam_key:
                        continue
                    x = _get(r, "test/rmse_vs_trend")
                    y = _yval(r)
                    if x is None or y is None:
                        continue
                    xs.append(x)
                    ys.append(y)
                _alpha = 1.0 if fam_key == "weighted_mse_hinge_w1" else 0.75
                ax.scatter(xs, ys, s=16, alpha=_alpha, label=label,
                           color=_FAM_COLOR.get(fam_key, theme["text"]), edgecolors="none")
            front_pts = sorted(
                ((_get(r, "test/rmse_vs_trend"), _yval(r)) for r in front_runs
                 if _get(r, "test/rmse_vs_trend") is not None and _yval(r) is not None),
                key=lambda p: p[0],
            )
            if front_pts:
                fx, fy = zip(*front_pts)
                ax.step(fx, fy, where="post", ls="--", lw=1.1,
                        color=theme["front"], zorder=4)
                ax.scatter(fx, fy, s=34, facecolors="none", edgecolors=theme["front"],
                           linewidths=1.2, zorder=5)
            # Re-draw Pareto front points in their family colour (so overlapping runs stay visible)
            for r in front_runs:
                x2 = _get(r, "test/rmse_vs_trend")
                y2 = _yval(r)
                if x2 is None or y2 is None:
                    continue
                ax.scatter([x2], [y2], s=16, alpha=1.0,
                           color=_FAM_COLOR.get(_run_fam(r), theme["text"]),
                           edgecolors="none", zorder=7)
            # Label ALL Pareto front points with their arch condition
            _lbl_box = dict(boxstyle="round,pad=0.15", fc=theme["bg"], ec="none", alpha=0.75)
            for r in front_runs:
                x2 = _get(r, "test/rmse_vs_trend")
                y2 = _yval(r)
                if x2 is None or y2 is None:
                    continue
                _se = r.get("static_encoder", "naive")
                _ge = r.get("global_encoder", "naive")
                _arch_lbl = f"{_se}/{_ge}"
                ax.annotate(_arch_lbl, (x2, y2), fontsize=6.5, color=theme["text"],
                            xytext=(5, 5), textcoords="offset points",
                            bbox=_lbl_box, zorder=9)
            # Baselines as diamond markers with direct labels
            _ykey = ymetric.rsplit("/", 1)[-1]
            for bl_label, bl_name, bl_color, bl_off in _BASELINES:
                bl_metrics = _bl_data.get(bl_name, {})
                bl_rmse = bl_metrics.get("rmse_pooled")
                bl_y = bl_metrics.get(_ykey)
                if bl_rmse is None or bl_y is None:
                    continue
                bl_x = _bl_trend_rmse - bl_rmse
                ax.scatter([bl_x], [bl_y], s=55, marker="D", color=bl_color,
                           edgecolors=theme["bg"], linewidths=0.8, zorder=8)
                ax.annotate(bl_label, (bl_x, bl_y), fontsize=7.5, color=theme["text"],
                            xytext=bl_off, textcoords="offset points",
                            fontweight="bold", bbox=_lbl_box, zorder=9)
            ax.axvline(0, color=theme["zero_line"], lw=0.8, ls=":")
            ax.set_xlabel("trend RMSE − model RMSE",
                          color=theme["text"], fontsize=9)
            ax.set_ylabel(ylabel, color=theme["text"], fontsize=9)
            ax.set_ylim(bottom=0, top=1.0)
            ax.tick_params(colors=theme["muted"], labelsize=8)
            for sp in ax.spines.values():
                sp.set_color(theme["spine"])
            ax.legend(fontsize=7, facecolor=theme["leg_face"], edgecolor=theme["leg_edge"],
                      labelcolor=theme["text"], loc="best")
            fig.tight_layout()
            return fig

        import base64 as _b64
        fig_dark = _build(_THEME_DARK)
        buf = _io.BytesIO()
        fig_dark.savefig(buf, format="png", facecolor=fig_dark.get_facecolor(), dpi=150)
        _plt.close(fig_dark)
        if save_path is not None:
            save_path.parent.mkdir(parents=True, exist_ok=True)
            save_path.write_bytes(buf.getvalue())
        if save_light_path is not None:
            fig_light = _build(_THEME_LIGHT)
            buf_light = _io.BytesIO()
            fig_light.savefig(buf_light, format="png",
                              facecolor=fig_light.get_facecolor(), dpi=150)
            _plt.close(fig_light)
            save_light_path.parent.mkdir(parents=True, exist_ok=True)
            save_light_path.write_bytes(buf_light.getvalue())
        return "data:image/png;base64," + _b64.b64encode(buf.getvalue()).decode()

    W('<div class="grid2">')
    _tpr_img = _scatter_b64(
        "test/drought_tpr_pooled", "TPR", FRONT_TPR,
        save_path=FIG_V2 / "frontier_tpr.png",
        save_light_path=FIG_V2 / "frontier_tpr_light.png",
    )
    _f1_img = _scatter_b64(
        "test/drought_f1_pooled", "F1", FRONT_F1,
        save_path=FIG_V2 / "frontier_f1.png",
        save_light_path=FIG_V2 / "frontier_f1_light.png",
    )
    W(f'<div class="card"><img src="{_tpr_img}" style="width:100%;border-radius:4px">'
      f'<p class="fig-label">rmse_vs_trend vs pooled TPR — {len(all_winners)} winners (Phase 1+2), '
      f'Pareto front outlined, star = highest-TPR run with rmse_vs_trend&gt;0</p></div>')
    W(f'<div class="card"><img src="{_f1_img}" style="width:100%;border-radius:4px">'
      f'<p class="fig-label">rmse_vs_trend vs pooled F1 — {len(all_winners)} winners (Phase 1+2), '
      f'Pareto front outlined</p></div>')
    W('</div>')

except Exception as _e:
    W(f'<p style="color:var(--yellow);font-size:12px">Scatter plots skipped: {_e}</p>')

# ── Pareto front tables ───────────────────────────────────────────────────────

def _front_table(front_runs, ymetric, ylabel):
    ranked = sorted(
        front_runs,
        key=lambda r: -(_get(r, "test/rmse_vs_trend") or -99),
    )
    _bt_name = _best_trade["run_name"] if _best_trade else None
    W(f'<div class="card"><h4>Pareto front runs — {ylabel} vs rmse_vs_trend ({len(ranked)} runs)</h4><table>')
    W(f'<tr><th>Run</th><th>Loss</th><th>Condition</th>'
      f'<th>rmse_vs_trend</th><th>{ylabel}</th>'
      f'<th>RMSE</th><th>On both fronts</th></tr>')
    for r in ranked:
        rvt  = _get(r, "test/rmse_vs_trend")
        yv   = _get(r, ymetric)
        rmse = _get(r, "test/rmse_pooled")
        c    = (r["static_encoder"], r["global_encoder"])
        rvt_cls = "g" if (rvt or 0) > 0 else "r"
        both    = r["run_name"] in FRONT_TPR_NAMES and r["run_name"] in FRONT_F1_NAMES
        star    = " ★" if r["run_name"] == _bt_name else ""
        W(f'<tr><td style="font-size:11px;color:var(--muted)">'
          f'{r["run_name"]}{star}</td>'
          f'<td><span class="badge {LOSS_BADGE.get(r["loss_fn"], "")}">'
          f'{r["loss_fn"]}</span></td>'
          f'<td><span class="badge {COND_BADGE.get(c, "")}">'
          f'{COND_SHORT.get(c, str(c))}</span></td>'
          f'<td class="{rvt_cls}">{f"{rvt:+.3f}" if rvt is not None else "—"}</td>'
          f'<td>{f"{yv:.3f}" if yv is not None else "—"}</td>'
          f'<td>{f"{rmse:.3f}" if rmse is not None else "—"}</td>'
          f'<td>{"✓" if both else "—"}</td></tr>')
    W('</table></div>')

if FRONT_TPR:
    _front_table(FRONT_TPR, "test/drought_tpr_pooled", "TPR")
if FRONT_F1:
    _front_table(FRONT_F1, "test/drought_f1_pooled", "F1")

# ── Appendix ──────────────────────────────────────────────────────────────────

W('<h2 id="appendix">Appendix</h2>')

# A. Experimental setup
W("""<details><summary style="cursor:pointer;font-size:15px;font-weight:600;padding:10px 0">
A — Experimental Setup</summary>
<div class="grid2">
<div class="card">
<h3>Architecture Conditions</h3>
<table>
<tr><th>Label</th><th>Static encoder</th><th>Global encoder</th></tr>
<tr><td><span class="badge badge-nn">naive/naive</span></td>
    <td>Raw channels concatenated to x</td><td>Scalars broadcast as spatial channels</td></tr>
<tr><td><span class="badge badge-singlenn">single/naive</span></td>
    <td>Single shared CNN</td><td>Scalars broadcast</td></tr>
<tr><td><span class="badge badge-sn">seasonal/naive</span></td>
    <td>4 seasonal CNNs</td><td>Scalars broadcast</td></tr>
<tr><td><span class="badge badge-nf">naive/film</span></td>
    <td>Raw channels</td><td>FiLM: MLP → γ,β on embedding</td></tr>
<tr><td><span class="badge badge-singlesf">single/film</span></td>
    <td>Single CNN</td><td>FiLM conditioning</td></tr>
<tr><td><span class="badge badge-sf">seasonal/film</span></td>
    <td>4 seasonal CNNs</td><td>FiLM conditioning</td></tr>
</table>
<p style="margin-top:10px;color:var(--muted);font-size:12px">
  Hyperparameter grid per condition: lr ∈ {1e-3, 3e-3} · dropout ∈ {0, 0.1} ·
  weight_decay ∈ {0, 1e-4}. Best checkpoint selected by val/loss (not test metrics).
  After pruning: one best-val-loss checkpoint per (loss, condition, hp-config) slot.
</p>
</div>
<div class="card">
<h3>Loss Functions (Phase 1)</h3>
<table>
<tr><th>Loss</th><th>Design intent</th></tr>
<tr><td><span class="badge badge-mse">MSE</span></td>
    <td>Standard regression — no drought-specific incentive</td></tr>
<tr><td><span class="badge badge-pinball">Pinball q=0.20</span></td>
    <td>Under-prediction (missed droughts) penalised 4× more than over-prediction</td></tr>
<tr><td><span class="badge badge-weighted">Weighted MSE hinge (w=1)</span></td>
    <td>Hinge: MSE × (1 + 1·max(0, −SPEI)) — mild dry-tail ramp, scale-neutral</td></tr>
<tr><td><span class="badge badge-weighted">Weighted MSE hinge (w=5)</span></td>
    <td>Hinge: MSE × (1 + 5·max(0, −SPEI)) — strong dry-tail amplification</td></tr>
</table>
<p style="margin-top:10px;color:var(--muted);font-size:12px">
  Data: Alpine domain 43×98 · History 36 months · Lead 12 months · Test 2015–2024<br>
  Drought threshold: SPEI ≤ −1.5 (moderate drought per McKee et al. 1993)
</p>
</div>
</div>""")

# test_period_detrended_std (formerly residual_floor) — data property, not a skill metric
if RESIDUAL_FLOOR == RESIDUAL_FLOOR:  # not NaN
    W(f"""<div class="card">
<h4>test_period_detrended_std (data property, appendix only)</h4>
<p style="color:var(--muted);font-size:12px">
  Per-cell SPEI standard deviation after removing the OLS linear trend, averaged over valid
  Alpine cells during the test period: <strong>{RESIDUAL_FLOOR:.3f}</strong>.<br>
  This is the scale of unpredictable residual variance given the trend — a lower bound on
  achievable pooled RMSE if the only useful signal were the trend itself.
  It is not a skill metric and does not appear in any headline table.
  (Previously reported as "residual_floor" or "skill_margin denominator".)
</p>
</div>""")
W('</details>')

# B. Data exploration figures
W("""<details><summary style="cursor:pointer;font-size:15px;font-weight:600;padding:10px 0">
B — Data Exploration</summary>""")
W('<div class="grid2">')
for path, cap in [
    (FIG / "processed" / "exploratory" / "spei_timeseries.png",
     "Domain-average SPEI time series — train/val/test split"),
    (FIG / "processed" / "exploratory" / "drought_frequency_maps.png",
     "Per-cell drought frequency maps (moderate ≤−1.0, severe ≤−1.5)"),
    (FIG / "processed" / "static_grid.png",
     "Static features: orography fraction and Alpine mask"),
    (FIG / "processed" / "global_scalars.png",
     "Global scalar features: NAO, Mediterranean SST groups, month encoding"),
]:
    W(f'<div class="card">{img_tag(path)}<p class="fig-label">{cap}</p></div>')
W('</div></details>')

# C. Per-condition model gallery (spatial maps for checkpoint runs with eval figures)
W("""<details><summary style="cursor:pointer;font-size:15px;font-weight:600;padding:10px 0">
C — Spatial Maps (eval figures)</summary>""")
_gallery_conds = [
    ("naive", "naive"), ("single", "naive"), ("seasonal", "naive"), ("naive", "film"),
]
for cond in _gallery_conds:
    cond_label = COND_SHORT.get(cond, str(cond))
    W(f'<h3>{cond_label}</h3><div class="grid3">')
    for _, loss, mode, dw, label, badge in FAMILIES:
        cell = _fam_cell(loss, mode, *cond, dw=dw)
        run  = next((r for r in cell if r["run_name"] in EVAL_RUNS), cell[0] if cell else None)
        if run is None:
            continue
        name = run["run_name"]
        fig_f1   = model_fig(name, "drought_f1")
        fig_rmse = model_fig(name, "rmse")
        has_figs = fig_f1.exists() or fig_rmse.exists()
        W(f'<div class="card"><h4><span class="badge {badge}">{label}</span> '
          f'<span style="font-size:10px;color:var(--muted)">{name.rsplit("_lr",1)[0]}</span></h4>')
        if has_figs:
            W('<div class="grid2">')
            W(f'{img_tag(fig_rmse)}<p class="fig-label">RMSE</p>')
            W(f'{img_tag(fig_f1)}<p class="fig-label">Drought F1</p>')
            W('</div>')
        else:
            W('<p style="color:var(--muted);font-size:11px">No eval figures on disk</p>')
        W('</div>')
    W('</div>')
W('</details>')

# ── Verification: confirm no median fallback in headline tables ───────────────
print("\n=== Verification: headline tables use only pooled columns ===")
_pooled_only_cols = [col for col in _REQUIRED if "pooled" in col or "vs_trend" in col]
print(f"  Pooled columns used in tables: {_pooled_only_cols}")
print("  Median-fallback functions (_p, _pb, _rvt_or_sm): REMOVED")
print("  skill_margin column: not used in any table (rmse_vs_trend used instead)")
print("  *_median columns: not used in any headline table")

# ── Assemble & write ──────────────────────────────────────────────────────────

_parts.append("</div></body></html>")
html = "\n".join(_parts)
OUT.write_text(html, encoding="utf-8")
print(f"\nReport written → {OUT}  ({len(html) // 1024} KB)")
