"""Generate reports/phase_summary.html — Phase 1 + Phase 2 summary.

Reads from saved_models_v2/gridsearch_comparison.csv.

Phase 1: one best-val/loss winner per (loss_fn, drought_weight) group — naive/naive arch.
Phase 2: all 4 loss groups × 5 arch conditions × 8 HP each (160 runs).
         Winners selected by val/loss within each (loss_fn, drought_weight, arch) group.

Run:
    uv run python reports/gen_phase_summary.py
"""

import csv
import re
from collections import defaultdict
from pathlib import Path

ROOT     = Path(__file__).parent.parent
CSV_PATH = ROOT / "saved_models_v2" / "gridsearch_comparison.csv"
OUT      = ROOT / "reports" / "phase_summary.html"

# ── Utilities ─────────────────────────────────────────────────────────────────

def _safe(v):
    try:
        f = float(v)
        return None if f != f else f
    except Exception:
        return None

_HP_FMT = {0.003: "3e-3", 0.001: "1e-3", 0.1: "0.1", 0.0: "0", 0.0001: "1e-4"}

def _fmt_hp(v):
    f = _safe(v)
    if f is None:
        return "?"
    return _HP_FMT.get(round(f, 6), f"{f:.4g}")

def fmt(v, dec=3):
    try:
        return f"{float(v):.{dec}f}"
    except Exception:
        return "—"

def pct(v):
    try:
        return f"{float(v)*100:.1f}%"
    except Exception:
        return "—"

def sgn(v, dec=3):
    try:
        f = float(v)
        return ("+" if f >= 0 else "") + f"{f:.{dec}f}"
    except Exception:
        return "—"

def _rvt_cls(v):
    f = _safe(v)
    if f is None:
        return ""
    return "g" if f >= 0 else "r"

# ── Load CSV ──────────────────────────────────────────────────────────────────
rows_all: list[dict] = []
with open(CSV_PATH) as fh:
    for r in csv.DictReader(fh):
        rows_all.append(r)

_REQUIRED = [
    "test/rmse_pooled",
    "test/drought_tpr_pooled",
    "test/drought_f1_pooled",
    "test/persistence/rmse_pooled",
    "test/trend/rmse_pooled",
    "test/rmse_vs_trend",
]

def _has_all(r):
    return all(_safe(r.get(c)) is not None for c in _REQUIRED)

# ── Phase 1 — one winner per (loss_fn, drought_weight) by val/loss ────────────
p1_all = [r for r in rows_all if str(r.get("phase", "")) == "1" and _has_all(r)]

def _p1_group_key(r):
    loss = r.get("loss_fn", "")
    dw   = r.get("drought_weight", "") if loss == "weighted_mse" else ""
    return (loss, dw)

p1_groups: dict = defaultdict(list)
for r in p1_all:
    p1_groups[_p1_group_key(r)].append(r)

p1_winners: dict = {
    k: sorted(v, key=lambda r: _safe(r.get("val/loss")) or float("inf"))[0]
    for k, v in p1_groups.items()
}
print(f"Phase 1 winners : {len(p1_winners)}")

# ── Phase 2 — arch extraction + winner per (loss, dw, arch) by val/loss ──────
p2_all = [r for r in rows_all if str(r.get("phase", "")) == "2" and _has_all(r)]

_LOSS_TAGS = ["wmse_w1_hinge", "wmse_w5_hinge", "pinball_q0.20", "mse"]

def _extract_arch(run_name: str):
    for tag in _LOSS_TAGS:
        prefix = f"phase2_{tag}_"
        if run_name.startswith(prefix):
            rest = run_name[len(prefix):]
            m = re.match(r"([a-z]+)_([a-z]+)_lr", rest)
            if m:
                return m.group(1), m.group(2)
    return None, None

def _p2_group_key(r):
    loss = r.get("loss_fn", "")
    dw   = r.get("drought_weight", "") if loss == "weighted_mse" else ""
    s, g = _extract_arch(r.get("run_name", ""))
    return (loss, dw, s, g)

p2_groups: dict = defaultdict(list)
for r in p2_all:
    p2_groups[_p2_group_key(r)].append(r)

p2_winners: dict = {
    k: sorted(v, key=lambda r: _safe(r.get("val/loss")) or float("inf"))[0]
    for k, v in p2_groups.items()
}
print(f"Phase 2 groups  : {len(p2_groups)}  ({len(p2_all)} rows total)")
for k in sorted(p2_groups):
    print(f"  {k}: {len(p2_groups[k])} rows")

# ── Baselines ─────────────────────────────────────────────────────────────────
_b = next(iter(p1_winners.values()))
PERSIST_RMSE = _safe(_b.get("test/persistence/rmse_pooled")) or float("nan")
PERSIST_F1   = _safe(_b.get("test/persistence/drought_f1_pooled")) or 0.0
PERSIST_TPR  = _safe(_b.get("test/persistence/drought_tpr_pooled")) or 0.0
CLIM_RMSE    = _safe(_b.get("test/clim/rmse_pooled")) or float("nan")
TREND_RMSE   = _safe(_b.get("test/trend/rmse_pooled")) or float("nan")

# ── Display config ────────────────────────────────────────────────────────────
# Phase 1: (loss_fn, dw|None, label, color, bg)
_P1_DISPLAY = [
    ("mse",          "",  "MSE",             "#5599ff", "#0d1830"),
    ("pinball",      "",  "Pinball q=0.20",  "#9b7dff", "#1a1440"),
    ("weighted_mse", "1", "WMse hinge w=1",  "#ffaa60", "#201808"),
    ("weighted_mse", "5", "WMse hinge w=5",  "#ff7b2c", "#241408"),
]

# Phase 2 loss groups (same 4, reused for P2 section headings)
_P2_LOSS_GROUPS = [
    ("mse",          "", "MSE",            "#5599ff"),
    ("pinball",      "", "Pinball q=0.20", "#9b7dff"),
    ("weighted_mse", "1","WMse hinge w=1", "#ffaa60"),
    ("weighted_mse", "5","WMse hinge w=5", "#ff7b2c"),
]

# Arch conditions — display order and styling
_ARCH_DISPLAY = [
    ("single",   "naive", "single / naive",   "#ffd098", "#201808"),
    ("seasonal", "naive", "seasonal / naive", "#ff7b2c", "#241408"),
    ("naive",    "film",  "naive / film",     "#4da6ff", "#0a1828"),
    ("single",   "film",  "single / film",    "#a78bfa", "#150e2a"),
    ("seasonal", "film",  "seasonal / film",  "#e85555", "#1e0a0a"),
]

def _find_p1(loss, dw):
    return p1_winners.get((loss, dw), {})

def _find_p2(loss, dw, static, global_):
    return p2_winners.get((loss, dw, static, global_), {})

# ── HTML helpers ──────────────────────────────────────────────────────────────

def _best_cls(val, vals, higher_better):
    f    = _safe(val)
    good = [v for v in vals if v is not None]
    if not good or f is None:
        return "mono"
    best = max(good) if higher_better else min(good)
    return "best mono" if abs(f - best) < 1e-9 else "mono"


def _p1_card(r, label, color, bg):
    rvt_cls = _rvt_cls(r.get("test/rmse_vs_trend"))
    hp = f"lr {_fmt_hp(r.get('lr'))} · do {_fmt_hp(r.get('dropout'))} · wd {_fmt_hp(r.get('weight_decay'))}"
    return f"""
    <div class="card" style="--c:{color}">
      <div class="family-label">{label}</div>
      <div class="big-metric mono">{fmt(r.get('test/drought_f1_pooled'))}</div>
      <div class="big-label">drought F1</div>
      <div class="secondary">
        <div class="kv"><span class="k">TPR</span>      <span class="v">{pct(r.get('test/drought_tpr_pooled'))}</span></div>
        <div class="kv"><span class="k">RMSE</span>     <span class="v">{fmt(r.get('test/rmse_pooled'))}</span></div>
        <div class="kv"><span class="k">vs trend</span> <span class="v {rvt_cls}">{sgn(r.get('test/rmse_vs_trend'))}</span></div>
        <div class="kv"><span class="k">ROCAUC</span>   <span class="v">{fmt(r.get('test/drought_rocauc_median'))}</span></div>
      </div>
      <div class="hp">{hp}</div>
    </div>"""


def _bar_w(val, max_val):
    f = _safe(val)
    if f is None or max_val == 0:
        return 0
    return min(int(f / max_val * 100), 100)


def _p1_bar_row(r, label, color, max_f1, max_rmse, kind):
    if kind == "f1":
        val = _safe(r.get("test/drought_f1_pooled"))
        w, txt = _bar_w(val, max_f1), fmt(val)
    else:
        val = _safe(r.get("test/rmse_pooled"))
        w, txt = _bar_w(val, max_rmse * 1.15), fmt(val)
    return f"""
      <div class="bar-row">
        <div class="bar-name">{label}</div>
        <div class="bar-track"><div class="bar-fill" style="width:{w}%;background:{color}"></div></div>
        <div class="bar-val">{txt}</div>
      </div>"""


def _p1_table_row(r, label, color, bg, f1_vals, rmse_vals):
    hp = f"{_fmt_hp(r.get('lr'))} / {_fmt_hp(r.get('dropout'))} / {_fmt_hp(r.get('weight_decay'))}"
    f1   = _safe(r.get("test/drought_f1_pooled"))
    rmse = _safe(r.get("test/rmse_pooled"))
    rvt  = _safe(r.get("test/rmse_vs_trend"))
    vs_p = (PERSIST_RMSE - rmse) if rmse is not None else None
    chip = f'<span class="family-chip" style="--c:{color};--c-bg:{bg}">{label}</span>'
    return f"""
        <tr>
          <td>{chip}</td>
          <td class="mono" style="color:var(--muted);font-size:11px">{hp}</td>
          <td class="mono">{pct(r.get('test/drought_tpr_pooled'))}</td>
          <td class="{_best_cls(f1, f1_vals, True)}">{fmt(f1)}</td>
          <td class="mono">{fmt(r.get('test/drought_rocauc_median'))}</td>
          <td class="{_best_cls(rmse, rmse_vals, False)}">{fmt(rmse)}</td>
          <td class="{_rvt_cls(rvt)}">{sgn(rvt)}</td>
          <td class="{_rvt_cls(vs_p)}">{sgn(vs_p)}</td>
        </tr>"""


def _p2_section(loss, dw, label, accent):
    """Generate full Phase 2 subsection for one loss group."""
    ref = _find_p1(loss, dw)
    arch_rows = [
        (s, g, lbl, col, bg, _find_p2(loss, dw, s, g))
        for s, g, lbl, col, bg in _ARCH_DISPLAY
    ]

    # Collect all F1/RMSE values for best-marking (ref + arch winners)
    all_rows = ([ref] if ref else []) + [r for *_, r in arch_rows if r]
    f1_vals   = [_safe(r.get("test/drought_f1_pooled"))  for r in all_rows]
    rmse_vals = [_safe(r.get("test/rmse_pooled"))         for r in all_rows]

    max_f1 = max((v for v in f1_vals if v is not None), default=0.5)

    # Bar chart
    ref_bar = ""
    if ref:
        w = _bar_w(_safe(ref.get("test/drought_f1_pooled")), max_f1)
        ref_bar = f"""
      <div class="bar-row">
        <div class="bar-name" style="color:var(--muted);font-style:italic">naive/naive ref</div>
        <div class="bar-track"><div class="bar-fill" style="width:{w}%;background:{accent};opacity:.45"></div></div>
        <div class="bar-val" style="color:var(--muted)">{fmt(ref.get('test/drought_f1_pooled'))}</div>
      </div>"""

    arch_bars = ""
    for s, g, lbl, col, bg, r in arch_rows:
        if not r:
            continue
        w   = _bar_w(_safe(r.get("test/drought_f1_pooled")), max_f1)
        arch_bars += f"""
      <div class="bar-row">
        <div class="bar-name">{lbl}</div>
        <div class="bar-track"><div class="bar-fill" style="width:{w}%;background:{col}"></div></div>
        <div class="bar-val">{fmt(r.get('test/drought_f1_pooled'))}</div>
      </div>"""

    # Table rows
    ref_row = ""
    if ref:
        chip = f'<span style="font-family:var(--mono);font-size:12px;color:var(--muted)">naive / naive</span><span style="font-size:10px;color:var(--muted);margin-left:6px">P1 ref</span>'
        rvt  = _safe(ref.get("test/rmse_vs_trend"))
        ref_row = f"""
        <tr style="opacity:.6">
          <td>{chip}</td>
          <td class="mono">{pct(ref.get('test/drought_tpr_pooled'))}</td>
          <td class="{_best_cls(_safe(ref.get('test/drought_f1_pooled')), f1_vals, True)}">{fmt(ref.get('test/drought_f1_pooled'))}</td>
          <td class="mono">{fmt(ref.get('test/drought_rocauc_median'))}</td>
          <td class="{_best_cls(_safe(ref.get('test/rmse_pooled')), rmse_vals, False)}">{fmt(ref.get('test/rmse_pooled'))}</td>
          <td class="{_rvt_cls(rvt)}">{sgn(rvt)}</td>
        </tr>"""

    arch_table_rows = ""
    for s, g, lbl, col, bg, r in arch_rows:
        if not r:
            arch_table_rows += f'<tr><td colspan="6" style="color:var(--muted);font-size:11px">{lbl} — no data</td></tr>'
            continue
        chip = f'<span class="family-chip" style="--c:{col};--c-bg:{bg}">{lbl}</span>'
        rvt  = _safe(r.get("test/rmse_vs_trend"))
        arch_table_rows += f"""
        <tr>
          <td>{chip}</td>
          <td class="mono">{pct(r.get('test/drought_tpr_pooled'))}</td>
          <td class="{_best_cls(_safe(r.get('test/drought_f1_pooled')), f1_vals, True)}">{fmt(r.get('test/drought_f1_pooled'))}</td>
          <td class="mono">{fmt(r.get('test/drought_rocauc_median'))}</td>
          <td class="{_best_cls(_safe(r.get('test/rmse_pooled')), rmse_vals, False)}">{fmt(r.get('test/rmse_pooled'))}</td>
          <td class="{_rvt_cls(rvt)}">{sgn(rvt)}</td>
        </tr>"""

    return f"""
  <div class="p2-loss-block">
    <div class="p2-loss-heading" style="--accent:{accent}">
      <span class="p2-loss-dot"></span>{label}
    </div>
    <div class="bar-panel" style="margin-bottom:16px">{ref_bar}{arch_bars}
    </div>
    <div class="tbl-wrap">
      <table>
        <thead>
          <tr>
            <th>Architecture</th>
            <th>TPR ↑</th><th>F1 ↑</th><th>ROCAUC† ↑</th><th>RMSE ↓</th><th>vs trend</th>
          </tr>
        </thead>
        <tbody>{ref_row}{arch_table_rows}
        </tbody>
      </table>
    </div>
  </div>"""


# ── Scatter data ──────────────────────────────────────────────────────────────

def _scatter_data():
    pts = []
    for loss, dw, label, color, _ in _P1_DISPLAY:
        r   = _find_p1(loss, dw)
        rvt = _safe(r.get("test/rmse_vs_trend")) or 0.0
        f1  = _safe(r.get("test/drought_f1_pooled")) or 0.0
        pts.append(f'  {{ name: {label!r}, x: {rvt:.4f}, y: {f1:.4f}, r: 9, color: {color!r} }}')
    persist_rvt = TREND_RMSE - PERSIST_RMSE
    pts.append(f'  {{ name: "Trend baseline", x: 0.0000, y: 0.0000, r: 5, color: "#4e5e7a", ref: true }}')
    pts.append(f'  {{ name: "Persistence",   x: {persist_rvt:.4f}, y: {PERSIST_F1:.4f}, r: 5, color: "#4e5e7a", ref: true }}')
    pts.append(f'  {{ name: "Climatology",   x: {TREND_RMSE - CLIM_RMSE:.4f}, y: 0.0000, r: 5, color: "#4e5e7a", ref: true }}')
    return "[\n" + ",\n".join(pts) + "\n]"


# ── Phase 2 scatter data ──────────────────────────────────────────────────────
# 20 winners: colour by loss family, shape/label by arch condition

_LOSS_COLOR = {
    ("mse",          ""): "#5599ff",
    ("pinball",      ""): "#9b7dff",
    ("weighted_mse", "1"): "#ffaa60",
    ("weighted_mse", "5"): "#ff7b2c",
}

def _p2_scatter_data():
    pts = []
    for s, g, arch_lbl, col, bg in _ARCH_DISPLAY:
        for loss, dw, loss_lbl, loss_col in _P2_LOSS_GROUPS:
            r = _find_p2(loss, dw, s, g)
            if not r:
                continue
            rvt = _safe(r.get("test/rmse_vs_trend")) or 0.0
            f1  = _safe(r.get("test/drought_f1_pooled")) or 0.0
            name = f"{loss_lbl} / {arch_lbl}"
            pts.append(f'  {{ name: {name!r}, x: {rvt:.4f}, y: {f1:.4f}, r: 7, '
                       f'color: {loss_col!r}, arch: {arch_lbl!r} }}')
    # Phase 1 refs (greyed out, for comparison)
    for loss, dw, loss_lbl, loss_col, _ in _P1_DISPLAY:
        r = _find_p1(loss, dw)
        if not r:
            continue
        rvt = _safe(r.get("test/rmse_vs_trend")) or 0.0
        f1  = _safe(r.get("test/drought_f1_pooled")) or 0.0
        pts.append(f'  {{ name: {("P1 " + loss_lbl)!r}, x: {rvt:.4f}, y: {f1:.4f}, '
                   f'r: 5, color: {loss_col!r}, ref: true }}')
    return "[\n" + ",\n".join(pts) + "\n]"


# ── Compute Phase 1 summary values ────────────────────────────────────────────

p1_data      = [(_find_p1(lf, dw), lbl, col, bg) for lf, dw, lbl, col, bg in _P1_DISPLAY]
p1_f1_vals   = [_safe(r.get("test/drought_f1_pooled")) for r, *_ in p1_data]
p1_rmse_vals = [_safe(r.get("test/rmse_pooled"))       for r, *_ in p1_data]
max_p1_f1    = max(v for v in p1_f1_vals  if v is not None)
max_p1_rmse  = max(v for v in p1_rmse_vals if v is not None)

p1_cards     = "".join(_p1_card(r, lbl, col, bg)          for r, lbl, col, bg in p1_data)
p1_f1_bars   = "".join(_p1_bar_row(r, lbl, col, max_p1_f1, max_p1_rmse, "f1")   for r, lbl, col, _ in p1_data)
p1_rmse_bars = "".join(_p1_bar_row(r, lbl, col, max_p1_f1, max_p1_rmse, "rmse") for r, lbl, col, _ in p1_data)
p1_table_rows = "".join(_p1_table_row(r, lbl, col, bg, p1_f1_vals, p1_rmse_vals) for r, lbl, col, bg in p1_data)

# Phase 2 sections for all 4 loss groups
p2_sections = "".join(_p2_section(loss, dw, label, accent) for loss, dw, label, accent in _P2_LOSS_GROUPS)

scatter_data    = _scatter_data()
p2_scatter_data = _p2_scatter_data()

all_p1_rvt  = [_safe(r.get("test/rmse_vs_trend")) for r, *_ in p1_data if _safe(r.get("test/rmse_vs_trend")) is not None]
persist_rvt = TREND_RMSE - PERSIST_RMSE
x_min = min(min(all_p1_rvt), persist_rvt) - 0.06
x_max = max(all_p1_rvt) + 0.06
y_max = max(p1_f1_vals + [PERSIST_F1]) * 1.20 + 0.02

# Phase 2 scatter bounds: all 20 winners
_p2_rvt_vals = [_safe(r.get("test/rmse_vs_trend")) for r in p2_winners.values() if _safe(r.get("test/rmse_vs_trend")) is not None]
_p2_f1_vals  = [_safe(r.get("test/drought_f1_pooled")) for r in p2_winners.values() if _safe(r.get("test/drought_f1_pooled")) is not None]
p2_x_min = min(_p2_rvt_vals + all_p1_rvt + [persist_rvt]) - 0.06
p2_x_max = max(_p2_rvt_vals + all_p1_rvt) + 0.06
p2_y_max = max(_p2_f1_vals + p1_f1_vals) * 1.15 + 0.02

# ── CSS ───────────────────────────────────────────────────────────────────────

CSS = """
:root {
  --bg:#0a0d1a; --surface:#0f1426; --surface2:#141b32; --border:#1a2140;
  --text:#c8d5ef; --muted:#4e5e7a; --accent:#4b8eff;
  --green:#20c896; --red:#e85555; --yellow:#dba030;
  --mono:'SF Mono','Fira Code','Consolas','Courier New',monospace;
  --sans:-apple-system,BlinkMacSystemFont,'Segoe UI','Helvetica Neue',Arial,sans-serif;
}
@media (prefers-color-scheme:light){:root{
  --bg:#f0f4fc;--surface:#ffffff;--surface2:#eaeff9;--border:#d0daf0;
  --text:#1a1f38;--muted:#5a6585;--accent:#2d6ee8;--green:#0caa78;--red:#cc3333;--yellow:#b87e10;
}}
:root[data-theme="light"]{--bg:#f0f4fc;--surface:#ffffff;--surface2:#eaeff9;--border:#d0daf0;--text:#1a1f38;--muted:#5a6585;--accent:#2d6ee8;--green:#0caa78;--red:#cc3333;--yellow:#b87e10;}
:root[data-theme="dark"]{--bg:#0a0d1a;--surface:#0f1426;--surface2:#141b32;--border:#1a2140;--text:#c8d5ef;--muted:#4e5e7a;--accent:#4b8eff;--green:#20c896;--red:#e85555;--yellow:#dba030;}
*,*::before,*::after{box-sizing:border-box;}
body{background:var(--bg);color:var(--text);font-family:var(--sans);margin:0;}
.wrap{max-width:940px;margin:0 auto;padding:32px 20px 80px;}
h1{font-size:1.8rem;font-weight:700;margin:0 0 6px;}
h2{font-size:.9rem;font-weight:600;text-transform:uppercase;letter-spacing:.08em;color:var(--muted);margin:0 0 20px;}
.mono{font-family:var(--mono);font-variant-numeric:tabular-nums;}
.g{color:var(--green);font-family:var(--mono);font-variant-numeric:tabular-nums;}
.r{color:var(--red);font-family:var(--mono);font-variant-numeric:tabular-nums;}
.best{font-weight:700;color:var(--text);}
.rule{border:none;border-top:1px solid var(--border);margin:40px 0;}
.hdr{margin-bottom:36px;}
.run-meta{display:flex;align-items:center;gap:8px;font-size:11px;color:var(--muted);text-transform:uppercase;letter-spacing:.08em;margin-bottom:12px;}
.dot{width:6px;height:6px;border-radius:50%;background:var(--green);}
.sub{font-size:13px;color:var(--muted);margin:4px 0 0;line-height:1.6;}
.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-bottom:40px;}
@media(max-width:700px){.cards{grid-template-columns:1fr 1fr;}}
.card{background:var(--surface);border:1px solid var(--border);border-top:3px solid var(--c,#555);border-radius:2px;padding:16px;}
.card .family-label{font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:.08em;color:var(--c,var(--muted));margin-bottom:10px;}
.card .big-metric{font-family:var(--mono);font-size:1.6rem;font-weight:700;color:var(--text);line-height:1;}
.card .big-label{font-size:10px;color:var(--muted);margin-top:2px;margin-bottom:12px;text-transform:uppercase;letter-spacing:.06em;}
.card .secondary{display:flex;flex-direction:column;gap:4px;}
.card .kv{display:flex;justify-content:space-between;font-family:var(--mono);font-size:11.5px;}
.card .kv .k{color:var(--muted);}
.card .kv .v{font-weight:600;}
.card .hp{margin-top:10px;padding-top:10px;border-top:1px solid var(--border);font-size:10.5px;font-family:var(--mono);color:var(--muted);}
.bars-grid{display:grid;grid-template-columns:1fr 1fr;gap:20px;margin-bottom:40px;}
@media(max-width:600px){.bars-grid{grid-template-columns:1fr;}}
.bar-panel{background:var(--surface);border:1px solid var(--border);border-radius:2px;padding:20px;}
.bar-row{display:flex;align-items:center;gap:10px;margin-bottom:10px;}
.bar-row:last-child{margin-bottom:0;}
.bar-name{font-size:11.5px;color:var(--muted);width:140px;flex-shrink:0;}
.bar-track{flex:1;background:var(--surface2);border-radius:2px;height:10px;}
.bar-fill{height:100%;border-radius:2px;}
.bar-val{font-family:var(--mono);font-size:11.5px;color:var(--text);width:44px;text-align:right;flex-shrink:0;}
.tbl-wrap{overflow-x:auto;margin-bottom:24px;}
table{width:100%;border-collapse:collapse;font-size:12.5px;}
th{padding:8px 12px;text-align:left;color:var(--muted);font-size:10.5px;text-transform:uppercase;
   letter-spacing:.07em;font-weight:600;border-bottom:1px solid var(--border);white-space:nowrap;background:var(--surface);}
td{padding:10px 12px;border-bottom:1px solid var(--border);background:var(--surface);white-space:nowrap;}
tr:hover td{background:var(--surface2);}
td.mono{font-family:var(--mono);font-variant-numeric:tabular-nums;}
td.best{font-weight:700;color:var(--text);}
.family-chip{display:inline-block;padding:2px 8px;border-radius:3px;font-size:11.5px;font-weight:600;
             color:var(--c,#aaa);background:var(--c-bg,#1a2140);border:1px solid var(--c,#555);white-space:nowrap;}
.scatter-wrap{background:var(--surface);border:1px solid var(--border);border-radius:2px;padding:20px;margin-bottom:40px;}
.scatter-wrap canvas{display:block;max-width:100%;}
.baselines{display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin-bottom:40px;}
@media(max-width:600px){.baselines{grid-template-columns:1fr;}}
.baseline-card{background:var(--surface);border:1px solid var(--border);border-radius:2px;padding:14px 16px;}
.baseline-card .blabel{font-size:11px;color:var(--muted);margin-bottom:4px;}
.baseline-card .bval{font-family:var(--mono);font-size:1.1rem;font-weight:700;color:var(--text);}
.baseline-card .bnote{font-size:10.5px;color:var(--muted);margin-top:4px;}
.decision{border:1px solid var(--border);border-radius:2px;padding:20px 24px;background:var(--surface);margin-bottom:40px;}
.decision h2{margin-bottom:12px;}
.decision ul{padding-left:18px;}
.decision ul li{font-size:13px;color:var(--muted);line-height:1.8;margin-bottom:4px;}
.decision ul li strong{color:var(--text);}
.decision code{font-family:var(--mono);font-size:11px;background:var(--surface2);padding:1px 5px;border-radius:3px;}
.phase-heading{display:flex;align-items:baseline;gap:12px;margin-bottom:20px;}
.phase-heading h2{margin:0;}
.phase-pill{font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.1em;padding:2px 8px;
            border-radius:2px;background:var(--surface2);color:var(--muted);border:1px solid var(--border);}
.p2-grid{display:grid;grid-template-columns:1fr 1fr;gap:28px;margin-bottom:40px;}
@media(max-width:700px){.p2-grid{grid-template-columns:1fr;}}
.p2-loss-block{background:var(--surface);border:1px solid var(--border);border-radius:2px;padding:20px;}
.p2-loss-heading{display:flex;align-items:center;gap:8px;font-size:12px;font-weight:700;
                 text-transform:uppercase;letter-spacing:.07em;margin-bottom:16px;color:var(--accent);}
.p2-loss-dot{width:8px;height:8px;border-radius:50%;background:var(--accent,#555);flex-shrink:0;}
"""

# ── Main HTML ─────────────────────────────────────────────────────────────────

html = f"""<title>Phase 1 + 2 — Loss &amp; Architecture Results</title>
<style>{CSS}</style>

<div class="wrap">

  <div class="hdr">
    <div class="run-meta">
      <span class="dot"></span>
      Phase 1 · naive/naive · 32 runs → 4 winners &nbsp;·&nbsp;
      Phase 2 · 4 losses × 5 archs × 8 HP = 160 runs → 20 winners
    </div>
    <h1>Loss Function &amp; Architecture Results</h1>
    <p class="sub">
      Phase 1: one best-val/loss winner per loss group on naive/naive architecture.
      Phase 2: full HP grid per (loss × architecture) condition, winner by val/loss.
      Test period 2015–2024 · SPEI regression · drought threshold ≤ −1.5.
    </p>
    <p class="sub" style="margin-top:6px;font-size:11.5px;color:var(--muted)">
      All metrics are <strong style="color:var(--text)">pooled test metrics</strong>
      across all valid Alpine cell-months, unless noted.
      Exception: ROCAUC is per-cell median (†).
    </p>
  </div>

  <!-- ── Phase 1 ────────────────────────────────────────────────────────────── -->
  <div class="phase-heading">
    <span class="phase-pill">Phase 1</span>
    <h2>Loss function winners — naive/naive architecture</h2>
  </div>

  <div class="cards">{p1_cards}
  </div>

  <div class="bars-grid">
    <div class="bar-panel">
      <p style="margin-bottom:14px;font-size:12px;color:var(--muted)">
        Drought F1 — <strong style="color:var(--text)">higher is better</strong>
      </p>{p1_f1_bars}
    </div>
    <div class="bar-panel">
      <p style="margin-bottom:14px;font-size:12px;color:var(--muted)">
        RMSE — <strong style="color:var(--text)">lower is better</strong>
        <span style="float:right;font-size:10px">trend = {fmt(TREND_RMSE)}</span>
      </p>{p1_rmse_bars}
    </div>
  </div>

  <div class="tbl-wrap">
    <table>
      <thead>
        <tr>
          <th>Loss family</th><th>HP (lr/do/wd)</th>
          <th>TPR ↑</th><th>F1 ↑</th><th>ROCAUC† ↑</th><th>RMSE ↓</th><th>vs trend</th><th>vs persist</th>
        </tr>
      </thead>
      <tbody>{p1_table_rows}
      </tbody>
    </table>
  </div>
  <p style="margin-top:-20px;margin-bottom:40px;font-size:11px">
    <strong style="color:var(--text)">Bold</strong> = best in column.
    vs trend / vs persist = reference RMSE − model RMSE (positive = model wins).
    † ROCAUC is per-cell median (not pooled).
  </p>

  <h2>Drought skill vs RMSE (Phase 1)</h2>
  <div class="scatter-wrap">
    <p style="margin-bottom:16px;font-size:12px">
      x: trend RMSE − model RMSE (positive = beats trend) · y: drought F1 · dotted = trend baseline
    </p>
    <canvas id="scatter" width="860" height="280"></canvas>
  </div>

  <h2>Reference baselines</h2>
  <div class="baselines">
    <div class="baseline-card">
      <div class="blabel">Persistence (12-month lag)</div>
      <div class="bval mono">{fmt(PERSIST_RMSE)} RMSE</div>
      <div class="bnote">TPR {pct(PERSIST_TPR)} · F1 {fmt(PERSIST_F1)}</div>
    </div>
    <div class="baseline-card">
      <div class="blabel">Climatology (cell training mean)</div>
      <div class="bval mono">{fmt(CLIM_RMSE)} RMSE</div>
      <div class="bnote">TPR 0% · F1 0 — never predicts drought</div>
    </div>
    <div class="baseline-card">
      <div class="blabel">Linear trend extrapolation</div>
      <div class="bval mono">{fmt(TREND_RMSE)} RMSE</div>
      <div class="bnote">TPR 0% · F1 0 — reference for vs trend column</div>
    </div>
  </div>

  <hr class="rule">

  <!-- ── Phase 2 ────────────────────────────────────────────────────────────── -->
  <div class="phase-heading">
    <span class="phase-pill">Phase 2</span>
    <h2>Architecture sweep — all loss groups</h2>
  </div>
  <p style="margin-top:-12px;margin-bottom:28px;font-size:12px;color:var(--muted)">
    5 architecture conditions: single/naive, seasonal/naive, naive/film, single/film, seasonal/film.
    HP grid: lr ∈ {{1e-3, 3e-3}} × do ∈ {{0, 0.1}} × wd ∈ {{0, 1e-4}} = 8 combos per condition.
    Winner selected by val/loss within each (loss, arch) group.
    naive/naive is the Phase 1 row at matching HP — not rerun.
  </p>

  <div class="p2-grid">{p2_sections}
  </div>
  <p style="margin-top:-20px;margin-bottom:40px;font-size:11px">
    <strong style="color:var(--text)">Bold</strong> = best in column (including P1 ref).
    P1 ref row shown greyed out. † ROCAUC is per-cell median (not pooled).
  </p>

  <h2>Drought skill vs RMSE (Phase 2 — all winners)</h2>
  <div class="scatter-wrap">
    <p style="margin-bottom:16px;font-size:12px">
      x: trend RMSE − model RMSE (positive = beats trend) · y: drought F1 ·
      colour = loss family · Phase 1 references shown faded
    </p>
    <canvas id="scatter2" width="860" height="320"></canvas>
    <div style="display:flex;flex-wrap:wrap;gap:16px;margin-top:14px;font-size:11px">
      <span><span style="display:inline-block;width:10px;height:10px;background:#5599ff;border-radius:50%;margin-right:4px"></span>MSE</span>
      <span><span style="display:inline-block;width:10px;height:10px;background:#9b7dff;border-radius:50%;margin-right:4px"></span>Pinball</span>
      <span><span style="display:inline-block;width:10px;height:10px;background:#ffaa60;border-radius:50%;margin-right:4px"></span>WMse w=1</span>
      <span><span style="display:inline-block;width:10px;height:10px;background:#ff7b2c;border-radius:50%;margin-right:4px"></span>WMse w=5</span>
      <span style="color:var(--muted)">Faded = Phase 1 naive/naive reference</span>
    </div>
  </div>

  <hr class="rule">

  <!-- ── Findings ───────────────────────────────────────────────────────────── -->
  <div class="decision">
    <h2>Phase 2 — findings</h2>
    <ul>
      <li>
        <strong>Pinball: all architectures improve on Phase 1.</strong>
        Every Phase 2 arch beats the naive/naive ref (F1 = 0.384). single/film and naive/film lead
        on F1 (≈0.449/0.448); seasonal/naive and single/naive give the best RMSE trade-off
        (−0.077/−0.079 vs trend). seasonal/film pushes TPR highest (57.7%) but pays the largest
        RMSE penalty (−0.126).
      </li>
      <li>
        <strong>WMse w=1: Phase 2 archs regress below the Phase 1 ref.</strong>
        naive/naive (F1 = 0.277) beats all 5 Phase 2 conditions after proper HP selection.
        seasonal/naive comes closest (0.251). The loss signal is too weak to drive consistent
        drought learning across varying architectures.
      </li>
      <li>
        <strong>WMse w=5: single/film competitive, seasonal/naive collapses.</strong>
        single/film (F1 = 0.275) nearly matches the Phase 1 ref (0.297). seasonal/naive drops to
        F1 = 0.077 — a likely training instability worth investigating.
      </li>
      <li>
        <strong>MSE: no architecture recovers drought detection.</strong>
        F1 stays below 0.165 across all conditions. seasonal/naive is marginally best (0.165) but
        this is not meaningful drought skill.
      </li>
      <li>
        <strong>HP confound confirmed for FILM architectures.</strong>
        With the single Phase 1 HP, naive/film (wmse w=1) had F1 = 0.096.
        After the grid sweep the best-val/loss HP still gives 0.094 — the architecture itself
        underperforms for this loss, not just the HP.
        For pinball, FILM architectures work well (F1 ≈ 0.45).
      </li>
    </ul>
  </div>

</div>

<script>
(function() {{
  const canvas = document.getElementById('scatter');
  const ctx = canvas.getContext('2d');
  const W = canvas.width, H = canvas.height;
  const PAD = {{ top:20, right:30, bottom:50, left:56 }};
  const cw = W-PAD.left-PAD.right, ch = H-PAD.top-PAD.bottom;
  const isDark = document.documentElement.dataset.theme
    ? document.documentElement.dataset.theme==='dark'
    : window.matchMedia('(prefers-color-scheme:dark)').matches;
  const C = isDark
    ? {{bg:'#0f1426',grid:'#1a2140',text:'#4e5e7a',zero:'#4e5e7a'}}
    : {{bg:'#ffffff',grid:'#d0daf0',text:'#8892b0',zero:'#8892b0'}};
  canvas.style.background = C.bg;
  const data = {scatter_data};
  const xMin={x_min:.3f}, xMax={x_max:.3f}, yMin=-0.02, yMax={y_max:.3f};
  const tx = v => PAD.left+(v-xMin)/(xMax-xMin)*cw;
  const ty = v => PAD.top+(1-(v-yMin)/(yMax-yMin))*ch;
  ctx.strokeStyle=C.grid; ctx.lineWidth=1;
  for(let y=0;y<={y_max:.1f};y=Math.round((y+.1)*100)/100){{
    ctx.beginPath();ctx.moveTo(PAD.left,ty(y));ctx.lineTo(PAD.left+cw,ty(y));
    ctx.setLineDash([3,4]);ctx.stroke();ctx.setLineDash([]);
  }}
  ctx.strokeStyle=C.zero;ctx.setLineDash([5,4]);
  ctx.beginPath();ctx.moveTo(tx(0),PAD.top);ctx.lineTo(tx(0),PAD.top+ch);ctx.stroke();ctx.setLineDash([]);
  ctx.strokeStyle=C.grid;ctx.lineWidth=1;ctx.beginPath();
  ctx.moveTo(PAD.left,PAD.top);ctx.lineTo(PAD.left,PAD.top+ch);
  ctx.moveTo(PAD.left,PAD.top+ch);ctx.lineTo(PAD.left+cw,PAD.top+ch);ctx.stroke();
  ctx.fillStyle=C.text;ctx.font='11px Consolas,monospace';ctx.textAlign='center';
  for(let x=Math.ceil(xMin*10)/10;x<=xMax;x=Math.round((x+.1)*100)/100)
    ctx.fillText((x>=0?'+':'')+x.toFixed(1),tx(x),PAD.top+ch+16);
  ctx.textAlign='right';
  for(let y=0;y<={y_max:.1f};y=Math.round((y+.1)*100)/100)
    ctx.fillText(y.toFixed(1),PAD.left-8,ty(y)+4);
  ctx.fillStyle=C.text;ctx.font='11px system-ui,sans-serif';ctx.textAlign='center';
  ctx.fillText('rmse_vs_trend → positive = model beats linear trend',PAD.left+cw/2,H-4);
  ctx.save();ctx.translate(14,PAD.top+ch/2);ctx.rotate(-Math.PI/2);
  ctx.fillText('test drought F1',0,0);ctx.restore();
  ctx.fillStyle=isDark?'rgba(32,200,150,.07)':'rgba(12,170,120,.06)';
  ctx.fillRect(tx(0),PAD.top,PAD.left+cw-tx(0),ch);
  ctx.fillStyle=isDark?'rgba(32,200,150,.3)':'rgba(12,170,120,.5)';
  ctx.font='10px system-ui,sans-serif';ctx.textAlign='left';
  ctx.fillText('beats trend →',tx(0)+6,PAD.top+14);
  data.forEach(d=>{{
    const px=tx(d.x),py=ty(d.y);
    ctx.beginPath();ctx.arc(px,py,d.r,0,Math.PI*2);
    ctx.fillStyle=d.ref?C.grid:d.color;ctx.fill();
    if(!d.ref){{ctx.strokeStyle='rgba(0,0,0,.3)';ctx.lineWidth=1;ctx.stroke();}}
    if(!d.ref){{
      ctx.fillStyle=d.color;ctx.font='11.5px system-ui,sans-serif';
      ctx.textAlign=d.x<0?'right':'left';
      const ox=d.x<0?-(d.r+5):(d.r+5);
      const oy=d.name.includes('w=1')?-12:d.name.includes('w=5')?12:0;
      ctx.fillText(d.name,px+ox,py+4+oy);
    }}else{{
      ctx.fillStyle=C.text;ctx.font='10px system-ui,sans-serif';ctx.textAlign='left';
      ctx.fillText(d.name,px+7,py+4);
    }}
  }});
}})();

(function() {{
  const canvas = document.getElementById('scatter2');
  if (!canvas) return;
  const ctx = canvas.getContext('2d');
  const W = canvas.width, H = canvas.height;
  const PAD = {{ top:20, right:30, bottom:50, left:56 }};
  const cw = W-PAD.left-PAD.right, ch = H-PAD.top-PAD.bottom;
  const isDark = document.documentElement.dataset.theme
    ? document.documentElement.dataset.theme==='dark'
    : window.matchMedia('(prefers-color-scheme:dark)').matches;
  const C = isDark
    ? {{bg:'#0f1426',grid:'#1a2140',text:'#4e5e7a',zero:'#4e5e7a'}}
    : {{bg:'#ffffff',grid:'#d0daf0',text:'#8892b0',zero:'#8892b0'}};
  canvas.style.background = C.bg;
  const data = {p2_scatter_data};
  const xMin={p2_x_min:.3f}, xMax={p2_x_max:.3f}, yMin=-0.02, yMax={p2_y_max:.3f};
  const tx = v => PAD.left+(v-xMin)/(xMax-xMin)*cw;
  const ty = v => PAD.top+(1-(v-yMin)/(yMax-yMin))*ch;
  ctx.strokeStyle=C.grid; ctx.lineWidth=1; ctx.setLineDash([3,4]);
  for (let y=0; y<={p2_y_max:.1f}; y=Math.round((y+.1)*100)/100) {{
    ctx.beginPath(); ctx.moveTo(PAD.left,ty(y)); ctx.lineTo(PAD.left+cw,ty(y)); ctx.stroke();
  }}
  ctx.setLineDash([]);
  ctx.strokeStyle=C.zero; ctx.setLineDash([5,4]);
  ctx.beginPath(); ctx.moveTo(tx(0),PAD.top); ctx.lineTo(tx(0),PAD.top+ch); ctx.stroke();
  ctx.setLineDash([]);
  ctx.strokeStyle=C.grid; ctx.lineWidth=1;
  ctx.beginPath();
  ctx.moveTo(PAD.left,PAD.top); ctx.lineTo(PAD.left,PAD.top+ch);
  ctx.moveTo(PAD.left,PAD.top+ch); ctx.lineTo(PAD.left+cw,PAD.top+ch);
  ctx.stroke();
  ctx.fillStyle=C.text; ctx.font='11px Consolas,monospace'; ctx.textAlign='center';
  for (let x=Math.ceil(xMin*10)/10; x<=xMax; x=Math.round((x+.1)*100)/100)
    ctx.fillText((x>=0?'+':'')+x.toFixed(1), tx(x), PAD.top+ch+16);
  ctx.textAlign='right';
  for (let y=0; y<={p2_y_max:.1f}; y=Math.round((y+.1)*100)/100)
    ctx.fillText(y.toFixed(1), PAD.left-8, ty(y)+4);
  ctx.fillStyle=C.text; ctx.font='11px system-ui,sans-serif'; ctx.textAlign='center';
  ctx.fillText('rmse_vs_trend → positive = beats linear trend', PAD.left+cw/2, H-4);
  ctx.save(); ctx.translate(14,PAD.top+ch/2); ctx.rotate(-Math.PI/2);
  ctx.fillText('test drought F1', 0, 0); ctx.restore();
  ctx.fillStyle=isDark?'rgba(32,200,150,.06)':'rgba(12,170,120,.05)';
  ctx.fillRect(tx(0),PAD.top,PAD.left+cw-tx(0),ch);
  // Draw ref points first (faded), then live points on top
  data.filter(d=>d.ref).forEach(d=>{{
    const px=tx(d.x), py=ty(d.y);
    ctx.globalAlpha=0.3;
    ctx.beginPath(); ctx.arc(px,py,d.r,0,Math.PI*2);
    ctx.fillStyle=d.color; ctx.fill();
    ctx.globalAlpha=1;
  }});
  data.filter(d=>!d.ref).forEach(d=>{{
    const px=tx(d.x), py=ty(d.y);
    ctx.beginPath(); ctx.arc(px,py,d.r,0,Math.PI*2);
    ctx.fillStyle=d.color; ctx.fill();
    ctx.strokeStyle='rgba(0,0,0,.25)'; ctx.lineWidth=0.8; ctx.stroke();
    // label arch condition in muted text
    ctx.fillStyle=isDark?'rgba(200,213,239,.55)':'rgba(26,31,56,.55)';
    ctx.font='9px system-ui,sans-serif'; ctx.textAlign='left';
    ctx.fillText(d.arch, px+d.r+3, py+3);
  }});
}})();
</script>
"""

OUT.write_text(html, encoding="utf-8")
print(f"\nWrote {OUT}  ({OUT.stat().st_size // 1024} KB)")
