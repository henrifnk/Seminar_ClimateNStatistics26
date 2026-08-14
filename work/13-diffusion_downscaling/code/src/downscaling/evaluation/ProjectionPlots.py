"""
Plotting routines for Parts 2 & 3 of ProjectionPeriodAssessment.py — the GWL-period
climatology diagnostics.

Every function here takes already-computed data (setup / gwl_results / pixel-value
arrays): nothing in this module runs the sampler or opens a data file. Output is a
different matter — each function writes its figure(s) under plots/projection/ as SVG,
so calling one is not side-effect free. Figures are not logged to Weights & Biases —
wandb.Image() can't read SVG.

Figures, in the order ProjectionPeriodAssessment calls them:
    _plot_delta                   2_delta/  — one 2x3 panel per period, plus
                                              all_periods.svg across all periods
    _plot_period_means            3_period_means.svg
    _plot_distribution            4_distribution_periods.svg
    _plot_quantile_delta          5_quantile_delta.svg
    _plot_spread_persistence      6_spread_persistence.svg

(the downscaling-correction map, formerly 4_downscaling_correction.svg here, now lives
in CalibrationPeriodAssessment.py — the held-out CERRA test period, not a GWL period)
"""
import os

import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import numpy as np
from matplotlib.gridspec import GridSpec

import wandb
from downscaling.evaluation.EnsembleInference import COLOR_PR, COLOR_TAS
from downscaling.paths import PROJECTION_DIR

OUT_DIR = str(PROJECTION_DIR)

# Global Warming Level periods: (label, window_start, window_end)
GWL_PERIODS = [
    ("PRESENT", 2001, 2020),
    ("GWL2", 2021, 2040),
    ("GWL3", 2041, 2060),
    ("GWL4", 2061, 2080),
]


def _cache_slug(label):
    """Cache-file slug for a GWL period label. PRESENT reuses the old 'gwl1' slug
    (PRESENT was renamed from GWL1) so existing caches are still recognized."""
    if label == "PRESENT":
        return "gwl1"
    return label.lower().replace(" ", "_")


def _log_x_wet_days(ax):
    """Switch `ax` to a log x-axis with readable wet-day-rate ticks (1, 2, 5, 10, …).

    The wet-day filter puts the left limit a hair above 1 mm/day, so only one whole
    decade lands strictly inside the view. Matplotlib then falls back to labelling
    every minor tick in scientific notation, which collides into unreadable mush at
    these panel widths — pinning the locator/formatter avoids that regardless of limits.
    """
    ax.set_xscale("log")
    ax.xaxis.set_major_locator(mticker.LogLocator(base=10.0, subs=(1.0, 2.0, 5.0)))
    ax.xaxis.set_major_formatter(mticker.FuncFormatter(lambda val, _: f"{val:g}"))
    ax.xaxis.set_minor_formatter(mticker.NullFormatter())


def _pixel_area_m2(setup):
    """Per-pixel grid-cell area in m².

    The spherical area element R² cos(lat) dlat dlon, with dlat/dlon taken as the local
    gradient of the fine lat/lon grids.
    """
    lat_rad = np.radians(setup["fine_lat"])
    dlat    = np.abs(np.gradient(lat_rad, axis=0))
    dlon    = np.abs(np.gradient(np.radians(setup["fine_lon"]), axis=1))
    return (6.371e6 ** 2) * np.abs(np.cos(lat_rad)) * dlat * dlon


def _plot_spread_persistence(gwl_results):
    """Ridgeline of the per-date ensemble-spread distribution, one row per GWL period.

    Unlike the ridgeline in _plot_distribution, every row here is normalised to its own
    peak, so row heights carry no information — only the location and shape of each
    period's spread distribution are comparable across rows.

    Requires n_clim > 1: with a single climatology member no spread records exist and the
    figure is skipped.
    """
    gwl_tas = {label: [] for label, _, _, r in gwl_results if r is not None}
    gwl_pr  = {label: [] for label, _, _, r in gwl_results if r is not None}
    for label, _, _, result in gwl_results:
        if result is None:
            continue
        *_, spread_records = result
        for _, s_tas, s_pr in spread_records:
            gwl_tas[label].append(s_tas)
            gwl_pr[label].append(s_pr)

    if not any(gwl_tas.values()):
        print("  No spread records (n_clim=1) - skipping spread persistence plot")
        return

    valid_labels = [label for label, _, _, r in gwl_results if r is not None]
    n_gwl = len(valid_labels)

    fig, axs = plt.subplots(1, 2, figsize=(14, max(5, n_gwl * 1.5 + 2)))
    fig.subplots_adjust(top=0.88, bottom=0.1, left=0.08, right=0.97, wspace=0.3)

    for ax, gwl_data, var_name, unit, var_color in [
        (axs[0], gwl_tas, "tas", "Daily ensemble spread (°C)",        COLOR_TAS),
        (axs[1], gwl_pr,  "pr",  "Daily ensemble spread (mm/day)",    COLOR_PR),
    ]:
        offset = 0.0
        yticks, ylabels = [], []
        for label in reversed(valid_labels):
            vals = np.array(gwl_data.get(label, []))
            if len(vals) < 2:
                offset += 1.0
                continue
            n_bins = min(30, max(5, len(vals) // 3))
            cnt, edges = np.histogram(vals, bins=n_bins, density=True)
            cnt_norm = cnt / cnt.max() * 0.85
            centers  = 0.5 * (edges[:-1] + edges[1:])
            ax.fill_between(centers, offset, offset + cnt_norm, color=var_color, alpha=0.6, step="mid")
            ax.step(centers, offset + cnt_norm, color=var_color, lw=1.5, where="mid")
            yticks.append(offset + 0.4)
            ylabels.append(label)
            offset += 1.0
        ax.set_xlabel(unit)
        ax.set_title("Surface air temperature" if var_name == "tas" else "Precipitation",
                     fontweight="bold", pad=12)
        ax.set_yticks(yticks)
        ax.set_yticklabels(ylabels, fontsize=9)
        ax.grid(True, axis="x", alpha=0.3)

    out = os.path.abspath(f"{OUT_DIR}/6_spread_persistence.svg")
    fig.savefig(out, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  Saved {out}")


def _plot_delta(gwl_results, setup):
    """Climate change signal: one 2x3 figure per projection GWL period vs the PRESENT baseline.

    Rows = tas, pr. Columns, left to right, follow `col_types`:
        Δ coarse    the bilinear-interpolated driving field
        Δ downscaled
        Δ downscaled − Δ coarse — what the model adds to, or distorts in, that signal.
    The residual column is drawn on its own tighter colour scale (typically ~an order of
    magnitude below the raw deltas). Every panel is annotated with a single domain-level
    aggregate (a mean for tas, a sum for pr, converted from depth to mass in Gt/yr).

    Writes two things under plots/projection/2_delta/: the per-period figure, and
    all_periods.svg (every period and both variables at once).
    """
    baseline_label, baseline_start, baseline_end, baseline_r = gwl_results[0]
    if baseline_r is None:
        print("  Baseline GWL missing — skipping delta plot.")
        return

    baseline_down, baseline_coarse = baseline_r[0], baseline_r[1]
    higher = [(label, start, end, r)
              for label, start, end, r in gwl_results[1:] if r is not None]
    if not higher:
        print("  No projection GWL periods available — skipping delta plot.")
        return

    valid_mask = setup["valid_mask"]
    extent = [float(setup["fine_lon"].min()), float(setup["fine_lon"].max()),
              float(setup["fine_lat"].min()), float(setup["fine_lat"].max())]
    crs = ccrs.PlateCarree()

    # True width:height of the domain (longitude degrees compress by cos(lat) at this
    # latitude — ~1.23:1 here, not square). `_map` forces set_aspect("auto") so panels
    # fill their cell exactly instead of being cartopy-padded; sizing that cell to this
    # ratio is what keeps the map itself from being stretched taller than it really is.
    # panel_h matches _plot_comparison's panel height (HottestWettestDayInference.py).
    panel_h = 3.6
    panel_aspect = ((extent[1] - extent[0]) * np.cos(np.radians((extent[2] + extent[3]) / 2))
                     / (extent[3] - extent[2]))
    panel_w = panel_h * panel_aspect

    var_cfg = [
        ("tas", "RdBu_r", 1.0,   "°C"),
        ("pr",  "BrBG",   365.25, "Gt/yr"),
    ]
    col_types = ["coarse", "down", "resid"]
    var_display = {"tas": "Temperature", "pr": "Precipitation"}

    def _agg(vname, field, unit):
        """Domain-level summary of a delta field, consistent with 3_period_means.svg:
        pr is a per-pixel mass (Gt), so the domain quantity is a sum —
        averaging it (as tas legitimately does) would discard most of the field."""
        if vname == "pr":
            return float(np.nansum(field[valid_mask])), "total", unit
        return float(np.nanmean(field[valid_mask])), "mean", unit

    def _signed(val, decimals):
        """Format a signed value with a proper Unicode minus sign (U+2212) instead of
        the ASCII hyphen that f-string formatting produces for negative numbers."""
        return f"{val:+.{decimals}f}".replace("-", "−")

    # Precip mass instead of depth: mm(kg/m²) × pixel area(km²) = kt, so multiplying the
    # depth delta by pixel_area_km2 converts it directly to a per-pixel mass delta;
    # /1e6 converts kt to Gt.
    pixel_area_km2 = _pixel_area_m2(setup) / 1e6

    # Precompute deltas and shared colorbar limits per variable, consistent across periods
    scaled = {}
    for vname, delta_cmap, scale, unit in var_cfg:
        v = 0 if vname == "tas" else 1
        bd = baseline_down[v]   * scale
        bc = baseline_coarse[v] * scale
        down_deltas   = [(lbl, r[0][v] * scale - bd) for lbl, _, _, r in higher]
        coarse_deltas = [(lbl, r[1][v] * scale - bc) for lbl, _, _, r in higher]
        if vname == "pr":
            down_deltas   = [(lbl, d * pixel_area_km2 / 1e6) for lbl, d in down_deltas]
            coarse_deltas = [(lbl, d * pixel_area_km2 / 1e6) for lbl, d in coarse_deltas]
        resid_deltas  = [(lbl, dd - dict(coarse_deltas)[lbl]) for lbl, dd in down_deltas]
        all_d = np.concatenate([d[valid_mask] for _, d in down_deltas + coarse_deltas])
        delta_abs = float(np.nanpercentile(np.abs(all_d), 99))
        # Residuals are typically an order of magnitude smaller than the raw deltas, so they
        # get their own tight color scale — a shared scale would wash them out to near-white.
        all_r = np.concatenate([r[valid_mask] for _, r in resid_deltas])
        resid_abs = float(np.nanpercentile(np.abs(all_r), 99))
        scaled[vname] = dict(delta_cmap=delta_cmap, unit=unit,
                             down_deltas=down_deltas, coarse_deltas=coarse_deltas,
                             resid_deltas=resid_deltas, delta_abs=delta_abs, resid_abs=resid_abs)

    delta_dir = os.path.join(OUT_DIR, "2_delta")
    os.makedirs(delta_dir, exist_ok=True)

    def _map(fig, ax, data, cmap_name, vabs, title, unit=None, annotation=None, monospace=False,
             annotation_fontsize=11, show_colorbar=True, title_fontsize=9):
        """Draw one delta map on `ax`, symmetric about zero at ±`vabs`.

        `annotation` is the domain-aggregate label, printed as a caption just below the
        map rather than overlaid on top of it. `show_colorbar` is False for the
        Interpolated column, which shares its colour scale (and so its colorbar) with the
        Downscaled column right next to it — drawing it twice would be redundant.
        """
        d = np.where(valid_mask, data, np.nan)
        cm = plt.get_cmap(cmap_name).copy(); cm.set_bad("lightgrey")
        im = ax.pcolormesh(setup["fine_lon"], setup["fine_lat"], d,
                           vmin=-vabs, vmax=vabs, cmap=cm, transform=crs, shading="nearest",
                           rasterized=True)
        ax.set_extent(extent, crs=crs)
        # Without set_aspect("auto"), GeoAxes preserves the *geographic* aspect ratio by
        # padding whichever dimension is short relative to the panel's own (pixel) aspect
        # ratio — in a dense multi-column grid the panels are rarely the same shape as the
        # CERRA domain, so that padding shows extra area around it, and coastlines/borders
        # end up drawn well outside the domain the data actually covers. "auto" fixes that
        # by filling the axes box exactly at `extent` — but a wrong-shaped box then stretches
        # the map instead of padding it, so set_box_aspect pins the box itself to the
        # domain's true width:height first (constrained_layout reflows space for colorbars/
        # titles/spacing, so getting this from figsize alone doesn't survive layout).
        ax.set_box_aspect(1 / panel_aspect)
        ax.set_aspect("auto")
        ax.coastlines(); ax.set_facecolor("lightgrey")
        ax.set_title(title, fontsize=title_fontsize)
        if show_colorbar:
            # shrink=1 stretches the colorbar to the panel's own top/bottom edges
            # (matplotlib's default shrinks it to 0.7); anchor=(0.5, 1) top-aligns it
            # and centers it horizontally in its slot (default anchor is (0, 0.5) —
            # vertically centered and hugging the map rather than centered).
            cbar = fig.colorbar(im, ax=ax, shrink=1.0, pad=0.03, anchor=(0.5, 1.0))
            if unit is not None:
                cbar.set_label(unit, fontsize=8)
        if annotation is not None:
            ax.text(0.0, -0.05, annotation, transform=ax.transAxes, fontsize=annotation_fontsize,
                    fontweight="bold", ha="left", va="top",
                    family="monospace" if monospace else None)

    log = {}
    for lbl, start, end, _ in higher:
        fig = plt.figure(figsize=(panel_w * 3, panel_h * 2), constrained_layout=True)
        # hspace has room for the annotation caption below each row now that it's no
        # longer overlaid on the map itself.
        fig.get_layout_engine().set(w_pad=0.01, h_pad=0.01, wspace=0.02, hspace=0.15)
        gs = GridSpec(2, 3, figure=fig)

        for row_idx, (vname, delta_cmap, scale, unit) in enumerate(var_cfg):
            s = scaled[vname]
            d_delta = dict(s["down_deltas"])[lbl]
            c_delta = dict(s["coarse_deltas"])[lbl]
            r_delta = dict(s["resid_deltas"])[lbl]
            dd, agg_label, agg_unit = _agg(vname, d_delta, unit)
            dc, _, _ = _agg(vname, c_delta, unit)
            dr, _, _ = _agg(vname, r_delta, unit)
            ratio = dd / dc if dc != 0 else float("nan")
            pct = 100 * dr / dc if dc != 0 else float("nan")

            name = var_display[vname]
            # Interpolated and Downscaled share one colour scale (s["delta_abs"]) — the
            # colorbar on the Downscaled panel already speaks for both, so the Interpolated
            # one doesn't need its own.
            ax = fig.add_subplot(gs[row_idx, 0], projection=crs)
            _map(fig, ax, c_delta, delta_cmap, s["delta_abs"],
                 f"{name} - Interpolated field",
                 annotation=f"{agg_label} Δ = {_signed(dc, 2)} {agg_unit}",
                 show_colorbar=False)

            ax = fig.add_subplot(gs[row_idx, 1], projection=crs)
            _map(fig, ax, d_delta, delta_cmap, s["delta_abs"],
                 f"{name} - Downscaled field", unit=unit,
                 annotation=f"{agg_label} Δ = {_signed(dd, 2)} {agg_unit}")

            ax = fig.add_subplot(gs[row_idx, 2], projection=crs)
            _map(fig, ax, r_delta, delta_cmap, s["resid_abs"],
                 f"{name} - Downscaled − interpolated field", unit=unit,
                 annotation=f"residual = {_signed(dr, 2)} {agg_unit}")

            print(f"  {vname} {lbl}: Δ_down={dd:+.3f}  Δ_coarse={dc:+.3f}  "
                  f"residual={dr:+.3f}  ratio={ratio:.2f}  pct_of_signal={pct:+.2f}%")
            log[f"delta/mean_down_{vname}_{lbl}"]     = dd
            log[f"delta/mean_coarse_{vname}_{lbl}"]   = dc
            log[f"delta/mean_residual_{vname}_{lbl}"] = dr
            log[f"delta/ratio_{vname}_{lbl}"]         = ratio
            log[f"delta/pct_of_signal_{vname}_{lbl}"] = pct

        slug = _cache_slug(lbl)

        out = os.path.join(delta_dir, f"{slug}.svg")
        fig.savefig(out, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  Saved {out}")

    # Everything in one plot: rows = tas, pr. Columns = every GWL period's Δ downscaled |
    # Δ coarse | Δ residual, grouped period by period, so the full climate-change-signal
    # picture across all periods and both variables is visible at a glance.
    type_titles = {"down": "{name} - Downscaled field",
                   "coarse": "{name} - Interpolated field",
                   "resid": "{name} - Downscaled − interpolated field"}
    n_periods, n_types = len(higher), len(col_types)
    fig_all = plt.figure(figsize=(panel_w * n_periods * n_types, panel_h * 2), constrained_layout=True)
    gs_all = GridSpec(2, n_periods * n_types, figure=fig_all)

    for row_idx, (vname, delta_cmap, scale, unit) in enumerate(var_cfg):
        s = scaled[vname]
        for p_idx, (lbl, start, end, _) in enumerate(higher):
            for t_idx, ctype in enumerate(col_types):
                data, vabs = {
                    "down":   (dict(s["down_deltas"])[lbl],   s["delta_abs"]),
                    "coarse": (dict(s["coarse_deltas"])[lbl], s["delta_abs"]),
                    "resid":  (dict(s["resid_deltas"])[lbl],  s["resid_abs"]),
                }[ctype]
                mean_v, agg_label, agg_unit = _agg(vname, data, unit)
                ax = fig_all.add_subplot(gs_all[row_idx, p_idx * n_types + t_idx], projection=crs)
                title = type_titles[ctype].format(name=var_display[vname])
                # "coarse" (Interpolated) shares its colour scale with "down" (Downscaled)
                # right next to it, so only the latter needs its own colorbar.
                # Smaller than the per-period figure's titles — this figure packs many more,
                # much narrower columns, so the default size overflows into neighbors.
                _map(fig_all, ax, data, delta_cmap, vabs,
                     f"{title} — climate change signal\n{lbl} vs {baseline_label}",
                     unit=unit if ctype != "coarse" else None,
                     annotation=f"{agg_label} = {mean_v:+.2f} {agg_unit}",
                     show_colorbar=(ctype != "coarse"), title_fontsize=8, annotation_fontsize=8)

    all_periods_path = os.path.join(delta_dir, "all_periods.svg")
    fig_all.savefig(all_periods_path, dpi=150, bbox_inches="tight")
    plt.close(fig_all)
    print(f"  Saved {all_periods_path}")

    wandb.log(log)
    wandb.summary.update({k: v for k, v in log.items() if not k.startswith("delta/figure")})


def _plot_period_means(gwl_results, setup, mbcn_native_per_gwl=None, show_native_coarse=False):
    """Bar per GWL period — percent change from the reference field to the downscaled field.

    The reference is the native coarse field when it was loaded, otherwise the interpolated
    one. The bilinear-interpolation step's own contribution to this change is negligible
    (verified separately against the native field) and is not broken out as its own segment.
    Underlying quantities: tas domain mean (°C), pr total domain precipitation
    (sum over valid pixels × 365.25, Gt/yr).

    Both variables are shown as a percent change relative to the reference, not an absolute
    one. °C has no true zero, so a plain (down − ref) / ref in °C blows up whenever a
    period's reference mean sits near 0°C — tas's percent change instead uses the
    Kelvin-equivalent reference as the denominator, which keeps it physically meaningful
    (temperatures sit at ~250–300 K, nowhere near zero) while staying a genuine relative
    measure rather than °C's absolute delta.
    """
    show_native = show_native_coarse and mbcn_native_per_gwl is not None
    valid_entries = [(label, start, end, r, mbcn_native_per_gwl[i] if mbcn_native_per_gwl else None)
                     for i, (label, start, end, r) in enumerate(gwl_results) if r is not None]
    if not valid_entries:
        return
    valid_results = [(label, start, end, r) for label, start, end, r, _ in valid_entries]

    valid_mask = setup["valid_mask"]
    labels = [f"{label}\n({start}–{end})" for label, start, end, _ in valid_results]
    x = np.arange(len(labels))
    width = 0.5
    ref_name = "Native coarse field" if show_native else "Interpolated climate field"

    fig, axs = plt.subplots(1, 2, figsize=(12, 5))
    fig.subplots_adjust(bottom=0.18, top=0.83, left=0.08, right=0.97, wspace=0.3)

    pixel_area       = _pixel_area_m2(setup)  # m²
    pixel_area_valid = pixel_area[valid_mask]
    total_area       = float(pixel_area_valid.sum())

    log = {}
    for v, vname in enumerate(["tas", "pr"]):
        ax = axs[v]
        if vname == "tas":
            vals_down   = [float(np.nanmean(r[0][v][valid_mask]))                                    for *_, r in valid_results]
            vals_coarse = [float(np.nanmean(r[1][v][valid_mask]))                                    for *_, r in valid_results]
            vals_native = [float(np.nanmean(native[v])) for *_, native in valid_entries] if show_native else None
        else:
            vals_down   = [float(np.nansum(r[0][v][valid_mask] * pixel_area_valid)) * 365.25 / 1e12 for *_, r in valid_results]
            vals_coarse = [float(np.nansum(r[1][v][valid_mask] * pixel_area_valid)) * 365.25 / 1e12 for *_, r in valid_results]
            vals_native = ([float(np.nanmean(native[v])) * total_area * 365.25 / 1e12 for *_, native in valid_entries]
                           if show_native else None)

        vals_ref = vals_native if show_native else vals_coarse
        if vname == "tas":
            # Kelvin-equivalent denominator (see docstring) — never near zero, so no
            # guard against a zero/near-zero reference is needed here.
            ref_kelvin = [r + 273.15 for r in vals_ref]
            pct_diff = np.array([100 * (d - r) / rk
                                 for d, r, rk in zip(vals_down, vals_ref, ref_kelvin)])
        else:
            pct_diff = np.array([100 * (d - r) / r if r != 0 else float("nan")
                                 for d, r in zip(vals_down, vals_ref)])

        down_color = COLOR_TAS if vname == "tas" else COLOR_PR
        unit       = "%"
        fmt        = ".2f"

        ax.bar(x, np.nan_to_num(pct_diff), width, color=down_color, alpha=0.6,
               edgecolor=down_color, linewidth=2.0)

        span = max(float(np.nanmax(np.abs(pct_diff))), 1e-6)
        ax.set_ylim(-span * 1.3, span * 1.3)

        for i, val in enumerate(pct_diff):
            if not np.isfinite(val):
                continue
            up = val >= 0
            ax.text(x[i], val + (span * 0.03 if up else -span * 0.03), f"{val:+{fmt}} {unit}",
                    ha="center", va="bottom" if up else "top", fontsize=9, fontweight="bold")

        ax.axhline(0.0, color="black", lw=1.2, zorder=3)
        ax.set_xticks(x); ax.set_xticklabels(labels, fontsize=9)
        ax.set_ylabel(f"% change vs. {ref_name.lower()}")
        ax.set_title("Average temperature" if vname == "tas" else "Total precipitation mass")
        ax.grid(True, axis="y", alpha=0.3)

        for i, (label, start, end, _) in enumerate(valid_results):
            log[f"period_means/{label}_{vname}_down"]        = vals_down[i]
            log[f"period_means/{label}_{vname}_coarse"]      = vals_coarse[i]
            log[f"period_means/{label}_{vname}_pct_change"]  = float(pct_diff[i])
            if show_native:
                log[f"period_means/{label}_{vname}_native_coarse"] = vals_native[i]

    import matplotlib.lines as mlines
    import matplotlib.patches as mpatches
    from matplotlib.legend_handler import HandlerTuple

    legend_handles = [mlines.Line2D([], [], color="black", lw=1.2)]
    legend_labels  = [f"{ref_name} (reference)"]
    # tas and pr use different colors for the same concept — one legend entry with a
    # paired red/blue swatch instead of two near-duplicate "Downscaled" rows.
    legend_handles.append((
        mpatches.Patch(facecolor=COLOR_TAS, alpha=0.5, edgecolor=COLOR_TAS, linewidth=1.5),
        mpatches.Patch(facecolor=COLOR_PR,  alpha=0.5, edgecolor=COLOR_PR,  linewidth=1.5),
    ))
    legend_labels.append("Downscaled climate field")
    fig.legend(handles=legend_handles, labels=legend_labels, loc="lower center", bbox_to_anchor=(0.5, 0.01),
               ncol=len(legend_handles), fontsize=9, frameon=True,
               handler_map={tuple: HandlerTuple(ndivide=None)})

    out = os.path.abspath(f"{OUT_DIR}/3_period_means.svg")
    fig.savefig(out, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  Saved {out}")
    wandb.log(log)
    wandb.summary.update(log)


def _plot_quantile_delta(gwl_results):
    """Percentile-wise shift vs PRESENT, one line per projection period.

    Reveals redistribution — e.g. precipitation's low percentiles flat or shrinking
    while the upper tail grows — that neither a raw distribution shape nor a single
    mean/median marker can show: a net shift of a few percent is invisible against
    the full spread of the data, but the *difference by percentile* isolates exactly
    where the change actually is.
    """
    baseline = next((r for label, _, _, r in gwl_results if label == "PRESENT" and r is not None), None)
    if baseline is None:
        print("  PRESENT missing — skipping quantile delta plot.")
        return
    projections = [(label, start, end, r) for label, start, end, r in gwl_results
                   if r is not None and label != "PRESENT"]
    if not projections:
        print("  No projection GWL periods available — skipping quantile delta plot.")
        return

    pixel_vals_down_base = baseline[2]

    # Dense percentile grid with extra resolution in the upper tail, where the most
    # interesting precipitation intensification typically lives.
    pcts = np.unique(np.concatenate([
        np.linspace(1, 95, 95),
        np.array([96, 97, 98, 99, 99.5, 99.9]),
    ]))

    def _shades(cmap_name, n):
        cmap = plt.get_cmap(cmap_name)
        return [cmap(x) for x in np.linspace(0.35, 0.9, n)]
    period_colors = {"tas": _shades("Reds", len(projections)), "pr": _shades("Blues", len(projections))}

    fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
    fig.subplots_adjust(top=0.87, bottom=0.14, left=0.08, right=0.97, wspace=0.28)

    for ax, v, vname, ylabel in [
        (axes[0], 0, "tas", "Δ temperature (°C)"),
        (axes[1], 1, "pr",  "Δ precipitation (mm/day)"),
    ]:
        q_base = np.nanpercentile(pixel_vals_down_base[v], pcts)
        ax.axhline(0, color="grey", lw=0.8, ls="--", alpha=0.6)
        for (label, start, end, r), color in zip(projections, period_colors[vname]):
            q_proj = np.nanpercentile(r[2][v], pcts)
            ax.plot(pcts, q_proj - q_base, color=color, lw=1.8, label=f"{label} ({start}–{end})")
        ax.set_xlabel("Percentile", fontsize=9)
        ax.set_ylabel(ylabel, fontsize=9)
        ax.set_title("Surface air temperature" if vname == "tas" else "Precipitation", fontweight="bold")
        ax.tick_params(labelsize=8)
        ax.grid(True, alpha=0.3)
        ax.legend(fontsize=8, frameon=True)

    out = os.path.abspath(f"{OUT_DIR}/5_quantile_delta.svg")
    fig.savefig(out, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  Saved {out}")


def _plot_distribution(gwl_results, cerra_pixel_vals=None, mbcn_native_per_gwl=None):
    """Part 3: distribution consistency — 4_distribution_periods.svg.

    Ridgeline of the per-period distributions, one row per period rather than overlaid
    curves, so periods stop competing for the same pixels and the shift reads as a
    staircase. Rows share one real density-to-height scale, so their heights are
    directly comparable. `cerra_pixel_vals`, when given, adds the reanalysis
    distribution as a fixed reference row at the top of the stack. Each row prefers the
    native, pre-interpolation coarse MBCn field (`mbcn_native_per_gwl`), falling back to
    the downscaled field only if native data wasn't loaded.
    """
    valid_results = [(label, start, end, r) for label, start, end, r in gwl_results if r is not None]

    def _finite(arr):
        arr = np.asarray(arr, dtype=np.float64).ravel()
        return arr[np.isfinite(arr)]

    def _density_line(vals, edges):
        """Binned empirical density of `vals` over `edges` (per unit x, not per log-unit
        even when `edges` is log-spaced) — no KDE smoothing or bandwidth choice, so the
        curve is exactly the data's own histogram shape, just interpolated between bin
        centers instead of drawn as a stepped outline.

        `vals` is normalised to its own area = 1, so curves across periods are directly
        comparable regardless of sample size.
        """
        if len(vals) < 2:
            return None
        counts, _ = np.histogram(vals, bins=edges)
        widths = np.diff(edges)
        return counts / len(vals) / widths

    def _shades(cmap_name, n):
        """`n` colors from `cmap_name`, light -> dark, avoiding the near-white/near-black ends."""
        cmap = plt.get_cmap(cmap_name)
        return [cmap(x) for x in np.linspace(0.35, 0.9, n)]

    n_periods = len(valid_results)
    period_colors_by_var = {"tas": _shades("Reds", n_periods), "pr": _shades("Blues", n_periods)}

    def _density_formatter(row_h, peak, axis_top):
        """Read a right-hand y value back as the local density within whichever row's
        baseline it falls above — valid everywhere because every row shares the same
        real (unrescaled) density-to-height scale, only the baseline differs.

        `axis_top` suppresses the one tick that lands exactly on the axis's own top
        edge: every other row-boundary multiple of row_h is shared between two ridges
        (top of one, bottom of the next) and "0" there is a real reading, but the
        topmost one has no row above it — it's just the empty headroom above the
        highest ridge, so labelling it "0" reads as a stray axis value, not a density.
        """
        def fmt(y, _):
            if abs(y - axis_top) < 1e-9:
                return ""
            local = y % row_h
            # A row boundary should land exactly on 0, but float rounding through the
            # modulo can leave a tiny non-zero residual (e.g. 2e-17) that "%.3g" would
            # otherwise print in scientific notation instead of "0".
            if local < 1e-9 or row_h - local < 1e-9:
                return "0"
            if local > peak * 1.02:
                return ""
            return f"{local:.3g}"
        return fmt

    fig2, axs2 = plt.subplots(1, 2, figsize=(12, max(6, (n_periods + 1) * 1.7)))
    fig2.subplots_adjust(top=0.9, bottom=0.09, left=0.1, right=0.97, wspace=0.28)

    for v, (vname, unit, fullname) in enumerate([
        ("tas", "Temperature (°C)",    "Surface air temperature"),
        ("pr",  "Precipitation (mm/day)", "Precipitation"),
    ]):
        ax = axs2[v]
        is_pr = vname == "pr"
        period_colors = period_colors_by_var[vname]
        WET_DAY_MM = 1.0
        per_period_vals = []
        for j, (label, start, end, result) in enumerate(valid_results):
            if mbcn_native_per_gwl is not None and j < len(mbcn_native_per_gwl):
                vals = _finite(mbcn_native_per_gwl[j][v])
            else:
                _, _, pixel_vals_down, *_ = result
                vals = _finite(pixel_vals_down[v])
            if is_pr:
                vals = vals[vals > WET_DAY_MM]
            per_period_vals.append((label, start, end, vals))

        series = [vals for _, _, _, vals in per_period_vals]
        cerra_v = None
        if cerra_pixel_vals is not None:
            cerra_v = _finite(cerra_pixel_vals[v])
            if is_pr:
                cerra_v = cerra_v[cerra_v > WET_DAY_MM]
            series = series + [cerra_v]

        all_vals = np.concatenate(series)
        if is_pr:
            vmin, vmax = WET_DAY_MM, float(np.percentile(all_vals, 99.9))
            edges = np.logspace(np.log10(vmin), np.log10(vmax), 41)
        else:
            vmin, vmax = float(all_vals.min()), float(all_vals.max())
            edges = np.linspace(vmin, vmax, 81)
        xs = 0.5 * (edges[:-1] + edges[1:])

        # Rows bottom -> top: GWL4 up through PRESENT, with CERRA reference (the fixed
        # anchor everything else is judged against) at the very top — reading down the
        # stack reads forward in time, ending at today's reanalysis baseline.
        rows = [(f"{label}\n({start}–{end})", vals, color, False)
               for (label, start, end, vals), color in reversed(list(zip(per_period_vals, period_colors)))]
        if cerra_v is not None:
            rows.append((f"Reanalysis data\n({valid_results[0][1]}–{valid_results[0][2]})",
                        cerra_v, "forestgreen", True))

        # First pass: compute every row's density curve once and find the tallest peak
        # in this panel. Rows are NOT independently rescaled to a fixed height — they
        # all share this one real density-to-height factor — so a row's visual height
        # is honestly comparable to any other row's, and a single density scale (added
        # below) is valid for all of them at once.
        row_ys = [_density_line(vals, edges) for _, vals, _, _ in rows]
        global_peak = max((float(ys.max()) for ys in row_ys if ys is not None), default=0.0)
        ROW_H = global_peak * 1.15 if global_peak > 0 else 1.0

        # Background reference grid drawn first (low zorder) so it sits behind every
        # ridge — a ruler the eye can use to read off how far each row's peak has
        # moved, rather than having to compare curve-to-curve.
        ax.set_axisbelow(True)
        ax.grid(True, axis="x", alpha=0.45, zorder=0, lw=0.8)

        anchor_x = float(np.mean(cerra_v)) if cerra_v is not None else None

        yticks, ylabels = [], []
        for i, ((row_label, vals, color, is_ref), ys) in enumerate(zip(rows, row_ys)):
            offset = i * ROW_H
            yticks.append(offset + global_peak / 2); ylabels.append(row_label)
            if ys is None:
                continue
            if is_ref:
                ax.fill_between(xs, offset, offset + ys, color=color, alpha=0.18, zorder=2 + 2 * i)
                ax.plot(xs, offset + ys, color=color, lw=2.0, ls="--", zorder=3 + 2 * i)
            else:
                ax.fill_between(xs, offset, offset + ys, color=color, alpha=0.8, lw=0, zorder=2 + 2 * i)
                ax.plot(xs, offset + ys, color=color, lw=1.3, zorder=3 + 2 * i)

        # One dashed vertical anchor at the reference mean, spanning the full stack —
        # the single fixed x every row's own mean is implicitly compared against.
        if anchor_x is not None:
            ax.axvline(anchor_x, color="black", lw=1.0, ls=":", alpha=0.6, zorder=1)

        ax.set_title("Precipitation (wet days > 1mm/day)" if is_pr else fullname, fontweight="bold")
        ax.set_xlabel(unit)
        ax.set_xlim(edges[0], edges[-1])
        ax.set_ylim(-0.15 * ROW_H, (len(rows) - 1) * ROW_H + ROW_H)
        ax.set_yticks(yticks)
        # Period labels only on the left-hand (tas) panel — the pr panel's own copy
        # sat directly against the tas panel's right-hand density axis in the gap
        # between the two, so both are shown once, on the true left and right edges.
        ax.set_yticklabels(ylabels if v == 0 else [], fontsize=8.5)

        # Right-hand density axis: same real scale for every row (only the baseline
        # differs), so ticks read as "density above this row's own zero" no matter
        # which row's band of the axis they land in.
        ax_r = ax.twinx()
        ax_r.set_ylim(ax.get_ylim())
        ax_r.yaxis.set_major_locator(mticker.MultipleLocator(ROW_H / 4))
        ax_r.yaxis.set_major_formatter(mticker.FuncFormatter(_density_formatter(ROW_H, global_peak, ax.get_ylim()[1])))
        ax_r.set_ylabel("Density", fontsize=8.5)
        ax_r.tick_params(labelsize=7.5)

        if is_pr:
            _log_x_wet_days(ax)

    out2 = os.path.abspath(f"{OUT_DIR}/4_distribution_periods.svg")
    fig2.savefig(out2, dpi=150, bbox_inches="tight")
    plt.close(fig2)
    print(f"  Saved {out2}")
