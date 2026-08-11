"""
Visualize raw and processed drought-prediction data with cartopy map overlays.

Figures are saved under:
  figures/raw/        — original NetCDF files
  figures/processed/  — output of preprocessing.py

All spatial maps are displayed in PlateCarree (standard geographic) projection.
The rotated-pole CRS is used only as the data transform so cartopy can correctly
project AL/topography data onto the geographic display.
"""

import sys
from pathlib import Path

sys.stdout.reconfigure(encoding="utf-8")
sys.stderr.reconfigure(encoding="utf-8")

import cartopy.crs as ccrs  # noqa: E402
import cartopy.feature as cfeature  # noqa: E402
import matplotlib.cm as mpl_cm  # noqa: E402
import matplotlib.colors as mpl_colors  # noqa: E402
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import xarray as xr  # noqa: E402
from dataset import _MED_GROUPS  # noqa: E402
from utils.paths import processed_data_dir, project_root, raw_data_dir  # noqa: E402

PROJECT_ROOT = project_root()
DATA_RAW = raw_data_dir()
DATA_PROCESSED = processed_data_dir()
FIG_RAW = PROJECT_ROOT / "figures" / "raw"
FIG_PROCESSED = PROJECT_ROOT / "figures" / "processed"

# EUR-11 rotated-pole CRS — used as pcolormesh transform only, NOT as display projection.
# Parameters derived from the rotated_pole variable:
#   grid_north_pole_latitude=39.25, grid_north_pole_longitude=198.0
#   cartopy pole_longitude = 198.0 - 360 = -162.0
_CRS_ROTATED = ccrs.RotatedPole(pole_longitude=-162.0, pole_latitude=39.25)
_CRS_PLATE = ccrs.PlateCarree()

_VAR_LABELS: dict[str, str] = {
    "wb": "Water balance (wb) [kg m⁻² s⁻¹]",
    "pr": "Precipitation (pr) [mm day⁻¹]",
    "ps": "Surface pressure (ps) [Pa]",
    "spei": "SPEI – Standardized Precipitation-Evapotranspiration Index",
    "tas": "Near-surface air temperature (tas) [K]",
}

# Short titles for compact slide panels (vs. the full _VAR_LABELS used in the report)
_VAR_LABELS_SHORT: dict[str, str] = {
    "wb": "Water balance",
    "pr": "Precipitation",
    "ps": "Surface pressure",
    "spei": "SPEI",
    "tas": "Temperature",
}

FIG_SLIDES = PROJECT_ROOT / "figures" / "raw" / "slides"


# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------


def _save(fig: plt.Figure, path: Path, dpi: int = 150, svg: bool = True) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path, dpi=dpi, bbox_inches="tight")
    # Every figure gets an SVG twin for the book by default. pcolormesh maps
    # (one vector path per grid cell) can run 10x+ larger as SVG than PNG,
    # especially multi-panel snapshot grids -- pass svg=False to opt a
    # specific call out if its file size turns out to be a problem.
    if svg:
        fig.savefig(path.with_suffix(".svg"), bbox_inches="tight")
    plt.close(fig)
    print(f"  saved → {path.relative_to(PROJECT_ROOT)}")


def _add_map_features(ax: plt.Axes) -> None:
    """Overlay country borders and coastlines. Call AFTER pcolormesh."""
    ax.add_feature(cfeature.COASTLINE.with_scale("10m"), linewidth=0.6, edgecolor="black")
    ax.add_feature(cfeature.BORDERS.with_scale("10m"), linewidth=0.5, edgecolor="black", linestyle="--")


def _rotated_extent(rlat: np.ndarray, rlon: np.ndarray, pad: float = 0.3) -> list[float]:
    """Convert rotated-pole bounding box to geographic [lon_min, lon_max, lat_min, lat_max]."""
    xs = [rlon.min(), rlon.max(), rlon.min(), rlon.max()]
    ys = [rlat.min(), rlat.min(), rlat.max(), rlat.max()]
    pts = _CRS_PLATE.transform_points(_CRS_ROTATED, np.array(xs), np.array(ys))
    return [pts[:, 0].min() - pad, pts[:, 0].max() + pad, pts[:, 1].min() - pad, pts[:, 1].max() + pad]


def _plot_timeseries(ax: plt.Axes, da: xr.DataArray, label: str) -> None:
    ax.plot(da["time"].values, da.values, lw=0.8, label=label)


# ---------------------------------------------------------------------------
# Single 2D map
# ---------------------------------------------------------------------------


def _plot_single_map(
    data2d: xr.DataArray,
    title: str,
    out_path: Path,
    data_crs: ccrs.Projection = _CRS_ROTATED,
    cmap: str = "viridis",
    figsize: tuple[float, float] = (8, 6),
    title_fontsize: int = 10,
    overlay_mask: xr.DataArray | None = None,
    overlay_color: str = "red",
    cbar_label: str = "",
) -> None:
    """Plot one 2D spatial field in PlateCarree with country/coast overlay.

    title="" skips the title entirely (for figures whose caption already
    covers it -- pass cbar_label instead if the title was carrying units).

    overlay_mask: optional boolean/0-1 field, on its own rlat/rlon (can be a
    smaller/cropped grid than data2d, e.g. the model's valid-cell mask within
    a larger raw domain) -- its boundary is drawn as a contour line on top,
    since both share the same rotated-pole CRS regardless of extent. Padded
    with a border of zeros before contouring so the outline still closes even
    where valid cells reach the array's own edge (e.g. the Alpine mask's
    northern edge) -- otherwise that side is left open.
    """
    x_dim = data2d.dims[-1]
    y_dim = data2d.dims[-2]
    rlon = data2d[x_dim].values
    rlat = data2d[y_dim].values

    fig = plt.figure(figsize=figsize, layout="constrained")
    ax = fig.add_subplot(1, 1, 1, projection=_CRS_PLATE)

    im = ax.pcolormesh(
        rlon,
        rlat,
        data2d.values,
        transform=data_crs,
        cmap=cmap,
        shading="auto",
        rasterized=True,
    )
    _add_map_features(ax)

    if overlay_mask is not None:
        m_x_dim = overlay_mask.dims[-1]
        m_y_dim = overlay_mask.dims[-2]
        m_lon = overlay_mask[m_x_dim].values
        m_lat = overlay_mask[m_y_dim].values
        m_data = overlay_mask.values.astype(float)
        dlon = m_lon[1] - m_lon[0]
        dlat = m_lat[1] - m_lat[0]
        m_lon_padded = np.concatenate([[m_lon[0] - dlon], m_lon, [m_lon[-1] + dlon]])
        m_lat_padded = np.concatenate([[m_lat[0] - dlat], m_lat, [m_lat[-1] + dlat]])
        m_data_padded = np.pad(m_data, 1, mode="constant", constant_values=0)
        ax.contour(
            m_lon_padded,
            m_lat_padded,
            m_data_padded,
            levels=[0.5],
            colors=overlay_color,
            linewidths=1.5,
            transform=data_crs,
        )

    ax.set_extent(_rotated_extent(rlat, rlon), crs=_CRS_PLATE)
    cbar = plt.colorbar(im, ax=ax, fraction=0.03, pad=0.04, shrink=0.8)
    if cbar_label:
        cbar.set_label(cbar_label)
    if title:
        ax.set_title(title, fontsize=title_fontsize)
    _save(fig, out_path)


# ---------------------------------------------------------------------------
# Multi-panel time-window map
# ---------------------------------------------------------------------------


def _plot_map_window(
    da: xr.DataArray,
    title: str,
    out_path: Path,
    start_idx: int = 0,
    n_panels: int = 12,
    cols: int = 4,
    data_crs: ccrs.Projection = _CRS_ROTATED,
    cmap: str = "viridis",
) -> None:
    """Plot n_panels consecutive time steps as geographic maps with border overlay."""
    if da.dims[0] != "time":
        da = da.transpose("time", ...)

    x_dim = da.dims[-1]
    y_dim = da.dims[-2]
    rlon = da[x_dim].values
    rlat = da[y_dim].values
    extent = _rotated_extent(rlat, rlon)

    n = min(n_panels, da.sizes["time"] - start_idx)
    rows = int(np.ceil(n / cols))

    window = da.isel(time=slice(start_idx, start_idx + n))
    vmin, vmax = float(window.min()), float(window.max())

    fig = plt.figure(figsize=(cols * 4.5, rows * 2.2), layout="constrained")
    for i in range(n):
        ax = fig.add_subplot(rows, cols, i + 1, projection=_CRS_PLATE)
        frame = da.isel(time=start_idx + i)
        ax.pcolormesh(
            rlon,
            rlat,
            frame.values,
            transform=data_crs,
            cmap=cmap,
            vmin=vmin,
            vmax=vmax,
            shading="auto",
            rasterized=True,
        )
        _add_map_features(ax)
        ax.set_extent(extent, crs=_CRS_PLATE)

        label = str(da.coords["time"].values[start_idx + i])[:7] if "time" in da.coords else f"t={start_idx + i}"
        ax.set_title(label, fontsize=11)

    sm = mpl_cm.ScalarMappable(cmap=cmap, norm=mpl_colors.Normalize(vmin=vmin, vmax=vmax))
    cb = fig.colorbar(sm, ax=fig.axes, fraction=0.03, pad=0.04, shrink=0.9)
    cb.ax.tick_params(labelsize=12)
    if title:
        fig.suptitle(title, fontsize=14)
    _save(fig, out_path)


# ---------------------------------------------------------------------------
# RAW data figures
# ---------------------------------------------------------------------------


def visualize_raw_nao_pc1() -> None:
    print("NAO PC1 time series ...")
    ds = xr.open_dataset(DATA_RAW / "atlantic/nao/psl_nao_pc1_reanalysis_1970_2025.nc")
    fig, ax = plt.subplots(figsize=(12, 3))
    _plot_timeseries(ax, ds["pcs"], "NAO PC1")
    ax.axhline(0, color="k", lw=0.5, ls="--")
    ax.set_title("NAO index – PC1 (1970–2025)")
    ax.set_ylabel("PC amplitude")
    fig.tight_layout()
    _save(fig, FIG_RAW / "nao_pc1.png")
    ds.close()


def visualize_raw_nao_eof() -> None:
    print("NAO EOF1 monthly patterns ...")
    ds = xr.open_dataset(DATA_RAW / "atlantic/eof/psl_nao_eof1_reanalysis_1970_2025.nc")
    da = ds["eofs"]  # (time=12, latitude, longitude) — already in geographic coords
    n, cols = da.sizes["time"], 4
    rows = int(np.ceil(n / cols))

    fig = plt.figure(figsize=(cols * 4, rows * 3), layout="constrained")
    for i in range(n):
        ax = fig.add_subplot(rows, cols, i + 1, projection=_CRS_PLATE)
        frame = da.isel(time=i)
        im = ax.pcolormesh(
            frame["longitude"].values,
            frame["latitude"].values,
            frame.values,
            transform=_CRS_PLATE,
            cmap="RdBu_r",
            shading="auto",
            rasterized=True,
        )
        _add_map_features(ax)
        ax.set_title(f"month {i + 1}", fontsize=9)

    fig.colorbar(im, ax=fig.axes, fraction=0.02, pad=0.04, shrink=0.6, label="EOF loading")
    fig.suptitle("NAO EOF1 – monthly spatial patterns", fontsize=11)
    # Was svg=False (12 panels x the 281x480 North Atlantic grid -> ~290MB with
    # vector-path-per-cell rendering). Now safe with rasterized=True on the mesh
    # above -- SVG size follows render resolution, not cell count.
    _save(fig, FIG_RAW / "nao_eof1.png")
    ds.close()


def visualize_raw_mediterranean_slt() -> None:
    print("Mediterranean SST basin time series ...")
    ds = xr.open_dataset(DATA_RAW / "mediteranean/slt/slt_avg_mediteranean_reanalysis_1970_2025.nc")
    fig, ax = plt.subplots(figsize=(12, 4))
    for basin in ds.data_vars:
        _plot_timeseries(ax, ds[basin], basin)
    ax.set_title("Mediterranean SST – basin averages (1970–2025)")
    ax.set_ylabel("Temperature")
    ax.legend(fontsize=7, ncol=3, loc="upper left")
    fig.tight_layout()
    _save(fig, FIG_RAW / "mediterranean_slt.png")
    ds.close()


def visualize_raw_topography() -> None:
    print("Topography (full EUR-11 domain) ...")
    ds = xr.open_dataset(DATA_RAW / "topography/topo_EUR-11_CCCma-CanESM2_OURANOS-CRCM5.nc")
    ds_mask = xr.open_dataset(DATA_PROCESSED / "static_grid.nc")
    _plot_single_map(
        ds["orogf"], "", FIG_RAW / "topography.png", cmap="terrain", figsize=(9, 7),
        overlay_mask=ds_mask["mask"], overlay_color="red", cbar_label="Elevation (m)",
    )
    ds.close()
    ds_mask.close()


def visualize_raw_al_variables(start_idx: int = 0, n_panels: int = 12) -> None:
    variables: dict[str, tuple[Path, str, str]] = {
        "wb": (DATA_RAW / "al/wb/wb_al_reanalysis_1971_2025.nc", "wb", "BrBG"),
        "pr": (DATA_RAW / "al/pr/pr_al_reanalysis_1971_2025.nc", "tp", "Blues"),
        "ps": (DATA_RAW / "al/ps/ps_al_reanalysis_1971_2025.nc", "sp", "RdYlBu"),
        "spei": (DATA_RAW / "al/spei/spei_al_reanalysis_1971_2025.nc", "spei", "RdBu"),
        "tas": (DATA_RAW / "al/tas/tas_al_reanalysis_1971_2025.nc", "t2m", "hot_r"),
    }
    for varname, (path, nc_var, cmap) in variables.items():
        print(f"AL raw – {varname} ...")
        ds = xr.open_dataset(path)
        da = ds[nc_var]
        if da.dims[0] != "time":
            da = da.transpose("time", ...)
        _plot_map_window(
            da, "", FIG_RAW / "al" / f"{varname}.png", start_idx=start_idx, n_panels=n_panels, cmap=cmap
        )
        ds.close()


# ---------------------------------------------------------------------------
# SLIDE-formatted figures — sized/fonted for a 16:9 beamer slide
# ---------------------------------------------------------------------------

_SLIDE_FIGSIZE = (13.3, 7.5)  # 16:9
_SLIDE_SNAPSHOT_TIME = "1971-08-01"  # matches the SPEI example used elsewhere in the slides


def visualize_raw_al_variables_slide(snapshot_time: str = _SLIDE_SNAPSHOT_TIME) -> None:
    """One-snapshot overview of all 5 AL input variables, one panel each, sized for a slide."""
    print(f"AL raw – slide snapshot ({snapshot_time}) ...")
    variables: dict[str, tuple[Path, str, str]] = {
        "wb":   (DATA_RAW / "al/wb/wb_al_reanalysis_1971_2025.nc",     "wb",   "BrBG"),
        "pr":   (DATA_RAW / "al/pr/pr_al_reanalysis_1971_2025.nc",     "tp",   "Blues"),
        "ps":   (DATA_RAW / "al/ps/ps_al_reanalysis_1971_2025.nc",     "sp",   "RdYlBu"),
        "spei": (DATA_RAW / "al/spei/spei_al_reanalysis_1971_2025.nc", "spei", "RdBu"),
        "tas":  (DATA_RAW / "al/tas/tas_al_reanalysis_1971_2025.nc",   "t2m",  "hot_r"),
    }

    # The AL domain's aspect ratio (~2.5, wide and flat) is far from a 3-per-row grid
    # cell's aspect at 16:9; size the figure to match it directly instead of constrained
    # layout, which leaves a large gap between rows (see drought-frequency fix).
    ref_ds = xr.open_dataset(next(iter(variables.values()))[0])
    ref_var = ref_ds[next(iter(variables.values()))[1]]
    extent = _rotated_extent(ref_var["rlat"].values, ref_var["rlon"].values)
    ref_ds.close()
    aspect = (extent[1] - extent[0]) / (extent[3] - extent[2])

    n_cols, n_rows = 3, 2
    panel_w = _SLIDE_FIGSIZE[0] / n_cols
    fig, axes = plt.subplots(
        n_rows, n_cols,
        figsize=(_SLIDE_FIGSIZE[0], panel_w / aspect * n_rows + 1.6),
        subplot_kw={"projection": _CRS_PLATE},
    )
    fig.subplots_adjust(left=0.02, right=0.98, top=0.85, bottom=0.03, hspace=0.5, wspace=0.35)

    for ax, (varname, (path, nc_var, cmap)) in zip(axes.ravel(), variables.items(), strict=False):
        ds = xr.open_dataset(path)
        da = ds[nc_var]
        if da.dims[0] != "time":
            da = da.transpose("time", ...)
        frame = da.sel(time=snapshot_time, method="nearest")
        rlon, rlat = da["rlon"].values, da["rlat"].values

        im = ax.pcolormesh(
            rlon, rlat, frame.values, transform=_CRS_ROTATED, cmap=cmap, shading="auto", rasterized=True
        )
        _add_map_features(ax)
        ax.set_extent(_rotated_extent(rlat, rlon), crs=_CRS_PLATE)
        ax.set_title(_VAR_LABELS_SHORT.get(varname, varname), fontsize=16)
        fig.colorbar(im, ax=ax, fraction=0.045, pad=0.03, shrink=0.85)
        ds.close()

    for ax in axes.ravel()[len(variables):]:
        ax.remove()

    fig.suptitle(f"Raw AL input variables — {snapshot_time[:7]}", fontsize=19)
    _save(fig, FIG_SLIDES / "al_variables_snapshot.png")


def visualize_raw_nao_pc1_slide() -> None:
    print("NAO PC1 – slide ...")
    ds = xr.open_dataset(DATA_RAW / "atlantic/nao/psl_nao_pc1_reanalysis_1970_2025.nc")
    fig, ax = plt.subplots(figsize=_SLIDE_FIGSIZE)
    ax.plot(ds["time"].values, ds["pcs"].values, lw=1.2, color="steelblue")
    ax.axhline(0, color="k", lw=0.8, ls="--")
    ax.set_title("NAO index – PC1 (1970–2025)", fontsize=20)
    ax.set_ylabel("PC amplitude", fontsize=16)
    ax.tick_params(labelsize=13)
    fig.tight_layout()
    _save(fig, FIG_SLIDES / "nao_pc1.png")
    ds.close()


def visualize_raw_nao_eof_slide(months: tuple[int, ...] = (1, 4, 7, 10)) -> None:
    """NAO EOF1 spatial pattern for one representative month per season."""
    print("NAO EOF1 – slide ...")
    ds = xr.open_dataset(DATA_RAW / "atlantic/eof/psl_nao_eof1_reanalysis_1970_2025.nc")
    da = ds["eofs"]  # (time=12, latitude, longitude)

    # The Atlantic domain's aspect ratio (~1.7) is far from 16:9 (~1.8 per panel would
    # need); size the figure to match it directly instead of constrained layout, which
    # leaves a large gap between the suptitle and the row (see drought-frequency fix).
    lat, lon = da["latitude"].values, da["longitude"].values
    aspect = (lon.max() - lon.min()) / (lat.max() - lat.min())
    n = len(months)
    panel_w = _SLIDE_FIGSIZE[0] / n
    fig, axes = plt.subplots(
        1, n,
        figsize=(_SLIDE_FIGSIZE[0], panel_w / aspect + 1.1),
        subplot_kw={"projection": _CRS_PLATE},
    )
    fig.subplots_adjust(left=0.02, right=0.9, top=0.8, bottom=0.05, wspace=0.05)

    im = None
    for ax, m in zip(axes, months, strict=True):
        frame = da.isel(time=m - 1)
        im = ax.pcolormesh(
            frame["longitude"].values, frame["latitude"].values, frame.values,
            transform=_CRS_PLATE, cmap="RdBu_r", shading="auto", rasterized=True,
        )
        _add_map_features(ax)
        ax.set_title(f"Month {m}", fontsize=16)

    fig.colorbar(im, ax=axes.tolist(), fraction=0.03, pad=0.02, shrink=0.75, label="EOF loading")
    fig.suptitle("NAO EOF1 – seasonal patterns", fontsize=19)
    _save(fig, FIG_SLIDES / "nao_eof1.png")
    ds.close()


def visualize_raw_mediterranean_slt_slide() -> None:
    """Mediterranean SLT time series, aggregated into the same 3 regional groups
    (Western Med / Eastern Med / Black Sea) used as global scalars in dataset.py's
    med_sst_agg='grouped' mode — rather than raw per-basin series."""
    print("Mediterranean SLT – slide ...")
    ds = xr.open_dataset(DATA_RAW / "mediteranean/slt/slt_avg_mediteranean_reanalysis_1970_2025.nc")
    # Crop to the AL study period (1971-2024) -- the raw record runs 1970-2025,
    # wider than the input data actually used by the model.
    ds = ds.sel(time=slice("1971-01-01", "2024-12-31"))
    # Not the full 16:9 _SLIDE_FIGSIZE: a single line plot of 3 cyclical series
    # doesn't need that much vertical space, so use a shorter, wider aspect.
    fig, ax = plt.subplots(figsize=(_SLIDE_FIGSIZE[0], 3.2))
    for group_name, basins in _MED_GROUPS.items():
        group_mean = np.mean([ds[b].values for b in basins], axis=0)
        ax.plot(ds["time"].values, group_mean, lw=1.2, label=group_name.replace("_", " ").title())
    ax.set_ylabel("SLT", fontsize=16)
    ax.tick_params(labelsize=13)
    ax.legend(fontsize=14, ncol=1, loc="upper left")
    fig.tight_layout()
    _save(fig, FIG_SLIDES / "mediterranean_slt.png")
    ds.close()


def visualize_raw_topography_slide() -> None:
    print("Topography – slide ...")
    ds = xr.open_dataset(DATA_RAW / "topography/topo_EUR-11_CCCma-CanESM2_OURANOS-CRCM5.nc")
    _plot_single_map(
        ds["orogf"], "Topography – EUR-11 domain (m)", FIG_SLIDES / "topography.png",
        cmap="terrain", figsize=_SLIDE_FIGSIZE, title_fontsize=20,
    )
    ds.close()


# ---------------------------------------------------------------------------
# PROCESSED data figures
# ---------------------------------------------------------------------------


def visualize_processed_dynamic(start_idx: int = 0, n_panels: int = 12) -> None:
    ds = xr.open_dataset(DATA_PROCESSED / "dynamic_grid.nc")
    cmaps = {"wb": "BrBG", "pr": "Blues", "ps": "RdYlBu", "spei": "RdBu", "tas": "hot_r"}
    for varname in ds.data_vars:
        print(f"processed dynamic – {varname} ...")
        label = _VAR_LABELS.get(varname, varname)
        _plot_map_window(
            ds[varname],
            f"{label} — processed",
            FIG_PROCESSED / "dynamic" / f"{varname}.png",
            start_idx=start_idx,
            n_panels=n_panels,
            cmap=cmaps.get(varname, "viridis"),
        )
    ds.close()


def visualize_processed_static() -> None:
    print("processed static – topography + mask ...")
    ds = xr.open_dataset(DATA_PROCESSED / "static_grid.nc")
    rlon = ds.rlon.values
    rlat = ds.rlat.values
    extent = _rotated_extent(rlat, rlon)

    fig = plt.figure(figsize=(13, 5), layout="constrained")

    ax1 = fig.add_subplot(1, 2, 1, projection=_CRS_PLATE)
    im1 = ax1.pcolormesh(
        rlon, rlat, ds["orogf"].values, transform=_CRS_ROTATED, cmap="terrain", shading="auto", rasterized=True
    )
    _add_map_features(ax1)
    ax1.set_extent(extent, crs=_CRS_PLATE)
    fig.colorbar(im1, ax=ax1, fraction=0.04, pad=0.04, shrink=0.8, label="m")
    ax1.set_title("Topography – AL domain (cropped)")

    ax2 = fig.add_subplot(1, 2, 2, projection=_CRS_PLATE)
    im2 = ax2.pcolormesh(
        rlon, rlat, ds["mask"].values, transform=_CRS_ROTATED, cmap="Greens", shading="auto", vmin=0, vmax=1,
        rasterized=True,
    )
    _add_map_features(ax2)
    ax2.set_extent(extent, crs=_CRS_PLATE)
    fig.colorbar(im2, ax=ax2, fraction=0.04, pad=0.04, shrink=0.8, label="valid (1) / masked (0)")
    ax2.set_title("Valid-cell mask")

    fig.suptitle("Static grid (processed)", fontsize=11)
    _save(fig, FIG_PROCESSED / "static_grid.png")
    ds.close()


def visualize_processed_global_scalars() -> None:
    print("processed global scalars ...")
    ds = xr.open_dataset(DATA_PROCESSED / "global_scalars.nc")

    n_vars = len(ds.data_vars)
    fig, axes = plt.subplots(n_vars, 1, figsize=(12, 2.2 * n_vars), sharex=True)
    for ax, varname in zip(axes, ds.data_vars, strict=True):
        ax.plot(ds["time"].values, ds[varname].values, lw=0.8)
        ax.set_ylabel(varname, fontsize=8)
        ax.axhline(0, color="k", lw=0.4, ls="--")

    axes[0].set_title("Global scalars (processed, 1971–2024)")
    fig.tight_layout()
    _save(fig, FIG_PROCESSED / "global_scalars.png")
    ds.close()


# ---------------------------------------------------------------------------
# Exploratory drought analysis
# ---------------------------------------------------------------------------

# Standard WMO/McKee SPEI drought severity thresholds
_SPEI_LEVELS = [
    (-0.5, "Light drought", "gold"),
    (-1.0, "Moderate drought", "darkorange"),
    (-1.5, "Severe drought", "orangered"),
    (-2.0, "Extreme drought", "darkred"),
]


def visualize_drought_exploratory(
    val_from_year: int = 2005,
    test_from_year: int = 2015,
    map_thresholds: tuple[float, float] = (-1.0, -1.5),
) -> None:
    """
    Exploratory drought frequency analysis split by train / val / test.

    Produces two figures in figures/processed/exploratory/:
      spei_timeseries.png        — domain-average SPEI with all severity markers
      drought_frequency_maps.png — per-cell event count maps for two thresholds

    Also prints a per-period summary table.

    Args:
        map_thresholds: two SPEI cut-offs used for the count maps
                        (default: moderate ≤-1.0 and severe ≤-1.5)
    """
    print("Drought exploratory analysis ...")
    OUT = FIG_PROCESSED / "exploratory"

    ds_spei = xr.open_dataset(DATA_PROCESSED / "dynamic_grid.nc")
    ds_mask = xr.open_dataset(DATA_PROCESSED / "static_grid.nc")

    spei = ds_spei["spei"].values  # (T, H, W) — raw SPEI, NaN→0 outside mask
    mask = ds_mask["mask"].values.astype(bool)
    rlon = ds_spei["rlon"].values
    rlat = ds_spei["rlat"].values
    times = ds_spei["time"].values
    years = times.astype("datetime64[Y]").astype(int) + 1970
    extent = _rotated_extent(rlat, rlon)

    period_masks = {
        f"Train (<{val_from_year})": years < val_from_year,
        f"Val ({val_from_year}–{test_from_year - 1})": (years >= val_from_year) & (years < test_from_year),
        f"Test (≥{test_from_year})": years >= test_from_year,
    }

    # ── Figure 1: domain-average SPEI time series ─────────────────────────
    spei_flt = spei.copy().astype(float)
    spei_flt[:, ~mask] = np.nan
    domain_mean = np.nanmean(spei_flt, axis=(1, 2))

    fig, ax = plt.subplots(figsize=(14, 3.8))
    shade_colors = ["#d6eaf8", "#d5f5e3", "#fdebd0"]
    for (label, tmask), color in zip(period_masks.items(), shade_colors, strict=False):
        ax.axvspan(times[tmask][0], times[tmask][-1], alpha=0.35, color=color, label=label, zorder=0)
    for _, tmask in list(period_masks.items())[1:]:
        ax.axvline(times[tmask][0], color="gray", lw=1.0, ls="--", zorder=4)

    ax.plot(times, domain_mean, lw=0.9, color="steelblue", label="Domain-mean SPEI", zorder=2)
    ax.axhline(0, color="k", lw=0.4, ls=":", zorder=1)

    for thr, cat_label, color in _SPEI_LEVELS:
        ax.axhline(thr, color=color, lw=1.0, ls="--", label=cat_label, zorder=3)

    ax.set_ylabel("SPEI")
    ax.legend(fontsize=8, ncol=4, loc="upper left")
    ax.set_title("Domain-average SPEI — train / val / test periods")
    fig.tight_layout()
    _save(fig, OUT / "spei_timeseries.png")

    # ── Figure 2: per-cell drought event count maps ─────────────────────────
    thr_labels = {thr: next(lab for t, lab, _ in _SPEI_LEVELS if t == thr) for thr in map_thresholds}
    # Fall back to a generic label if the threshold isn't in _SPEI_LEVELS
    for thr in map_thresholds:
        if thr not in thr_labels:
            thr_labels[thr] = f"SPEI ≤ {thr}"

    period_items = list(period_masks.items())
    n_rows = len(map_thresholds)
    n_cols = len(period_items)

    fig, axes = plt.subplots(
        n_rows,
        n_cols,
        figsize=(5.5 * n_cols, 4.5 * n_rows),
        subplot_kw={"projection": _CRS_PLATE},
        layout="constrained",
    )

    # Pre-compute all rates so a single vmax can be shared across both rows
    all_rates: list[list[tuple[np.ndarray, float]]] = []
    for thr in map_thresholds:
        rates_row = []
        for _, pm in period_items:
            n_years = pm.sum() / 12.0
            rate = (spei[pm] <= thr).sum(axis=0).astype(float) / n_years
            rate[~mask] = np.nan
            rates_row.append((rate, n_years))
        all_rates.append(rates_row)

    vmax = max(np.nanmax(r) for rates_row in all_rates for r, _ in rates_row)
    norm = mpl_colors.Normalize(0, vmax)

    for row, (thr, rates_row) in enumerate(zip(map_thresholds, all_rates, strict=True)):
        for col, ((label, _tmask), (rate, n_years)) in enumerate(zip(period_items, rates_row, strict=True)):
            ax = axes[row, col]
            valid_rate = rate[mask]
            pct_any = 100.0 * float(np.mean(valid_rate > 0))
            mean_rate = float(np.nanmean(valid_rate))

            ax.pcolormesh(
                rlon,
                rlat,
                rate,
                transform=_CRS_ROTATED,
                cmap="YlOrRd",
                norm=norm,
                shading="auto",
                rasterized=True,
            )
            _add_map_features(ax)
            ax.set_extent(extent, crs=_CRS_PLATE)
            ax.set_title(
                f"{label}  ({n_years:.0f} years)\n"
                f"mean {mean_rate:.2f} events/yr/cell  ·  {pct_any:.0f}% cells ≥ 1 event/yr",
                fontsize=9,
            )
        axes[row, 0].text(
            -0.1,
            0.5,
            thr_labels.get(thr, f"SPEI ≤ {thr}"),
            transform=axes[row, 0].transAxes,
            fontsize=10,
            va="center",
            rotation=90,
        )

    # Single shared colorbar for the whole figure
    fig.colorbar(
        mpl_cm.ScalarMappable(norm=norm, cmap="YlOrRd"),
        ax=axes.ravel().tolist(),
        fraction=0.015,
        pad=0.02,
        shrink=0.6,
        label="Events per year",
    )

    fig.suptitle("Per-cell drought frequency (events per year) by period", fontsize=13)
    _save(fig, OUT / "drought_frequency_maps.png")

    # ── Summary table ─────────────────────────────────────────────────────
    headers = ["Period", "Months", "Light%", "Moderate%", "Severe%", "Extreme%", "Cells w/ ≥1 mod."]
    print("\n  " + "  ".join(f"{h:>17}" for h in headers))
    for label, tmask in period_masks.items():
        n_months = int(tmask.sum())
        sub = spei[tmask][:, mask]
        pcts = [100.0 * float((sub <= thr).mean()) for thr, _, _ in _SPEI_LEVELS]
        cells_any = 100.0 * float(np.mean((sub <= -1.0).any(axis=0)))
        row_vals = [label, str(n_months)] + [f"{p:.1f}%" for p in pcts] + [f"{cells_any:.1f}%"]
        print("  " + "  ".join(f"{v:>17}" for v in row_vals))

    ds_spei.close()
    ds_mask.close()


def visualize_drought_frequency_severe(
    val_from_year: int = 2005,
    test_from_year: int = 2015,
    threshold: float = -1.5,
) -> None:
    """
    Per-cell severe-drought (SPEI <= threshold) frequency maps, one panel per
    train/val/test period. Cropped horizontally to the valid-cell bounding box
    (instead of the full model domain) so the panels read less wide.

    Saves figures/processed/exploratory/drought_frequency_severe.png
    """
    print("Drought frequency – severe, cropped ...")
    OUT = FIG_PROCESSED / "exploratory"

    ds_spei = xr.open_dataset(DATA_PROCESSED / "dynamic_grid.nc")
    ds_mask = xr.open_dataset(DATA_PROCESSED / "static_grid.nc")

    spei = ds_spei["spei"].values
    mask = ds_mask["mask"].values.astype(bool)
    rlon = ds_spei["rlon"].values
    rlat = ds_spei["rlat"].values
    times = ds_spei["time"].values
    years = times.astype("datetime64[Y]").astype(int) + 1970

    # Crop the display extent to the valid-cell bounding box, not the full domain.
    rows, cols = np.where(mask)
    crop_rlon = rlon[[cols.min(), cols.max()]]
    crop_rlat = rlat[[rows.min(), rows.max()]]
    extent = _rotated_extent(crop_rlat, crop_rlon, pad=0.5)
    # Match the figure height to the cropped extent's aspect ratio so the GeoAxes'
    # equal-aspect map fills its cell.
    aspect = (extent[1] - extent[0]) / (extent[3] - extent[2])

    period_masks = {
        "Train": years < val_from_year,
        "Val": (years >= val_from_year) & (years < test_from_year),
        "Test": years >= test_from_year,
    }
    period_items = list(period_masks.items())

    rates = []
    for _, pm in period_items:
        n_years = pm.sum() / 12.0
        y_min, y_max = int(years[pm].min()), int(years[pm].max())
        rate = (spei[pm] <= threshold).sum(axis=0).astype(float) / n_years
        rate[~mask] = np.nan
        rates.append((rate, y_min, y_max))

    vmax = max(np.nanmax(r) for r, _, _ in rates)
    norm = mpl_colors.Normalize(0, vmax)

    panel_w = 4.5
    n_cols = len(period_items)
    title_pad = 0.5  # inches reserved above each map for its 2-line title
    fig, axes = plt.subplots(
        1,
        n_cols,
        figsize=(panel_w * n_cols, panel_w / aspect + title_pad),
        subplot_kw={"projection": _CRS_PLATE},
    )
    # Constrained layout doesn't account for the equal-aspect shrinkage GeoAxes
    # apply to fit set_extent, so it leaves large gaps between panels. Manual
    # subplots_adjust with a figsize matched to the extent aspect keeps them tight.
    fig.subplots_adjust(wspace=0.05, left=0.02, right=0.9, top=0.85, bottom=0.02)

    for ax, (label, _), (rate, y_min, y_max) in zip(axes, period_items, rates, strict=True):
        mean_rate = float(np.nanmean(rate[mask]))
        ax.pcolormesh(
            rlon,
            rlat,
            rate,
            transform=_CRS_ROTATED,
            cmap="YlOrRd",
            norm=norm,
            shading="auto",
            rasterized=True,
        )
        _add_map_features(ax)
        ax.set_extent(extent, crs=_CRS_PLATE)
        ax.set_title(
            f"{label}  ({y_min}–{y_max})\nmean {mean_rate:.2f} events/yr/cell",
            fontsize=11,
        )

    cbar = fig.colorbar(
        mpl_cm.ScalarMappable(norm=norm, cmap="YlOrRd"),
        ax=axes.tolist(),
        fraction=0.025,
        pad=0.02,
        shrink=0.85,
        label="Events per year",
    )
    cbar.set_label("Events per year", fontsize=12)

    _save(fig, OUT / "drought_frequency_severe.png")

    ds_spei.close()
    ds_mask.close()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    print("=== Raw data ===")
    visualize_raw_nao_pc1()
    visualize_raw_nao_eof()
    visualize_raw_mediterranean_slt()
    visualize_raw_topography()
    visualize_raw_al_variables(start_idx=0, n_panels=12)

    print("\n=== Processed data ===")
    visualize_processed_dynamic(start_idx=0, n_panels=12)
    visualize_processed_static()
    visualize_processed_global_scalars()
    visualize_drought_frequency_severe()
