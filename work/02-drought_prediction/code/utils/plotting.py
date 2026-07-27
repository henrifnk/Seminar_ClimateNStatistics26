import random
from pathlib import Path
from typing import Optional

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import numpy as np
import seaborn as sns
import torch
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure

# EUR-11 rotated-pole CRS — same parameters as visualization.py
_CRS_ROTATED = ccrs.RotatedPole(pole_longitude=-162.0, pole_latitude=39.25)
_CRS_PLATE   = ccrs.PlateCarree()


def _rotated_extent(rlat: np.ndarray, rlon: np.ndarray, pad: float = 0.5) -> list[float]:
    xs = [rlon.min(), rlon.max(), rlon.min(), rlon.max()]
    ys = [rlat.min(), rlat.min(), rlat.max(), rlat.max()]
    pts = _CRS_PLATE.transform_points(_CRS_ROTATED, np.array(xs), np.array(ys))
    return [pts[:, 0].min() - pad, pts[:, 0].max() + pad,
            pts[:, 1].min() - pad, pts[:, 1].max() + pad]


def make_heatmap(
    table: torch.Tensor,
    path: Path | str,
    mask: Optional[torch.Tensor] = None,
    rlat: Optional[np.ndarray] = None,
    rlon: Optional[np.ndarray] = None,
    size: tuple[int, int] = (8, 6),
    vmin: Optional[float] = None,
    vmax: Optional[float] = None,
    title: str = "",
    cmap: str = "RdYlBu_r",
) -> str:
    """
    Save a spatial metric heatmap.

    If rlat and rlon are provided, renders a cartopy map with country borders
    in PlateCarree projection (same style as visualization.py).  Falls back to
    a plain seaborn heatmap (north-up corrected) when coordinates are absent.

    Args:
        table: 2D tensor of metric values (H, W).
        path:  Full save path including filename.
        mask:  Boolean tensor (H, W). Cells where mask=False are shown as white.
        rlat:  1D array of rotated latitudes  (H,).
        rlon:  1D array of rotated longitudes (W,).
        title: Plot title.
        cmap:  Matplotlib colormap.
    """
    data = table.float().cpu().numpy()
    mask_np = mask.cpu().numpy().astype(bool) if mask is not None else None

    out = Path(path).expanduser().resolve()
    out.parent.mkdir(parents=True, exist_ok=True)

    if rlat is not None and rlon is not None:
        _make_cartopy_heatmap(data, mask_np, rlat, rlon, out, size, vmin, vmax, title, cmap)
    else:
        _make_seaborn_heatmap(data, mask_np, out, size, vmin, vmax, title, cmap)

    return str(out)


def _make_cartopy_heatmap(
    data: np.ndarray,
    mask_np: Optional[np.ndarray],
    rlat: np.ndarray,
    rlon: np.ndarray,
    out: Path,
    size: tuple[int, int],
    vmin: Optional[float],
    vmax: Optional[float],
    title: str,
    cmap: str,
) -> None:
    if mask_np is not None:
        data = data.copy()
        data[~mask_np] = np.nan

    fig = Figure(figsize=size, layout="constrained")
    FigureCanvas(fig)
    ax = fig.add_subplot(1, 1, 1, projection=_CRS_PLATE)

    im = ax.pcolormesh(
        rlon, rlat, data,
        transform=_CRS_ROTATED,
        cmap=cmap, vmin=vmin, vmax=vmax,
        shading="auto",
    )
    ax.add_feature(cfeature.COASTLINE.with_scale("10m"), linewidth=0.6, edgecolor="black")
    ax.add_feature(cfeature.BORDERS.with_scale("10m"),   linewidth=0.5, edgecolor="black",
                   linestyle="--")
    ax.set_extent(_rotated_extent(rlat, rlon), crs=_CRS_PLATE)

    fig.colorbar(im, ax=ax, fraction=0.04, pad=0.04, shrink=0.8)
    if title:
        ax.set_title(title, fontsize=10)

    fig.savefig(out, dpi=150, bbox_inches="tight")


def _make_seaborn_heatmap(
    data: np.ndarray,
    mask_np: Optional[np.ndarray],
    out: Path,
    size: tuple[int, int],
    vmin: Optional[float],
    vmax: Optional[float],
    title: str,
    cmap: str,
) -> None:
    # Flip vertically so north (high rlat index) appears at the top
    data = np.flipud(data)
    nan_mask = None
    if mask_np is not None:
        data = data.copy()
        data[~np.flipud(mask_np)] = np.nan
        nan_mask = np.isnan(data)

    fig = Figure(figsize=size, layout="constrained")
    FigureCanvas(fig)
    ax = fig.add_subplot(1, 1, 1)

    sns.heatmap(
        data, ax=ax,
        mask=nan_mask,
        vmin=vmin, vmax=vmax,
        cmap=cmap,
        xticklabels=False, yticklabels=False,
        cbar_kws={"shrink": 0.8},
    )
    if title:
        ax.set_title(title, fontsize=10)

    fig.savefig(out, dpi=150, bbox_inches="tight")


def make_pred_vs_target_plot(
    preds,
    targets,
    title="Comparison",
    size=(8, 6),
    xlabel=None,
    ylabel=None,
    filename="forecasts.png",
):
    fig = Figure(figsize=size, frameon=False)
    FigureCanvas(fig)
    ax = fig.add_subplot(111)

    targets = torch.mean(targets.cpu(), dim=[1, 2])
    preds   = torch.mean(preds.cpu(),   dim=[1, 2])
    time_periods = np.arange(targets.shape[0])

    ax.plot(time_periods, targets, "g-",  label="actual")
    ax.plot(time_periods, preds,   "b--", label="predictions")
    ax.legend()

    if xlabel:
        ax.set_xlabel(xlabel)
    if ylabel:
        ax.set_ylabel(ylabel)
    if title:
        ax.set_title(title)

    fig.tight_layout()
    out = Path(filename).expanduser().resolve()
    fig.savefig(out)
    return str(out)