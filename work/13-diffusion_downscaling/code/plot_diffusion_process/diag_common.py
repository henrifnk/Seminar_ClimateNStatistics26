"""
Plot and data-loading helpers shared by the diag_*.py diagnostic scripts.
"""
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import torch
import xarray as xr
from scipy.spatial import cKDTree

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

import wm2024model
from DatasetAL import doy_hour_label
from grids import compute_cerra_grid
from inference.Inference import (
    CERRA_REF,
    LOADERS,
    NORM_RAW_MEAN,
    NORM_RAW_STD,
    NORM_RES_MEAN,
    NORM_RES_STD,
    load_static_constants,
)

DEVICE = "cuda" if torch.cuda.is_available() else "cpu"

# Colorbar geometry used when save_frame is called with legend=True
DEFAULT_CBAR_KWARGS = dict(shrink=0.6, pad=0.04, fraction=0.05, aspect=30)


def save_frame(outpath, data, vmin, vmax, cmap, lon2d, lat2d, extent,
               borders=False, legend=False, unit="",
               cbar_kwargs=None, tick_labelsize=11):
    """Save a single bare map frame (format inferred from outpath's extension).

    borders=True draws the field on a PlateCarree projection with coastlines;
    otherwise the array is drawn directly with imshow. legend=True adds a
    colorbar (and the extra margin needed for it).
    """
    cmap_obj = plt.get_cmap(cmap).copy()
    cmap_obj.set_bad("lightgrey")
    figsize = (8.5, 6) if legend else None
    if borders:
        import cartopy.crs as ccrs
        proj = ccrs.PlateCarree()
        fig, ax = plt.subplots(1, 1, figsize=figsize, subplot_kw={"projection": proj})
        fig.patch.set_facecolor("lightgrey")
        im = ax.pcolormesh(lon2d, lat2d, data, vmin=vmin, vmax=vmax, cmap=cmap_obj,
                           transform=proj, shading="nearest", rasterized=True)
        ax.set_extent(extent, crs=proj)
        ax.coastlines()
        ax.set_facecolor("lightgrey")
        ax.axis("off")
    else:
        fig, ax = plt.subplots(1, 1, figsize=figsize)
        fig.patch.set_facecolor("lightgrey")
        im = ax.imshow(data, origin="upper", aspect="equal",
                       cmap=cmap_obj, vmin=vmin, vmax=vmax, interpolation="nearest",
                       extent=extent, rasterized=True)
        ax.set_facecolor("lightgrey")
        ax.axis("off")
    if legend:
        cbar = fig.colorbar(im, ax=ax, **(cbar_kwargs or DEFAULT_CBAR_KWARGS))
        cbar.ax.tick_params(labelsize=tick_labelsize)
        cbar.set_label(unit, fontsize=9, labelpad=3)
        fig.savefig(outpath, dpi=130, bbox_inches="tight", facecolor="lightgrey")
    else:
        fig.subplots_adjust(left=0, right=1, top=1, bottom=0)
        fig.savefig(outpath, dpi=130, bbox_inches="tight", pad_inches=0, facecolor="lightgrey")
    plt.close(fig)


def domain_mask(lat2d, lon2d, fine_lat, fine_lon, valid_mask, max_dist_deg=0.1):
    """Boolean mask over lat2d/lon2d marking points that lie near a truly valid
    CERRA pixel. Uses nearest-neighbor distance rather than a Delaunay/convex-hull
    test, since the CERRA domain is concave (a crescent along the Alps) — a convex
    hull of the valid points fills in a large chunk of area that isn't actually
    covered by CERRA."""
    valid_pts = np.column_stack([fine_lat[valid_mask].ravel(), fine_lon[valid_mask].ravel()])
    tree = cKDTree(valid_pts)
    pts = np.column_stack([lat2d.ravel(), lon2d.ravel()])
    dist, _ = tree.query(pts)
    return (dist <= max_dist_deg).reshape(lat2d.shape)


def orient_native(lat2d, lon2d, *fields):
    """Flip a native grid that runs south → north.

    The bare (borders=False) frames are drawn with imshow, which puts row 0 at the top,
    so a grid stored south-first would come out upside down."""
    if lat2d[0, 0] < lat2d[-1, 0]:
        return (np.flipud(lat2d), np.flipud(lon2d), *(np.flipud(f) for f in fields))
    return (lat2d, lon2d, *fields)


# ── inputs for the reverse-process diagnostics ────────────────────────────────

def load_cerra_grid(cache_dir="cache"):
    """(target_grid, valid_mask, fine_lat, fine_lon) for the CERRA target domain.

    target_grid is the lat/lon Dataset the coarse fields are regridded onto; the rest is
    at OUTPUT_SIZE, for masking and plotting.

    Read from the raw reanalysis reference file when it is available — regridding then
    lands on CERRA's native grid and to_output_grid downsamples from there. On an
    MBCn-only setup that file is missing, so the grid comes from cache/grid.npz (written
    by generate_grid_cache.py) and regridding lands on OUTPUT_SIZE directly, which
    to_output_grid leaves untouched. The static-variables file is not usable as a
    stand-in: it holds the full CERRA domain, not the Alpine crop this project targets.
    """
    if Path(CERRA_REF).exists():
        cerra_ds = xr.open_dataset(CERRA_REF, engine="netcdf4")
        valid_mask, fine_lat, fine_lon = compute_cerra_grid(cerra_ds)
        target_grid = xr.Dataset({"lat": (("y", "x"), cerra_ds["lat"].values),
                                  "lon": (("y", "x"), cerra_ds["lon"].values)})
        cerra_ds.close()
        return target_grid, valid_mask, fine_lat, fine_lon

    cache = Path(cache_dir) / "grid.npz"
    if not cache.exists():
        raise FileNotFoundError(
            f"Neither {CERRA_REF} nor {cache} exists. Run generate_grid_cache.py on a "
            "machine that has the reanalysis, then copy the cache over.")
    grid = np.load(cache)
    valid_mask = grid["valid_mask"].astype(bool)
    fine_lat, fine_lon = grid["fine_lat"], grid["fine_lon"]
    target_grid = xr.Dataset({"lat": (("y", "x"), fine_lat), "lon": (("y", "x"), fine_lon)})
    return target_grid, valid_mask, fine_lat, fine_lon


def load_inputs(source, date_str, cache_dir="cache"):
    """Everything the reverse-process diagnostics need before sampling.

    source is "cerra" (coarsened reanalysis, so fine reanalysis exists as ground truth)
    or "mbcn" (bias-corrected climate model output, no ground truth). Returns the coarse
    fields on the model grid, their native-grid originals for the raw-input frames, the
    conditioning tensor the network sees, and the plotting grid.
    """
    print(f"Loading {source} data for {date_str} ...")
    target_grid, valid_mask, fine_lat, fine_lon = load_cerra_grid(cache_dir)
    fields = LOADERS[source](date_str, target_grid, valid_mask)

    print("Loading constants...")
    const_tensor = load_static_constants()
    coarse_stack = torch.stack([torch.from_numpy(fields["coarse_tas"]).float(),
                                torch.from_numpy(fields["coarse_pr"]).float()], dim=0)
    coarse_norm = torch.nan_to_num(
        (coarse_stack - NORM_RAW_MEAN.view(2, 1, 1)) / NORM_RAW_STD.view(2, 1, 1), nan=0.0)

    return {
        **fields,
        "source": source,
        "valid_mask": valid_mask,
        "fine_lat": fine_lat,
        "fine_lon": fine_lon,
        "extent": [float(fine_lon.min()), float(fine_lon.max()),
                   float(fine_lat.min()), float(fine_lat.max())],
        "coarse_input": torch.cat([coarse_norm, const_tensor], dim=0).unsqueeze(0).to(DEVICE),
        "labels": doy_hour_label(date_str),
    }


def load_network(checkpoint):
    print(f"Loading model from {checkpoint} ...")
    network = wm2024model.EDMPrecond((256, 320), 6, 2, label_dim=2, model_channels=64)
    network.load_state_dict(torch.load(checkpoint, map_location=DEVICE))
    network.to(DEVICE).eval()
    return network


def full_field(ctx, snapshot):
    """One sampler snapshot as (residual, full field) pairs in physical units.

    The network predicts a residual on top of the interpolated coarse field, so the
    downscaled field is coarse + residual; both are NaN outside the reanalysis domain.
    """
    valid_mask = ctx["valid_mask"]
    res = (snapshot * NORM_RES_STD.view(2, 1, 1) + NORM_RES_MEAN.view(2, 1, 1)).numpy()
    res[0][~valid_mask] = np.nan
    res[1][~valid_mask] = np.nan
    full_t = ctx["coarse_tas"] + res[0]; full_t[~valid_mask] = np.nan
    full_p = ctx["coarse_pr"]  + res[1]; full_p[~valid_mask] = np.nan
    return res, (full_t, full_p)


def color_ranges(ctx, final_full):
    """(vmin_t, vmax_t, vmin_p, vmax_p), shared by every frame of a run.

    Anchored to the CERRA ground truth where it exists, so an under-dispersed prediction
    reads as one; for a climate projection there is no truth, so the final clean sample
    sets the range instead.
    """
    valid_mask = ctx["valid_mask"]
    ref_t, ref_p = ((ctx["truth_tas"], ctx["truth_pr"])
                    if ctx["truth_tas"] is not None else final_full)
    return (float(np.nanpercentile(ref_t[valid_mask],  1)),
            float(np.nanpercentile(ref_t[valid_mask], 99)),
            0.0,
            float(np.nanpercentile(ref_p[valid_mask], 99)))


def save_context_frames(outdir, ctx, ranges, borders=False, legend=False):
    """The frames the sampler steps are read against: ground truth (cerra only), the raw
    coarse input on its own grid, and the interpolated coarse field the network sees."""
    vmin_t, vmax_t, vmin_p, vmax_p = ranges
    valid_mask, fine_lat, fine_lon = ctx["valid_mask"], ctx["fine_lat"], ctx["fine_lon"]
    extent = ctx["extent"]

    def frame(name, data, vmin, vmax, cmap, lon2d, lat2d, unit):
        save_frame(outdir / f"{name}.svg", data, vmin, vmax, cmap,
                   lon2d, lat2d, extent, borders, legend, unit=unit)

    if ctx["truth_tas"] is not None:
        frame("cerra_truth_tas", ctx["truth_tas"], vmin_t, vmax_t, "RdBu_r", fine_lon, fine_lat, "°C")
        frame("cerra_truth_pr",  ctx["truth_pr"],  vmin_p, vmax_p, "Blues",  fine_lon, fine_lat, "mm/day")

    # Raw coarse input on its own (blocky, unregridded) grid, clipped to the CERRA domain
    native_mask = domain_mask(ctx["native_lat"], ctx["native_lon"], fine_lat, fine_lon, valid_mask)
    lat_n, lon_n, tas_n, pr_n = orient_native(
        ctx["native_lat"], ctx["native_lon"],
        np.where(native_mask, ctx["native_tas"], np.nan),
        np.where(native_mask, ctx["native_pr"],  np.nan))
    raw_stem = "native_coarse" if ctx["source"] == "cerra" else "coarse_raw"
    frame(f"{raw_stem}_tas", tas_n, vmin_t, vmax_t, "RdBu_r", lon_n, lat_n, "°C")
    frame(f"{raw_stem}_pr",  pr_n,  vmin_p, vmax_p, "Blues",  lon_n, lat_n, "mm/day")

    # Interpolated coarse field — what the model actually receives as conditioning
    interp_stem = "coarse" if ctx["source"] == "cerra" else "coarse_bilinear"
    frame(f"{interp_stem}_tas", np.where(valid_mask, ctx["coarse_tas"], np.nan),
          vmin_t, vmax_t, "RdBu_r", fine_lon, fine_lat, "°C")
    frame(f"{interp_stem}_pr", np.where(valid_mask, ctx["coarse_pr"], np.nan),
          vmin_p, vmax_p, "Blues", fine_lon, fine_lat, "mm/day")
