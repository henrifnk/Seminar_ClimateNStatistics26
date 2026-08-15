"""
Visualize the forward (noising) process of the EDM diffusion model.

For each sigma level in SIGMAS, saves one PNG showing the full field
(coarse bilinear + noised residual) at that noise level.
σ=0 shows the clean downscaled field (CERRA); σ=80 is pure noise added on top
of the coarse field — the starting point of the reverse diffusion sampler.

Output folder:  plots/diagnostics/forward_process_<date>/
  tas and pr are written as separate bare PNGs per sigma level, not as one paired frame.

Usage:
    uv run python src/plot_diffusion_process/diag_forward_process.py [YYYY-MM-DD]  (default: 2019-01-05)
"""
import sys
from datetime import datetime
from functools import partial
from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F
import xarray as xr

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

from grids import OUTPUT_SIZE, compute_cerra_grid
from inference.Inference import (
    CERRA_REF,
    NORM_RES_MEAN,
    NORM_RES_STD,
    build_interpolator,
    interpolate_to_cerra,
)
from paths import plot_dir
from plot_diffusion_process import diag_common

# this script labels its colorbar ticks smaller than the other diag_* scripts
save_frame = partial(diag_common.save_frame, tick_labelsize=7)

DATADIR_FINE   = "data/reanalysis"
DATADIR_COARSE = "data/reanalysis_coarsened"

# EDM sigma_min=0.002 … sigma_max=80; cover the full range with representative steps
SIGMAS = [0.0, 0.01, 0.05, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 40.0, 80.0]
RNG    = np.random.default_rng(0)


# ── data loading ─────────────────────────────────────────────────────────────

def load_date(date_str):
    date        = datetime.strptime(date_str, "%Y-%m-%d")
    year, month = date.year, date.month

    cp_tas = Path(DATADIR_COARSE) / f"CERRA_tas_daily_{year}_{month:02d}_coarsened.nc"
    cp_pr  = Path(DATADIR_COARSE) / f"CERRA_pr_daily_{year}_{month:02d}_coarsened.nc"
    fp_tas = Path(DATADIR_FINE)   / "tas" / f"CERRA_tas_daily_{year}_{month:02d}.nc"
    fp_pr  = Path(DATADIR_FINE)   / "pr"  / f"CERRA_pr_daily_{year}_{month:02d}.nc"
    for p in [cp_tas, cp_pr, fp_tas, fp_pr]:
        if not p.exists():
            raise FileNotFoundError(f"Missing: {p}")

    ds_ct = xr.open_dataset(cp_tas, engine="netcdf4")
    ds_cp = xr.open_dataset(cp_pr,  engine="netcdf4")
    ds_ft = xr.open_dataset(fp_tas, engine="netcdf4")
    ds_fp = xr.open_dataset(fp_pr,  engine="netcdf4")

    dates = [str(t)[:10] for t in ds_ct.time.values]
    if date_str not in dates:
        raise ValueError(f"{date_str} not found. Available: {dates[0]} – {dates[-1]}")
    t_idx = dates.index(date_str)

    coarse_tas_np = ds_ct["tas"].isel(time=t_idx).values.astype(np.float32)
    coarse_pr_np  = ds_cp["pr"].isel(time=t_idx).values.astype(np.float32)

    fine_dates_t = [str(x)[:10] for x in ds_ft.time.values]
    fine_dates_p = [str(x)[:10] for x in ds_fp.time.values]
    fine_tas_np  = ds_ft["tas"].isel(time=fine_dates_t.index(date_str)).values.astype(np.float32)
    fine_pr_np   = ds_fp["pr"].isel(time=fine_dates_p.index(date_str)).values.astype(np.float32)

    cerra_ref = xr.open_dataset(CERRA_REF, engine="netcdf4")
    valid, idx, weights, fine_shape = build_interpolator(ds_ct, cerra_ref)

    valid_mask, fine_lat, fine_lon = compute_cerra_grid(cerra_ref)

    def coarse_to_grid(arr):
        native = interpolate_to_cerra(arr, valid, idx, weights, fine_shape)
        out = F.interpolate(
            torch.from_numpy(native).unsqueeze(0).unsqueeze(0).float(),
            size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze().numpy()
        out[~valid_mask] = np.nan
        return out

    def fine_to_grid(arr):
        out = F.interpolate(
            torch.from_numpy(arr).unsqueeze(0).unsqueeze(0).float(),
            size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze().numpy()
        out[~valid_mask] = np.nan
        return out

    coarse_tas = coarse_to_grid(coarse_tas_np)
    coarse_pr  = coarse_to_grid(coarse_pr_np)
    fine_tas   = fine_to_grid(fine_tas_np)
    fine_pr    = fine_to_grid(fine_pr_np)

    for ds in [ds_ct, ds_cp, ds_ft, ds_fp, cerra_ref]:
        ds.close()

    return coarse_tas, coarse_pr, fine_tas, fine_pr, valid_mask, fine_lat, fine_lon


# ── main ──────────────────────────────────────────────────────────────────────

def main(date_str, borders=False, legend=False):
    print(f"Loading data for {date_str} ...")
    coarse_tas, coarse_pr, fine_tas, fine_pr, valid_mask, fine_lat, fine_lon = load_date(date_str)
    extent = [float(fine_lon.min()), float(fine_lon.max()),
              float(fine_lat.min()), float(fine_lat.max())]

    res_tas = fine_tas - coarse_tas
    res_pr  = fine_pr  - coarse_pr

    nm, ns = NORM_RES_MEAN.numpy(), NORM_RES_STD.numpy()   # (2,)
    res_norm_tas = (res_tas - nm[0]) / ns[0]
    res_norm_pr  = (res_pr  - nm[1]) / ns[1]

    noise_tas = RNG.standard_normal(res_norm_tas.shape).astype(np.float32)
    noise_pr  = RNG.standard_normal(res_norm_pr.shape).astype(np.float32)

    # Colour ranges fixed to the clean full field (coarse + clean residual = fine_tas/pr)
    vmin_t = float(np.nanpercentile(fine_tas[valid_mask],  1))
    vmax_t = float(np.nanpercentile(fine_tas[valid_mask], 99))
    vmin_p = 0.0
    vmax_p = float(np.nanpercentile(fine_pr[valid_mask],  99))

    suffix = ("_borders" if borders else "") + ("_legend" if legend else "")
    outdir = plot_dir("diagnostics", f"forward_process_{date_str}{suffix}")
    print(f"Saving {len(SIGMAS)} frames to {outdir}/")

    for i, sigma in enumerate(SIGMAS):
        noisy_res_tas = (res_norm_tas + sigma * noise_tas) * ns[0] + nm[0]
        noisy_res_pr  = (res_norm_pr  + sigma * noise_pr)  * ns[1] + nm[1]

        # Full field: coarse + noised residual
        full_tas = coarse_tas + noisy_res_tas
        full_pr  = coarse_pr  + noisy_res_pr
        full_tas[~valid_mask] = np.nan
        full_pr[~valid_mask]  = np.nan

        stem = f"frame_{i:02d}_sigma_{sigma:05.2f}"
        save_frame(outdir / f"{stem}_tas.svg", full_tas, vmin_t, vmax_t, "RdBu_r", fine_lon, fine_lat, extent, borders, legend, unit="°C")
        save_frame(outdir / f"{stem}_pr.svg",  full_pr,  vmin_p, vmax_p, "Blues",  fine_lon, fine_lat, extent, borders, legend, unit="mm/day")
        print(f"  [{i+1:2d}/{len(SIGMAS)}] sigma={sigma}  ->  {stem}_{{tas,pr}}.svg")

    print("Done.")


if __name__ == "__main__":
    flags    = {a for a in sys.argv[1:] if a.startswith("-")}
    pos      = [a for a in sys.argv[1:] if not a.startswith("-")]
    borders  = "--borders" in flags or "-b" in flags
    legend   = "--legend" in flags or "-l" in flags
    date_arg = pos[0] if pos else "2019-01-05"
    main(date_arg, borders=borders, legend=legend)
