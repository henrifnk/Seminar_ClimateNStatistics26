"""
Visualize the reverse diffusion process of the EDM diffusion model, one folder per
denoising step. For a given date, model checkpoint and starting Gaussian noise (seed),
runs the sampler once and saves both halves of what every step of the Karras noise
schedule does:

    estimate — D_theta(x_hat, sigma), the network's direct one-shot "denoised" guess:
               what the downscaled field would look like if sampling stopped at that
               noise level. This is what the model is actually "estimating" at each
               sigma, uncontaminated by the noise the sampler keeps for the next step.
    state    — x_next, the running trajectory the sampler holds after the ODE/Heun
               step, still carrying the noise the remaining steps remove.

Both come from the same sampling run, so the estimate and the state it leads to are
directly comparable. On the last step the two coincide: with t_next = 0 the Euler
update lands exactly on the denoised estimate.

Runs from either coarse data source:

    cerra — coarsened CERRA reanalysis; fine reanalysis exists for the same date and
            anchors the colour range, so an under-dispersed prediction reads as one.
    mbcn  — bias-corrected (MBCn) climate model output; no ground truth, so the final
            (lowest-sigma) estimate sets the colour range.

Inputs: noise level (the fixed Karras schedule, overridable via --num-steps /
--sigma-min / --sigma-max / --rho), date, and model checkpoint. The starting Gaussian
noise is controlled by --seed (deterministic — same seed always draws the same noise).

Output folder: plots/diagnostics/denoiser_estimate_<date>_seed<seed>/
               (mbcn: denoiser_estimate_mbcn_<date>_seed<seed>/)
  the context frames (coarse input, and CERRA ground truth for cerra), then per step
  step_01/
    estimate_sigma{sigma:.3f}_{tas,pr}.svg  + _{tas,pr}_residual.svg
    state_sigma{sigma:.3f}_{tas,pr}.svg     + _{tas,pr}_residual.svg
  step_02/ ... step_N/
  (step 1 = highest noise level -> step N = sigma ~ 0, the final clean sample. The
   sigma in a filename is the noise level of the field in it: the level the estimate
   was made at, and the lower level the resulting state carries.)
  filmstrip_{tas,pr}.svg, filmstrip_{tas,pr}_residual.svg — step 1 plus every 5th
   step after it as one grid: top row the state going INTO that step (its
   predecessor's outcome — the sampler's starting noise itself, for step 1), middle
   row that step's estimate, bottom row the resulting outcome state — a full
   input -> estimate -> outcome picture of that one denoising step, in a single image.

Usage:
    uv run python src/plot_diffusion_process/diag_denoiser_estimate.py cerra [YYYY-MM-DD] --checkpoint ./model/20.pt --seed 42
    uv run python src/plot_diffusion_process/diag_denoiser_estimate.py mbcn  [YYYY-MM-DD] --seed 42
"""
import argparse
import sys
from pathlib import Path

import numpy as np
import matplotlib.pyplot as plt

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

from inference.Inference import CHECKPOINT, LOADERS
from paths import plot_dir
from sampling import sample_once
from plot_diffusion_process.diag_common import (
    color_ranges,
    full_field,
    load_inputs,
    load_network,
    save_context_frames,
    save_frame,
)

NUM_STEPS     = 40
DEFAULT_DATES = {"cerra": "2019-01-05", "mbcn": "2010-07-15"}


def _save_filmstrip(outdir, vname, kind, states, estimates, sigmas, vmin, vmax, cmap,
                    step_stride=5):
    """One wide figure: 3 rows (input state on top, that step's estimate in the
    middle, resulting outcome state on the bottom) x one column per kept step,
    left-to-right in sampling order — a full input -> estimate -> outcome picture
    of each shown step, using the same values already saved per-step instead of
    spread across step_NN/ folders.

    kind: "full" draws the field itself (state/estimate as returned by full_field);
    "residual" draws the network's residual instead (the other half of the same
    full_field() pair). Step 1 is always kept, plus every `step_stride`-th step
    after it — all 40 is too dense to read at a glance, and adjacent steps look
    nearly identical anyway.

    states/estimates are indexed by step number directly: index 0 is the sampler's
    starting noise (state only — estimates[0] is None, there is no denoiser output
    yet), indices 1..num_steps are the real steps. A shown step i's "input" is
    states[i - 1], its predecessor's outcome — index 0 for step 1's input, so that
    lookup is always in range.
    """
    v = 0 if vname == "tas" else 1
    idx = 0 if kind == "residual" else 1
    step_indices = [i for i in range(1, len(states)) if i == 1 or i % step_stride == 0]
    if not step_indices:
        step_indices = [len(states) - 1]
    n_cols = len(step_indices)
    # constrained_layout, not manual subplots_adjust fractions: a hand-tuned fixed
    # header height collides with matplotlib's own fixed suptitle y once the figure
    # gets short — constrained_layout keeps suptitle/titles/axes from overlapping
    # regardless of figure size.
    fig, axs = plt.subplots(3, n_cols, figsize=(1.35 * n_cols + 0.6, 4.6),
                            constrained_layout=True)
    if n_cols == 1:
        axs = axs[:, np.newaxis]

    for col, i in enumerate(step_indices):
        sigma_est, sigma_state = sigmas[i]
        input_data = states[i - 1][idx][v]
        est_data   = estimates[i][idx][v]
        out_data   = states[i][idx][v]
        for row, data in enumerate([input_data, est_data, out_data]):
            ax = axs[row, col]
            ax.imshow(data, origin="upper", aspect="equal", cmap=cmap,
                     vmin=vmin, vmax=vmax, interpolation="nearest", rasterized=True)
            ax.set_xticks([]); ax.set_yticks([])
            ax.set_facecolor("lightgrey")
        axs[0, col].set_title(f"step {i}\nσ={sigma_state:.2g}", fontsize=8)

    # Notation matches the training objective: x_sigma = x_0 + eps is the noisy field
    # the sampler is holding at that step's noise level; x_hat_0 = D_theta(x_sigma; sigma, y)
    # is the denoiser's one-shot estimate of the clean field from it.
    axs[0, 0].set_ylabel("Input\n" + r"($x_\sigma$)", fontsize=9)
    axs[1, 0].set_ylabel("Estimate\n" + r"($\hat{x}_0 = D_\theta$)", fontsize=9)
    axs[2, 0].set_ylabel("Outcome\n" + r"($x_{\sigma'}$)", fontsize=9)
    title_suffix = " residual" if kind == "residual" else ""
    fig.suptitle(f"Reverse diffusion filmstrip — {vname}{title_suffix}",
                fontsize=12, fontweight="bold")

    stem = f"filmstrip_{vname}" + ("_residual" if kind == "residual" else "")
    out = outdir / f"{stem}.svg"
    fig.savefig(out, dpi=200, facecolor="white")
    plt.close(fig)
    print(f"  Saved {out}")


# ── main ────────────────────────────────────────────────────────────────────

def main(source, date_str, checkpoint=CHECKPOINT, seed=42, num_steps=NUM_STEPS,
         sigma_min=0.002, sigma_max=80, rho=7, borders=False, legend=False):
    ctx     = load_inputs(source, date_str)
    network = load_network(checkpoint)

    print(f"Running reverse diffusion (seed={seed}), capturing the denoiser's one-shot "
          f"estimate and the resulting sampler state at each of {num_steps} steps...")
    _, snapshots, sigmas = sample_once(
        network, ctx["coarse_input"], labels=ctx["labels"], seed=seed, num_steps=num_steps,
        sigma_min=sigma_min, sigma_max=sigma_max, rho=rho,
        progress=True, desc="Reverse diffusion",
        return_intermediates=True, capture="both", include_step0=True)

    # Index 0 is the sampler's starting point (pure noise, before any denoising
    # step) — no denoiser estimate exists yet there, so its snapshot is
    # (None, initial_noise). Indices 1..num_steps are the real steps, and (unlike
    # before include_step0) the index now equals the step number directly.
    estimates = [full_field(ctx, est) if est is not None else None for est, _ in snapshots]
    states    = [full_field(ctx, state) for _,   state in snapshots]

    ranges = color_ranges(ctx, estimates[-1][1])
    vmin_t, vmax_t, vmin_p, vmax_p = ranges

    # Residuals share a range too, set by the final (cleanest) estimate
    valid_mask = ctx["valid_mask"]
    final_res  = estimates[-1][0]
    vabs_res_t = float(np.nanpercentile(np.abs(final_res[0][valid_mask]), 99))
    vabs_res_p = float(np.nanpercentile(np.abs(final_res[1][valid_mask]), 99))

    suffix = ("_borders" if borders else "") + ("_legend" if legend else "")
    stem   = "denoiser_estimate" + ("_mbcn" if source == "mbcn" else "")
    outdir = plot_dir("diagnostics", f"{stem}_{date_str}_seed{seed}{suffix}")
    print(f"Saving {num_steps} step folders to {outdir}/")

    save_context_frames(outdir, ctx, ranges, borders, legend)
    print("  Saved coarse input frames" + (" + CERRA ground truth" if source == "cerra" else ""))

    fine_lat, fine_lon, extent = ctx["fine_lat"], ctx["fine_lon"], ctx["extent"]

    def save_fields(stepdir, name, res, full):
        """The four frames of one field: tas and pr, each as full field and residual."""
        full_t, full_p = full
        save_frame(stepdir / f"{name}_tas.svg", full_t, vmin_t, vmax_t, "RdBu_r", fine_lon, fine_lat, extent, borders, legend, unit="°C")
        save_frame(stepdir / f"{name}_pr.svg",  full_p, vmin_p, vmax_p, "Blues",  fine_lon, fine_lat, extent, borders, legend, unit="mm/day")
        save_frame(stepdir / f"{name}_tas_residual.svg", res[0], -vabs_res_t, vabs_res_t, "RdBu_r", fine_lon, fine_lat, extent, borders, legend, unit="°C")
        save_frame(stepdir / f"{name}_pr_residual.svg",  res[1], -vabs_res_p, vabs_res_p, "BrBG",   fine_lon, fine_lat, extent, borders, legend, unit="mm/day")

    # Index 0 is the initial-noise entry, not a step in its own right — skip it here,
    # it only exists to give the filmstrip's step-1 column a real "input" to show.
    for i, (sigma_est, sigma_state) in enumerate(sigmas):
        if i == 0:
            continue
        stepdir = outdir / f"step_{i:02d}"
        stepdir.mkdir(exist_ok=True)
        save_fields(stepdir, f"estimate_sigma{sigma_est:.3f}",   *estimates[i])
        save_fields(stepdir, f"state_sigma{sigma_state:.3f}",    *states[i])
        print(f"  [step {i:2d}/{num_steps}]  estimate at sigma={sigma_est:8.4f}"
              f"  ->  state at sigma={sigma_state:8.4f}  ->  {stepdir.name}/")

    _save_filmstrip(outdir, "tas", "full", states, estimates, sigmas, vmin_t, vmax_t, "RdBu_r")
    _save_filmstrip(outdir, "pr",  "full", states, estimates, sigmas, vmin_p, vmax_p, "Blues")
    _save_filmstrip(outdir, "tas", "residual", states, estimates, sigmas,
                    -vabs_res_t, vabs_res_t, "RdBu_r")
    _save_filmstrip(outdir, "pr",  "residual", states, estimates, sigmas,
                    -vabs_res_p, vabs_res_p, "BrBG")

    print("Done.")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("source", choices=sorted(LOADERS), help="coarse data source")
    ap.add_argument("date", nargs="?", default=None,
                    help="YYYY-MM-DD (default: 2019-01-05 for cerra, 2010-07-15 for mbcn); "
                         "must exist in the chosen source's files")
    ap.add_argument("-c", "--checkpoint", default=CHECKPOINT)
    ap.add_argument("--seed", type=int, default=42,
                    help="seed for the starting Gaussian noise draw (deterministic)")
    ap.add_argument("-n", "--num-steps", type=int, default=NUM_STEPS)
    ap.add_argument("--sigma-min", type=float, default=0.002)
    ap.add_argument("--sigma-max", type=float, default=80)
    ap.add_argument("--rho",       type=float, default=7)
    ap.add_argument("-b", "--borders", action="store_true")
    ap.add_argument("-l", "--legend",  action="store_true")
    args = ap.parse_args()

    main(args.source, args.date or DEFAULT_DATES[args.source],
         checkpoint=args.checkpoint, seed=args.seed, num_steps=args.num_steps,
         sigma_min=args.sigma_min, sigma_max=args.sigma_max, rho=args.rho,
         borders=args.borders, legend=args.legend)
