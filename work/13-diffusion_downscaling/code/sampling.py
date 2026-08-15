"""
Shared reverse-diffusion sampler.

Single implementation of the 2nd-order Heun (EDM) ODE sampler from
"Elucidating the Design Space of Diffusion-Based Generative Models" (Karras et al., 2022),
used by every inference, evaluation and diagnostic script in this repo.
"""
import numpy as np
import torch
from tqdm import tqdm


def _snap(x):
    """One sampler tensor as a (2, H, W) CPU float snapshot."""
    return x.squeeze(0).cpu().float()


@torch.no_grad()
def sample_once(model, coarse_input, labels=None, seed=42, num_steps=40, sigma_min=0.002,
                sigma_max=80, rho=7, S_churn=40, S_min=0, S_max=float('inf'),
                S_noise=1, progress=False, desc="Sampling",
                return_intermediates=False, capture="trajectory", include_step0=False):
    """Draw one residual field per batch element by running reverse diffusion.

    Args:
        model: an EDMPrecond network.
        coarse_input: (B, C, H, W) conditioning tensor; the sampled residual has 2 channels.
        labels: (B, 2) or (1, 2) day-of-year/hour tensor, as built by
            DatasetAL.doy_hour_label(). Passing None does NOT disable conditioning —
            EDMPrecond substitutes a zero vector. Always pass the real labels for
            physical output.
        seed: seeds the initial noise and the churn noise, so a given seed
            reproduces a given ensemble member.
        num_steps: number of denoising steps.
        sigma_min, sigma_max, rho: noise schedule; clipped to the range the model was trained on.
        S_churn, S_min, S_max, S_noise: stochastic churn parameters.
        progress: show a tqdm bar over the denoising steps.
        desc: label for that progress bar.
        return_intermediates: also return the per-step snapshots and the noise level
            each one was taken at.
        capture: what a snapshot holds. "trajectory" — the running x_next after the
            step, which still carries the noise the remaining steps remove.
            "denoised" — the denoiser's one-shot estimate D_theta(x_hat, sigma), i.e.
            the clean field the model would predict if sampling stopped right there.
            "both" — the pair, from the same run, so the estimate and the state it
            leads to can be shown side by side without sampling twice.
        include_step0: also prepend the sampler's starting point (pure Gaussian noise
            scaled by sigma_max, before any denoising step) as snapshot/sigma index 0.
            There is no denoiser estimate yet at that point, so under capture="both"
            the prepended entry is (None, initial_noise) — callers must handle a None
            first element. Only takes effect with return_intermediates=True.

    Returns:
        x, the final residual (B, 2, H, W), or (x, snapshots, sigmas) if
        return_intermediates. Each snapshot is a (2, H, W) CPU float tensor and each
        sigma the noise level it was taken at; under capture="both" both are
        (denoised, trajectory) tuples instead.
    """
    if capture not in ("trajectory", "denoised", "both"):
        raise ValueError(
            f"capture must be 'trajectory', 'denoised' or 'both', got {capture!r}")
    torch.manual_seed(seed)
    sigma_min = max(sigma_min, model.sigma_min)
    sigma_max = min(sigma_max, model.sigma_max)

    device = coarse_input.device
    if labels is not None:
        labels = labels.to(device)
    B, _, H, W = coarse_input.shape
    init_noise = torch.randn((B, 2, H, W), dtype=torch.float64, device=device)
    step_idx = torch.arange(num_steps, dtype=torch.float64, device=device)
    # Karras noise schedule: dense near sigma_min, sparse near sigma_max
    t_steps = (sigma_max ** (1 / rho) + step_idx / (num_steps - 1)
               * (sigma_min ** (1 / rho) - sigma_max ** (1 / rho))) ** rho
    t_steps = torch.cat([model.round_sigma(t_steps), torch.zeros_like(t_steps[:1])])

    x_next = init_noise * t_steps[0]
    snapshots = []
    sigmas = []

    if return_intermediates and include_step0:
        if capture == "both":
            snapshots.append((None, _snap(x_next)))
            sigmas.append((float(t_steps[0]), float(t_steps[0])))
        else:
            snapshots.append(_snap(x_next))
            sigmas.append(float(t_steps[0]))

    steps = zip(t_steps[:-1], t_steps[1:])
    if progress:
        steps = tqdm(steps, total=num_steps, desc=desc)

    for i, (t_cur, t_next) in enumerate(steps):
        x_cur = x_next
        # stochastic churn: bump the noise level up before denoising to add sample diversity
        gamma = min(S_churn / num_steps, np.sqrt(2) - 1) if S_min <= t_cur <= S_max else 0
        t_hat = model.round_sigma(t_cur + gamma * t_cur)
        x_hat = x_cur + (t_hat ** 2 - t_cur ** 2).sqrt() * S_noise * torch.randn_like(x_cur)

        # Euler step
        denoised = model(x_hat, t_hat, coarse_input, labels).to(torch.float64)
        d_cur = (x_hat - denoised) / t_hat
        x_next = x_hat + (t_next - t_hat) * d_cur

        # Heun correction — skipped on the last step, where t_next=0 would divide by zero
        if i < num_steps - 1:
            denoised_prime = model(x_next, t_next, coarse_input, labels).to(torch.float64)
            d_prime = (x_next - denoised_prime) / t_next
            x_next = x_hat + (t_next - t_hat) * (0.5 * d_cur + 0.5 * d_prime)

        if return_intermediates:
            if capture == "denoised":
                snapshots.append(_snap(denoised)); sigmas.append(float(t_hat))
            elif capture == "trajectory":
                snapshots.append(_snap(x_next)); sigmas.append(float(t_next))
            else:
                snapshots.append((_snap(denoised), _snap(x_next)))
                sigmas.append((float(t_hat), float(t_next)))

    if return_intermediates:
        return x_next, snapshots, sigmas
    return x_next
