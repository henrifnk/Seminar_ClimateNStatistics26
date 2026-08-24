"""
Model loading, diffusion sampling, ensemble generation, and evaluation metrics shared
across the evaluation package.

Imported by CalibrationPeriodAssessment.py, ProjectionPeriodAssessment.py,
ProjectionPlots.py (for the shared variable colours) and
inference/HottestWettestDayInference.py.
"""
import hashlib
import sys
from pathlib import Path

import numpy as np
import torch

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

import wm2024model

# re-exported so the assessment scripts can pull the whole shared evaluation API from here
from grids import compute_cerra_grid  # noqa: F401
from sampling import sample_once

DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'

COLOR_TAS = "#C0392B"  # deep red  — temperature
COLOR_PR  = "#2980B9"  # medium blue — precipitation
VAR_COLORS = [COLOR_TAS, COLOR_PR]

# Bump whenever sampling behaviour changes, so prediction caches from the old behaviour
# become a cache miss rather than being silently reused.
SAMPLER_VERSION = 2


def cache_tag(checkpoint):
    """Short (checkpoint, sampler-behaviour) identifier for prediction-cache filenames.

    Prediction caches hold sampler output, so reusing one across a different checkpoint
    or a changed sampler silently mixes runs — which is exactly what made the previous
    "compare checkpoints by re-running" workflow return identical numbers. Only caches of
    model output should carry this; caches of input data (native MBCn / coarse CERRA
    pixel values) are unaffected by either and keep their plain slug.
    """
    p = Path(checkpoint)
    st = p.stat()
    digest = hashlib.sha256(
        f"{p.resolve()}:{st.st_size}:{st.st_mtime_ns}".encode()).hexdigest()[:8]
    return f"{p.stem}-{digest}-v{SAMPLER_VERSION}"


def load_model(checkpoint):
    network = wm2024model.EDMPrecond((256, 320), 6, 2, label_dim=2, model_channels=64)
    network.load_state_dict(torch.load(checkpoint, map_location=DEVICE))
    network.to(DEVICE).eval()
    return network


def run_ensemble(network, coarse_input, coarse, norm_res_mean, norm_res_std, valid_mask, n,
                 labels=None):
    """Draw n ensemble members and add the sampled residual back onto the coarse field.

    labels is the (1, 2) day-of-year/hour tensor for the date being downscaled. See
    sample_once() for the conditioning behaviour when it is omitted.
    """
    samples = []
    for s in range(n):
        res_norm = sample_once(network, coarse_input, labels=labels,
                               seed=s).squeeze(0).cpu().float()
        res = (res_norm * norm_res_std.view(2,1,1) + norm_res_mean.view(2,1,1)).numpy()
        res[:, ~valid_mask] = 0.0
        pred = coarse + res
        pred[:, ~valid_mask] = np.nan
        samples.append(pred)
    return np.stack(samples)  # (N, 2, H, W)


def crps_ensemble(obs, ensemble):
    """
    obs:      (2, H, W) numpy, NaN outside domain
    ensemble: (N, 2, H, W) numpy
    returns:  (2,) mean CRPS per variable

    Uses the fair estimator: the plug-in pairwise term is biased for finite N, so
    i==j pairs are excluded and the sum is normalized by N(N-1) instead of N^2, i.e.
    scaled by N/(N-1) relative to the naive/"unfair" NRG estimator.
    """
    N = ensemble.shape[0]
    mae_term  = np.nanmean(np.abs(ensemble - obs[None]), axis=0)
    pair_diff = np.mean(np.abs(ensemble[:, None] - ensemble[None, :]), axis=(0, 1))
    crps_map  = mae_term - 0.5 * (N / (N - 1)) * pair_diff
    return np.array([np.nanmean(crps_map[0]), np.nanmean(crps_map[1])])


def radial_power_spectrum(field):
    """
    field: (H, W) numpy, NaNs filled with field mean before FFT.
    Returns 1D radially averaged power spectrum.
    """
    filled = field.copy()
    filled[np.isnan(filled)] = np.nanmean(filled)
    f = np.fft.fft2(filled)
    power = np.abs(np.fft.fftshift(f))**2
    H, W = field.shape
    cy, cx = H // 2, W // 2
    y, x = np.ogrid[-cy:H - cy, -cx:W - cx]
    r = np.sqrt(x**2 + y**2).astype(int)
    counts = np.bincount(r.ravel())
    radial = np.bincount(r.ravel(), weights=power.ravel()) / counts
    return radial


