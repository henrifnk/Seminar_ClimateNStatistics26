"""
Shared CERRA target-grid definitions.

Lives in its own module (rather than in Inference) so that both the inference
scripts and the evaluation package can use it without importing each other.
"""
import numpy as np
import torch
import torch.nn.functional as F

# Model input/output resolution: the native CERRA grid downsampled from ~500m to ~3.4km
OUTPUT_SIZE = (256, 320)


def compute_cerra_grid(cerra_ds):
    """valid_mask, fine_lat, fine_lon at OUTPUT_SIZE, derived from a CERRA reference dataset."""
    cerra_valid_native = ~np.isnan(cerra_ds["tas"].isel(time=0).values)
    valid_mask = F.interpolate(
        torch.from_numpy(cerra_valid_native.astype(np.float32)).unsqueeze(0).unsqueeze(0),
        size=OUTPUT_SIZE, mode="nearest").squeeze().numpy().astype(bool)
    fine_lat = F.interpolate(
        torch.from_numpy(cerra_ds["lat"].values).float().unsqueeze(0).unsqueeze(0),
        size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze().numpy()
    fine_lon = F.interpolate(
        torch.from_numpy(cerra_ds["lon"].values).float().unsqueeze(0).unsqueeze(0),
        size=OUTPUT_SIZE, mode="bilinear", align_corners=True).squeeze().numpy()
    return valid_mask, fine_lat, fine_lon
