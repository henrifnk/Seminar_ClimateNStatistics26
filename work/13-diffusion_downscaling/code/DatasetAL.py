"""
UpscaleDataset — the coarse/fine training pairs the diffusion model is trained on.

Walks the monthly CERRA files, pairs each fine timestep with its coarsened counterpart,
regrids the coarse field onto the CERRA grid and returns the *residual* (fine − coarse)
as the target, so the network never has to reproduce the large-scale field it is given.
Day-of-year and hour ride along as the EDM label pair.

Normalization statistics are computed from the dataset and cached to
data/norm_stats.npz by save_norm_stats(); TrainDiffusion writes that file as a side
effect of training, and ComputeNormStats.py is the standalone entry point for it.
"""
from pathlib import Path

import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
import xarray as xr
from scipy.spatial import Delaunay

NORM_STATS_FILE = "data/norm_stats.npz"


def doy_hour_label(date_str, hour=0):
    """The (1, 2) EDM label tensor for a date, in the encoding the model was trained on.

    Must stay bit-identical to the vectorized version in UpscaleDataset.__init__ (the
    `doy_norm` / `hour_norm` block): a synthetic 360-day calendar of 30-day months, NOT
    the true day-of-year. Using calendar.timetuple().tm_yday / 365 here would drift ~5
    days from training by mid-year and reintroduce a silent conditioning offset.

    The training data is daily and midnight-stamped, so hour_norm is 0 throughout and
    that component carries no signal; it is kept only because the network was built with
    label_dim=2.

        doy_hour_label("2019-01-05") -> tensor([[0.0111, 0.0000]])
    """
    month, day = int(date_str[5:7]), int(date_str[8:10])
    doy_norm = ((month - 1.) * 30 + (day - 1.)) / 360.
    return torch.tensor([[doy_norm, hour / 24.]], dtype=torch.float32)


def save_norm_stats(dataset, path=NORM_STATS_FILE):
    """Write a dataset's normalization stats to an .npz for the inference scripts to reuse."""
    np.savez(path,
             norm_raw_mean=dataset._norm_raw_mean.numpy(),
             norm_raw_std=dataset._norm_raw_std.numpy(),
             norm_res_mean=dataset._norm_res_mean.numpy(),
             norm_res_std=dataset._norm_res_std.numpy())
    print(f"Saved normalization stats to {path}")
    return path


class UpscaleDataset(torch.utils.data.Dataset):
    """
    Lazy-loading dataset: fine CERRA (1749x2198, UTM) + coarsened CERRA (77x91, rotated lat/lon).
    Two variables: tas (temperature) and pr (precipitation), loaded jointly.
    Both fine and coarse are subsampled to output_size=(256, 320) at ~3.4km resolution.
    """

    def __init__(self, data_dir_fine, data_dir_coarse, data_dir_cons,
                 year_start=2000, year_end=2021,
                 output_size=(256, 320),
                 normalize_rawdata_mean=None,
                 normalize_rawdata_std=None,
                 normalize_residual_mean=None,
                 normalize_residual_std=None,
                 constant_variables=None,
                 constant_variables_filename="cerra_static_variables.nc"
                 ):
        print("Building file index...")

        self.index = []
        time_list = []

        for year in range(year_start, year_end):
            for month in range(1, 13):
                fp_tas = Path(data_dir_fine) / "tas" / f"CERRA_tas_daily_{year}_{month:02d}.nc"
                fp_pr  = Path(data_dir_fine) / "pr"  / f"CERRA_pr_daily_{year}_{month:02d}.nc"
                cp_tas = Path(data_dir_coarse) / f"CERRA_tas_daily_{year}_{month:02d}_coarsened.nc"
                cp_pr  = Path(data_dir_coarse) / f"CERRA_pr_daily_{year}_{month:02d}_coarsened.nc"
                if not fp_tas.exists() or not fp_pr.exists():
                    continue
                if not cp_tas.exists() or not cp_pr.exists():
                    print(f"  Warning: coarse files missing for {year}-{month:02d}, skipping.")
                    continue
                ds = xr.open_dataset(fp_tas, engine="netcdf4")
                n_t = ds.sizes["time"]
                times = ds.time.values
                ds.close()
                for t in range(n_t):
                    self.index.append((str(fp_tas), str(fp_pr), str(cp_tas), str(cp_pr), t))
                    time_list.append(times[t])

        self.ntime = len(self.index)
        self.time_index = time_list
        print(f"Total timesteps found: {self.ntime}")
        assert self.ntime > 0, "No matching file pairs found. Run coarsen_cerra.py first."

        self.H_fine, self.W_fine = output_size
        self.n_var = 2
        self.varnames = ["tas", "pr"]

        # Grid info from first fine tas file
        ds0 = xr.open_dataset(self.index[0][0], engine="netcdf4")
        # pixel counts of native CERRA grid (1749x2198) before cropping and resampling
        self.H_native = ds0.sizes["y"]
        self.W_native = ds0.sizes["x"]
        # determine latitude and longitude value for every pixel in the native CERRA grid (1749x2198), to be used for interpolation from coarse grid
        lat2d_native = torch.from_numpy(ds0["lat"].values).float()
        lon2d_native = torch.from_numpy(ds0["lon"].values).float()
        ds0.close()

        # Grid info from first coarse file
        ds0c = xr.open_dataset(self.index[0][2], engine="netcdf4")
        # pixel counts of coarse grid (77x91)
        self.H_coarse = ds0c.sizes["rlat"]
        self.W_coarse = ds0c.sizes["rlon"]
        # determine latitude and longitude value for every pixel in the native coarse grid (77x91), to be used for interpolation to native CERRA grid
        coarse_lat2d = ds0c["lat"].values
        coarse_lon2d = ds0c["lon"].values
        ds0c.close()
        # Store the native coarse lat/lon for potential use in plotting or other operations that require the original coarse grid coordinates.
        self.coarse_lat_raw = coarse_lat2d  # (H_coarse, W_coarse) native coarse grid
        self.coarse_lon_raw = coarse_lon2d

        # Delaunay interpolation weights, coarse -> fine: coarsened-CERRA (on MBCn's rlat/rlon
        # grid) -> native CERRA UTM grid. Training-only; real MBCn output at inference is
        # regridded separately (ProjectionPeriodAssessment.py, via scipy.griddata).
        print("Precomputing geographic interpolation weights (one-time)...")

        fine_lat2d = lat2d_native.numpy()
        fine_lon2d = lon2d_native.numpy()
        # Flatten the 2D lat/lon arrays into (H*W, 2) arrays of (lat, lon) points for Delaunay triangulation and interpolation
        coarse_pts = np.column_stack([coarse_lat2d.ravel(), coarse_lon2d.ravel()])
        fine_pts   = np.column_stack([fine_lat2d.ravel(),   fine_lon2d.ravel()])
        # Delaunay triangulation of the coarse grid points, and finding which triangle each fine point falls into. Points outside the convex hull of the coarse grid will have simplex=-1 and be marked as invalid.
        tri = Delaunay(coarse_pts)
        # find simplex index for each fine point
        simplex = tri.find_simplex(fine_pts)
        self._interp_valid = simplex >= 0
        # For valid points, compute the barycentric coordinates (weights -> b) for interpolation from the vertices of the corresponding triangle in the coarse grid.
        T = tri.transform[simplex[self._interp_valid]]
        # The transformation T has shape (n_valid, 3, 2), where T[:, :2, :2] is the linear transformation matrix for the triangle, and T[:, 2, :] is the translation vector.
        # The barycentric coordinates can be computed by applying the inverse of the linear transformation to the difference between the fine points and the translation vector.
        b = np.einsum('...ij,...j->...i',
                      T[:, :2, :2],
                      fine_pts[self._interp_valid] - T[:, 2, :])
        # weights per fine pixel for the 3 vertices of the corresponding triangle in the coarse grid.
        self._interp_weights = np.c_[b, 1 - b.sum(axis=-1)]
        # the indices of the 3 vertices in the coarse grid for each valid fine pixel
        self._interp_idx     = tri.simplices[simplex[self._interp_valid]]
        print("  Done.")

        # Bounding box of MBCn-covered region in native CERRA grid
        # valid pixels in 2D not as 1D vector anymore
        valid_2d = self._interp_valid.reshape(self.H_native, self.W_native)
        # for row check if any pixel in that row is valid
        rows = np.where(valid_2d.any(axis=1))[0]
        cols = np.where(valid_2d.any(axis=0))[0]
        # take first and last valid row/column to get bounding box of valid region
        self.row_slice = slice(int(rows[0]), int(rows[-1]) + 1)
        self.col_slice = slice(int(cols[0]), int(cols[-1]) + 1)
        # cut down lon/lat coordinate arrays down to bounding box
        lat2d_crop = lat2d_native[self.row_slice, self.col_slice]
        lon2d_crop = lon2d_native[self.row_slice, self.col_slice]
        # standardize coordinate grid to 256x320 for model input, to be used for plotting (500m -> 3km)
        self.lat_2d = torch.nn.functional.interpolate(
            lat2d_crop.unsqueeze(0).unsqueeze(0),
            size=(self.H_fine, self.W_fine), mode="bilinear", align_corners=True
        ).squeeze().numpy()
        self.lon_2d = torch.nn.functional.interpolate(
            lon2d_crop.unsqueeze(0).unsqueeze(0),
            size=(self.H_fine, self.W_fine), mode="bilinear", align_corners=True
        ).squeeze().numpy()

        # Time embeddings
        # convert time_list to pandas DatetimeIndex for easy extraction of month/day/hour features,
        times_pd = pd.DatetimeIndex(np.array(time_list, dtype="datetime64[ns]"))
        # treat month/day as 30-day months for simplicity, and convert to normalized [0, 1] range
        # keep in sync with doy_hour_label() above, which inference uses to rebuild this
        doy = (times_pd.month - 1.) * 30 + (times_pd.day - 1.)
        self.doy_norm  = torch.from_numpy(doy.to_numpy() / 360.).float()
        # normalize hour and year also to [0, 1] range
        self.hour_norm = torch.from_numpy(times_pd.hour.to_numpy() / 24.).float()
        self.year_norm = torch.from_numpy((times_pd.year.to_numpy() - 2000.) / 100).float()

        # Normalization stats — shape (n_var,) tensors, one value per variable
        self._norm_raw_mean = normalize_rawdata_mean
        self._norm_raw_std  = normalize_rawdata_std
        self._norm_res_mean = normalize_residual_mean
        self._norm_res_std  = normalize_residual_std

        if any(x is None for x in [normalize_rawdata_mean, normalize_rawdata_std,
                                    normalize_residual_mean, normalize_residual_std]):
            print("No normalization stats provided — computing from data sample...")
            self._compute_normalization_stats()

        # Constant variables (land-sea mask, topography)
        self.const_tensor = None
        if constant_variables is not None:
            print("Opening constant variables file (land-sea mask, topography)...")
            ds_const = xr.open_dataset(
                Path(data_dir_cons) / constant_variables_filename, engine="netcdf4")

            # CERRA static format: valid_time dim + 2D lat/lon coords
            # ERA5 format: time dim + 1D lat/lon dims
            # check whether the format is "valid_time" dim or "time" dim to determine how to read the data and crop to the fine CERRA domain
            is_cerra_format = "valid_time" in ds_const.dims
            if is_cerra_format:
                # remove time dimension
                ds_const = ds_const.squeeze("valid_time")
                # remove dummy time dimension
                const_lat = ds_const.latitude.values   # (H, W) 2D
                const_lon = ds_const.longitude.values  # (H, W) 2D
                # Crop to bounding box of the fine CERRA domain
                # takes lat/lon values at the corners of the native CERRA grid, and add a small buffer
                lat_min = float(lat2d_native.min()) - 0.5
                lat_max = float(lat2d_native.max()) + 0.5
                lon_min = float(lon2d_native.min()) - 0.5
                lon_max = float(lon2d_native.max()) + 0.5
                # True for pixels in the constant variable grid that fall within the bounding box of the fine CERRA domain
                in_region = ((const_lat >= lat_min) & (const_lat <= lat_max) &
                             (const_lon >= lon_min) & (const_lon <= lon_max))
                # find the bounding box of the valid region in the constant variable grid, and crop to that region for efficiency
                valid_rows = np.where(in_region.any(axis=1))[0]
                valid_cols = np.where(in_region.any(axis=0))[0]
                row_s = slice(int(valid_rows[0]), int(valid_rows[-1]) + 1)
                col_s = slice(int(valid_cols[0]), int(valid_cols[-1]) + 1)
                # application of crop - latitude values in the cropped region, to be used for latitude-based normalization of constant variables
                # latitude correction necessary because earths curvature causes the pixel size to vary with latitude, which affects the normalization of variables like topography that are sensitive to pixel area.
                # latitude weighting makes normalisation more accurate by accounting for the varying pixel area (pixels at lower latitude count more because they represent a larger patch of the Earth)
                crop_lat = const_lat[row_s, col_s]
                const_list = []
                for varname in constant_variables:
                    # loads variables and crops variable to relevant region
                    cv_np = ds_const[varname].values[row_s, col_s].astype(np.float32)
                    if varname != "lsm":
                        # does normalization for orography
                        print(f"  Normalize {varname}")
                        # compute weight per pixel, while giving higher weight to lower latitude
                        weights = np.cos(np.radians(crop_lat))
                        # sum of all weights as denominator for weighted average
                        wsum = weights.sum()
                        # latitude-weighted mean and std for normalization
                        wmean = float((cv_np * weights).sum() / wsum)
                        wstd = float(np.sqrt(((cv_np - wmean) ** 2 * weights).sum() / wsum))
                        # normalization
                        cv_np = (cv_np - wmean) / wstd
                    const_list.append(torch.from_numpy(cv_np).float())
            else:
                # ERA5 format; we do not do it here
                ds_const = ds_const.sel(
                    latitude=slice(48.0, 44.0), longitude=slice(5.0, 15.0)).squeeze("time")
                const_list = []
                for varname in constant_variables:
                    cv = ds_const[varname]
                    if varname != "lsm":
                        print(f"  Normalize {varname}")
                        wv = cv.weighted(np.cos(np.radians(ds_const.latitude)))
                        cv = (cv - wv.mean()) / wv.std()
                    const_list.append(torch.from_numpy(cv.to_numpy()).float())

            # stack tensors so we get (n_const, H, W) and add batch dimension for interpolation, then remove batch dimension after interpolation
            const_stack = torch.stack(const_list, dim=0).unsqueeze(0)
            # interpolate constant variables to fine CERRA grid (256x320 pixel), using bilinear interpolation
            self.const_tensor = torch.nn.functional.interpolate(
                const_stack, size=(self.H_fine, self.W_fine), mode="bilinear", align_corners=True
            ).squeeze(0)  # (n_const, H, W)

        print(f"Dataset initialized: {self.ntime} timesteps | "
              f"fine {self.H_fine}x{self.W_fine} | coarse {self.H_coarse}x{self.W_coarse}")

    def _compute_normalization_stats(self, n_samples=200):
        # we sample 200 instances
        indices = np.random.choice(self.ntime, size=min(n_samples, self.ntime), replace=False)

        raw_means = [[] for _ in range(self.n_var)]
        raw_stds  = [[] for _ in range(self.n_var)]
        res_means = [[] for _ in range(self.n_var)]
        res_stds  = [[] for _ in range(self.n_var)]

        for i, idx in enumerate(indices):
            if i % 50 == 0:
                print(f"  Stats: {i}/{len(indices)}")
            # loads fine data and coarse data after Delaunay inteprolation
            fine_np, coarse_np, _ = self._load_pair(int(idx))
            # residual_np -- what the model should actually learn
            residual_np = fine_np - coarse_np
            # calculates mean and std for each variable
            for v in range(self.n_var):
                # compute mean and std for raw coarse and residual, ignoring NaNs (which can occur due to interpolation outside convex hull)
                raw_means[v].append(np.nanmean(coarse_np[v]))
                raw_stds[v].append(np.nanstd(coarse_np[v]))
                res_means[v].append(np.nanmean(residual_np[v]))
                res_stds[v].append(np.nanstd(residual_np[v]))
        # safety net when the normalization stats are not known yet
        if self._norm_raw_mean is None:
            self._norm_raw_mean = torch.tensor([float(np.mean(raw_means[v])) for v in range(self.n_var)])
        if self._norm_raw_std is None:
            self._norm_raw_std  = torch.tensor([float(np.mean(raw_stds[v]))  for v in range(self.n_var)])
        if self._norm_res_mean is None:
            self._norm_res_mean = torch.tensor([float(np.mean(res_means[v])) for v in range(self.n_var)])
        if self._norm_res_std is None:
            self._norm_res_std  = torch.tensor([float(np.mean(res_stds[v]))  for v in range(self.n_var)])

        for v, name in enumerate(self.varnames):
            print(f"  {name} raw:      mean={self._norm_raw_mean[v].item():.4f}, std={self._norm_raw_std[v].item():.4f}")
            print(f"  {name} residual: mean={self._norm_res_mean[v].item():.4f}, std={self._norm_res_std[v].item():.4f}")

    def _interp_coarse(self, coarse_np):
        """Interpolate (77, 91) coarse array to native CERRA grid via barycentric weights."""
        # create a H*W 1D object filled with NaNs
        result = np.full(self._interp_valid.shape[0], np.nan, dtype=np.float32)
        # calculates interpolated data points and fills them into the empty array
        result[self._interp_valid] = np.einsum(
            'ni,ni->n', coarse_np.ravel()[self._interp_idx], self._interp_weights)
        # reshape back from 1D to 2D
        return result.reshape(self.H_native, self.W_native)

    def _load_pair(self, index):
        """Load fine and coarse for both variables, resampled to output_size."""
        # determines path to every relevant file based on the index
        fp_tas, fp_pr, cp_tas, cp_pr, t = self.index[index]
        # opens file based on path
        ds_ft = xr.open_dataset(fp_tas, engine="netcdf4")
        ds_fp = xr.open_dataset(fp_pr,  engine="netcdf4")
        ds_ct = xr.open_dataset(cp_tas, engine="netcdf4")
        ds_cp = xr.open_dataset(cp_pr,  engine="netcdf4")
        # filter time to take out time position; extracts underlying NumPy array (no metadata anymore); casts to 32-bit float
        fine_tas_np   = ds_ft["tas"].isel(time=t).values.astype(np.float32)
        fine_pr_np    = ds_fp["pr"].isel(time=t).values.astype(np.float32)
        coarse_tas_np = ds_ct["tas"].isel(time=t).values.astype(np.float32)
        coarse_pr_np  = ds_cp["pr"].isel(time=t).values.astype(np.float32)
        # closes files after data is moved to memory
        ds_ft.close(); ds_fp.close(); ds_ct.close(); ds_cp.close()
        # stacks coarse data together
        raw_coarse_np = np.stack([coarse_tas_np, coarse_pr_np], axis=0)  # (2, H_coarse, W_coarse)

        # Interpolate coarse to native CERRA grid (3km)
        coarse_native_tas = self._interp_coarse(coarse_tas_np)
        coarse_native_pr  = self._interp_coarse(coarse_pr_np)

        # Crop to MBCn bounding box
        fine_tas_np       = fine_tas_np[self.row_slice, self.col_slice]
        fine_pr_np        = fine_pr_np[self.row_slice, self.col_slice]
        coarse_native_tas = coarse_native_tas[self.row_slice, self.col_slice]
        coarse_native_pr  = coarse_native_pr[self.row_slice, self.col_slice]
        # downsampling of fine target 500m to 3km
        def resize_to_output(arr):
            return torch.nn.functional.interpolate(
                torch.from_numpy(arr).unsqueeze(0).unsqueeze(0),
                size=(self.H_fine, self.W_fine), mode="bilinear", align_corners=True
            ).squeeze().numpy()
        # stack fine data together
        fine_np   = np.stack([resize_to_output(fine_tas_np),      resize_to_output(fine_pr_np)],      axis=0)
        coarse_np = np.stack([resize_to_output(coarse_native_tas), resize_to_output(coarse_native_pr)], axis=0)
        # so now stack very coarse, the coarse interpolated and the fine one
        return fine_np, coarse_np, raw_coarse_np  # (2,H,W), (2,H,W), (2,H_coarse,W_coarse)

    def __len__(self):
        # tell DataLoader how many samples are in the dataset
        return self.ntime

    def __getitem__(self, index):
        # load raw data arrays
        fine_np, coarse_np, raw_coarse_np = self._load_pair(index)
        # convert from numpy to torch
        fine          = torch.from_numpy(fine_np).float()        # (2, H, W)
        coarse_interp = torch.from_numpy(coarse_np).float()      # (2, H, W)
        coarse_raw    = torch.from_numpy(raw_coarse_np).float()  # (2, H_coarse, W_coarse)
        # instead of predicting the fine field directly, the difference between fine and coarse is predicted
        residual      = fine - coarse_interp                # (2, H, W)
        # change shape so we can later on normalize the climate field
        raw_mean = self._norm_raw_mean.view(self.n_var, 1, 1)
        raw_std  = self._norm_raw_std.view(self.n_var, 1, 1)
        res_mean = self._norm_res_mean.view(self.n_var, 1, 1)
        res_std  = self._norm_res_std.view(self.n_var, 1, 1)
        # normalization of the coarse input (bilinearly interpolated)
        coarse_norm   = torch.nan_to_num((coarse_interp - raw_mean) / raw_std, nan=0.0)
        # normalization of the residual climate field
        residual_norm = (residual - res_mean) / res_std
        # stack static fields to the normalized bil. coarse field
        if self.const_tensor is not None:
            inputs = torch.cat([coarse_norm, self.const_tensor], dim=0)  # (n_var + n_const, H, W)
        else:
            inputs = coarse_norm
        # package everything the training loop might need
        return {
            "inputs":     inputs,                       # normalized - conditioning signal
            "targets":    residual_norm,                # normalized - what the model learns to generate
            "fine":       fine,                         # not normalized
            "coarse":     coarse_interp,                # no
            "coarse_raw": coarse_raw,                   # no
            "year":       self.year_norm[index],
            "doy":        self.doy_norm[index],
            "hour":       self.hour_norm[index],
        }

    def inverse_normalize_residual(self, r):
        """Unnormalize residual. Handles both (n_var, H, W) and (B, n_var, H, W)."""
        # workflow to handle a leading dimension before n_var, H, W
        n_lead = r.dim() - 3
        shape = (1,) * n_lead + (self.n_var, 1, 1)
        # take stored 1D tensor and reshape => move them to the same location as the input tensor r
        std  = self._norm_res_std.reshape(shape).to(r.device)
        mean = self._norm_res_mean.reshape(shape).to(r.device)
        # return to original scaling
        return r * std + mean

    def residual_to_fine_image(self, residual, coarse_image):
        # reconstruct the fine field from the models prediction
        return coarse_image + self.inverse_normalize_residual(residual)

    # draw one field on the fine (256x320) lat/lon grid onto a Cartopy axis
    def plot_fine(self, image_fine, ax, vmin=None, vmax=None):
        plt.sca(ax)
        lon = self.lon_2d
        lat = self.lat_2d
        ax.set_extent([float(lon.min()), float(lon.max()),
                       float(lat.min()), float(lat.max())], crs=ccrs.PlateCarree())
        ax.coastlines()
        plt.pcolormesh(lon, lat, image_fine, vmin=vmin, vmax=vmax,
                       shading='nearest', transform=ccrs.PlateCarree())

    # per-epoch QA figure: raw coarse | interp'd coarse | prediction | truth, N samples x 2 vars
    def plot_batch(self, coarse_raw, coarse_image, fine_image, fine_image_pred, N=3):
        fig, axs = plt.subplots(self.n_var * N, 4, figsize=(16, self.n_var * N * 2),
                                subplot_kw={'projection': ccrs.PlateCarree()})
        for j in range(N):
            for i in range(self.n_var):
                row = j * self.n_var + i
                vmin = float(np.nanmin(fine_image[:N, i].numpy()))
                vmax = float(np.nanmax(fine_image[:N, i].numpy()))
                # Raw coarse on native coarse grid
                ax = axs[row, 0]
                ax.set_extent([float(self.coarse_lon_raw.min()), float(self.coarse_lon_raw.max()),
                               float(self.coarse_lat_raw.min()), float(self.coarse_lat_raw.max())],
                              crs=ccrs.PlateCarree())
                ax.coastlines()
                ax.pcolormesh(self.coarse_lon_raw, self.coarse_lat_raw, coarse_raw[j, i].numpy(),
                              vmin=vmin, vmax=vmax, shading='nearest', transform=ccrs.PlateCarree())
                self.plot_fine(coarse_image[j, i],    axs[row, 1], vmin=vmin, vmax=vmax)
                self.plot_fine(fine_image_pred[j, i], axs[row, 2], vmin=vmin, vmax=vmax)
                self.plot_fine(fine_image[j, i],      axs[row, 3], vmin=vmin, vmax=vmax)
        plt.tight_layout()
        return fig, axs
