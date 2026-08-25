import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
sys.stdout.reconfigure(encoding="utf-8")
sys.stderr.reconfigure(encoding="utf-8")

import numpy as np  # noqa: E402
import xarray as xr  # noqa: E402
from utils.paths import processed_data_dir, raw_data_dir, require_raw_dir  # noqa: E402

DATA_RAW = raw_data_dir()
DATA_PROCESSED = processed_data_dir()

# Internal NetCDF variable names differ from the concept names used here
_AL_VARS: dict[str, tuple[Path, str]] = {
    "wb":   (DATA_RAW / "al/wb/wb_al_reanalysis_1971_2025.nc",     "wb"),
    "pr":   (DATA_RAW / "al/pr/pr_al_reanalysis_1971_2025.nc",     "tp"),
    "ps":   (DATA_RAW / "al/ps/ps_al_reanalysis_1971_2025.nc",     "sp"),
    "spei": (DATA_RAW / "al/spei/spei_al_reanalysis_1971_2025.nc", "spei"),
    "tas":  (DATA_RAW / "al/tas/tas_al_reanalysis_1971_2025.nc",   "t2m"),
}


def _derive_mask(ds: xr.Dataset) -> xr.DataArray:
    """Derive the valid-cell mask from the NaN pattern of the first time step."""
    ref_var = next(iter(ds.data_vars))
    frame = ds[ref_var].isel(time=0)
    return (~np.isnan(frame)).astype(np.int8).rename("mask")


def load_dynamic_grid() -> xr.Dataset:
    """
    Load all AL gridded variables, fill NaN → 0, and merge.

    NaN cells (outside the Alps domain) are filled with 0. The companion
    mask in static_grid.nc identifies which cells are valid.

    Returns:
        Dataset with variables: wb, pr, ps, spei, tas  –  dims: (time, rlat=43, rlon=98)
    """
    arrays: list[xr.DataArray] = []
    for concept_name, (path, nc_varname) in _AL_VARS.items():
        ds = xr.open_dataset(path)
        da = ds[nc_varname]
        if da.dims[0] != "time":
            da = da.transpose("time", ...)
        da = da.fillna(0.0)
        arrays.append(da.rename(concept_name))

    return xr.merge(arrays, compat="override")


def load_static_grid() -> xr.Dataset:
    """
    Crop the EUR-11 topography to the AL domain and derive the valid-cell mask.

    The mask (int8, 1=valid 0=outside) is saved alongside topography so the
    model can restrict loss and metric computation to real Alpine cells.

    Returns:
        Dataset with variables: orogf, mask  –  dims: (rlat=43, rlon=98)
    """
    topo = xr.open_dataset(
        DATA_RAW / "topography/topo_EUR-11_CCCma-CanESM2_OURANOS-CRCM5.nc"
    )
    al_ref = xr.open_dataset(DATA_RAW / "al/tas/tas_al_reanalysis_1971_2025.nc")
    orogf = topo["orogf"].sel(rlat=al_ref.rlat, rlon=al_ref.rlon, method="nearest")
    mask  = _derive_mask(al_ref[["t2m"]])

    orogf = orogf.fillna(0.0)
    return xr.merge([orogf.to_dataset(name="orogf"), mask.to_dataset(name="mask")],
                    compat="override")


def load_global_scalars(time_ref: xr.DataArray) -> xr.Dataset:
    """
    Load NAO PC1 index, Mediterranean SST basin averages, and cyclical month
    encoding, trimmed to the AL time axis (1971-01 – 2024-12).

    month_sin / month_cos encode the calendar month of each timestep.  When
    accessed via ScalarSubset at last_t (the final input step), they represent
    the current month — not the target month.

    Args:
        time_ref: Time coordinate of the dynamic grid.

    Returns:
        Dataset with variables: nao_index + 14 Med basin columns + month_sin +
        month_cos  –  dim: (time=648,)
    """
    nao = xr.open_dataset(DATA_RAW / "atlantic/nao/psl_nao_pc1_reanalysis_1970_2025.nc")
    med = xr.open_dataset(DATA_RAW / "mediteranean/slt/slt_avg_mediteranean_reanalysis_1970_2025.nc")

    nao = nao.rename({"pcs": "nao_index"})

    months = time_ref.values.astype("datetime64[M]").astype("int64") % 12 + 1
    month_ds = xr.Dataset(
        {
            "month_sin": xr.DataArray(
                np.sin(2 * np.pi * months / 12).astype(np.float32),
                coords={"time": time_ref},
                dims=["time"],
            ),
            "month_cos": xr.DataArray(
                np.cos(2 * np.pi * months / 12).astype(np.float32),
                coords={"time": time_ref},
                dims=["time"],
            ),
        }
    )

    scalars = xr.merge([nao, med, month_ds], compat="override", join="outer")
    t_start, t_end = time_ref.values[0], time_ref.values[-1]
    return scalars.sel(time=slice(t_start, t_end))


def compute_normalization_stats(val_from_year: int = 2005) -> None:
    """
    Compute per-variable mean and std over the training period.

    Dynamic grid variables: statistics over valid Alpine cells only (mask==1).
    Global scalar variables: statistics over all training timesteps (1-D series).

    All statistics use only time steps before val_from_year to avoid leakage.
    Saves data/processed/normalization.json.
    """
    ds   = xr.open_dataset(DATA_PROCESSED / "dynamic_grid.nc")
    mask = xr.open_dataset(DATA_PROCESSED / "static_grid.nc")["mask"].values.astype(bool)

    times = ds["time"].values
    years = times.astype("datetime64[Y]").astype(int) + 1970
    is_train = years < val_from_year

    stats: dict = {}
    for var in ds.data_vars:
        arr = ds[var].transpose("time", "rlat", "rlon").values  # (T, H, W)
        train_valid = arr[is_train][:, mask]                    # (n_train, n_valid)
        stats[var] = {
            "mean": float(train_valid.mean()),
            "std":  float(train_valid.std()),
        }
    ds.close()

    glob_path = DATA_PROCESSED / "global_scalars.nc"
    if glob_path.exists():
        ds_glob = xr.open_dataset(glob_path)
        g_times = ds_glob["time"].values
        g_years = g_times.astype("datetime64[Y]").astype(int) + 1970
        is_train_g = g_years < val_from_year
        glob_stats: dict[str, dict[str, float]] = {}
        for var in ds_glob.data_vars:
            arr = ds_glob[var].values[is_train_g].astype(float)
            glob_stats[var] = {
                "mean": float(arr.mean()),
                "std":  float(max(arr.std(), 1e-8)),
            }
        stats["_global"] = glob_stats
        ds_glob.close()

    out = DATA_PROCESSED / "normalization.json"
    with open(out, "w") as f:
        json.dump(stats, f, indent=2)

    print(f"  -> {out.name}  (training period: years < {val_from_year})")
    for var, s in stats.items():
        if var == "_global":
            for gvar, gs in s.items():
                print(f"     {gvar:25s}  mean={gs['mean']:8.3f}  std={gs['std']:.3f}")
        else:
            print(f"     {var:6s}  mean={s['mean']:10.3f}  std={s['std']:.3f}")


def run() -> None:
    require_raw_dir(DATA_RAW)
    DATA_PROCESSED.mkdir(parents=True, exist_ok=True)

    print("Loading dynamic grid (wb, pr, ps, spei, tas) ...")
    dynamic = load_dynamic_grid()
    out = DATA_PROCESSED / "dynamic_grid.nc"
    dynamic.to_netcdf(out)
    print(f"  -> {out.name}   dims: {dict(dynamic.sizes)}")

    print("Loading static grid (topography + mask) ...")
    static = load_static_grid()
    out = DATA_PROCESSED / "static_grid.nc"
    static.to_netcdf(out)
    print(f"  -> {out.name}   dims: {dict(static.sizes)}")
    valid_cells = int(static["mask"].values.sum())
    total_cells = static["mask"].values.size
    print(f"     valid cells: {valid_cells}/{total_cells} ({100*valid_cells/total_cells:.1f}%)")

    print("Loading global scalars (NAO PC1, Mediterranean SST) ...")
    scalars = load_global_scalars(dynamic["time"])
    out = DATA_PROCESSED / "global_scalars.nc"
    scalars.to_netcdf(out)
    print(f"  -> {out.name}   dims: {dict(scalars.sizes)}   variables: {list(scalars.data_vars)}")

    print("Computing normalization statistics (training period only) ...")
    compute_normalization_stats()


if __name__ == "__main__":
    run()