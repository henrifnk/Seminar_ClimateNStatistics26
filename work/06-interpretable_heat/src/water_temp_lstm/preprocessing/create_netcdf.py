import pandas as pd
import xarray as xr
from pathlib import Path

def save_netcdf(df : pd.DataFrame, path : str) -> None:
    """
    Saves a pandas DataFrame with a DatetimeIndex as NetCDF file.

    The DataFrame's index becomes the NetCDF time dimension/ coordinate.
    NeuralHydrology requires the time coordinate to be named 'date'. 
    The DataFrame's index is therefore expected to be named 'date'.
    If the index has a different name, it is renamed to 'date' automatically.

    Parameters
    ----------
    df : pd.DataFrame
        DataFrame with a DatetimeIndex, ideally named 'date'
    path : str
        Output file path (e.g. "data/processed/river_station.nc")

    Returns
    -------
    None
        Writes the NetCDF file to disk at the given path.
    """

    if not isinstance(df.index, pd.DatetimeIndex):
        raise TypeError("df must have a DatetimeIndex")
    
    # Ensure chronological order
    df = df.sort_index()

    # NeuralHydrology expects the time coordinate to be named 'date'
    if df.index.name != "date":
        df = df.rename_axis("date")

    Path(path).parent.mkdir(parents = True, exist_ok = True)

    ds = df.to_xarray()
    ds.to_netcdf(path)
