import pandas as pd
from pathlib import Path

def filter_static_features(static_path: str, station_ids: set[str]) -> pd.DataFrame:
    """
    Loads the static station attributes and filters them down to the 
    stations that are actually present in the time series datasets.

    Parameters
    ----------
    static_path : str
        Path to the static_features.csv file. Must contain a 'Station'
        column whose values math the time series CSV filenames (without extension).
    station_ids : set[str]
        Set of station IDs (derived from the time series CSV filenames) to keep
    
        
    Returns
    -------
    pd.DataFrame
        Static features filtered to the given station_ids, in the same column 
        order as the input file.
    """

    df = pd.read_csv(static_path, sep=";")

    static_ids = set(df["Station"].astype(str))
    missing_in_static = station_ids - static_ids
    missing_in_timeseries = static_ids - station_ids

    if missing_in_static:
        print(f"Warning: {len(missing_in_static)} station(s) have time series "
              f"data but no static features: {sorted(missing_in_static)}")
    if missing_in_timeseries:
        print(f"Info: {len(missing_in_timeseries)} station(s) in {Path(static_path).stem}"
              f"have no time series data and will be dropped: "
              f"{sorted(missing_in_timeseries)}")
        
    return df[df["Station"].astype(str).isin(static_ids)]