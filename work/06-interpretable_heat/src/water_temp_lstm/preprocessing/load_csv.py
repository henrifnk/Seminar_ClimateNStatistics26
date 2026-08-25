import pandas as pd

def load_station_csv(path: str) -> pd.DataFrame:
    """
    Reads the station CSV and builds a DatetimeIndex from the
    year/month/day/hour columns.

    Parameters
    ----------
    path: str
        Path to the CSV file with columns year, month, day, hour, wt, ...
    
    Returns
    -------
    pd.DataFrame
        DataFrame with a DatetimeIndex named 'date'
    """
    # Read in date columns as nullable integer type, 
    # since some rows may have NaN in these columns
    # (a plain int dtype would fail here)
    df = pd.read_csv(path)

    df["date"] = pd.to_datetime(
        {
            "year": df.year,
            "month": df.month,
            "day": df.day,
            "hour": df.hour
        },
        errors = "coerce"
    )

    df = df.set_index("date")

    return  df

