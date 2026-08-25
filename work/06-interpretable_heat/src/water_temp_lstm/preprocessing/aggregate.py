import pandas as pd

def daily_average(df: pd.DataFrame) -> pd.DataFrame:
    """
    Aggregates hourly/ sub-daily observations into daily values.

    Mean is used for state-like variables (temperature, wind, humidity, discharge), 
    sum is used for accumulation-like variables (precipitation, radiation).
    min_count=1 ensures that a day with no valid observations produces NaN instead of 0.0 
    for the summed columns (pandas' default behavior for sum() over an empty/all-NaN group is 0.0, not NaN).

    Parameters
    ----------
    df : pd.DataFrame
        DataFrame with a DatetimeIndex at sub-daily frequency 
        (day gaps are filled with NaN rows by resample("D")) and
        columns wt, Ta_C, P_mm, wind_ms, rad_whm2, relhum, Q.

    Returns
    -------
    pd.DataFrame
        DataFrame with a daily DatetimeIndex, resampled to daily frequency ("D"), 
        with one row per calendar day across the full date range 
        (missing days included as NaN rows)
    """
    return df.resample("D").agg(
        {
            "wt": "mean",
            "Ta_C": "mean",
            "P_mm": lambda x: x.sum(min_count = 1),
            "wind_ms": "mean",
            "rad_whm2": lambda x: x.sum(min_count = 1),
            "relhum": "mean",
            "Q": "mean"
        }
    )