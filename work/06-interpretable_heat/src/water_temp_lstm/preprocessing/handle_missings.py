import pandas as pd
import numpy as np

def determine_missings(df: pd.DataFrame, 
                       expected_hours: set = {0, 3, 6, 9, 12, 15, 18, 21}) -> pd.DataFrame:
    """
    Marks all days as completly missing (NaN) that do not contain 
    exactly the expected hours.

    Rows with an hour value not in expected_hours (or with NaT/NaN timestamps)
    are dropped entirely. Among the remaining rows, days that contain all
    expected_hours are left untouched; days that are missing one or more 
    expected hours have their value columns set to NaN for all of that day's rows
    (the rows themselves are kept, not dropped)

    Parameters
    ----------
    df : pd.DataFrame
        DataFrame with a DatetimeIndex and an 'hour' column.
    expected_hours : set, default {0, 3, 6, 9, 12, 15, 18, 21}
        The set of hours that must all be present for a day 
        to be considered complete.

    Returns
    -------
    pd.DataFrame
        DataFrame with rows of unexpected/ missing hours dropped, 
        and value columns set to NaN for every remaining day 
        that did not contain exactly expected_hours.
    """
    if not isinstance(df.index, pd.DatetimeIndex):
        raise TypeError("df must have a DatetimeIndex")
    
    df = df.copy()

    # Only keep expected hours; 
    # this also drops rows with NaT/NaN hour
    df = df[df["hour"].isin(expected_hours)]

    # Check on a daily basis if all expected hours are available
    day_index = df.index.normalize()
    hours_per_day = df.groupby(day_index)["hour"].apply(set)
    complete_days = hours_per_day[hours_per_day == expected_hours].index
    
    n_total = len(hours_per_day)
    n_complete = len(complete_days)
    n_incomplete = n_total - n_complete

    print(f"Complete Days: {n_complete} / {n_total}",
          f"{n_incomplete} incomplete days are set to NaN")


    meta_cols = {"year", "month", "day", "hour"}
    value_cols = [c for c in df.columns if c not in meta_cols]

    incomplete_mask = ~day_index.isin(complete_days)
    df.loc[incomplete_mask, value_cols] = np.nan

    return df