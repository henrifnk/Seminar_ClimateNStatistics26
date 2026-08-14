from pathlib import Path

from water_temp_lstm.preprocessing.load_csv import load_station_csv
from water_temp_lstm.preprocessing.handle_missings import determine_missings
from water_temp_lstm.preprocessing.aggregate import daily_average
from water_temp_lstm.preprocessing.create_netcdf import save_netcdf
from water_temp_lstm.preprocessing.filter_static_features import filter_static_features

raw_dir = Path("data/raw")
out_dir_time_series = Path("data/processed/time_series")
out_dir_attributes = Path("data/processed/attributes")

path_static_features = raw_dir / "static_features.csv"


def process_time_series() -> set[str]:
    """
    Runs the full preprocessing pipeline for all station time series CSVs
    in raw_dir and writes one NetCDF file per station to out_dir_time_series.

    Returns
    -------
    set[str]
        Station IDs (derived form the CSV filenames) that were processed
    """

    out_dir_time_series.mkdir(parents=True, exist_ok=True)

    station_ids = set()

    csv_files = [f for f in raw_dir.glob("*.csv") if f != path_static_features]

    for csv_file in csv_files:
        station_id = csv_file.stem
        print(f"Preprocessing {csv_file.name}")

        df = load_station_csv(csv_file)
        df = determine_missings(df)
        df = daily_average(df)

        out_path = out_dir_time_series / f"{station_id}.nc"
        save_netcdf(df, path=str(out_path))

        station_ids.add(station_id)
    
    return station_ids


def process_static_features(station_ids: set[str]) -> None:
    """
    Filters the static station attributes down to the given station_ids
    and writes the results to out_dir_attributes with 'Station' as index.

    Parameters
    ----------
    station_ids : set[str]
        Station IDs to keep (typically the ones returnes by process_time_series()).
    """

    out_dir_attributes.mkdir(parents=True, exist_ok=True)

    df_static = filter_static_features(str(path_static_features), station_ids)

    df_static = df_static.set_index("Station")

    out_path = out_dir_attributes / "attributes.csv"

    df_static.to_csv(out_path)
    print(f"Saved filtered static features to {out_path}")


if __name__ == "__main__":
    station_ids = process_time_series()
    process_static_features(station_ids)
