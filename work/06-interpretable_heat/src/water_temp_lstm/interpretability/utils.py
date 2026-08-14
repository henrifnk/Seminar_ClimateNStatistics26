import pandas as pd

# Scaler-Statistiken für Rück-/ Vortransformation
def get_feature_stats(tester, feature):
    """Mean und Std eines Features aus dem NeuralHydrology-Scaler"""
    mean = float(tester.scaler["xarray_feature_center"][feature].values)
    std = float(tester.scaler["xarray_feature_scale"][feature].values)
    return mean, std


# Datumsinfo pro Sample sammeln (für saisonale Aufspaltung)
def collect_target_dates(loader):
    """
    Letztes Datum jedes Samples (= Zieldatum der Prediction).
    """
    all_dates = []
    for batch in loader:
        # batch["date"].shape -> [n_samples, seq_length]
        for d in batch["date"]:
            all_dates.append(pd.to_datetime(d[-1] if hasattr(d, "__len__") else d))
    return pd.to_datetime(all_dates)