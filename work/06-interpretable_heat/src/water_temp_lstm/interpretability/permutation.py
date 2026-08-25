import numpy as np
import torch
from water_temp_lstm.interpretability.metrics import nse

def permute_feature_temporally(batch, feature_name, seed=None):
    """
    Permutiert ein dynamisches Feature zeitlich, innerhalb jedes Samples separat.
    (Ein Sample ist eine 365 Tage Zeitreihe (mit letzem Tag irgendwann im Testzeitraum) für ein Feature an einer Basin)
    (bzw. eine Sammlung an Zeitreihen (eine pro Feature))
    """

    batch_copy = {
        "x_d": {k: v.clone() for k, v in batch["x_d"].items()},
        "x_s": batch["x_s"].clone(),
        "y": batch["y"],
        "date": batch["date"],
    }

    rng = np.random.default_rng(seed)
    feature_tensor = batch_copy["x_d"][feature_name] # Shape: [n_samples, seq_length, 1]
    n_samples = feature_tensor.shape[0]
    seq_length = feature_tensor.shape[1]


    # Für jedes Sample eine eigene, zufällige Zeit-Permutation anwenden
    for i in range(n_samples):
        perm_idx = rng.permutation(seq_length)
        feature_tensor[i, :, :] = feature_tensor[i, perm_idx, :]

    batch_copy["x_d"][feature_name] = feature_tensor
    return batch_copy



def permute_static_feature(batch, feature_idx, seed=None):
    batch_copy = {
        "x_d": batch["x_d"],
        "x_s": batch["x_s"].clone(),
        "y": batch["y"],
        "date": batch["date"],
    }

    rng = np.random.default_rng(seed)
    n_samples = batch_copy["x_s"].shape[0]
    perm_idx = rng.permutation(n_samples)
    batch_copy["x_s"][:, feature_idx] = batch_copy["x_s"][perm_idx, feature_idx]
    return batch_copy


def compute_importance(feature_names, permute_fn, loader, model, baseline_nse, observations_last, n_repeats=5):
    """Permutiert jedes Feature n_repeats Mal und berechnet für jede Permutation den Rückgang des NSE Scores gegenüber der Baseline"""
    importances = {feature: [] for feature in feature_names}
    for feature in feature_names:
        for repeat in range(n_repeats):
            preds_all = []
            for batch in loader:
                permuted_batch = permute_fn(batch, feature, seed=repeat)
                with torch.no_grad():
                    output = model(permuted_batch)
                preds_all.append(output["y_hat"][:, -1, :])
            preds_last = torch.cat(preds_all, dim=0)
            score = nse(observations_last, preds_last)
            importances[feature].append(baseline_nse - score)
    return importances

