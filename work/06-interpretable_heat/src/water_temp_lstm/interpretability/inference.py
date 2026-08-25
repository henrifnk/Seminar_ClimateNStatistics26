import torch

def run_inference(loader, model):
    """
    Sammelt Predictions und Observations über die Batches.
    """

    preds, obs = [], []
    with torch.no_grad():
        for batch in loader:
            output = model(batch)
            preds.append(output["y_hat"])
            obs.append(batch["y"])

    return torch.cat(preds, dim=0), torch.cat(obs, dim=0)