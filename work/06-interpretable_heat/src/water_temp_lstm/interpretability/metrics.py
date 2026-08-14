import numpy as np

def nse(obs, sim, verbose=False):
    obs = obs.numpy().flatten()
    sim = sim.numpy().flatten()

    mask = ~np.isnan(obs) & ~np.isnan(sim)
    obs, sim = obs[mask], sim[mask]

    if verbose:
        print(f"NSE berechnet auf {mask.sum()} von {len(mask)} Samples ({100*mask.mean():.1f}%)")

    return 1 - np.sum((obs - sim)**2) / np.sum((obs - obs.mean())**2)