from pathlib import Path
import torch
import copy
from neuralhydrology.utils.config import Config
from neuralhydrology.evaluation import get_tester
from neuralhydrology.datasetzoo import get_dataset
from torch.utils.data import DataLoader, ConcatDataset

def load_cfg(run_dir):
    return Config(Path(run_dir) / "config.yml")

def load_model(cfg, run_dir, epoch=None):
    tester = get_tester(cfg=cfg, run_dir=Path(run_dir), period="test", init_model=True)
    if epoch is not None:
        weight_file = Path(run_dir) / f"model_epoch{epoch:03d}.pt"
        tester.model.load_state_dict(torch.load(weight_file, map_location="cpu"))
    tester.model.eval()
    return tester

def load_test_datasets(cfg, tester):
    return {
        basin: get_dataset(cfg=cfg, is_train=False, period="test", basin=basin, scaler=tester.scaler)
        for basin in tester.basins
    }

def build_loader(datasets, batch_size):
    combined = ConcatDataset(list(datasets.values()))
    first_ds = list(datasets.values())[0]
    return DataLoader(combined, batch_size=batch_size, shuffle=False, collate_fn=first_ds.collate_fn)

def clone_batch(batch):
    return {
        "x_d": {k: v.clone() for k, v in batch["x_d"].items()},
        "x_s": batch["x_s"].clone(),
        "y": batch["y"].clone(),
        "date": copy.deepcopy(batch["date"]),
    }