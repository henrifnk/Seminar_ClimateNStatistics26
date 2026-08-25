# -*- coding: utf-8 -*-
"""
run_pretrain_ea.py — Train the small "global" base model using EA-LSTM.

Usage:
    python run_pretrain_ea.py
"""

from pathlib import Path

from neuralhydrology.nh_run import start_run

HERE = Path(__file__).resolve().parent


def main() -> None:
    cfg = HERE / "pretrain_ea.yml"
    print(f"Starting pretraining with EA-LSTM: {cfg}")
    start_run(config_file=cfg)
    print("\nPretraining done. Base run stored under:", HERE / "runs")


if __name__ == "__main__":
    main()
