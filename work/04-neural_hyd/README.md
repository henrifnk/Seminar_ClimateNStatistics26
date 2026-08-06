# 04-neural_hyd: End-to-End Reproducible Guide

This README is the only documentation needed to reproduce the workflow in `work/04-neural_hyd`.

## 1. Reproducibility scope

This project reproduces the EA-LSTM workflow in three steps:

1. Training: global pretrain + group-wise local/finetune
2. Results: evaluation and CSV summary generation
3. Plotting: figure generation with matplotlib from CSVs

Expected outputs:

1. Model run folders/checkpoints under configured runs root
2. CSV tables in `work/04-neural_hyd/results/`
3. PNG figures in `work/04-neural_hyd/figures/`

## 2. CAMELS_US data source

Download CAMELS_US from official NCAR/UCAR sources:

1. https://ral.ucar.edu/solutions/products/camels
2. https://dx.doi.org/10.5065/D6MW2F4D

Reference:

1. https://neuralhydrology.readthedocs.io/en/latest/tutorials/data-prerequisites.html

## 3. Required data directory structure

```text
work/04-neural_hyd/data/
  CAMELS_US/
    basin_mean_forcing/
      daymet/
        01/
          01013500_lump_cida_forcing_leap.txt
          ...
        02/
        ...
        18/
    usgs_streamflow/
      01/
        01013500_streamflow_qc.txt
        ...
      02/
      ...
      18/
    camels_attributes_v2.0/
      camels_clim.txt
      camels_geol.txt
      camels_hydro.txt
      camels_name.txt
      camels_soil.txt
      camels_topo.txt
      camels_vege.txt
```

Notes:

1. Forcing files: `*_lump_cida_forcing_leap.txt`
2. Streamflow files: `*_streamflow_qc.txt`
3. Group folders `01` to `18` are required by the group experiments

## 4. Path dependency (important)

Core scripts currently use absolute paths under `C:/GitHub/climate-change`, for example:

1. `C:/GitHub/climate-change/runs`
2. `C:/GitHub/climate-change/NeuralHydrology/rq3_finetune`
3. `C:/GitHub/climate-change/NeuralHydrology/data/CAMELS_US`

Before running, choose one mode:

1. Keep this default layout and ensure those paths exist.
2. Modify path constants in core scripts and `.yml` configs to your local layout.

If paths are not aligned, training and evaluation may read/write in unexpected locations.

## 5. Code structure

```text
work/04-neural_hyd/code/
  train.py
  results.py
  plot.py
  90_core/rq3_finetune_full/
    run_pretrain_ea.py
    run_folder_group_compare_ea.py
    eval_global_true_by_group.py
    gen_three_way_comparison.py
    *.yml
    basin_lists_by_folder/
```

## 6. Prerequisites

Use a Python environment with NeuralHydrology and plotting dependencies:

1. `neuralhydrology`
2. `pandas`
3. `numpy`
4. `matplotlib`

## 7. How to run

Run from `work/04-neural_hyd/code`:

```powershell
python .\train.py
python .\results.py
python .\plot.py
```

## 8. Stage details

### 8.1 Training (`train.py`)

Runs:

1. `run_pretrain_ea.py`
2. `run_folder_group_compare_ea.py`

Typical run folder prefixes:

1. `rq3_base_global_ea_*`
2. `rq3_scratch_folder_XX_ea_*`
3. `rq3_finetuned_folder_XX_ea_*`

### 8.2 Results (`results.py`)

Runs:

1. `eval_global_true_by_group.py`
2. `gen_three_way_comparison.py`
3. Copies selected CSVs into `work/04-neural_hyd/results/`

Key CSV outputs include:

1. `results_folder_groups_summary_with_global_true_ea.csv`
2. `results_folder_groups_all_basins_ea.csv`
3. `results_folder_05_local_vs_finetune_ea.csv`
4. `results_folder_05_global_eval_true_ea.csv`

### 8.3 Plotting (`plot.py`)

1. Reads CSVs from `work/04-neural_hyd/results/`
2. Writes PNGs to `work/04-neural_hyd/figures/`
3. Produces the full 14-figure set used in analysis slides

## 9. Verification checklist

Before running:

1. CAMELS_US structure matches Section 3.
2. Path mode in Section 4 is valid on your machine.
3. Python environment has required packages.

After running:

1. Run/checkpoint folders exist under runs root.
2. `work/04-neural_hyd/results/` contains expected CSVs.
3. `work/04-neural_hyd/figures/` contains generated figures.

## 10. Common issues

1. `python .\plot.py` fails with missing CSV: run `python .\results.py` first.
2. Training scripts cannot find data: check absolute paths and `data_dir` in `.yml`.
3. Outputs appear in unexpected location: absolute paths still point to `C:/GitHub/climate-change/...`.
4. Plot step fails with missing package: install `matplotlib pandas numpy`.

## 11. Recommended execution order

1. Prepare CAMELS_US data
2. Validate path mode
3. Run `python .\train.py`
4. Run `python .\results.py`
5. Run `python .\plot.py`
6. Verify outputs
