# 04-neural_hyd: End-to-End Reproducible Guide

This README is the only documentation needed to reproduce the workflow in `work/04-neural_hyd`.

More detailed code structure is stored in https://github.com/yuxinqiu9/NeuralHydrology.

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

## 12. What the results mean and how they link to the report

This section explains how to interpret the generated result files and how each piece is
used in Chapter 4 (`04-neural_hyd.Rmd`).

### 12.1 Core interpretation

The three-way benchmark compares:

1. Local: trained from scratch within each target group
2. Global: zero-shot inference from one globally pretrained model
3. Fine-tune: global pretrained model adapted on each target group

Interpretation rule:

1. If `Fine-tune > Local`, transfer learning improves over local-only training.
2. If `Fine-tune > Global`, local adaptation adds value beyond zero-shot transfer.
3. If both are true broadly across groups/basins, transfer gains are structural rather
  than isolated outliers.

### 12.2 Headline findings used in the report

In the current rendered chapter, the basin-weighted aggregate table reports:

1. NSE: Local = 0.281, Global = 0.565, Fine-tune = 0.589
2. Delta NSE (Fine-tune - Local) = +0.308
3. Delta NSE (Fine-tune - Global) = +0.023
4. KGE: Local = 0.191, Global = 0.485, Fine-tune = 0.535

Meaning:

1. Global pretraining contributes the largest baseline jump over local-only training.
2. Fine-tuning contributes additional group-specific adaptation on top of global priors.
3. The recommended strategy in this setup is: pretrain globally, then fine-tune locally.

### 12.3 CSV-to-report mapping

Main files and their role in `04-neural_hyd.Rmd`:

1. `results_folder_groups_summary_with_global_true_ea.csv`
  - Source for group-level means and overall weighted NSE/KGE
  - Feeds Table 4.1 and group delta plots

2. `results_folder_groups_all_basins_ea.csv`
  - Source for basin-level distribution diagnostics
  - Feeds boxplot and ECDF comparisons (Local vs Fine-tune)

3. `results_folder_05_local_vs_finetune_ea.csv`
4. `results_folder_05_global_eval_true_ea.csv`
  - Merged for Group 05 case study
  - Feeds Group 05 table and per-basin three-way chart

### 12.4 How this supports Chapter 4 claims

The chapter argument is layered and each layer is backed by a different view:

1. Aggregate effect direction: weighted table (overall NSE/KGE ordering)
2. Regional consistency: group-level bars and delta-to-zero checks
3. Distributional robustness: basin-level boxplot + ECDF shift
4. Architecture robustness: EA-LSTM vs CudaLSTM directional agreement

When all four layers agree, the report conclusion is justified:

1. Fine-tuning consistently improves over local-only baselines.
2. Fine-tuning also improves over zero-shot global in this benchmark setting.
3. The practical deployment rule is therefore evidence-based, not single-plot driven.
