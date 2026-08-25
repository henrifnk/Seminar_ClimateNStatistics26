# Interpretable Riverine Heatwaves

Modelling and interpretation of riverine water temperatures at the river Main using a Long Short-Term Memory (LSTM) model.

## Setup

Requires Python >= 3.11 and [uv](https://docs.astral.sh/uv/).
Requires R >= 4.5 and [renv](https://rstudio.github.io/renv/).

```bash
cd work/06-interpretable_heat
uv sync                       # creates a virtual environment (.venv) with all required Python packages
Rscript -e 'renv::restore()'  # installs all required R packages
```

`nh-run`, used below to train and evaluate the model, is provided by the `neuralhydrology` package and is installed automatically as part of `uv sync`.

## Project Structure

```
├── data/
│   ├── raw/                    # raw input data
│   ├── processed/              # processed data for the Neural Hydrology model
│   │   ├── attributes/         # static features
│   │   ├── time_series/        # dynamic features
│   │   ├── basins_train.txt    # stations included in the train set
│   │   ├── basins_val.txt      # stations included in the validation set
│   │   └── basins_test.txt     # stations included in the test set
│   ├── intermediate/           # cleaned and merged data used for data description
│   └── results/                # tables/data frames underlying the interpretability plots
│
├── src/
│   └── water_temp_lstm/
│       ├── preprocessing/      # functions for data preprocessing
│       └── interpretability/   # functions for loading and interpreting model results
│
├── Python/                     # scripts for data preprocessing and generating tables for interpretation plots
├── R/                          # scripts for figures
│
├── configs/
│   └── config.yml              # model config
│
├── figures/                    # figures for the seminar report
│   └── images/                 # images not created with R scripts
│
├── runs/                       # model run outputs
│
├── pyproject.toml
├── .python-version
├── uv.lock
├── renv.lock
├── .Rprofile
└── README.md
```

## Reproducibility

### Reproducing the model results

1. Place the raw data files in `data/raw/`.
2. Run the preprocessing script:
   ```bash
   uv run python Python/prepare_dataset.py
   ```
3. Train the model:
   ```bash
   uv run nh-run train --config-file configs/config.yml
   ```
   Results are stored in `runs/water_temp_lstm_<date>/`.
4. Evaluate the model for epoch n:
   ```bash
   uv run nh-run evaluate --run-dir runs/water_temp_lstm_<date> --epoch <n>
   ```
   Results are stored in `runs/water_temp_lstm_<date>/test/`.

### Reproducing the report figures

1. Run all scripts in `Python/` starting with `compute_` to generate the data frames needed for the plots (saved to `data/results/`):
   ```bash
   uv run python Python/compute_permutation_importance.py
   uv run python Python/compute_pdp.py
   uv run python Python/compute_ale.py
   uv run python Python/compute_lag_importance.py
   uv run python Python/compute_pdp_2d.py
   ```
2. Source all scripts in `R/` starting with `plot_` to generate the figures using ggplot2 (saved to `figures/`).
   ```bash
   Rscript R/plot_data_descriptives.R
   Rscript R/plot_permutation_importance.R
   Rscript R/plot_pdp.R
   Rscript R/plot_ale.R
   Rscript R/plot_lag_importance.R
   Rscript R/plot_pdp_2d.R
   ```