

# Chapter 5: Riverine Heatwaves

##  Overview 

This chapter gives an overview on riverine heatwave and shows how they can be detected and quantified. Their development 
over time and differences between measuring stations was analysed using a dataset from Main river system in Bavaria, Germany.

## Repository structure

This repository includes:

- **data**  
  Includes the raw data from the Main dataset. This dataset includes measurements of water temperature and other static 
  and dynamic features of rivers at 14 gauges along the Main river system.
  The data are available in CSV format. Each station has its own file, while the static features of all stations are 
  provided in a separate CSV file.

- **R**  
  Includes the code used to produce the results and figures for the analysis.

  Before running the scripts, install the required R packages:

  ```r
  install.packages(c("ggplot2", "dplyr", "lubridate", "purrr", "tidyr"))
  ```
- **R**  
  Includes the analysis scripts:

  - `load_and_process_data.R`
    - Run before the other R scripts
    - Aggregates sub-daily measurements to daily mean temperatures
    - Calculates heatwave thresholds
    - Detects heatwave events
    - Creates annual and station-level summaries

  - `plots_threshold.R`
    - Creates heatwave threshold plots

  - `plots_analysis.R`
    - Creates exploratory plots
    - Visualizes frequency, duration, and intensity

  - `models_analysis.R`
    - Fits statistical models for temporal trends
    - Examines relationships with river kilometer
    
    
-  **figures** 

   Includes the figures used in the corresponding paper.
    
    
    