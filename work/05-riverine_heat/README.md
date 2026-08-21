

# Chapter 5: Riverine Heatwaves

##  Overview 

This chapter gives an overview on riverine heatwave and shows how they can be detected and quantified. Their development 
over time and differences between measuring stations was analysed using a dataset from Main river system in Bavaria, Germany.

The data contains measurements from 14 different gauges along th Main River system, including measurements of temperature 
and dynamic feautures and the static features of the stations.

## Repository structure

This repository includes:

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
    
    
    