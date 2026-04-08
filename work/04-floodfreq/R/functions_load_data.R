"
Functions to create the useable data based on the csv input. 
File does not need to be loaded separately as it is implicitly loaded in function.R
"
# Libraries ---------------------------------------------------------------
library(doParallel)
library(ggplot2)

# Functions ---------------------------------------------------------------
load_data = function(rel_path, 
                      header_main = T, 
                      sep = ";", 
                      skip = 9, 
                      col_names = c("date", "discharge", "status"), 
                      rows_meta = 10, 
                      header_meta = FALSE,
                      tz_list = list(MEZ = "MET"),
                      date_format = "%Y-%m-%d %H:%M",
                      nonNA_cols = c("date", "status"),
                      logfile = "../data/output/rdata/extended_dfs/issues.log"
                      ){
  "
  Use a relative path to read in a csv file from GKD-website.
  Path is relative to Code folder.
  
  Discharge data starts at row 10 in all CSVs
  Before, there is only meta data info
  Goal: Extract actual discharge data and add meta data to data frame
  
  Returns a data frame with 3 columns (date, discharge, status) and meta data attributes.
  "
  
  # Maindata:
  dat <- read.csv(
    file = rel_path,
    skip = skip,
    sep = sep,
    header = header_main 
  )
  names(dat) = col_names
  
  # Metadata
  # NOTE: When reading in the data, it is forced into 2 columns. Think this is bc csv file starts with two columns only
  # Leads to varying shape in rows, e.g.:
  # V1                                                  V2
  # 1                Quelle: Bayerisches Landesamt für Umwelt, www.gkd.bayern.de
  # 2      Datenbankabfrage:                                    18.02.2025 14:33
  # 3             Zeitbezug:                                                 MEZ
  # 4      Messstellen-Name:                                          Mittenwald
  # 5       Messstellen-Nr.:                                            16000708
  # 6              Gewässer:                                                Isar
  # 7               Ostwert:                                              671208
  # 8              Nordwert:                                             5256930
  # 9  ETRS89 / UTM Zone 32N                                                    
  # 10  Pegelnullpunktshöhe:                                902,92 m NN (DHHN12) 
  
  # Attributes:
  # row 1: Source
  attr1 = "source"
  # row 2: Data base
  attr2 = "data base"
  # row 3: Reference time
  attr3 = "time zone"
  # row 4: Measurement unit
  attr4 = "measurement unit"
  # row 5: Measurement unit ID
  attr5 = "measurement id"
  # row 6: Body of water
  attr6 = "body of water"
  # row 7: East coordinate
  attr7 = "east"
  # row 8: North coordinate
  attr8 = "north"
  # row 9: kinda useless --> skip
  attr9 = "skip"
  # row 10: Point zero ("Pegelnullpunkt")
  attr10 = "point zero"
  attrs = c(attr1, attr2, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10)
  
  # Read in the first rows containing the meta data
  meta <- read.csv(
    file = rel_path,
    nrows = rows_meta,
    sep = sep,
    header = header_meta
  )
  # Assign meta-data attributes to the data frame
  for (i in 1:length(attrs)){
    # trimws: Function to remove the white space on both sides
    attr(dat, attrs[i]) <- trimws(meta[i, 2], which = "both")
  }
  
  # Data 
  dat$date = lubridate::parse_date_time(dat$date, orders = date_format)
  # Discharge 
  # NAs possible: Some are just empty --> NAs
  # If that is the case, let user know:
  if (anyNA(as.numeric(sub(",", ".", dat$discharge, fixed = TRUE)))){
    totalNAs = sum(is.na(as.numeric(sub(",", ".", dat$discharge, fixed = TRUE))))
    emptyDis = sum(dat$discharge[is.na(as.numeric(sub(",", ".", dat$discharge, fixed = TRUE)))] == "", na.rm = TRUE)
    NADis = sum(is.na(dat$discharge[is.na(as.numeric(sub(",", ".", dat$discharge, fixed = TRUE)))]), na.rm = TRUE)
    otherDis = totalNAs - emptyDis - NADis
    msg = paste(
        "Note: 
        Transformation for column 'discharge' in data set '", 
        attr(dat, "measurement unit"), 
        "-", 
        attr(dat, "measurement id"), 
        "'[station - id] resulted in NAs:
        ", totalNAs, " NAs of which:
        - ", emptyDis, " due to empty discharge entries
        - ", NADis, " due to NA discharge entries
        - ", otherDis, " non-'' or non-NA values turned to NA. Check those!",
        sep = "")
    write_log(logfile, msg)
    message(msg)
  }
  
  # Transform into numerical values
  dat$discharge = as.numeric(sub(",", ".", dat$discharge, fixed = TRUE))
  
  # Check if any of the columns contains NAs. Should not be the case
  for (name in nonNA_cols){
    if (anyNA(dat[name])) {
      msg = paste(
        "Error: Column '", 
        name, 
        "' in data set for '", 
        attr(dat, "measurement unit"), 
        "-", 
        attr(dat, "measurement id"), 
        "'[station - id] contains NA values!",
        sep = "")
      write_log(logfile, msg)
      stop(msg)
    }
  }
  
  return(dat)
}

extend_columns = function(dat){
  "
  Basically, take the slim data frame from load_data and turn it into a large data frame.
  It is a lot easier to work with that format, even tho less efficient in storage usage.
  "
  dat |> 
    dplyr::mutate(
      day = lubridate::day(date),
      month = lubridate::month(date),
      doy = lubridate::yday(date),# day of year, used to have some x-axis between years
      year = lubridate::year(date),
      hour = lubridate::hour(date),
      min = lubridate::minute(date),
      unit = attr(dat, "measurement unit"),
      id = attr(dat, "measurement id"),
      river = attr(dat, "body of water"),
      pos_east = attr(dat, "east"),
      pos_north = attr(dat, "north")
    )
}


grab_flood_events = function(df, return_plot = F){
  # Based on: https://onlinelibrary.wiley.com/doi/epdf/10.1002/hyp.14563 
  # Find break points only using days average to reduce noise and faster model fitting
  
  baseflow = hydroEvents::baseflowB(df$discharge) # Water flow idependent of event
  streamflow = df$discharge - baseflow$bf # Water flow due to event
  df = df |> 
    dplyr::mutate(
      baseflow = baseflow$bf,
      streamflow = streamflow
    )
  
  events = hydroEvents::eventBaseflow(streamflow) |> 
    dplyr::mutate(
      discharge = max + baseflow$bf[which.max]
    )
  peak = events |> dplyr::filter(discharge == max(events$discharge))
  
  # Graph based on which most extreme flood is identified
  event_plot = ggplot(df, aes(x = doy, y = discharge)) + 
    geom_line(color = "blue") +
    geom_line(aes(x = doy, y = baseflow)) +
    geom_point(data = events, aes(x = which.max, y = discharge), color = "darkgreen") + 
    geom_point(data = peak, aes(x = which.max, y = discharge), color = "red") + 
    geom_vline(xintercept = c(peak$srt, peak$end))
  
  if (return_plot) return(event_plot)
  return(peak)
}

grab_most_extreme_flood_event = function(df){
  events = grab_flood_events(df)
  
  peak = events |> dplyr::filter(discharge == max(events$discharge))
  return(peak)
}

apply_flood_detection <- function(df, debug = F) {
  # Remove all discharge NAs in the data. They are meaningless anyway to determine the peak flood, but ensure package works fine
  peak = grab_most_extreme_flood_event(df[!is.na(df$discharge), ])
  df = df |> dplyr::mutate(
    peak_flood = doy %in% peak$srt:peak$end
  )
  
  return(df)
}

file_to_df = function(rel_path,
                      header_main = T, 
                      sep = ";", 
                      skip = 9, 
                      col_names = c("date", "discharge", "status"), 
                      rows_meta = 10, 
                      header_meta = FALSE,
                      tz_list = list(MEZ = "MET"),
                      date_format = "%Y-%m-%d %H:%M",
                      nonNA_cols = c("date", "status"),
                      excluded_years = c(2025),
                      drop_first_year = TRUE,
                      completenessThreshold = 0.85,
                      logfile = "../data/output/rdata/extended_dfs/issues.log"
  ){
  "
    This function takes in a path to a CSV file and return a data frame (Initally part of 'create_and_save_dfs').
    The returned df is the extended one, i.e. the one with all the rows, no attributes.
    
    Some additional filter steps are possible, too.
  "
  # Read in CSV and truncate it according to load_data (e.g. first 10 rows are kinda empty)
  df = load_data(rel_path, header_main, sep, skip, col_names, rows_meta, header_meta, tz_list, date_format, nonNA_cols)
  # Enlargen the slim data frame to simplify working with it
  df = extend_columns(df)
  
  # Due to the data strucutre, there are some different filters by year allowed:
  # Filter first observed year. Reason is that the data here is usually pretty odd...
  first_year = min(df$year)
  if (drop_first_year) df = df |> dplyr::filter(year != first_year)
  # Allow manuel exclusion of selected years. Like 2025, somehow it is still included in the data even 
  # tho the data is supposed to only include 31.12.24
  df = df |> dplyr::filter(!year %in% excluded_years)
  # Apply a filter according to some completeness threshold. That is, any year that has a smaller 
  # ratio of complete cases than the threshold is dropped.
  # Reason: No need to consider the most extreme flood event if there are only 100 observations
  # Where 1 observation is a 15min timeframe of a whole year. lol. 
  years_below_thresh = calcCompRatio(df, filterby = c("year")) |> 
    dplyr::filter(ratio < completenessThreshold) |> 
    dplyr::select(year)
  df = df |> dplyr::filter(!year %in% years_below_thresh$year)
  if (length(years_below_thresh$year > 0)) {
    msg = paste(attr(df, "measurement id"), "Due to threshold on complete cases per year, removed", length(years_below_thresh$year), "years.")
    message(msg)
    write_log(logfile, msg) 
  }
  
  return(df)
}

calcCompRatio = function(df, filterby = c("id", "year")){
  # I an observation every 15min. Determine ratio by using ratio of the observed 15min intervals and the 15min contained in a year
  # Hinweis: Falls ratio = 1.00274, we have a leap year where 366 days in a year
  timeInYear = 365 * 24 * 60 / 15
  return(df |> dplyr::summarise(ratio = sum(!is.na(discharge)) / timeInYear, .by = dplyr::all_of(filterby)))
}

create_and_save_dfs <- function(
    in_dir = "../data/isar data/bis311224/",
    out_dir = "../data/output/rdata/extended/",
    header_main = T, 
    sep = ";", 
    skip = 9, 
    col_names = c("date", "discharge", "status"), 
    rows_meta = 10, 
    header_meta = FALSE,
    tz_list = list(MEZ = "MET"),
    date_format = "%Y-%m-%d %H:%M",
    nonNA_cols = c("date", "status")
  ){
  "
  Create and save all (extended) data frames. (i.e. the full data frames).
  
  This function takes all input-CSVs and creates the data frames containing the columns:
  date, discharge, status, day, month, doy, year, hour, min, unit, id, river, pos_east, pos_north
  -> date: date and time in Posixct format based on lubridate package
  -> discharge: discharge values (numeric)
  -> status: status of the discharge value; provided by the GKD and shows if value has been checked for validity or not 
  -> day: day of the date
  -> month: month of the date
  -> year: year of the date
  -> hour: hour of the date
  -> min: min of the date
  -> unit: name of the measurement station
  -> id: id of the measurement station
  -> river: name of the river the station is located at
  -> pos_east, pos_north: coordinates of station
  "
  # Get files in the input directory
  filenames = list.files(in_dir)
  
  # Iterate through all file names
  foreach(
    filename = filenames
  ) %dopar% {
    
    # Create df from CSV consisting of 15min data
    df = file_to_df(
      paste(in_dir, filename, sep = ""),
      header_main, sep, skip, col_names, rows_meta, header_meta,
      tz_list, date_format, nonNA_cols
    )
    
    # Filename to save df in
    df_filename = paste(df$id[1], ".Rdata", sep = "")
    # Save df in output path
    save(df, file = paste(out_dir, df_filename, sep = ""))
  }
}

write_log = function(logfilepath, logmessage){
  cat(logmessage, file = logfilepath, append = TRUE, sep = "\n")
}

apply_and_save_flood_detection = function(
    in_dir = "../data/output/rdata/extended_dfs/",
    out_dir = "../data/output/rdata/threshold_dfs/",
    pattern = "*.Rdata",
    debug = F
  ){
  "
  This function applies the logic in the straight line method to multiple data frames to identify the year's most extreme flood event.
  
  Input to this functions are the data frame as Rdata file from the previous step (i.e.from 'create_and_save_dfs')!
  date, discharge, status, day, month, doy, year, hour, min, unit, id, river, pos_east, pos_north, p_threshold, threshold, peak_flood
  -> date: date and time in Posixct format based on lubridate package
  -> discharge: discharge values (numeric)
  -> status: status of the discharge value; provided by the GKD and shows if value has been checked for validity or not 
  -> day: day of the date
  -> month: month of the date
  -> year: year of the date
  -> hour: hour of the date
  -> min: min of the date
  -> unit: name of the measurement station
  -> id: id of the measurement station
  -> river: name of the river the station is located at
  -> pos_east, pos_north: coordinates of station
  -> p_threshold: p-th quantile used as threshold
  -> threshold: The exact threshold value
  -> peak_flood: Boolean: Is current observation part of the peak flood?
  
  Note: Separating these steps and even saving the Rdata helps working on intermediate steps. Probably not most efficient.
  "
  # Get the names of the files in the input dir
  filenames = list.files(in_dir, pattern = pattern)
  
  # Run parallelized
  foreach(
    filename = filenames
  ) %dopar% {
    # Load the df in the Rdata files. These are the data frame produced by create_and_save_dfs
    load(paste(in_dir, filename, sep = ""))

    # Add p to identify quantile value
    # df = df |> dplyr::mutate(p_threshold = p_threshold)
    # A df contains all observations
    # Here, we split the observations by year and determine the most extreme flood event
    df = df |>
      # Instead of dealing with 15min data, reduce the data to daily averages. 
      # These are more robust towards measurement error in peak values 
      dplyr::summarise(
        discharge = mean(discharge, na.rm = T), 
        .by = c(year, doy, unit, id, river, pos_east, pos_north)
      ) |> 
      dplyr::group_by(year) |>
      # This part takes each sub-data.frame (grouped by year) and applies the straight line method (via function apply_slm_quants)
      # Theapply_slm_quants return a df containing the original columns and the column peak_flood indicating if flood was most extreme
      # dplyr::group_modify(~ apply_slm_quants(.x, p_threshold = p_threshold, plot = F)) |>
      dplyr::group_modify(~ apply_flood_detection(.x)) |>
      # Join sub-data.frames into one big one containing all years
      dplyr::ungroup()
      
    # Save the data frames with the addition _t followed by the p used to determine the quantile
    #   _t: means "threshold" and the following p denotes the used p-th quantile used to identify the most extreme flood
    df_filename = paste(unique(df$id), "_t", ".Rdata", sep = "")
    save(df, file = paste(out_dir, df_filename, sep = ""))
  }
}

create_hydrograph = function(df){
  "
  This function simply plots the hydrograph for one given year and a given station.
  "
  library(patchwork)
  # For the x-axis to be compareable, fix it to the same length
  days_in_year = c(0, 366)
  
  ## Clean Hydrograph Plot with Flood Indicator ------------------------------
  hydro = ggplot(df, aes(x = doy, y = discharge)) + 
    # Hydrograph-line
    geom_line(color = "darkgreen") + 
    geom_line(data = df |> dplyr::filter(peak_flood == TRUE), color = "red") + 
    geom_point(data = df |> dplyr::filter(discharge == max(df$discharge)) |> head(n = 1)) + 
    geom_ribbon(
      data = df |> dplyr::filter(peak_flood == TRUE), 
      aes(ymin = rep(0, df |> dplyr::filter(peak_flood == TRUE) |> nrow()), ymax = discharge), 
      alpha = 0.2,
      fill = "red"
    ) + 
    # Fix x-axis
    xlim(days_in_year) +
    # In the title, note:
    # station: Name of the station
    # year: Year the data refers to
    labs(
      x = "Day of the Year",
      y = latex2exp::TeX("Discharge ($m^3/s$)")
    ) 
  
  events = grab_flood_events(df[!is.na(df$discharge), ], return_plot = T) + 
    # Remove y-axis labels as it is redundant here
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) + 
    labs(x = "Day of the Year")
  
  plts =  (hydro + ggtheme |events + ggtheme) + 
    plot_annotation(
      # title = paste("Hydrograph Munich", unique(df$year)),
      theme = theme(
        plot.title = element_text(size = title_size, face = "bold")
      )
    )
  
  return(plts)
}

create_and_save_hydrographs <- function(
    in_dir = "../data/output/rdata/threshold_dfs/",
    out_dir = "../data/output/graphs/hydrographs/"
  ){
  "
  Create the hydrograph for all the data frames contained in the input folder.
  Uses the data frames created by apply_and_save_slm. That is, these plots require the knowledge of the most extreme flood event.
  
  Each hydrograph is for one station and one year. 
  Helps to assess if threshold values are working and if data looks realistic, i.e. sanity check.
  "
  # Filenames of Rdata files 
  filenames = list.files(in_dir)
  
  foreach(
    filename = filenames
  ) %dopar% {
  # Iterate through input files, i.e. data frames
    load(paste(in_dir, filename, sep = "")) # Loads data frame called df
    
    # Create plot for each year
    # This is done by splitting the df of a station by year and then applying the create_hydrograph to each of these sub-data.frames
    plots = df |> dplyr::group_split(year) |> purrr::map(create_hydrograph) 
    
    # Assign graph names to each output graph
    graphnames = paste0(out_dir, df$id[[1]], "_", unique(df$year), ".png", sep = "")
    # We have a list of graphs and a list of names. Use both and the ggsave function to save the graphs in the output dir
    purrr::walk2(
      graphnames, 
      plots, 
      ~{
        message("Saving:", .x)
        ggsave(filename = .x, plot = .y, dpi = 400, width = 10, height = 5)
      }
    )
  }
}

create_copula_df = function(df){
  "
  Create the data frame used to identify the copula / dependence structure.
  Columns: year, duration, peak, volume, id, unit, river, n, p_threshold MORE?
  -> year: year of observation
  -> n: number observations in year
  -> duration: duration of the flood event in minutes [min]
  -> peak: peak discharge of the flood event [m^3 / s]
  -> volume: total volume discharged during flood event [m^3]
  -> id: id of station
  -> unit: name of station
  -> river: name of river
  -> p_threshold: p as in p-quantile used as threshold to determine when flood starts / ends
  
  Note on calculating the (total) volume: 
  Every 15min, an observation is made. To calculate the total volume, we 
  a) assume constant discharge over the 15mins --> unrealistic and did not do that
  b) assume linear change in discharge over the 15mins. i.e. connect the two discharge values with a line
      Area is then given by area under the line (extrapolation).
      Mathematically, it is equivalent to determine 
      area under the line between the two discharge values
      or 
      area under the average of the two discharge values 
  This is done in 2 step approach:
  1) Create df with 15min volume sequences
  2) summarise the previous data frame for each year
  "
  # Base cop_df on daily average:
  # It does barely change anything for duration and volume, BUT for peak
  # There are some peak values in the data where I am not sure if its a measurement error or not
  # And since actual peak value is of interest, I want one that is somewhat robust towards measurement error
  
  copula_df = df |> 
    dplyr::summarise(
        discharge = mean(discharge, na.rm = T), 
        peak_flood = mean(peak_flood, na.rm = T) == 1,
        .by = c(year, doy)
      ) |> 
    dplyr::filter(peak_flood == TRUE) |> 
    dplyr::summarise(
      peak = max(discharge),
      duration_days = max(doy) - min(doy) + 1, # +1 so the last day is also counted as day where flood took place
      volume = sum(discharge) * 60^2 * 24 / 1e6, 
      .by = year
    )
  
  # Add meta data: id, unit, river
  copula_df = copula_df |> 
    dplyr::mutate(
      id = df$id[1],
      unit = df$unit[1],
      east = df$pos_east[1],
      north = df$pos_north[1],
      river = df$river[1],
      .before = peak
    )
  
  return(copula_df)
}

create_and_save_copula_dfs = function(
    in_dir = "../data/output/rdata/threshold_dfs/",
    out_dir = "../data/output/rdata/copula_dfs/"
  ){
  # Get files in the input directory
  filenames = list.files(in_dir)
  
  # Iterate through all file names
  for (filename in filenames){
    
    # Create copula df from df where peak flood is identified
    
    load(paste(in_dir, filename, sep = "")) # Loads data frame called df
    cop_df = create_copula_df(df)
    
    # Filename to save df in
    df_filename = paste(df$id[[1]], "_copula.Rdata", sep = "")
    # Save df in output path
    save(cop_df, file = paste(out_dir, df_filename, sep = ""))
  }
}

create_dfs = function(
    data_path = "../data/0 input data/",
    extended_dfs_path = "../data/output/rdata/extended_dfs/",
    threshold_dfs_path = "../data/output/rdata/threshold_dfs/",
    hydrograph_path = "../data/output/graphs/hydrographs/",
    copula_dfs_path = "../data/output/rdata/copula_dfs/",
    pattern = "*.Rdata",
    hydros = F
  ){
  # Initialize multiprocessing
  n_cores = parallel::detectCores() - 2
  cluster = parallel::makeCluster(n_cores, outfile = "load_data.log")
  doParallel::registerDoParallel(cluster)
  parallel::clusterEvalQ(cluster, source("functions.R"))
  
  create_and_save_dfs(in_dir = data_path, out_dir = extended_dfs_path)
  apply_and_save_flood_detection(in_dir = extended_dfs_path, out_dir = threshold_dfs_path, pattern = pattern)
  if (hydros) create_and_save_hydrographs(in_dir = threshold_dfs_path, out_dir = hydrograph_path)
  create_and_save_copula_dfs(in_dir = threshold_dfs_path, out_dir = copula_dfs_path)
  
  stopCluster(cluster)
}
