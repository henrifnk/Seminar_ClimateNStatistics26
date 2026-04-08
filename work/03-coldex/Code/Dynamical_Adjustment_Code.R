# ----------------------------------------------------------------------------------------------
# Dynamical Adjustment script:
# ----------------------------------------------------------------------------------------------

# Zhuoyang Li
# 8.04.2025

# ------------------------------------------------------------------------------------------
# 0.a) Read Relevant Packages
# ------------------------------------------------------------------------------------------
library(ncdf4)
library(xts)
library(zoo)
library(dplyr)
library(raster)
library(tidyr)
library(glmnet)
library(fields)
library(lubridate)
library(sf)
library(ggplot2)
library(patchwork)
library(cowplot)

# ------------------------------------------------------------------------------------------
# 0.b) Read Relevant Functions
# ------------------------------------------------------------------------------------------

# Function to calculate area-weighted mean on rasterbrick:
fldmean.RB <- function(RB, w = "area", mask = NULL, maskvalue = NA, ret.xts = T) {
  
  # check for potential mask:
  if (!is.null(mask)) RB = mask(RB, mask = mask, maskvalue = maskvalue)
  
  RB.values = values(RB)
  area.vec = values(raster::area(RB))
  
  if (w == "area") {
    RB.mean.ts = apply(X = RB.values, MARGIN = 2, FUN=function(x) {
      weighted.mean(x, w = area.vec, na.rm = T)
    })  
  } else if (w == "none") {
    RB.mean.ts = cellStats(RB, stat="mean", na.rm = T)
  }
  
  if (ret.xts == T) {
    ret.ts = xts(x = RB.mean.ts, order.by = as.Date(substring(names(RB), first = 2, last = 11), "%Y.%m.%d"))
  } else if (ret.xts == F) {
    ret.ts = RB.mean.ts
  }
  
  return(ret.ts)
}

# Deseasonalize time series:
deseasonalize.ts.1 <- function(ts, ref.year = 1971:2000, filt = 1, ret.seas.comp = F, na.rm = T) {
  
  # Convert time series to xts object:
  if (!is.xts(ts)) {
    xts.ts = as.xts(ts)
  } else {
    xts.ts = ts
  }
  
  # Return iff all values are NA
  if (all(is.na(xts.ts))) return(xts.ts)
  
  # Get monthly means via zoo::aggregate:
  
  # aggregate ref time series to monthly:
  ref.ts.filt = apply.monthly(x = window(xts.ts, start = (paste(head(ref.year, 1)-1, "-01-01", sep="")), 
                                         end = (paste(tail(ref.year, 1)+1, "-12-31", sep=""))), FUN=mean, na.rm = na.rm)
  
  ref.ts.filt = rollapply(data = ref.ts.filt, width = filt, FUN = mean, na.rm=T)
  
  ref.ts.filt_1981_2010= window(ref.ts.filt, start = "1981-01-01", end = "2010-12-31")
  
  ref.ts.monmean = aggregate(x = ref.ts.filt_1981_2010, by = format(as.Date(time(as.xts(ref.ts.filt_1981_2010))), "%m"), FUN=mean, na.rm = na.rm)#按月份分组，计算长期的每个月的均
  
  
  # make new time series that contains monthly values:
  xts.ts.seasonal = xts(x = as.numeric(ref.ts.monmean)[as.numeric(format(as.Date(time(xts.ts)), "%m"))], order.by = index(xts.ts))
  
  if (ret.seas.comp == T) {
    return(xts.ts.seasonal)
  } else {
    return(xts.ts - xts.ts.seasonal) #anomalies
  }
}

# TRAINING AND PREDICTION OF TIME SERIES (with same dataset):
#' @title Train a model for a climatic target variable Y based on circulation pattern X
#' 训练一个模型 基于大气环流模式 (X) 预测气候目标变量 (Y)
#' @description 
#' @param Y.train time series of (univariate) target variable to train on (an XTS object)
#' @param X Circulation patterns predict
#' @param X.train Circulation patterns to train on (if NULL, X is used). If specified, MUST be of same length as train.years in Y.train
#' @param X.date.seq Sequence of Dates in X (which will be used for forward prediction)
#' @param train.years subset of years to train on for dynamical adjustment (if NULL: all years are used)
## #' @param na.rm Automatically remove rows (from training) if there is an NA in Y?
#' @details
#' Some details about the function
#' @return A deseasonalized (daily or monthly) time series in xts format.
#' @references None
#' @author Sebastian Sippel
#' @examples
train.dyn.adj.elastnet.annual <- function(Y.train, X, X.train = NULL, X.date.seq = NULL, 
                                          train.years = NULL, train.months = 1:12, add.mon = 2, lambda.min.ratio = NULL,
                                          alpha = 1, nfolds = 10, ret.cv.model=F, s = "lambda.min", 
                                          lags.to.aggregate = list(1:2, 3:7, c(8:30)), n.pc = 10, EOF.penalty = 1, cv.parallel = F, family = "gaussian", single.index.mod = NULL, keep = F) {
  
  ## (0) SANITY CHECKS AND TRANSFORMATION ON INPUT DATA:
  ## ---------------------------------------------------
  {
    # CHECK IF Separate dataset of TRAIN Years are given (can EITHER GIVE train.years OR train on entire Y time):
    if (is.null(train.years)) train.years = as.numeric(unique(format(as.Date(time(Y.train)), "%Y")))
    
    # CHECK IF X is a raster and transform to matrix if necessary:
    if (!is.matrix(X)) {
      # area.vec = values(area(X)) / max(values(area(X)))  # -> EOF in pred. field could be weighted by area -> but rather not..
      if (is.null(X.date.seq)) X.date.seq = as.Date(substring(text = names(X), first = 2, last = 11), format = "%Y.%m.%d")
      X = t(values(X))
    }
    # CHECK if X is matrix and X.date.seq can be taken from matrix rownames:
    if (is.matrix(X) & is.null(X.date.seq)) X.date.seq = as.Date(substring(text = rownames(X), first = 2, last = 11), format = "%Y.%m.%d")
    
    # CHECK if X.train is given; and whether it is raster?
    if (!is.null(X.train) & !is.matrix(X.train)) X.train = t(values(X.train))
    # if (is.null(X.train)) X.train = X[which(X.date.seq %in% as.Date(time(Y.train))),]  ## DERIVE X.train from X
    
    # CHECK FOR NA's in ENTIRE COLUMNS in X (and remove if necessary):  
    # na.idx.col = apply(X = X, MARGIN=2, FUN=function(x) all(is.na(x)))
    # if( any(na.idx.col) ) X = X[,!na.idx.col] 
    
    train.seasonal = F
    if (train.months[1] == "seasonal") {
      train.months = c(1, 4, 7, 10)
      train.seasonal = T
    }
  }
  
  
  ## (1) INCLUDE LAG PREDICTORS IF DESIRED:
  ## ------------------------------------------------
  {
    if (!is.null(lags.to.aggregate) & is.null(X.train)) {
      X = add.lag.predictors(X = X, X.train = X.train, 
                             svd.idx = which(as.numeric(format(time(Y.train), "%Y")) %in% train.years), 
                             lags.to.aggregate = lags.to.aggregate, n.pc = n.pc)
    } else if (!is.null(lags.to.aggregate) & !is.null(X.train)) {
      ret.list = add.lag.predictors(X = X, X.train = X.train, 
                                    svd.idx = which(as.numeric(format(time(Y.train), "%Y")) %in% train.years), 
                                    lags.to.aggregate = lags.to.aggregate, n.pc = n.pc)
      X = ret.list$X
      X.train = ret.list$X.train
    }
  }
  
  ## (2) TRAINING & PREDICTION STEPS:
  ## --------------------------------
  cur.glmnet = list()   
  Yhat = rep(NA, length(X.date.seq)) 
  
  for (i in train.months) {
    print(i)
    
    ## TRAINING IDX:
    cur.mon = (i-add.mon):(i+add.mon)
    if (add.mon == 1) {
      if (i == 1) cur.mon = c(12,1,2)
      if (i == 12) cur.mon = c(11,12,1)
    } else if (add.mon == 2) {
      if (i == 1) cur.mon = c(11,12,1,2,3)
      if (i == 2) cur.mon = c(12,1,2,3,4)
      if (i == 11) cur.mon = c(9,10,11,12,1)
      if (i == 12) cur.mon = c(10,11,12,1,2)
    }
    Y.train.idx = which(as.numeric(format(as.Date(time(Y.train)), "%m")) %in% cur.mon & 
                          as.numeric(format(as.Date(time(Y.train)), "%Y")) %in% train.years)    # WHICH INDICES ARE IN PRESENT MONTHS AND IN train.years?
    if (any(is.na(Y.train[Y.train.idx]))) Y.train.idx=Y.train.idx[-which(is.na(Y.train[Y.train.idx]))]  # is any Y value NA?
    if (!is.null(lags.to.aggregate)) if (any((1:max(unlist(lags.to.aggregate))) %in% Y.train.idx)) Y.train.idx=Y.train.idx[-which(Y.train.idx %in% (1:max(unlist(lags.to.aggregate))))]
    
    
    ## (2.1) GLMNET TRAINING BASED ON CROSS-VALIDATION: lambda.min.ratio
    crossclass = c(rep.row(x = 1:nfolds, n = ceiling(length(Y.train.idx)/nfolds)))[1:length(Y.train.idx)]
    if(is.null(lambda.min.ratio) & is.null(X.train)) lambda.min.ratio = ifelse(length(Y.train[Y.train.idx]) < c(dim(X[Y.train.idx,])[2]),0.01,0.0001)
    if(is.null(lambda.min.ratio) & !is.null(X.train)) lambda.min.ratio = ifelse(length(Y.train[Y.train.idx]) < c(dim(X.train[Y.train.idx,])[2]),0.01,0.0001)
    if (is.null(X.train)) {
      glmnet.control(fdev=0, devmax=1, mnlam = 100)
      
      # set penalty.factor to X for lagged EOF's:penalty.factor 
      penalty.factor = rep(1, dim(X)[2])
      penalty.factor[(length(penalty.factor)-length(lags.to.aggregate) * n.pc + 1):length(penalty.factor)] = EOF.penalty
      
      cur.glmnet[[i]] = cv.glmnet(x = X[Y.train.idx,], y = as.numeric(Y.train[Y.train.idx]), family = family, nfolds = nfolds, foldid = crossclass, alpha = alpha, parallel = cv.parallel, penalty.factor = penalty.factor, keep = keep)
      cur.glmnet[[i]]$Y = as.numeric(Y.train[Y.train.idx])
    } else { 
      glmnet.control(fdev=0, devmax=1, mnlam = 100)
      
      # set penalty.factor to X for lagged EOF's:
      penalty.factor = rep(1, dim(X)[2])
      penalty.factor[(length(penalty.factor)-length(lags.to.aggregate) * n.pc + 1):length(penalty.factor)] = EOF.penalty
      
      cur.glmnet[[i]] = cv.glmnet(x = X.train[Y.train.idx,], y = as.numeric(Y.train[Y.train.idx]), family = family, nfolds = nfolds, foldid = crossclass, alpha = alpha, parallel = cv.parallel, penalty.factor = penalty.factor, keep = keep)
      cur.glmnet[[i]]$Y = as.numeric(Y.train[Y.train.idx])
    }
    
    ## (2.2) If Applicable: LOWESS SMOOTHER:
    yhat_train = predict(object=cur.glmnet[[i]], newx = X[Y.train.idx,], s = s)
    
    # predict(cur.glmnet[[i]], newx = X[Y.train.idx,], s = s)
    
    if (!is.null(single.index.mod) & !all(diff(yhat_train) == 0))  {
      print(" Training of LOWESS smoother in single index model")
      
      plot(yhat_train, as.numeric(Y.train[Y.train.idx]))    # very non-linear surface... 
      # cor(yhat_train, as.numeric(Y.train[Y.train.idx])) ^ 2
      lowess.smooth.mod = loess( formula = as.numeric(Y.train[Y.train.idx]) ~ yhat_train, family = "gaussian", span = single.index.mod$span, alpha = single.index.mod$alpha,
                                 control = loess.control(surface = c("direct"), statistics = c("approximate"), trace.hat = c("approximate")))
      
      # yhat_hat = predict(object = lowess.smooth.mod, newdata = yhat_train)
      # plot(yhat_hat, as.numeric(Y.train[Y.train.idx]))
      # cor(yhat_hat, as.numeric(Y.train[Y.train.idx])) ^ 2
    }
    
    
    ## (2.3) PREDICTION STEP FOR RESPECTIVE MONTH:
    if ( train.seasonal == T) {   
      X.pred.idx = which(as.numeric(format(X.date.seq, "%m")) %in% cur.mon)    ## PREDICTION IDX: idx to fill with predicted value in X
    } else {#按普通月预测模式，只有一个月的index
      X.pred.idx = which(as.numeric(format(X.date.seq, "%m")) == i)    ## PREDICTION IDX: idx to fill with predicted value in X
    }
    Yhat[X.pred.idx] = predict(cur.glmnet[[i]], newx = X[X.pred.idx,], s = s)  
    if (family == "binomial") Yhat[X.pred.idx] = predict(cur.glmnet[[i]], newx = X[X.pred.idx,], s = s, type="class")
    if (!is.null(single.index.mod)) Yhat_loess = predict(object = lowess.smooth.mod, newdata = Yhat)
    if (keep == T) Yhat[X.pred.idx] = cur.glmnet[[i]]$fit.preval[match(X.pred.idx, table = Y.train.idx),which(cur.glmnet[[i]]$lambda == cur.glmnet[[i]][[s]])] # predict.cv.glmnet(cur.glmnet[[i]], newx = X[X.pred.idx,], s = s)  
  }
  
  
  ## (3) Return different quantities:
  ## --------------------------------
  if (ret.cv.model) return(cur.glmnet)
  
  Yhat.xts = xts(x = Yhat, order.by = X.date.seq)
  if (!is.null(single.index.mod)) Yhat.xts = xts(x = Yhat_loess, order.by = X.date.seq)
  return(Yhat.xts)
}

# Add lag predictors to circulation matrix X and X.train:
add.lag.predictors <- function(X, X.train = NULL, svd.idx = NULL, lags.to.aggregate = list(1:2, 3:7, c(8:30)), n.pc = 10) {
  
  # svd.idx = which(as.numeric(format(time(Y.train), "%Y")) %in% train.years)
  
  # (0) RUN Singular value decomposition on the whole matrix:
  if (is.null(X.train)) {
    X.svd = svd( t(X[svd.idx,]) )
  } else {
    X.svd = svd( t(X.train[svd.idx,]) )
  }
  
  # (1) PROJECT anomaly pattern on entire X: X.svd$u 
  X.pc = (t(X.svd$u) %*% t(X))[1:n.pc,] # first 10 PC's as lag predictors (!!)
  if (!is.null(X.train)) X.train.pc = (t(X.svd$u) %*% t(X.train))[1:n.pc,]
  # plot(X.svd$v[,1], X.pc[1,1:7305])
  
  # (2) EXTRACT averages of past lags:
  test = lapply(X = lags.to.aggregate, FUN=function(cur.lag) {
    X.lag = sapply(X = 1:n.pc, FUN=function(idx) c(rep(NA, cur.lag[1]), rollmean(x = X.pc[idx,], k = length(cur.lag), na.pad = T, align = "right")))[1:(dim(X)[1]),]
    return(X.lag)
  })
  X.lag = do.call(cbind, test)
  
  if (!is.null(X.train)) {
    test = lapply(X = lags.to.aggregate, FUN=function(cur.lag) {
      X.lag = sapply(X = 1:n.pc, FUN=function(idx) c(rep(NA, cur.lag[1]), rollmean(x = X.train.pc[idx,], k = length(cur.lag), na.pad = T, align = "right")))[1:(dim(X.train)[1]),]
      return(X.lag)
    })
    X.train.lag = do.call(cbind, test)
  }
  
  # (3) Return lag values for X and X.train
  if (is.null(X.train)) {
    return( cbind(X, X.lag) )
  } else {
    return(list(X = cbind(X, X.lag), X.train = cbind(X.train, X.train.lag)))
  }
}


# ------------------------------------------------------------------------------------------ 
# 1. Read model data from CESM1.2.2 and ERA5
# ------------------------------------------------------------------------------------------
setwd("/Users/sunny/Desktop/cold extremes") # Modify this path to your local working directory as needed
# CESM2 data 
psl_file <- "PSL_Europe_2000y_NDJF_anom.nc"
trefht_file <- "TREFHT_Europe_2000y_NDJF_anom.nc"
full_url <- "ftp://climphys:friendly@data.iac.ethz.ch/sippels/dynamical_adjustment_elasticnet/PSL_Europe_2000y_NDJF_anom.nc"
full_url2 <-"ftp://climphys:friendly@data.iac.ethz.ch/sippels/dynamical_adjustment_elasticnet/TREFHT_Europe_2000y_NDJF_anom.nc"
download.file(url = full_url, destfile = psl_file, mode = "wb", method = "curl")
download.file(url = full_url2, destfile = trefht_file, mode = "wb", method = "curl")

PSL_EUROPE = brick("PSL_Europe_2000y_NDJF_anom.nc") + 0
TREFHT_EUROPE = brick("TREFHT_Europe_2000y_NDJF_anom.nc") + 0

# ERA5 data
# Data source: https://doi.org/10.24381/cds.adbb2d47 
# Variable: t2m, msl 
# Year: 1950-2024
# All months and all days with time at 12:00 
# Extent:(45° W–35° E, 22–72° N)
era5full_nc <- nc_open("era5full.nc")
raw_time_full <- ncvar_get(era5full_nc, "valid_time")  
time_unit_full <- ncatt_get(era5full_nc, "valid_time", "units")$value  
converted_time_full <- as.POSIXct(raw_time_full, origin = "1970-01-01", tz = "UTC")
ERA5full_T2M = brick("era5full.nc", varname = "t2m") +0
ERA5full_MSL = brick("era5full.nc", varname = "msl") +0

# ------------------------------------------------------------------------------------------ 
# 2.a) Data Processing
# ------------------------------------------------------------------------------------------ 

# Crop Bavaria 
BAV.TREFHT.extent = extent(c(8.8, 13.8, 47.2, 50.6))
BAV.PSL.extent = extent(c(5.8, 16.8, 44.2, 53.6))
## For CESM2
PSL_BAV = crop(PSL_EUROPE, BAV.PSL.extent)
TREFHT_BAV = crop(TREFHT_EUROPE, BAV.TREFHT.extent)
## For ERA5
MSLfull_BAV = crop(ERA5full_MSL, BAV.PSL.extent)
names(MSLfull_BAV) <- as.character(converted_time_full)

T2Mfull_BAV = crop(ERA5full_T2M, BAV.TREFHT.extent)
T2Mfull_BAV <- calc(T2Mfull_BAV, fun = function(x) x - 273.15) #from Kelvin to Celsius
names(T2Mfull_BAV) <- as.character(converted_time_full)#change time format

# ------------------------------------------------------------------------------------------ 
# 2.b) Calculate ERA5 Original T2M & MSL Anomalies
# ------------------------------------------------------------------------------------------ 

## T2M Anomalies
# Claculate weighted mean, return xts.
TS_T2Mfull_BAV <- fldmean.RB(RB = T2Mfull_BAV, w = "area", ret.xts = TRUE)
# Calculate era5 original anomalies with 3 months moving average
TS_T2Mfull_BAV_anomalies <- deseasonalize.ts.1(ts = TS_T2Mfull_BAV, ref.year = 1981:2010, filt = 3, ret.seas.comp = FALSE)
winter_months_T2M <- c(12, 01, 02) #choose DJF
TS_T2Mfull_BAV_anomalies_DJF <- TS_T2Mfull_BAV_anomalies[format(index(TS_T2Mfull_BAV_anomalies), "%m") %in% sprintf("%02d", winter_months_T2M)]

## MSL Anomalies
winter_months <- c("12", "01", "02")
# Extract the date in "YYYY.MM.DD" format and convert it to Date class
dates <- as.Date(substring(names(MSLfull_BAV), 2, 11), format = "%Y.%m.%d")
# Get DJF indices
DJF_indices <- which(format(dates, "%m") %in% winter_months)
# Subset RasterBrick
RB_MSLfull_BAV_DJF <- MSLfull_BAV[[DJF_indices]]

# Subtract 1981-2010 winter average temperature to calculate MSL anomalies
selected_layers <- which( format(dates, "%m") %in% c("12", "01", "02"))
rb_1981_2010_DJF <- subset(MSLfull_BAV, selected_layers)
rb_all_DJF <- calc(rb_1981_2010_DJF, mean, na.rm = TRUE)
MSL_anomalies <- RB_MSLfull_BAV_DJF - rb_all_DJF
names(MSL_anomalies) <- names(MSLfull_BAV)[DJF_indices]

# ------------------------------------------------------------------------------------------ 
# 3.Train Model Based on Elastic Net Regression
# ------------------------------------------------------------------------------------------ 

# For ERA5
T2M_BAV_hat = train.dyn.adj.elastnet.annual(Y.train = TS_T2Mfull_BAV_anomalies_DJF, X = MSL_anomalies, train.years = 1950:2000, train.months = c(1,2,12), add.mon = 1, alpha = 0.2, nfolds = 10, s = "lambda.1se", 
                                            lags.to.aggregate = list(1), n.pc = 10, cv.parallel = T)

T2M_BAV_hat_filtered <- window(T2M_BAV_hat, start = as.Date("1950-01-01"), end = as.Date("2024-12-31"))
T2M_BAV_hat1 <- extract.months.from.ts(T2M_BAV_hat_filtered, months = c(1,2,12))
T2M_BAV_hat1 <- na.approx(T2M_BAV_hat1, rule = 2)# Perform linear interpolation to fill missing values introduced by lag
# 'rule = 2' ensures the first (or last) NA is filled using nearest non-NA value
TS_T2Mfull_BAV_anomalies_DJF_filtered <- window(TS_T2Mfull_BAV_anomalies_DJF, start = as.Date("1950-01-01"), end = as.Date("2024-12-31"))
T2M_BAV1 <- extract.months.from.ts(TS_T2Mfull_BAV_anomalies_DJF_filtered, months = c(1,2,12))

## Assess how well this works:
plot(x = as.numeric(T2M_BAV_hat1), y = as.numeric(T2M_BAV1), xlab = "Predicted", ylab = "Observed")
cor(as.numeric(T2M_BAV_hat1), as.numeric(T2M_BAV1)) ^ 2

# For CESM2
## Change trefht_bav to ts and use train.dyn.adj.elastnet.annua
TS_TREFHT_BAV <- fldmean.RB(RB = TREFHT_BAV, w = "area", ret.xts = TRUE)
TS_TREFHT_BAV_1950_2024 <- TS_TREFHT_BAV[format(index(TS_TREFHT_BAV), "%Y") %in% 1950:2024]
## Increase the resolution of psl_bav to match the ERA5 msl grid (more grid points)
PSL_BAV <- crop(PSL_EUROPE, extent(MSL_anomalies),snap = "out")
PSL_BAV <- resample(PSL_BAV, MSL_anomalies, method = "bilinear")
extent(PSL_BAV) <- extent(MSL_anomalies)



TS_TREFHT_BAV_model = train.dyn.adj.elastnet.annual(
  Y.train = TS_TREFHT_BAV,  
  X = PSL_BAV,  
  train.years = 1950:2000,  
  train.months = c(1,2,12),  
  add.mon = 1,  
  alpha = 0.2,  
  nfolds = 10,  
  s = "lambda.1se",  
  lags.to.aggregate = list(1),  
  n.pc = 10,  
  cv.parallel = T,  
  ret.cv.model = TRUE  # return model, not predictions
)
# ------------------------------------------------------------------------------------------ 
## Use CESM2 trained model on ERA5 for comparison
# Extract date information from layer names and convert to Date format
era5_dates <- as.Date(substring(text = names(MSL_anomalies), first = 2, last = 11), format = "%Y.%m.%d")

# Convert raster data (MSL anomalies) to matrix format (time × space)
MSL_anomalies_mat <- t(values(MSL_anomalies))

# Add lagged predictors
MSL_anomalies_lag <- add.lag.predictors(
  X = MSL_anomalies_mat, 
  svd.idx = which(as.numeric(format(era5_dates, "%Y")) %in% c(1950:2000)),  
  lags.to.aggregate =  list(1), 
  n.pc = 10
)
# Extract month (numeric) from date vector
era5_months <- as.numeric(format(era5_dates, "%m"))

# Predict separately for each month
era5_month_idx1 <- which(era5_months == 1)
era5_input1 <- MSL_anomalies_lag[era5_month_idx1, , drop = FALSE] 
era5_input1 <- na.approx(era5_input1, rule = 2)
pre1 <- predict(TS_TREFHT_BAV_model[[1]]$glmnet.fit, newx = era5_input1, s = TS_TREFHT_BAV_model[[1]]$lambda.1se)
pre1_xts <- xts(pre1, order.by =era5_dates[era5_month_idx1])

era5_month_idx2 <- which(era5_months == 2)
era5_input2 <- MSL_anomalies_lag[era5_month_idx2, , drop = FALSE] 
pre2 <- predict(TS_TREFHT_BAV_model[[2]]$glmnet.fit, newx = era5_input2, s = TS_TREFHT_BAV_model[[2]]$lambda.1se)
pre2_xts <- xts(pre2, order.by = era5_dates[era5_month_idx2])

era5_month_idx12 <- which(era5_months == 12)
era5_input12 <- MSL_anomalies_lag[era5_month_idx12, , drop = FALSE] 
pre12 <- predict(TS_TREFHT_BAV_model[[12]]$glmnet.fit, newx = era5_input12, s = TS_TREFHT_BAV_model[[12]]$lambda.1se)
pre12_xts <- xts(pre12, order.by = era5_dates[era5_month_idx12])

# Merge monthly predictions into a single time series in chronological order
cesm2_pre_era5 <- rbind(pre1_xts, pre2_xts, pre12_xts)
cesm2_pre_era5 <- cesm2_pre_era5[order(index(cesm2_pre_era5))]

# ------------------------------------------------------------------------------------------ 
# 4.Prepare The Dataframe For Plotting
# ------------------------------------------------------------------------------------------ 

# Convert to dataframe
df_original <- data.frame(Date = index(TS_T2Mfull_BAV_anomalies_DJF), Value = coredata(TS_T2Mfull_BAV_anomalies_DJF))
df_era5 <- data.frame(Date = index(T2M_BAV_hat1), Value = coredata(T2M_BAV_hat1))
df_cesm2 <- data.frame(Date = index(cesm2_pre_era5), Value = coredata(cesm2_pre_era5))
colnames(df_cesm2)[2] <- "Value"

# Extract year
df_original$Year <- format(df_original$Date, "%Y")
df_era5$Year <- format(df_era5$Date, "%Y")
df_cesm2$Year <- format(df_cesm2$Date, "%Y")

# Calculate yearly means
df_original_avg <- df_original %>%
  group_by(Year) %>%
  summarise(Original = mean(Value, na.rm = TRUE))

df_era5_avg <- df_era5 %>%
  group_by(Year) %>%
  summarise(Adjusted_ERA5 = mean(Value, na.rm = TRUE))

df_cesm2_avg <- df_cesm2 %>%
  group_by(Year) %>%
  summarise(Adjusted_CESM2 = mean(Value, na.rm = TRUE))

# Merge dataframes
df_final <- Reduce(function(x, y) full_join(x, y, by = "Year"), 
                   list(df_original_avg, df_era5_avg, df_cesm2_avg))
df_final$Year <- as.numeric(df_final$Year)

# Add residual columns# add residual columns
df_final <- df_final %>%
  mutate(
    Residual_ERA5 = Original - Adjusted_ERA5,
    Residual_CESM2 = Original - Adjusted_CESM2
  )

# Convert to long format
df_final_long <- df_final %>%
  pivot_longer(cols = -Year, names_to = "Type", values_to = "Temperature_Anomaly") %>% 
  filter(Type %in% c("Original", "Adjusted_CESM2", "Adjusted_ERA5"))
df_residuals_long <- df_final %>%
  select(Year, Residual_ERA5, Residual_CESM2) %>%
  pivot_longer(cols = -Year, names_to = "Type", values_to = "Residual")

# Set colors
colors <- c("Original" = "black", "Adjusted_ERA5" = "blue", "Adjusted_CESM2" = "lightblue")
residual_colors <- c("Residual_ERA5" = "blue", "Residual_CESM2" = "lightblue")

# ------------------------------------------------------------------------------------------ 
# 5.Plot
# ------------------------------------------------------------------------------------------ 

# Plot for temperature anomalies
## Calculate R
R_Value <-cor(as.numeric(period.mean.ts(TS_T2Mfull_BAV_anomalies_DJF, years=c(1950:2024), months="annual")), as.numeric(period.mean.ts(cesm2_pre_era5, years=c(1950:2024), months="annual")), use = "complete.obs")
R_Value_rounded <- round(R_Value, 1)

p <- ggplot(df_final_long, aes(x = Year, y = Temperature_Anomaly, color = Type, group = Type)) +
  geom_line(size = 1) +  
  geom_point(data = df_final_long %>% filter(Type == "Original"), size = 2, shape = 16) +  
  geom_smooth(aes(group = Type), method = "lm", linetype = "dashed", se = FALSE, size = 1) +  
  scale_color_manual(values = colors) +  
  labs(title = "Bavaria DJF Temperature Anomalies (1950-2024)",
       y = "Temperature Anomaly (°C)",
       x = "Year") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )  + theme(
    panel.grid.major = element_blank(),  # 去掉主要网格线
    panel.grid.minor = element_blank(),  # 去掉次要网格线
    panel.background = element_blank(),  # 去掉背景填充
    axis.line = element_line(color = "black"),  # 添加黑色轴线
    axis.ticks = element_line(color = "black")  # 添加刻度
  ) + scale_x_continuous(
    breaks = seq(1950, 2025, by = 10),  # 主要刻度：每 10 年一个
    minor_breaks = seq(1950, 2025, by = 5)  # 小刻度：每 5 年一个
  )  + annotate(
    "text", x = 2000, y = -7,   # 设定 R 值的位置（可调整）
    label = paste("Circulation-induced variability (R =", R_Value_rounded, ")"),
    color = "blue", size = 5, fontface = "bold"
  ) + scale_y_continuous(
    limits = c(-8, 4),  # 设置 y 轴范围
    breaks = seq(-8, 4, by = 2),  # **主要刻度：每 2°C 一个**
    minor_breaks = seq(-8, 4, by = 1)  # **次级刻度：每 1°C 一个**
  )

print(p)

# Plot for residuals
p_residual <- ggplot(df_residuals_long, aes(x = Year, y = Residual, color = Type, group = Type)) +
  geom_line(size = 1) + 
  geom_smooth(aes(group = Type), method = "lm", linetype = "dashed", se = FALSE, size = 1) +  
  scale_color_manual(values = residual_colors) +  
  labs(title = "Residual from Circulation",
       x = "Year") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) + theme(
    panel.grid.major = element_blank(),  # 去掉主要网格线
    panel.grid.minor = element_blank(),  # 去掉次要网格线
    panel.background = element_blank(),  # 去掉背景填充
    axis.line = element_line(color = "black"),  # 添加黑色轴线
    axis.ticks = element_line(color = "black")  # 添加刻度
  )+ scale_x_continuous(
    breaks = seq(1950, 2025, by = 10),  # 主要刻度：每 10 年一个
    minor_breaks = seq(1950, 2025, by = 5)  # 小刻度：每 5 年一个
  )+ scale_y_continuous(
    limits = c(-8, 4),  # 设置 y 轴范围
    breaks = seq(-8, 4, by = 2),  # **主要刻度：每 2°C 一个**
    minor_breaks = seq(-8, 4, by = 1)  # **次级刻度：每 1°C 一个**
  )

print(p_residual)

# combine plots
combined_plot <- p / p_residual+ plot_layout(heights = c(2, 1))
print(combined_plot)
options(repr.plot.width = 14, repr.plot.height = 9)

#ggsave("dynamical adjustment.png", combined_plot, width = 12, height = 8, dpi = 300)





















