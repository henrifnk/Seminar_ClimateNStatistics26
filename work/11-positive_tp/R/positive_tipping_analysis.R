library(dplyr)
library(ggplot2)

#data
yearlyFile <- "work/11-positive_tp/data/yearly2.csv"
monthlyFile <- "work/11-positive_tp/data/monthly2.csv"

yearlyDf <- read.csv2(yearlyFile)
monthlyDf <- read.csv(monthlyFile)
yearlyDf$Year <- as.numeric(yearlyDf$Year)
yearlyDf$Value <- as.numeric(yearlyDf$Value)
monthlyDf$Date <- as.Date(monthlyDf$Date)
monthlyDf$Value <- as.numeric(monthlyDf$Value)

#benchmark context
benchCountries <- c("Denmark","United Kingdom")

benchWs <- filter(yearlyDf,Area %in% benchCountries,Category=="Electricity generation",Subcategory=="Aggregate fuel",Variable %in% c("Fossil","Wind and Solar"),Unit=="%")
benchWs$Variable[benchWs$Variable=="Wind and Solar"] <- "Wind + solar"

ukCoal <- filter(yearlyDf,Area=="United Kingdom",Category=="Electricity generation",Subcategory=="Fuel",Variable=="Coal",Unit=="%")
ukCoal$Variable <- "Coal"

benchHist <- bind_rows(benchWs,ukCoal)
benchHist$Country <- factor(benchHist$Area,levels=benchCountries)
benchHist$Variable <- factor(benchHist$Variable,levels=c("Fossil","Coal","Wind + solar"))

historical_context_benchmark_plot <- ggplot(benchHist,aes(Year,Value,color=Variable)) +
  geom_line(linewidth=1.25) + geom_point(size=1.7) +
  facet_wrap(~Country,ncol=1) +
  scale_color_manual(values=c("Fossil"="#6b6b6b","Coal"="#8B4513","Wind + solar"="#00843D")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20),labels=function(x) paste(x,"%",sep="")) +
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020,2025)) +
  labs(x=NULL,y="Share of electricity generation",color=NULL) +
  theme_minimal(base_size=14) +
  theme(legend.position="top",panel.grid.minor=element_blank(),strip.text=element_text(face="bold",size=12))

print(historical_context_benchmark_plot)


#main context
modelCountries <- c("Spain","Germany","Poland")

mainHist <- filter(yearlyDf,Area %in% modelCountries,Category=="Electricity generation",Subcategory=="Aggregate fuel",Variable %in% c("Fossil","Wind and Solar"),Unit=="%")
mainHist$Country <- factor(mainHist$Area,levels=modelCountries)
mainHist$Variable[mainHist$Variable=="Wind and Solar"] <- "Wind + solar"
mainHist$Variable <- factor(mainHist$Variable,levels=c("Fossil","Wind + solar"))

historical_context_main_plot <- ggplot(mainHist,aes(Year,Value,color=Variable)) +
  geom_line(linewidth=1.25) + geom_point(size=1.7) +
  facet_wrap(~Country,ncol=1) +
  scale_color_manual(values=c("Fossil"="#6b6b6b","Wind + solar"="#00843D")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20),labels=function(x) paste(x,"%",sep="")) +scale_x_continuous(breaks=c(2000,2005,2010,2015,2020,2025)) +
  labs(x=NULL,y="Share of electricity generation",color=NULL)+
  theme_minimal(base_size=14)+
  theme(legend.position="top",panel.grid.minor=element_blank(),strip.text=element_text(face="bold",size=12))
print(historical_context_main_plot)

#wind and solar data
wsData <- filter(yearlyDf,Area %in% modelCountries,Category=="Electricity generation",Subcategory=="Aggregate fuel",Variable=="Wind and Solar",Unit=="%")
wsData <- arrange(wsData,Area,Year)

##FUNCTIONS

#fixed K model
fitK <- function(countryDf,kFixed) {
  nls(Value~kFixed/(1+exp(-r*(Year-t0))),data=countryDf,start=list(r=0.20,t0=2025),algorithm="port",
      lower=c(r=0.001,t0=1990),upper=c(r=2,t0=2050),control=nls.control(maxiter=500,warnOnly=TRUE))
}

#bootstrap country
predYears <- seq(2000,2035,by=0.1)
bootReps <- 200
bootCountry <- function(countryName) {
  countryDf <- wsData[wsData$Area==countryName,]
  selectedK <- best_k$K[best_k$Country==countryName][1]
  originalFit <- gridFits[[paste(countryName,selectedK,sep="_K_")]]
  mainCoef <- coef(originalFit)
  originalR <- as.numeric(mainCoef["r"])
  originalT0 <- as.numeric(mainCoef["t0"])
  fittedVals <- fitted(originalFit)
  residualVals <- residuals(originalFit)
  bootPred <- matrix(NA,nrow=length(predYears),ncol=bootReps)
  
  for (b in 1:bootReps) {
    sampledRes <- sample(residualVals,length(residualVals),replace=TRUE)
    bootDf <- countryDf
    bootDf$Value <- fittedVals+sampledRes
    bootDf$Value <- pmax(0,pmin(bootDf$Value,selectedK-0.001))
    bootFit <- try(
      nls(Value~selectedK/(1+exp(-r*(Year-t0))),data=bootDf,start=list(r=originalR,t0=originalT0),algorithm="port",
          lower=c(r=0.001,t0=1990),upper=c(r=2,t0=2050),control=nls.control(maxiter=500,warnOnly=TRUE)),
      silent=TRUE
    )
    if (!inherits(bootFit,"try-error")) {
      bootCoef <- coef(bootFit)
      bootR <- as.numeric(bootCoef["r"])
      bootT0 <- as.numeric(bootCoef["t0"])
      bootPred[,b] <- selectedK/(1+exp(-bootR*(predYears-bootT0)))
    }
  }
  mainFit <- selectedK/(1+exp(-originalR*(predYears-originalT0)))
  data.frame(
    Country=countryName,Year=predYears,fit=mainFit,
    lower=apply(bootPred,1,quantile,probs=0.025,na.rm=TRUE),
    upper=apply(bootPred,1,quantile,probs=0.975,na.rm=TRUE),
    K=selectedK,r=originalR,t0=originalT0
  )
}

#country plot
countryPlot <- function(countryName) {
  confDf <- confidence_data[confidence_data$Country==countryName,]
  obsDf <- observed_points[observed_points$Country==countryName,]
  statDf <- best_k[best_k$Country==countryName,]
  altCurves <- sensitivity_predictions[sensitivity_predictions$Country==countryName,]
  selectedK <- statDf$K[1]
  selectedRmse <- statDf$RMSE[1]
  selectedT0 <- statDf$t0[1]
  selectedR <- statDf$r[1]
  modelLabel <- sprintf("K = %.0f%% | RMSE = %.2f pp | r = %.3f",selectedK,selectedRmse,selectedR)
  ggplot() +
    geom_ribbon(data=confDf,aes(Year,ymin=lower,ymax=upper),fill="#2ecc71",alpha=0.25) +
    geom_line(data=altCurves,aes(Year,fit,color=K_label,group=K_label),linewidth=1,linetype="dashed",alpha=0.85) +geom_line(data=confDf,aes(Year,fit),color="#00843D",linewidth=1.45)+
    geom_point(data=obsDf,aes(Year,Value),color="black",size=2) +
    geom_vline(xintercept=2025,linetype="dotted",color="gray30",linewidth=0.9)+geom_vline(xintercept=selectedT0,linetype="longdash",color="#1f78b4",linewidth=0.95) +
    annotate("label",x=selectedT0,y=101,label=sprintf("t0 = %.1f",selectedT0),color="#1f78b4",fill="white",linewidth=0.25,size=3.4)+
    annotate("label",x=2000,y=108,label=modelLabel,hjust=0,color="black",fill="white",linewidth=0.20,size=3.5) +
    scale_color_manual(values=c("K = 50%"="#d73027","K = 70%"="#f1c40f","K = 80%"="#1a9850")) +
    scale_y_continuous(limits=c(0,112),breaks=seq(0,100,20),labels=function(x) paste(x,"%",sep=""))+scale_x_continuous(breaks=c(2000,2005,2010,2015,2020,2025,2030,2035)) +
    labs(x=NULL,y="Wind and solar share of electricity generation",color="Sensitivity") +
    theme_minimal(base_size=14)+
    theme(legend.position="right",panel.grid.minor=element_blank())
}


#rolling autocorr
rollAr1 <- function(x) {
  cor(x[-length(x)],x[-1])
}

#ews for every country of interest
ewsWindow <- 36
calcEws <- function(countryName) {
  countryDf <- monthlyEws[monthlyEws$Country==countryName,]
  countryDf <- countryDf[order(countryDf$Date),]
  firstDate <- min(countryDf$Date)
  series <- ts(countryDf$Value,start=c(as.numeric(format(firstDate,"%Y")),as.numeric(format(firstDate,"%m"))),frequency=12)
  stlfit <- stl(series,s.window="periodic",robust=TRUE)
  countryDf$residual <- as.numeric(stlfit$time.series[,"remainder"])
  
  arVals <- rep(NA,nrow(countryDf))
  varVals <- rep(NA,nrow(countryDf))
  for (i in ewsWindow:nrow(countryDf)) {
    windowRes <- countryDf$residual[(i-ewsWindow+1):i]
    arVals[i] <- rollAr1(windowRes)
    varVals[i] <- var(windowRes)
  }
  countryDf$selected_t0_date <- as.Date(paste(floor(countryDf$selected_t0),"01","01",sep="-"))+
    round((countryDf$selected_t0-floor(countryDf$selected_t0))*365)
  countryDf$rolling_AR1 <- arVals
  countryDf$rolling_variance <- varVals
  countryDf
}

#model selection
kMax <- 90
gridRes <- data.frame()
gridFits <- list()

for (country in modelCountries) {
  countryDf <- wsData[wsData$Area==country,]
  kMin <- ceiling(max(countryDf$Value))+1
  kGrid <- seq(kMin,kMax,by=1)
  for (kVal in kGrid) {
    fit <- fitK(countryDf,kVal)
    fitCoef <- coef(fit)
    rmse <- sqrt(mean(residuals(fit)^2))
    gridFits[[paste(country,kVal,sep="_K_")]] <- fit
    newRow <- data.frame(
      Country=country,K=kVal,r=as.numeric(fitCoef["r"]),
      t0=as.numeric(fitCoef["t0"]),RMSE=rmse
    )
    gridRes <- bind_rows(gridRes,newRow)
  }
}

#setting up the final model
best_k <- group_by(gridRes,Country)
best_k <- slice_min(best_k,RMSE,n=1,with_ties=FALSE)
best_k <- ungroup(best_k)
selected_model_table <- mutate(best_k,K=round(K,0),r=round(r,3),t0=round(t0,2),RMSE=round(RMSE,3))
selected_model_table <- arrange(selected_model_table,factor(Country,levels=modelCountries))
print(selected_model_table)

#bootstrap
set.seed(361)
confidence_data <- bind_rows(lapply(modelCountries,bootCountry))
confidence_data$Country <- factor(confidence_data$Country,levels=modelCountries)
observed_points <- rename(wsData,Country=Area)
observed_points$Country <- factor(observed_points$Country,levels=modelCountries)

#alternative K curves
kVals <- c(50,70,80)
sensitivity_predictions <- data.frame()

for (country in modelCountries) {
  countryDf <- wsData[wsData$Area==country,]
  startR <- best_k$r[best_k$Country==country][1]
  startT0 <- best_k$t0[best_k$Country==country][1]
  for (kVal in kVals) {
    fit <- nls(
      Value~kVal/(1+exp(-r*(Year-t0))),
      data=countryDf,
      start=list(r=startR,t0=startT0),
      control=nls.control(maxiter=500)
    )
    fitCoef <- coef(fit)
    rVal <- as.numeric(fitCoef["r"])
    t0Val <- as.numeric(fitCoef["t0"])
    newCurve <- data.frame(
      Country=country,K=kVal,Year=predYears,
      fit=kVal/(1+exp(-rVal*(predYears-t0Val)))
    )
    sensitivity_predictions <- bind_rows(sensitivity_predictions,newCurve)
  }
}

sensitivity_predictions$Country <- factor(sensitivity_predictions$Country,levels=modelCountries)
sensitivity_predictions$K_label <- factor(paste("K = ",sensitivity_predictions$K,"%",sep=""),levels=c("K = 50%","K = 70%","K = 80%"))

#country plots
spain_scurve_plot <- countryPlot("Spain")
germany_scurve_plot <- countryPlot("Germany")
poland_scurve_plot <- countryPlot("Poland")
print(spain_scurve_plot)
print(germany_scurve_plot)
print(poland_scurve_plot)

#fossil displacement
annualFossil <- filter(yearlyDf,Area %in% modelCountries,Category=="Electricity generation",Subcategory=="Aggregate fuel",Variable=="Fossil",Unit=="%")
annualFossil <- rename(annualFossil,Country=Area)
annualFossil$Country <- factor(annualFossil$Country,levels=modelCountries)
annualFossil <- arrange(annualFossil,Country,Year)
fossilLabels <- best_k
fossilLabels$Country <- factor(fossilLabels$Country,levels=modelCountries)
fossilLabels$label_x <- pmin(fossilLabels$t0,2027.5)

fossil_context_plot <- ggplot(annualFossil,aes(Year,Value))+
  geom_line(color="#6b6b6b",linewidth=1.25) + geom_point(color="#6b6b6b",size=1.9) +
  geom_vline(data=fossilLabels,aes(xintercept=t0),inherit.aes=FALSE,linetype="longdash",color="#1f78b4",linewidth=0.9) +
  geom_label(data=fossilLabels,aes(x=label_x,y=96,label=sprintf("wind+solar t0 = %.1f",t0)),inherit.aes=FALSE,color="#1f78b4",fill="white",linewidth=0.25,size=3)+
  facet_wrap(~Country,ncol=1)+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20),labels=function(x) paste(x,"%",sep=""))+scale_x_continuous(limits=c(2000,2030),breaks=c(2000,2005,2010,2015,2020,2025,2030)) +
  labs(x=NULL,y="Fossil share of electricity generation") +
  theme_minimal(base_size=14) +
  theme(panel.grid.minor=element_blank(),strip.text=element_text(face="bold",size=12))

print(fossil_context_plot)

#monthly EWS
monthlyEws <- filter(monthlyDf,Area %in% modelCountries,Category=="Electricity generation",Subcategory=="Aggregate fuel",Variable=="Fossil",Unit=="%")
monthlyEws <- rename(monthlyEws,Country=Area)
monthlyEws <- left_join(monthlyEws,select(best_k,Country,selected_K=K,selected_t0=t0),by="Country")
ews_results <- bind_rows(lapply(modelCountries,calcEws))
ews_results$Country <- factor(ews_results$Country,levels=modelCountries)

arSummary <- group_by(ews_results,Country)
arSummary <- summarise(arSummary,selected_K=first(selected_K),selected_t0=first(selected_t0),mean_indicator=mean(rolling_AR1,na.rm=TRUE),last_indicator=last(rolling_AR1[!is.na(rolling_AR1)]),.groups="drop")
arSummary$Indicator <- "Rolling AR(1)"
varianceSummary <- group_by(ews_results,Country)
varianceSummary <- summarise(varianceSummary,selected_K=first(selected_K),selected_t0=first(selected_t0),mean_indicator=mean(rolling_variance,na.rm=TRUE),last_indicator=last(rolling_variance[!is.na(rolling_variance)]),.groups="drop")
varianceSummary$Indicator <- "Rolling variance"

ews_summary_table <- bind_rows(arSummary,varianceSummary)
ews_summary_table$Indicator <- factor(ews_summary_table$Indicator,levels=c("Rolling AR(1)","Rolling variance"))
ews_summary_table <- arrange(ews_summary_table,Country,Indicator)
ews_summary_table <- select(ews_summary_table,Country,Indicator,selected_K,selected_t0,mean_indicator,last_indicator)
ews_summary_table <- mutate(ews_summary_table,selected_K=round(selected_K,0),selected_t0=round(selected_t0,2),mean_indicator=round(mean_indicator,3),last_indicator=round(last_indicator,3))
print(ews_summary_table)

#EWS plots
ews_ar_plot <- ggplot(ews_results,aes(Date,rolling_AR1)) +
  geom_line(color="#00843D",linewidth=0.9) +
  geom_vline(aes(xintercept=selected_t0_date),linetype="longdash",color="#1f78b4",linewidth=0.8) +
  facet_wrap(~Country,ncol=1) +
  scale_x_date(limits=as.Date(c("2015-01-01","2030-12-31")),date_breaks="5 years",date_labels="%Y") +
  scale_y_continuous(limits=c(-0.6,0.8),breaks=seq(-0.6,0.8,0.2),labels=function(x) sprintf("%.1f",x)) +
  labs(x=NULL,y="Rolling AR(1)") +
  theme_minimal(base_size=13) +
  theme(panel.grid.minor=element_blank(),strip.text=element_text(face="bold",size=11))
print(ews_ar_plot)

ews_variance_plot <- ggplot(ews_results,aes(Date,rolling_variance)) +
  geom_line(color="#00843D",linewidth=0.9) +
  geom_vline(aes(xintercept=selected_t0_date),linetype="longdash",color="#1f78b4",linewidth=0.8) +
  facet_wrap(~Country,ncol=1,scales="free_y") +
  scale_x_date(limits=as.Date(c("2015-01-01","2030-12-31")),date_breaks="5 years",date_labels="%Y") +
  labs(x=NULL,y="Rolling variance") +
  theme_minimal(base_size=13) +
  theme(panel.grid.minor=element_blank(),strip.text=element_text(face="bold",size=11))
print(ews_variance_plot)

#final interpretation
final_interpretation_table <- select(best_k,Country,K,r,t0,RMSE)
final_interpretation_table <- mutate(
  final_interpretation_table,
  Role=case_when(
    Country=="Spain"~"Observed inflection case",
    Country=="Germany"~"Near-term inflection case",
    Country=="Poland"~"Future inflection case"
  ),
  K=round(K,0),r=round(r,3),t0=round(t0,2),RMSE=round(RMSE,3)
)

final_interpretation_table <- select(final_interpretation_table,Country,Role,K,r,t0,RMSE)
final_interpretation_table <- arrange(final_interpretation_table,factor(Country,levels=modelCountries))
print(final_interpretation_table)