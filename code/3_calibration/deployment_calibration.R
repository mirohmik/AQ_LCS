# deployment_calibration.R
# Script introduction --------------------------------
#
# Objectives : This script calibrate the sensor raw signal during its deployment
#             at the city of Zurich.
#
# Author: Horim Kim
#
# Library and Import ------------------------------------------------------

## libraries
library(MASS)
library(randomForest)
require(chron)
require(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tdr)
library(lubridate)
library(GGally)
library(tsoutliers)
library(tibbletime)
library(doBy)

setwd("C:/") # change the directory

plot_ts <- function(figname,date,yyy,window_width,
                    xrange_unixTS,yrange,xlabString,
                    ylabString,legend_str){
  
  #
  
  if(!window_width%in%c("day","week","month","year","all","all_day2day")){
    stop("Error 1 in plot function.")
  }
  
  #
  
  if(!is.matrix(yyy)){
    yyy <- matrix(yyy,ncol=1)
  }
  
  n_yyy <- dim(yyy)[2]
  
  
  # Date limits
  
  if(!is.null(xrange_unixTS)){
    xrange_date_min <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[1]
    xrange_date_max <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[2]
    
    id   <- which(date>=xrange_date_min & date<=xrange_date_max)
    n_id <- length(id)
    
    if(n_id>0){
      date <- date[id]
      yyy  <- yyy[id,]
    }else{
      stop()
    }
  }
  
  # Window width x
  
  if(is.null(window_width)){
    stop() 
  }
  if(window_width=="all"){
    intervals <- seq(strptime("20080101000000","%Y%m%d%H%M%S",tz="UTC"),strptime("20170101000000","%Y%m%d%H%M%S",tz="UTC"),length.out = 2)
  }
  if(window_width=="all_day2day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,length.out = 2)
  }
  if(window_width=="year"){
    year_first <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    year_last  <- as.numeric(strftime(max(date),"%Y",tz="UTC")) + 1
    date_first <- strptime(paste(year_first,"0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, "0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="years")
  }
  if(window_width=="month"){
    year_first  <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    month_first <- as.numeric(strftime(min(date),"%m",tz="UTC"))
    year_last   <- as.numeric(strftime(max(date),"%Y",tz="UTC"))
    month_last  <- as.numeric(strftime(max(date),"%m",tz="UTC"))
    
    if(month_last==12){
      month_last <- 1
      year_last  <- year_last + 1
    }
    
    date_first <- strptime(paste(year_first,sprintf("%02.0f",month_first),"01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, sprintf("%02.0f",month_last), "01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="months")
  }
  if(window_width=="week"){
    delta_days <- ((as.numeric(strftime(min(date),"%w",tz="UTC"))-1)+7)%%7
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") - delta_days*86400
    delta_days <- ((as.numeric(strftime(max(date),"%w",tz="UTC"))-1)+7)%%7
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + (7-delta_days)*86400
    intervals  <- seq(date_first,date_last,by="weeks")
  }
  if(window_width=="day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,by="days")
  }
  
  n_intervals <- length(intervals)-1
  
  
  # Window width y
  
  if(is.null(yrange)){
    yrange <- c(min(yyy,na.rm=T),max(yyy,na.rm=T))
  }
  
  
  # Plot
  
  def_par <- par()
  pdf(file = figname, width=12, height=6, onefile=T, pointsize=13, colormodel="srgb")
  par(mai=c(1,1,0.1,0.1))
  
  for(ith_interval in 1:n_intervals){
    id   <- which(date>=intervals[ith_interval] & date<intervals[ith_interval+1])
    n_id <- length(id)
    
    if(n_id<1){
      next
    }
    
    if(window_width=="all"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 years")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="years")
    }
    if(window_width=="all_day2day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
    }
    if(window_width=="year"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
    }
    if(window_width=="month"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="7 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="week"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="4 hours")
      xticksLab <- c(strftime(xticks[1],"%d/%m/%Y",tz="UTC"),strftime(xticks[2:length(xticks)],"%H:%M",tz="UTC"))
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="hours")
    }
    
    n_x_grids <- length(x_grid)
    
    plot(c(intervals[ith_interval],intervals[ith_interval+1]),
         c(0,0),
         ylim=yrange,
         t="l",
         col="gray50",
         lwd=1,
         lty=1,
         xaxt="n",
         xlab=xlabString,
         ylab=ylabString,
         cex.axis=1.25,
         cex.lab=1.25)
    
    for(ith_x_grid in 1:n_x_grids){
      lines(c(x_grid[ith_x_grid],x_grid[ith_x_grid]),c(-1e9,1e9),col="gray80",lwd=1,lty=1)
    }
    
    axis(side=1,at=xticks,labels=xticksLab,cex.axis=1.25,cex.lab=1.25)
    
    for(ith_yyy in 1:1){
      lines(date[id],yyy[id,ith_yyy],lty=1, lwd=1, col=ith_yyy)
    }
    
    if(n_yyy>1){
      for(ith_yyy in 2:n_yyy){
        lines(date[id],yyy[id,ith_yyy],lty=1, lwd=1, col=ith_yyy)
      }
    }
    
    par(family="mono")
    legend("topright",legend=legend_str,col=1:n_yyy,lwd=2,lty=1,cex=1.25,bg="white")
    par(family="")
    
    
  }
  
  dev.off()
  par(def_par)
  
  
}

# --------------------------------------------------------------------

cal.dp <- function(sensorunit) {
  ## Import the 1st colocation data ---------------------------
  {
  data_col <- read.csv(paste("data_output/pre_processed_1st_period/",sensorunit,".csv", sep=""), header = TRUE)
  
  data_col$X <- seq(from = 1, to = length(data_col$date) ,by = 1)
  data_col$date <- ymd_hms(data_col$date)
  
  data_col <- as_tibble(data_col)
  
  data_col <- as_tbl_time(data_col, index = date)
  }
  ## Import the raw signal during deployment -------------------
  {
  data_dp <- read.csv(paste("data_output/raw_signal_deployment/",sensorunit,".csv",sep = ""), header = TRUE)
  
  data_dp$X <- seq(from = 1, to = length(data_dp$date) ,by = 1)
  data_dp$date <- ymd_hms(data_dp$date)
  
  data_dp <- as_tibble(data_dp)
  
  data_dp <- as_tbl_time(data_dp, index = date)
  }
  ## Calibration parameter --------------------------------------
  { 
  ## Robust Regression
  RLM_K_NO  = 1.345
  RLM_K_NO2 = 1.345
  RLM_MAXIT = 1e3
  
  ##Random Forest
  nodesize <- 100
  ntree <- 1000
  }
  ## training and testing data -----------------------------
  {
  TR <- data_col
  VA <- data_dp
  }
  ## Calibration -------------------------------------------
  {
   M_data_NO_00_RLM <- rlm(REFNO~I(NO_00)+I(T_01)+I(NO_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
   M_data_NO_00_RF <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
   M_data_NO_01_RLM <- rlm(REFNO~I(NO_01)+I(T_01)+I(NO_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
   M_data_NO_01_RF <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
   M_data_NO2_00_RLM <- rlm(REFNO2~I(NO2_00)+I(T_01)+I(NO2_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
   M_data_NO2_00_RF <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
   M_data_NO2_01_RLM <- rlm(REFNO2~I(NO2_01)+I(T_01)+I(NO2_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
   M_data_NO2_01_RF <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
   
   }
  ## Prediction ---------------------------------------------
  {
    PR_M_data_NO_00_RLM <- predict(M_data_NO_00_RLM, VA)
    PR_M_data_NO_00_RF <- predict(M_data_NO_00_RF, VA)
    PR_M_data_NO_01_RLM <- predict(M_data_NO_01_RLM, VA)
    PR_M_data_NO_01_RF <- predict(M_data_NO_01_RF, VA)
    PR_M_data_NO2_00_RLM <- predict(M_data_NO2_00_RLM, VA)
    PR_M_data_NO2_00_RF <- predict(M_data_NO2_00_RF, VA)
    PR_M_data_NO2_01_RLM <- predict(M_data_NO2_01_RLM, VA)
    PR_M_data_NO2_01_RF <- predict(M_data_NO2_01_RF, VA)
  }
  ## store the results in withe index ------------------------
  {
    RE_PR_M_data_NO_00_RLM <- as.data.frame(PR_M_data_NO_00_RLM)
    RE_PR_M_data_NO_00_RLM <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM$PR_M_data_NO_00_RLM,VA$X))
  
    RE_PR_M_data_NO_00_RF <- as.data.frame(PR_M_data_NO_00_RF)
    RE_PR_M_data_NO_00_RF <- as.data.frame(cbind(RE_PR_M_data_NO_00_RF$PR_M_data_NO_00_RF,VA$X))
    
    RE_PR_M_data_NO_01_RLM <- as.data.frame(PR_M_data_NO_01_RLM)
    RE_PR_M_data_NO_01_RLM <- as.data.frame(cbind(RE_PR_M_data_NO_01_RLM$PR_M_data_NO_01_RLM,VA$X))
    
    RE_PR_M_data_NO_01_RF <- as.data.frame(PR_M_data_NO_01_RF)
    RE_PR_M_data_NO_01_RF <- as.data.frame(cbind(RE_PR_M_data_NO_01_RF$PR_M_data_NO_01_RF,VA$X))
  
    RE_PR_M_data_NO2_00_RLM <- as.data.frame(PR_M_data_NO2_00_RLM)
    RE_PR_M_data_NO2_00_RLM <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM$PR_M_data_NO2_00_RLM,VA$X))
    
    RE_PR_M_data_NO2_00_RF <- as.data.frame(PR_M_data_NO2_00_RF)
    RE_PR_M_data_NO2_00_RF <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RF$PR_M_data_NO2_00_RF,VA$X))
  
    RE_PR_M_data_NO2_01_RLM <- as.data.frame(PR_M_data_NO2_01_RLM)
    RE_PR_M_data_NO2_01_RLM <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RLM$PR_M_data_NO2_01_RLM,VA$X))
    
    RE_PR_M_data_NO2_01_RF <- as.data.frame(PR_M_data_NO2_01_RF)
    RE_PR_M_data_NO2_01_RF <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RF$PR_M_data_NO2_01_RF,VA$X))
  }
  ## Sorting ------------------------------------------------
  {
    ### NO_00
    {
    RE_PR_M_data_NO_00_RLM$date <- data_dp$date
    RE_PR_M_data_NO_00_RF$date <- data_dp$date
    
    RE_PR_M_data_NO_00_RLM <- rename(RE_PR_M_data_NO_00_RLM, "NO[ppb]"="V1")
    RE_PR_M_data_NO_00_RF <- rename(RE_PR_M_data_NO_00_RF, "NO[ppb]"="V1")

    setcolorder(RE_PR_M_data_NO_00_RLM, neworder = c("date","NO[ppb]","V2"))
    setcolorder(RE_PR_M_data_NO_00_RF, neworder = c("date","NO[ppb]","V2"))
    
    RE_PR_M_data_NO_00_RLM <- as_tibble(RE_PR_M_data_NO_00_RLM)
    RE_PR_M_data_NO_00_RLM <- as_tbl_time(RE_PR_M_data_NO_00_RLM, index = date)
    
    RE_PR_M_data_NO_00_RF <- as_tibble(RE_PR_M_data_NO_00_RF)
    RE_PR_M_data_NO_00_RF <- as_tbl_time(RE_PR_M_data_NO_00_RF, index = date)
    }
    ### NO_01
    {
      RE_PR_M_data_NO_01_RLM$date <- data_dp$date
      RE_PR_M_data_NO_01_RF$date <- data_dp$date
      
      RE_PR_M_data_NO_01_RLM <- rename(RE_PR_M_data_NO_01_RLM, "NO[ppb]"="V1")
      RE_PR_M_data_NO_01_RF <- rename(RE_PR_M_data_NO_01_RF, "NO[ppb]"="V1")
      
      setcolorder(RE_PR_M_data_NO_01_RLM, neworder = c("date","NO[ppb]","V2"))
      setcolorder(RE_PR_M_data_NO_01_RF, neworder = c("date","NO[ppb]","V2"))
      
      RE_PR_M_data_NO_01_RLM <- as_tibble(RE_PR_M_data_NO_01_RLM)
      RE_PR_M_data_NO_01_RLM <- as_tbl_time(RE_PR_M_data_NO_01_RLM, index = date)
      
      RE_PR_M_data_NO_01_RF <- as_tibble(RE_PR_M_data_NO_01_RF)
      RE_PR_M_data_NO_01_RF <- as_tbl_time(RE_PR_M_data_NO_01_RF, index = date)
    }
    ### NO2_00
    {
      RE_PR_M_data_NO2_00_RLM$date <- data_dp$date
      RE_PR_M_data_NO2_00_RF$date <- data_dp$date
      
      RE_PR_M_data_NO2_00_RLM <- rename(RE_PR_M_data_NO2_00_RLM, "NO2[ppb]"="V1")
      RE_PR_M_data_NO2_00_RF <- rename(RE_PR_M_data_NO2_00_RF, "NO2[ppb]"="V1")
      
      setcolorder(RE_PR_M_data_NO2_00_RLM, neworder = c("date","NO2[ppb]","V2"))
      setcolorder(RE_PR_M_data_NO2_00_RF, neworder = c("date","NO2[ppb]","V2"))
      
      RE_PR_M_data_NO2_00_RLM <- as_tibble(RE_PR_M_data_NO2_00_RLM)
      RE_PR_M_data_NO2_00_RLM <- as_tbl_time(RE_PR_M_data_NO2_00_RLM, index = date)
      
      RE_PR_M_data_NO2_00_RF <- as_tibble(RE_PR_M_data_NO2_00_RF)
      RE_PR_M_data_NO2_00_RF <- as_tbl_time(RE_PR_M_data_NO2_00_RF, index = date)
    }
    ### NO2_01
    {
      RE_PR_M_data_NO2_01_RLM$date <- data_dp$date
      RE_PR_M_data_NO2_01_RF$date <- data_dp$date
      
      RE_PR_M_data_NO2_01_RLM <- rename(RE_PR_M_data_NO2_01_RLM, "NO2[ppb]"="V1")
      RE_PR_M_data_NO2_01_RF <- rename(RE_PR_M_data_NO2_01_RF, "NO2[ppb]"="V1")
      
      setcolorder(RE_PR_M_data_NO2_01_RLM, neworder = c("date","NO2[ppb]","V2"))
      setcolorder(RE_PR_M_data_NO2_01_RF, neworder = c("date","NO2[ppb]","V2"))
      
      RE_PR_M_data_NO2_01_RLM <- as_tibble(RE_PR_M_data_NO2_01_RLM)
      RE_PR_M_data_NO2_01_RLM <- as_tbl_time(RE_PR_M_data_NO2_01_RLM, index = date)
      
      RE_PR_M_data_NO2_01_RF <- as_tibble(RE_PR_M_data_NO2_01_RF)
      RE_PR_M_data_NO2_01_RF <- as_tbl_time(RE_PR_M_data_NO2_01_RF, index = date)
    }
    
    
  }
  ## Data store in data_output ------------------------------
  {
  write.csv(RE_PR_M_data_NO_00_RLM, paste("data_output/calibrated_deployment/",sensorunit,"_NO_00_RLM.csv",sep=""))
  write.csv(RE_PR_M_data_NO_00_RF, paste("data_output/calibrated_deployment/",sensorunit,"_NO_00_RF.csv",sep=""))
  write.csv(RE_PR_M_data_NO_01_RLM, paste("data_output/calibrated_deployment/",sensorunit,"_NO_01_RLM.csv",sep=""))
  write.csv(RE_PR_M_data_NO_01_RF, paste("data_output/calibrated_deployment/",sensorunit,"_NO_01_RF.csv",sep=""))
  write.csv(RE_PR_M_data_NO2_00_RLM, paste("data_output/calibrated_deployment/",sensorunit,"_NO2_00_RLM.csv",sep=""))
  write.csv(RE_PR_M_data_NO2_00_RF, paste("data_output/calibrated_deployment/",sensorunit,"_NO2_00_RF.csv",sep=""))
  write.csv(RE_PR_M_data_NO2_01_RLM, paste("data_output/calibrated_deployment/",sensorunit,"_NO2_01_RLM.csv",sep=""))
  write.csv(RE_PR_M_data_NO2_01_RF, paste("data_output/calibrated_deployment/",sensorunit,"_NO2_01_RF.csv",sep=""))
  
  }
  ## Timeseries plotting in figure/deployment/timeseries -----
  {
    data_NO_00_RLM <- RE_PR_M_data_NO_00_RLM
    data_NO_00_RF <- RE_PR_M_data_NO_00_RF
    data_NO_01_RLM <- RE_PR_M_data_NO_01_RLM
    data_NO_01_RF <- RE_PR_M_data_NO_01_RF
    data_NO2_00_RLM <- RE_PR_M_data_NO2_00_RLM
    data_NO2_00_RF <- RE_PR_M_data_NO2_00_RF
    data_NO2_01_RLM <- RE_PR_M_data_NO2_01_RLM
    data_NO2_01_RF <- RE_PR_M_data_NO2_01_RF
    
    ### Binding the data with date and timestamp
    {
      ### NO_00 --------------------
      {
        data_NO_00 <- as.data.frame(cbind(data_NO_00_RLM$`NO[ppb]`,data_NO_00_RF$`NO[ppb]`))
        data_NO_00 <- round(data_NO_00, digits = 2)
        data_NO_00 <- as.data.frame(cbind(data_NO_00_RLM$date,data_NO_00))
        data_NO_00 <- rename(data_NO_00,"date"="data_NO_00_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO_00$date <- ymd_hms(data_NO_00$date)
        data_NO_00$timestamp <- as.numeric(as.POSIXct(data_NO_00$date))
        setcolorder(data_NO_00, neworder = c("date","timestamp","RLM","RF"))
      }
      ### NO_01 --------------------
      {
        data_NO_01 <- as.data.frame(cbind(data_NO_01_RLM$`NO[ppb]`,data_NO_01_RF$`NO[ppb]`))
        data_NO_01 <- round(data_NO_01, digits = 2)
        data_NO_01 <- as.data.frame(cbind(data_NO_01_RLM$date,data_NO_01))
        data_NO_01 <- rename(data_NO_01,"date"="data_NO_01_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO_01$date <- ymd_hms(data_NO_01$date)
        data_NO_01$timestamp <- as.numeric(as.POSIXct(data_NO_01$date))
        setcolorder(data_NO_01, neworder = c("date","timestamp","RLM","RF"))
      }
      ### NO2_00 --------------------
      {
        data_NO2_00 <- as.data.frame(cbind(data_NO2_00_RLM$`NO2[ppb]`,data_NO2_00_RF$`NO2[ppb]`))
        data_NO2_00 <- round(data_NO2_00, digits = 2)
        data_NO2_00 <- as.data.frame(cbind(data_NO2_00_RLM$date,data_NO2_00))
        data_NO2_00 <- rename(data_NO2_00,"date"="data_NO2_00_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO2_00$date <- ymd_hms(data_NO2_00$date)
        data_NO2_00$timestamp <- as.numeric(as.POSIXct(data_NO2_00$date))
        setcolorder(data_NO2_00, neworder = c("date","timestamp","RLM","RF"))
      }
      ### NO2_01 --------------------
      {
        data_NO2_01 <- as.data.frame(cbind(data_NO2_01_RLM$`NO2[ppb]`,data_NO2_01_RF$`NO2[ppb]`))
        data_NO2_01 <- round(data_NO2_01, digits = 2)
        data_NO2_01 <- as.data.frame(cbind(data_NO2_01_RLM$date,data_NO2_01))
        data_NO2_01 <- rename(data_NO2_01,"date"="data_NO2_01_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO2_01$date <- ymd_hms(data_NO2_01$date)
        data_NO2_01$timestamp <- as.numeric(as.POSIXct(data_NO2_01$date))
        setcolorder(data_NO2_01, neworder = c("date","timestamp","RLM","RF"))
      }
    }
    
    ### NO_00
    {
      save_loc <- "figure/deployment/timeseries/"
      
      figname <- paste(save_loc ,sensorunit,"_NO_00.pdf",sep="")
      
      yyy     <- as.matrix(data_NO_00[,3:dim(data_NO_00)[2]])
      
      xlabString <- "Date [UTC]"
      ylabString <- paste("NO [ppb]")
      legend_str <- c(colnames(data_NO_00)[3:dim(data_NO_00)[2]])
      plot_ts(figname,data_NO_00$date,yyy,"week",NULL,c(-20,300),xlabString,ylabString,legend_str)
      dev.off()
    }
    ### NO_01
    {
      save_loc <- "figure/deployment/timeseries/"
      
      figname <- paste(save_loc ,sensorunit,"_NO_01.pdf",sep="")
      
      yyy     <- as.matrix(data_NO_01[,3:dim(data_NO_01)[2]])
      
      xlabString <- "Date [UTC]"
      ylabString <- paste("NO [ppb]")
      legend_str <- c(colnames(data_NO_01)[3:dim(data_NO_01)[2]])
      plot_ts(figname,data_NO_01$date,yyy,"week",NULL,c(-20,300),xlabString,ylabString,legend_str)
      dev.off()
    }
    ### NO2_00
    {
      save_loc <- "figure/deployment/timeseries/"
      
      figname <- paste(save_loc ,sensorunit,"_NO2_00.pdf",sep="")
      
      yyy     <- as.matrix(data_NO2_00[,3:dim(data_NO2_00)[2]])
      
      xlabString <- "Date [UTC]"
      ylabString <- paste("NO2 [ppb]")
      legend_str <- c(colnames(data_NO2_00)[3:dim(data_NO2_00)[2]])
      plot_ts(figname,data_NO2_00$date,yyy,"week",NULL,c(-20,300),xlabString,ylabString,legend_str)
      dev.off()
    }
    ### NO2_01
    {
      save_loc <- "figure/deployment/timeseries/"
      
      figname <- paste(save_loc ,sensorunit,"_NO2_01.pdf",sep="")
      
      yyy     <- as.matrix(data_NO2_01[,3:dim(data_NO2_01)[2]])
      
      xlabString <- "Date [UTC]"
      ylabString <- paste("NO2 [ppb]")
      legend_str <- c(colnames(data_NO2_01)[3:dim(data_NO2_01)[2]])
      plot_ts(figname,data_NO2_01$date,yyy,"week",NULL,c(-20,300),xlabString,ylabString,legend_str)
      dev.off()
    }
    
  }
  
  ## Return values for overall plot ---------------------------
  {r.list <- list(data_NO_00,
                  data_NO_01,
                  data_NO2_00,
                  data_NO2_01)
  return(r.list)
  }
  
}

AC9_func_res <- cal.dp("AC9")
AC10_func_res <- cal.dp("AC10")
AC11_func_res <- cal.dp("AC11")
AC12_func_res <- cal.dp("AC12")

# --------------------------------------------------------------------

## Return values for using in plotting the fig.7
{
  AC9_NO2_00 <- na.omit(as.data.frame(AC9_func_res[[3]]))
  AC9_NO2_01 <- na.omit(as.data.frame(AC9_func_res[[4]]))
  AC10_NO2_00 <- na.omit(as.data.frame(AC10_func_res[[3]]))
  AC10_NO2_01 <- na.omit(as.data.frame(AC10_func_res[[4]]))
  AC11_NO2_00 <- na.omit(as.data.frame(AC11_func_res[[3]]))
  AC11_NO2_01 <- na.omit(as.data.frame(AC11_func_res[[4]]))
  AC12_NO2_00 <- na.omit(as.data.frame(AC12_func_res[[3]]))
  AC12_NO2_01 <- na.omit(as.data.frame(AC12_func_res[[4]]))
}

# PS data import

PS_NO2 <- read.csv("data_input/PSdata.csv", header = TRUE)

# Bi-weekly averaging of sensor data
{
  ## AC9
  ### NO2_00 -----------------------
  {
    AC9_NO2_00$labeling <- 0
    
    for (i in 1:43596)
    {
      for (j in c(1:24))
      {
        if (AC9_NO2_00$timestamp[i] >= PS_NO2$start_ts[j] && AC9_NO2_00$timestamp[i] <= PS_NO2$end_ts[j])
        {
          AC9_NO2_00$labeling[i] <- str_c("Group_",letters[j])
        }
      }
    }
    
    AC9_NO2_00_PS_RLM <- summaryBy(RLM~labeling, data = AC9_NO2_00, FUN=c(mean))
    AC9_NO2_00_PS_RF <- summaryBy(RF~labeling, data = AC9_NO2_00, FUN=c(mean))
    
    AC9_NO2_00_PS_comparison <- data.table(PS_NO2$Start,PS_NO2$End,
                                           PS_NO2$ZRIS.ppb.,
                                           AC9_NO2_00_PS_RLM$RLM.mean,
                                           AC9_NO2_00_PS_RF$RF.mean)
    
    AC9_NO2_00_PS_comparison <- rename(AC9_NO2_00_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
  }
  ### NO2_01 -----------------------
  {
    AC9_NO2_01$labeling <- 0
    
    for (i in 1:43596)
    {
      for (j in c(1:24))
      {
        if (AC9_NO2_01$timestamp[i] >= PS_NO2$start_ts[j] && AC9_NO2_01$timestamp[i] <= PS_NO2$end_ts[j])
        {
          AC9_NO2_01$labeling[i] <- str_c("Group_",letters[j])
        }
      }
    }
    
    AC9_NO2_01_PS_RLM <- summaryBy(RLM~labeling, data = AC9_NO2_01, FUN=c(mean))
    AC9_NO2_01_PS_RF <- summaryBy(RF~labeling, data = AC9_NO2_01, FUN=c(mean))
    
    AC9_NO2_01_PS_comparison <- data.table(PS_NO2$Start,PS_NO2$End,
                                           PS_NO2$ZRIS.ppb.,
                                           AC9_NO2_01_PS_RLM$RLM.mean,
                                           AC9_NO2_01_PS_RF$RF.mean)
    AC9_NO2_01_PS_comparison <- rename(AC9_NO2_01_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
    
  }
  ## AC10
  ### NO2_00 -----------------------
  {
    AC10_NO2_00$labeling <- 0
    
    for (i in 1:38417)
    {
      for (j in c(1:22))
      {
        if (AC10_NO2_00$timestamp[i] >= PS_NO2$start_ts[j] && AC10_NO2_00$timestamp[i] <= PS_NO2$end_ts[j])
        {
          AC10_NO2_00$labeling[i] <- str_c("Group_",letters[j])
        }
      }
    }
    
    AC10_NO2_00_PS_RLM <- summaryBy(RLM~labeling, data = AC10_NO2_00, FUN=c(mean))
    AC10_NO2_00_PS_RF <- summaryBy(RF~labeling, data = AC10_NO2_00, FUN=c(mean))
    
    AC10_NO2_00_PS_comparison <- data.table(PS_NO2$Start[1:22],PS_NO2$End[1:22],
                                            PS_NO2$ZBLG.ppb.[1:22],
                                            AC10_NO2_00_PS_RLM$RLM.mean,
                                            AC10_NO2_00_PS_RF$RF.mean)
    
    AC10_NO2_00_PS_comparison <- rename(AC10_NO2_00_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
  }
  ### NO2_01 -----------------------
  {
    AC10_NO2_01$labeling <- 0
    
    for (i in 1:38417)
    {
      for (j in c(1:22))
      {
        if (AC10_NO2_01$timestamp[i] >= PS_NO2$start_ts[j] && AC10_NO2_01$timestamp[i] <= PS_NO2$end_ts[j])
        {
          AC10_NO2_01$labeling[i] <- str_c("Group_",letters[j])
        }
      }
    }
    
    AC10_NO2_01_PS_RLM <- summaryBy(RLM~labeling, data = AC10_NO2_01, FUN=c(mean))
    AC10_NO2_01_PS_RF <- summaryBy(RF~labeling, data = AC10_NO2_01, FUN=c(mean))
    
    AC10_NO2_01_PS_comparison <- data.table(PS_NO2$Start[1:22],PS_NO2$End[1:22],
                                            PS_NO2$ZBLG.ppb.[1:22],
                                            AC10_NO2_01_PS_RLM$RLM.mean,
                                            AC10_NO2_01_PS_RF$RF.mean)
    AC10_NO2_01_PS_comparison <- rename(AC10_NO2_01_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
    
  }
  ## AC11
  ### NO2_00 -----------------------
  {
    AC11_NO2_00$labeling <- 0
    
    for (i in 1:32050)
    {
      for (j in c(1:18))
      {
        if (AC11_NO2_00$timestamp[i] >= PS_NO2$start_ts[j] && AC11_NO2_00$timestamp[i] <= PS_NO2$end_ts[j])
        {
          AC11_NO2_00$labeling[i] <- str_c("Group_",letters[j])
        }
      }
    }
    
    AC11_NO2_00_PS_RLM <- summaryBy(RLM~labeling, data = AC11_NO2_00, FUN=c(mean))
    AC11_NO2_00_PS_RF <- summaryBy(RF~labeling, data = AC11_NO2_00, FUN=c(mean))
    
    AC11_NO2_00_PS_comparison <- data.table(PS_NO2$Start[1:18],PS_NO2$End[1:18],
                                            PS_NO2$ZSBS.ppb.[1:18],
                                            AC11_NO2_00_PS_RLM$RLM.mean,
                                            AC11_NO2_00_PS_RF$RF.mean)
    
    AC11_NO2_00_PS_comparison <- rename(AC11_NO2_00_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
  }
  ### NO2_01 -----------------------
  {
    AC11_NO2_01$labeling <- 0
    
    for (i in 1:32050)
    {
      for (j in c(1:18))
      {
        if (AC11_NO2_01$timestamp[i] >= PS_NO2$start_ts[j] && AC11_NO2_01$timestamp[i] <= PS_NO2$end_ts[j])
        {
          AC11_NO2_01$labeling[i] <- str_c("Group_",letters[j])
        }
      }
    }
    
    AC11_NO2_01_PS_RLM <- summaryBy(RLM~labeling, data = AC11_NO2_01, FUN=c(mean))
    AC11_NO2_01_PS_RF <- summaryBy(RF~labeling, data = AC11_NO2_01, FUN=c(mean))
    
    AC11_NO2_01_PS_comparison <- data.table(PS_NO2$Start[1:18],PS_NO2$End[1:18],
                                            PS_NO2$ZSBS.ppb.[1:18],
                                            AC11_NO2_01_PS_RLM$RLM.mean,
                                            AC11_NO2_01_PS_RF$RF.mean)
    AC11_NO2_01_PS_comparison <- rename(AC11_NO2_01_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
    
  }
  ## AC12
  ### NO2_00 -----------------------
  {
    AC12_NO2_00$labeling <- 0
    
    for (i in 1:43445)
    {
      for (j in c(1:24))
      {
        if (AC12_NO2_00$timestamp[i] >= PS_NO2$start_ts[j] && AC12_NO2_00$timestamp[i] <= PS_NO2$end_ts[j])
        {
          AC12_NO2_00$labeling[i] <- str_c("Group_",letters[j])
        }
      }
    }
    
    AC12_NO2_00_PS_RLM <- summaryBy(RLM~labeling, data = AC12_NO2_00, FUN=c(mean))
    AC12_NO2_00_PS_RF <- summaryBy(RF~labeling, data = AC12_NO2_00, FUN=c(mean))
    
    AC12_NO2_00_PS_comparison <- data.table(PS_NO2$Start,PS_NO2$End,
                                            PS_NO2$ZMAN.ppb.,
                                            AC12_NO2_00_PS_RLM$RLM.mean,
                                            AC12_NO2_00_PS_RF$RF.mean)
    
    AC12_NO2_00_PS_comparison <- rename(AC12_NO2_00_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
  }
  ### NO2_01 -----------------------
  {
    AC12_NO2_01$labeling <- 0
    
    for (i in 1:43445)
    {
      for (j in c(1:24))
      {
        if (AC12_NO2_01$timestamp[i] >= PS_NO2$start_ts[j] && AC12_NO2_01$timestamp[i] <= PS_NO2$end_ts[j])
        {
          AC12_NO2_01$labeling[i] <- str_c("Group_",letters[j])
        }
      }
    }
    
    AC12_NO2_01_PS_RLM <- summaryBy(RLM~labeling, data = AC12_NO2_01, FUN=c(mean))
    AC12_NO2_01_PS_RF <- summaryBy(RF~labeling, data = AC12_NO2_01, FUN=c(mean))
    
    AC12_NO2_01_PS_comparison <- data.table(PS_NO2$Start,PS_NO2$End,
                                            PS_NO2$ZMAN.ppb.,
                                            AC12_NO2_01_PS_RLM$RLM.mean,
                                            AC12_NO2_01_PS_RF$RF.mean)
    AC12_NO2_01_PS_comparison <- rename(AC12_NO2_01_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
    
  }
}

## rendering for plotting
{
  
  AC10_NO2_00_PS_comparison <- AC10_NO2_00_PS_comparison %>% add_row(AC9_NO2_00_PS_comparison[23:24,])
  AC10_NO2_00_PS_comparison$RLM[23:24] <- NA
  AC10_NO2_00_PS_comparison$RF[23:24] <- NA
  AC10_NO2_00_PS_comparison$Reference[23:24] <- NA
  
  AC10_NO2_01_PS_comparison <- AC10_NO2_01_PS_comparison %>% add_row(AC9_NO2_01_PS_comparison[23:24,])
  AC10_NO2_01_PS_comparison$RLM[23:24] <- NA
  AC10_NO2_01_PS_comparison$RF[23:24] <- NA
  AC10_NO2_01_PS_comparison$Reference[23:24] <- NA
  
  AC11_NO2_00_PS_comparison <- AC11_NO2_00_PS_comparison %>% add_row(AC9_NO2_00_PS_comparison[19:24,])
  AC11_NO2_00_PS_comparison$RLM[19:24] <- NA
  AC11_NO2_00_PS_comparison$RF[19:24] <- NA
  AC11_NO2_00_PS_comparison$Reference[19:24] <- NA
  
  AC11_NO2_01_PS_comparison <- AC11_NO2_01_PS_comparison %>% add_row(AC9_NO2_01_PS_comparison[19:24,])
  AC11_NO2_01_PS_comparison$RLM[19:24] <- NA
  AC11_NO2_01_PS_comparison$RF[19:24] <- NA
  AC11_NO2_01_PS_comparison$Reference[19:24] <- NA
  
  
  ## data for plotting ---------------
  
  forplot_PS <- as.data.frame(matrix(0,384,4))
  forplot_PS <- rename(forplot_PS, "Sensor" = "V1", "Model" = "V2", "Reference"="V3", "Calibrated"="V4")
  
  forplot_PS$Sensor[1:96] <- as.character("AC009")
  forplot_PS$Sensor[97:192] <- as.character("AC010")
  forplot_PS$Sensor[193:288] <- as.character("AC011")
  forplot_PS$Sensor[289:384] <- as.character("AC012")
  
  vrlm <- as.vector(rep("RLM",24))
  vrf <- as.vector(rep("RF",24))
  vt <- as.vector(c(vrlm,vrf,vrlm,vrf))
  vtotal <- as.vector(rep(vt,4))
  
  forplot_PS$Model <- vtotal
  
  vrefac9 <- as.vector(rep(AC9_NO2_00_PS_comparison$Reference,4))
  vrefac10 <- as.vector(rep(AC10_NO2_00_PS_comparison$Reference,4))
  vrefac11 <- as.vector(rep(AC11_NO2_00_PS_comparison$Reference,4))
  vrefac12 <- as.vector(rep(AC12_NO2_00_PS_comparison$Reference,4))
  
  forplot_PS$Reference[1:96] <- vrefac9
  forplot_PS$Reference[97:192] <- vrefac10
  forplot_PS$Reference[193:288] <- vrefac11
  forplot_PS$Reference[289:384] <- vrefac12
  
  forplot_PS$Calibrated[1:24] <- AC9_NO2_00_PS_comparison$RLM
  forplot_PS$Calibrated[25:48] <- AC9_NO2_00_PS_comparison$RF
  forplot_PS$Calibrated[49:72] <- AC9_NO2_01_PS_comparison$RLM
  forplot_PS$Calibrated[73:96] <- AC9_NO2_01_PS_comparison$RF
  forplot_PS$Calibrated[97:120] <- AC10_NO2_00_PS_comparison$RLM
  forplot_PS$Calibrated[121:144] <- AC10_NO2_00_PS_comparison$RF
  forplot_PS$Calibrated[145:168] <- AC10_NO2_01_PS_comparison$RLM
  forplot_PS$Calibrated[169:192] <- AC10_NO2_01_PS_comparison$RF
  forplot_PS$Calibrated[193:216] <- AC11_NO2_00_PS_comparison$RLM
  forplot_PS$Calibrated[217:240] <- AC11_NO2_00_PS_comparison$RF
  forplot_PS$Calibrated[241:264] <- AC11_NO2_01_PS_comparison$RLM
  forplot_PS$Calibrated[265:288] <- AC11_NO2_01_PS_comparison$RF
  forplot_PS$Calibrated[289:312] <- AC12_NO2_00_PS_comparison$RLM
  forplot_PS$Calibrated[313:336] <- AC12_NO2_00_PS_comparison$RF
  forplot_PS$Calibrated[337:360] <- AC12_NO2_01_PS_comparison$RLM
  forplot_PS$Calibrated[361:384] <- AC12_NO2_01_PS_comparison$RF
  
  forplot_PS <- na.omit(forplot_PS)
}

jpeg("figure/fig_7.jpeg", width = 15, height = 12, units = 'cm', res = 1000)
ggplot(forplot_PS, aes(Reference, Calibrated, color= Sensor)
)+geom_point(mapping = aes(fill=Sensor, shape=Model) ,size=1.6
)+geom_abline(intercept=0, slope=1, col="red"
)+xlab("Passive sampler concentration [ppb]"
)+ylab("Sensor concentration [ppb]"
)+xlim(c(0,50))+ylim(c(0,50)
)+ theme(axis.text = element_text(size = 6)
)+ theme(axis.title = element_text(size = 4)
)+ theme(plot.title = element_text(size = 5)
)+ theme(legend.text = element_text(size = 6)
)+ theme(legend.title = element_text(size = 5))+theme_bw()+
  guides(fill = guide_legend(override.aes=list(shape=21,size=4)))+ guides(shape = guide_legend(override.aes = list(size=4)))+scale_shape_manual(values=c(21,3))
dev.off()
