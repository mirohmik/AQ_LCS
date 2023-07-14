# 2nd_colocation_calibration.R
#
# Objectives : This script apply calibration model that has been developed in
#              the first colocation campaign to the period of sensor relocation
#              in the colocation site after the deployment period.
#
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
library(caret)
library(lattice)
library(ggforce)
library(ggrepel)

setwd("C:/") # change the directory

## Codes for creating time-series (Author : Michael Muller) --------------------------------
plot_ts <- function(figname,date,yyy,window_width,xrange_unixTS,yrange,xlabString,ylabString,legend_str){
  
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
  pdf(file = figname, width=16, height=8, onefile=T, pointsize=12, colormodel="srgb")
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
plot_ts_2YAxes <- function(figname,date,yyy1,yyy2,window_width,xrange_unixTS,yrange1,yrange2,xlabString,ylabString1,ylabString2,legend_str){
  
  #
  
  if(!window_width%in%c("day","week","month","year","all","all_day2day")){
    stop("Error 1 in plot function.")
  }
  
  #
  
  if(!is.matrix(yyy1)){
    yyy1 <- matrix(yyy1,ncol=1)
  }
  
  if(!is.matrix(yyy2)){
    yyy2 <- matrix(yyy2,ncol=1)
  }
  
  if(dim(yyy1)[2]>4){
    stop()
  }
  if(dim(yyy2)[2]>4){
    stop()
  }
  
  n_yyy1 <- dim(yyy1)[2]
  n_yyy2 <- dim(yyy2)[2]
  
  
  # Date limits
  
  if(!is.null(xrange_unixTS)){
    xrange_date_min <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[1]
    xrange_date_max <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[2]
    
    id   <- which(date>=xrange_date_min & date<=xrange_date_max)
    n_id <- length(id)
    
    if(n_id>0){
      date <- date[id]
      yyy1 <- yyy1[id,]
      yyy2 <- yyy2[id,]
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
  
  if(is.null(yrange1)){
    yrange1 <- c(min(yyy1,na.rm=T),max(yyy1,na.rm=T))
  }
  if(is.null(yrange2)){
    yrange2 <- c(min(yyy2,na.rm=T),max(yyy2,na.rm=T))
  }
  
  # Plot
  
  def_par <- par()
  pdf(file = figname, width=16, height=8, onefile=T, pointsize=14, colormodel="srgb")
  par(mai=c(1,1,0.1,1))
  
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
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="hours")
    }
    
    n_x_grids <- length(x_grid)
    
    plot(c(intervals[ith_interval],intervals[ith_interval+1]),
         c(0,0),
         ylim=yrange1,
         t="l",
         col="gray50",
         lwd=1,
         lty=1,
         xaxt="n",
         xlab=xlabString,
         ylab=ylabString1,
         cex.axis=1.25,
         cex.lab=1.25)
    
    for(ith_x_grid in 1:n_x_grids){
      lines(c(x_grid[ith_x_grid],x_grid[ith_x_grid]),c(-1e9,1e9),col="gray80",lwd=1,lty=1)
    }
    
    axis(side=1,at=xticks,labels=xticksLab,cex.axis=1.25,cex.lab=1.25)
    
    for(ith_yyy1 in 1:n_yyy1){
      lines(date[id],yyy1[id,ith_yyy1],lty=1, lwd=1, col=ith_yyy1)
    }
    
    par(new=T)
    
    plot(c(intervals[ith_interval],intervals[ith_interval+1]),
         c(0,0),
         ylim=yrange2,
         t="l",
         col="gray50",
         lwd=1,
         lty=1,
         xaxt="n",
         xlab="",
         ylab="",
         xaxt="n",
         yaxt="n")
    
    for(ith_yyy2 in 1:n_yyy2){
      lines(date[id],yyy2[id,ith_yyy2],lty=1, lwd=1, col=4+ith_yyy2)
    }
    
    mtext(text = ylabString2, side = 4,line = 2.5,cex=1.25)
    axis(side = 4,cex.lab=1.25,cex.axis=1.25)
    
    par(family="mono")
    legend("topright",legend=legend_str,col=(c(1:n_yyy1,4+(1:n_yyy2)))[1:length(legend_str)],lwd=2,lty=1,cex=1.25,bg="white")
    par(family="")
    
  }
  
  dev.off()
  par(def_par)
  
  
}


# Function ------------------------------------------------

reloc.cal <- function(sensorunit){
  ## import 1st colocation period data ---------------------------------------
  {
   data_1st <- read.csv(paste("data_output/pre_processed_1st_period/",sensorunit,".csv",sep = ""), header = TRUE)
  
   data_1st$X <- seq(from = 1, to = length(data_1st$date) ,by = 1)
   data_1st$date <- ymd_hms(data_1st$date)
  
   data_1st <- as_tibble(data_1st)
  
   data_1st <- as_tbl_time(data_1st, index = date)
  }
  ## import 2nd colocation period data ---------------------------------------
  {
    data_2nd <- read.csv(paste("data_output/pre_processed_2nd_period/",sensorunit,".csv",sep = ""), header = TRUE)
    
    data_2nd$X <- seq(from = 1, to = length(data_2nd$date) ,by = 1)
    data_2nd$date <- ymd_hms(data_2nd$date)
    
    data_2nd <- as_tibble(data_2nd)
    
    data_2nd <- as_tbl_time(data_2nd, index = date)
  }
  # Calibration parameter -----------------------------------------------------
  {
  ## Robust Regression
  RLM_K_NO  = 1.345
  RLM_K_NO2 = 1.345
  RLM_MAXIT = 1e3
  
  ##Random Forest
  nodesize <- 100
  ntree <- 1000
  }
  # set training and testing data --------------------------------------------
  {
  TR <- data_1st
  VA <- data_2nd
  }
  ## Calibration -------------------------------------------------------------
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
  ## Prediction ---------------------------------------------------------------
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
  ## store the results in withe index -----------------------------------------
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
  ## Sorting ------------------------------------------------------------------
  {
    ### NO_00
    {
      RE_PR_M_data_NO_00_RLM$date <- data_2nd$date
      RE_PR_M_data_NO_00_RF$date <- data_2nd$date
      
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
      RE_PR_M_data_NO_01_RLM$date <- data_2nd$date
      RE_PR_M_data_NO_01_RF$date <- data_2nd$date
      
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
      RE_PR_M_data_NO2_00_RLM$date <- data_2nd$date
      RE_PR_M_data_NO2_00_RF$date <- data_2nd$date
      
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
      RE_PR_M_data_NO2_01_RLM$date <- data_2nd$date
      RE_PR_M_data_NO2_01_RF$date <- data_2nd$date
      
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
  ## Data store in data_output ------------------------------------------------
  {
    write.csv(RE_PR_M_data_NO_00_RLM, paste("data_output/calibrated_2nd/",sensorunit,"_NO_00_RLM.csv",sep=""))
    write.csv(RE_PR_M_data_NO_00_RF, paste("data_output/calibrated_2nd/",sensorunit,"_NO_00_RF.csv",sep=""))
    write.csv(RE_PR_M_data_NO_01_RLM, paste("data_output/calibrated_2nd/",sensorunit,"_NO_01_RLM.csv",sep=""))
    write.csv(RE_PR_M_data_NO_01_RF, paste("data_output/calibrated_2nd/",sensorunit,"_NO_01_RF.csv",sep=""))
    write.csv(RE_PR_M_data_NO2_00_RLM, paste("data_output/calibrated_2nd/",sensorunit,"_NO2_00_RLM.csv",sep=""))
    write.csv(RE_PR_M_data_NO2_00_RF, paste("data_output/calibrated_2nd/",sensorunit,"_NO2_00_RF.csv",sep=""))
    write.csv(RE_PR_M_data_NO2_01_RLM, paste("data_output/calibrated_2nd/",sensorunit,"_NO2_01_RLM.csv",sep=""))
    write.csv(RE_PR_M_data_NO2_01_RF, paste("data_output/calibrated_2nd/",sensorunit,"_NO2_01_RF.csv",sep=""))

  }
  ## Timeseries with raw signal to detect the sensor malfunction period ------
  {
    ### Rename the sensor _00 -> _A, _01 -> _B
    {
      data_2nd <- rename(data_2nd, "NO_A" = "NO_00", "NO_B" = "NO_01", 
                         "NO2_A"="NO2_00", "NO2_B"="NO2_01", "RH_B"="RH_01", 
                         "V_A"="V_00")
    }
    ### Bind the calibrated data with raw signal
    {  
      ### NO_A --------------------
      {
        data_NO_A <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM$`NO[ppb]`,RE_PR_M_data_NO_00_RF$`NO[ppb]`))
        data_NO_A <- round(data_NO_A, digits = 2)
        data_NO_A <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM$date,data_NO_A))
        data_NO_A <- rename(data_NO_A,"Date[UTC]"="RE_PR_M_data_NO_00_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO_A$`Date[UTC]` <- ymd_hms(data_NO_A$`Date[UTC]`)
        data_NO_A$Reference <- data_2nd$REFNO
        data_NO_A$raw_NO_A <- data_2nd$NO_A
        data_NO_A$raw_RH <- data_2nd$RH_B
        data_NO_A$raw_V <- data_2nd$V_A
      }
      ### NO_B --------------------
      {
        data_NO_B <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM$`NO[ppb]`,RE_PR_M_data_NO_00_RF$`NO[ppb]`))
        data_NO_B <- round(data_NO_B, digits = 2)
        data_NO_B <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM$date,data_NO_B))
        data_NO_B <- rename(data_NO_B,"Date[UTC]"="RE_PR_M_data_NO_00_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO_B$`Date[UTC]` <- ymd_hms(data_NO_B$`Date[UTC]`)
        data_NO_B$Reference <- data_2nd$REFNO
        data_NO_B$raw_NO_B <- data_2nd$NO_B
        data_NO_B$raw_RH <- data_2nd$RH_B
        data_NO_B$raw_V <- data_2nd$V_A
      }
      ### NO2_A --------------------
      {
        data_NO2_A <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM$`NO2[ppb]`,RE_PR_M_data_NO2_00_RF$`NO2[ppb]`))
        data_NO2_A <- round(data_NO2_A, digits = 2)
        data_NO2_A <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM$date,data_NO2_A))
        data_NO2_A <- rename(data_NO2_A,"Date[UTC]"="RE_PR_M_data_NO2_00_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO2_A$`Date[UTC]` <- ymd_hms(data_NO2_A$`Date[UTC]`)
        data_NO2_A$Reference <- data_2nd$REFNO2
        data_NO2_A$raw_NO2_A <- data_2nd$NO2_A
        data_NO2_A$raw_RH <- data_2nd$RH_B
        data_NO2_A$raw_V <- data_2nd$V_A
      }
      ### NO2_B --------------------
      {
        data_NO2_B <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM$`NO2[ppb]`,RE_PR_M_data_NO2_00_RF$`NO2[ppb]`))
        data_NO2_B <- round(data_NO2_B, digits = 2)
        data_NO2_B <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM$date,data_NO2_B))
        data_NO2_B <- rename(data_NO2_B,"Date[UTC]"="RE_PR_M_data_NO2_00_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO2_B$`Date[UTC]` <- ymd_hms(data_NO2_B$`Date[UTC]`)
        data_NO2_B$Reference <- data_2nd$REFNO2
        data_NO2_B$raw_NO2_B <- data_2nd$NO2_B
        data_NO2_B$raw_RH <- data_2nd$RH_B
        data_NO2_B$raw_V <- data_2nd$V_A
      }
      
      
    }
    ### Produce delta raw signal
    {
      ### NO_A
      {
        data_NO_A$delta_ref <-0
        data_NO_A$delta_raw <-0
        
        for(i in 2:dim(data_NO_A)[1])
        {
          data_NO_A$delta_ref[i] <- data_NO_A$Reference[i]- data_NO_A$Reference[i-1]
          data_NO_A$delta_raw[i] <- data_NO_A$raw_NO_A[i]- data_NO_A$raw_NO_A[i-1]
        }
      }
      ### NO_B
      {
        data_NO_B$delta_ref <-0
        data_NO_B$delta_raw <-0
        
        for(i in 2:dim(data_NO_B)[1])
        {
          data_NO_B$delta_ref[i] <- data_NO_B$Reference[i]- data_NO_B$Reference[i-1]
          data_NO_B$delta_raw[i] <- data_NO_B$raw_NO_B[i]- data_NO_B$raw_NO_B[i-1]
        }
      }
      ### NO2_A
      {
        data_NO2_A$delta_ref <-0
        data_NO2_A$delta_raw <-0
        
        for(i in 2:dim(data_NO2_A)[1])
        {
          data_NO2_A$delta_ref[i] <- data_NO2_A$Reference[i]- data_NO2_A$Reference[i-1]
          data_NO2_A$delta_raw[i] <- data_NO2_A$raw_NO2_A[i]- data_NO2_A$raw_NO2_A[i-1]
        }
      }
      ### NO2_B
      {
        data_NO2_B$delta_ref <-0
        data_NO2_B$delta_raw <-0
        
        for(i in 2:dim(data_NO2_B)[1])
        {
          data_NO2_B$delta_ref[i] <- data_NO2_B$Reference[i]- data_NO2_B$Reference[i-1]
          data_NO2_B$delta_raw[i] <- data_NO2_B$raw_NO2_B[i]- data_NO2_B$raw_NO2_B[i-1]
        }
      }
    }
    ### Plotting data + raw signal
    {
      ### NO_A
      {
        save_loc <- "figure/2nd_colocation/timeseries/with_raw_signal/"
        
        figname <- paste(save_loc ,sensorunit,"_NO_A.pdf",sep="")
        
        yyy1     <- as.matrix(data_NO_A[,c(2,3,4)])
        yyy2     <- as.matrix(data_NO_A[,c(5)])
        
        xlabString <- "Date [UTC]"
        ylabString1 <- "Calibrated conc. + Reference [ppb]"
        ylabString2 <- "Raw signal of Sensor [V]"
                
        legend_str <- c(colnames(data_NO_A)[c(2,3,4,5)])
        
        plot_ts_2YAxes(figname,data_NO_A$`Date[UTC]`,yyy1,yyy2,"day",NULL,c(-20,200),c(-30,30),xlabString,ylabString1 = ylabString1,ylabString2=ylabString2,legend_str)
        dev.off()
      }
      ### NO_B
      {
        save_loc <- "figure/2nd_colocation/timeseries/with_raw_signal/"
        
        figname <- paste(save_loc ,sensorunit,"_NO_B.pdf",sep="")
        
        yyy1     <- as.matrix(data_NO_B[,c(2,3,4)])
        yyy2     <- as.matrix(data_NO_B[,c(5)])
        
        xlabString <- "Date [UTC]"
        ylabString1 <- "Calibrated conc. + Reference [ppb]"
        ylabString2 <- "Raw signal of Sensor [V]"
        
        legend_str <- c(colnames(data_NO_B)[c(2,3,4,5)])
        
        plot_ts_2YAxes(figname,data_NO_B$`Date[UTC]`,yyy1,yyy2,"day",NULL,c(-20,200),c(-30,30),xlabString,ylabString1,ylabString2,legend_str)
        dev.off()
      }
      ### NO2_A
      {
        save_loc <- "figure/2nd_colocation/timeseries/with_raw_signal/"
        
        figname <- paste(save_loc ,sensorunit,"_NO2_A.pdf",sep="")
        
        yyy1     <- as.matrix(data_NO2_A[,c(2,3,4)])
        yyy2     <- as.matrix(data_NO2_A[,c(5)])
        
        xlabString <- "Date [UTC]"
        ylabString1 <- "Calibrated conc. + Reference [ppb]"
        ylabString2 <- "Raw signal of Sensor [V]"
        
        legend_str <- c(colnames(data_NO2_A)[c(2,3,4,5)])
        
        plot_ts_2YAxes(figname,data_NO2_A$`Date[UTC]`,yyy1,yyy2,"day",NULL,c(-20,200),c(-30,30),xlabString,ylabString1,ylabString2,legend_str)
        dev.off()
      }
      ### NO2_B
      {
        save_loc <- "figure/2nd_colocation/timeseries/with_raw_signal/"
        
        figname <- paste(save_loc ,sensorunit,"_NO2_B.pdf",sep="")
        
        yyy1     <- as.matrix(data_NO2_B[,c(2,3,4)])
        yyy2     <- as.matrix(data_NO2_B[,c(5)])
        
        xlabString <- "Date [UTC]"
        ylabString1 <- "Calibrated conc. + Reference [ppb]"
        ylabString2 <- "Raw signal of Sensor [V]"
        
        legend_str <- c(colnames(data_NO2_B)[c(2,3,4,5)])
        
        plot_ts_2YAxes(figname,data_NO2_B$`Date[UTC]`,yyy1,yyy2,"day",NULL,c(-20,200),c(-30,30),xlabString,ylabString1,ylabString2,legend_str)
        dev.off()
      }
    } 
    ### Plotting delta raw signal
    {
      ### NO_A
      {
        save_loc <- "figure/2nd_colocation/timeseries/delta_raw/"
        
        figname <- paste(save_loc ,sensorunit,"_NO_A.pdf",sep="")
        
        yyy     <- as.matrix(data_NO_A[,c(8,9)])
        
        xlabString <- "Date [UTC]"
        ylabString <- "Delta (sensor raw signal [V] + reference data [ppb]) "
        
        legend_str <- c(colnames(data_NO_A)[c(8,9)])
        
        plot_ts(figname,data_NO_A$`Date[UTC]`,yyy,"day",NULL,c(-20,20),xlabString,ylabString,legend_str)
        dev.off()
      }
      ### NO_B
      {
        save_loc <- "figure/2nd_colocation/timeseries/delta_raw/"
        
        figname <- paste(save_loc ,sensorunit,"_NO_B.pdf",sep="")
        
        yyy     <- as.matrix(data_NO_B[,c(8,9)])
        
        xlabString <- "Date [UTC]"
        ylabString <- "Delta (sensor raw signal [V] + reference data [ppb]) "
        
        legend_str <- c(colnames(data_NO_B)[c(8,9)])
        
        plot_ts(figname,data_NO_B$`Date[UTC]`,yyy,"day",NULL,c(-20,20),xlabString,ylabString,legend_str)
        dev.off()
      }
      ### NO2_A
      {
        save_loc <- "figure/2nd_colocation/timeseries/delta_raw/"
        
        figname <- paste(save_loc ,sensorunit,"_NO2_A.pdf",sep="")
        
        yyy     <- as.matrix(data_NO2_A[,c(8,9)])
        
        xlabString <- "Date [UTC]"
        ylabString <- "Delta (sensor raw signal [V] + reference data [ppb]) "
        
        legend_str <- c(colnames(data_NO2_A)[c(8,9)])
        
        plot_ts(figname,data_NO2_A$`Date[UTC]`,yyy,"day",NULL,c(-20,20),xlabString,ylabString,legend_str)
        dev.off()
      }
      ### NO2_B
      {
        save_loc <- "figure/2nd_colocation/timeseries/delta_raw/"
        
        figname <- paste(save_loc ,sensorunit,"_NO2_B.pdf",sep="")
        
        yyy     <- as.matrix(data_NO2_B[,c(8,9)])
        
        xlabString <- "Date [UTC]"
        ylabString <- "Delta (sensor raw signal [V] + reference data [ppb]) "
        
        legend_str <- c(colnames(data_NO2_B)[c(8,9)])
        
        plot_ts(figname,data_NO2_B$`Date[UTC]`,yyy,"day",NULL,c(-20,20),xlabString,ylabString,legend_str)
        dev.off()
      }

    } 
  }
  
}

reloc.cal("AC9")
reloc.cal("AC10")
reloc.cal("AC11")
reloc.cal("AC12")