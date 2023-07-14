# Figure 8 : Plotting CRMSE/RMSE/MBE comparison during whole periods 
#
#
# Library ----------------------------------
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
library(stringr)
library(ggpubr)
library(doBy)

setwd("D:/empa_intern/Paper/data_and_code/")

# Data from 1st colocation period ---------------------------------
{
## import the calibrated data and merge with reference
import_1st_com <- function(sensorunit){
  # calibrated data ----------------------------------------------------
  RE_M_data_1st_NO_00_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RLM.csv",sep = ""), header=TRUE)
  RE_M_data_1st_NO_00_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RF.csv",sep = ""), header=TRUE)
  RE_M_data_1st_NO_01_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RLM.csv",sep = ""), header=TRUE)
  RE_M_data_1st_NO_01_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RF.csv",sep = ""), header=TRUE)
  RE_M_data_1st_NO2_00_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RLM.csv",sep = ""), header=TRUE)
  RE_M_data_1st_NO2_00_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RF.csv",sep = ""), header=TRUE)
  RE_M_data_1st_NO2_01_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RLM.csv",sep = ""), header=TRUE)
  RE_M_data_1st_NO2_01_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RF.csv",sep = ""), header=TRUE)
  
  ## Import the reference
  
  ref_1st <- read.csv(paste("data_output/pre_processed_1st_period/",sensorunit,".csv",sep = ""), header = TRUE)
  
  ref_1st$X <- seq(from = 1, to = length(ref_1st$date) ,by = 1)
  ref_1st$date <- ymd_hms(ref_1st$date)
  
  ref_1st <- as_tibble(ref_1st)
  
  ref_1st <- as_tbl_time(ref_1st, index = date)
  
  # Merging the calibration output ------------------------------------------
  
  one_data_1st_NO_00_RLM <- as.data.frame(cbind(ref_1st$date,ref_1st$timestamp,ref_1st$REFNO,RE_M_data_1st_NO_00_RLM$NO.ppb.))
  one_data_1st_NO_00_RF <- as.data.frame(cbind(ref_1st$date,ref_1st$timestamp,ref_1st$REFNO,RE_M_data_1st_NO_00_RF$NO.ppb.))
  one_data_1st_NO_01_RLM <- as.data.frame(cbind(ref_1st$date,ref_1st$timestamp,ref_1st$REFNO,RE_M_data_1st_NO_01_RLM$NO.ppb.))
  one_data_1st_NO_01_RF <- as.data.frame(cbind(ref_1st$date,ref_1st$timestamp,ref_1st$REFNO,RE_M_data_1st_NO_01_RF$NO.ppb.))
  one_data_1st_NO2_00_RLM <- as.data.frame(cbind(ref_1st$date,ref_1st$timestamp,ref_1st$REFNO2,RE_M_data_1st_NO2_00_RLM$NO2.ppb.))
  one_data_1st_NO2_00_RF <- as.data.frame(cbind(ref_1st$date,ref_1st$timestamp,ref_1st$REFNO2,RE_M_data_1st_NO2_00_RF$NO2.ppb.))
  one_data_1st_NO2_01_RLM <- as.data.frame(cbind(ref_1st$date,ref_1st$timestamp,ref_1st$REFNO2,RE_M_data_1st_NO2_01_RLM$NO2.ppb.))
  one_data_1st_NO2_01_RF <- as.data.frame(cbind(ref_1st$date,ref_1st$timestamp,ref_1st$REFNO2,RE_M_data_1st_NO2_01_RF$NO2.ppb.))
  
  one_data_1st_NO <- as.data.frame(rbind(one_data_1st_NO_00_RLM,
                                         one_data_1st_NO_00_RF,
                                         one_data_1st_NO_01_RLM,
                                         one_data_1st_NO_01_RF))
  
  one_data_1st_NO2 <- as.data.frame(rbind(one_data_1st_NO2_00_RLM,
                                          one_data_1st_NO2_00_RF,
                                          one_data_1st_NO2_01_RLM,
                                          one_data_1st_NO2_01_RF))
  
  
  one_data_1st_NO$V1 <- as.POSIXct(one_data_1st_NO$V1, origin="1970-01-01", tz='UTC')
  
  one_data_1st_NO <- rename(one_data_1st_NO, "Date"= "V1", "timestamp" = "V2",
                            "Ref"="V3","Obs"="V4")

  
  one_data_1st_NO2$V1 <- as.POSIXct(one_data_1st_NO2$V1, origin="1970-01-01", tz='UTC')
  
  one_data_1st_NO2 <- rename(one_data_1st_NO2, "Date"= "V1", "timestamp" = "V2",
                             "Ref"="V3","Obs"="V4")
  
  rm(one_data_1st_NO_00_RF,one_data_1st_NO_00_RLM,one_data_1st_NO_01_RF,one_data_1st_NO_01_RLM,
     one_data_1st_NO2_00_RF,one_data_1st_NO2_00_RLM,one_data_1st_NO2_01_RF,one_data_1st_NO2_01_RLM)
  
  rm(RE_M_data_1st_NO_00_RLM,RE_M_data_1st_NO_00_RF,RE_M_data_1st_NO_01_RLM,RE_M_data_1st_NO_01_RF,
     RE_M_data_1st_NO2_00_RLM,RE_M_data_1st_NO2_00_RF,RE_M_data_1st_NO2_01_RLM,RE_M_data_1st_NO2_01_RF)
  
  r.list <- list(one_data_1st_NO, one_data_1st_NO2)
  
  
  return(r.list)
}

AC9_1st_NO <- as.data.frame(import_1st_com("AC9")[[1]])
AC9_1st_NO2 <- as.data.frame(import_1st_com("AC9")[[2]])
AC10_1st_NO <- as.data.frame(import_1st_com("AC10")[[1]])
AC10_1st_NO2 <- as.data.frame(import_1st_com("AC10")[[2]])
AC11_1st_NO <- as.data.frame(import_1st_com("AC11")[[1]])
AC11_1st_NO2 <- as.data.frame(import_1st_com("AC11")[[2]])
AC12_1st_NO <- as.data.frame(import_1st_com("AC12")[[1]])
AC12_1st_NO2 <- as.data.frame(import_1st_com("AC12")[[2]])
}

# Data from 2nd colocation period ---------------------------------
{ 
  ## import the calibrated data and merge with reference
  import_2nd_com <- function(sensorunit){
    
    # calibrated data ----------------------------------------------------
    {
      data_2nd_NO_00 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO_00.csv",sep = ""), header=TRUE)
      data_2nd_NO_01 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO_01.csv",sep = ""), header=TRUE)
      data_2nd_NO2_00 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO2_00.csv",sep = ""), header=TRUE)
      data_2nd_NO2_01 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO2_01.csv",sep = ""), header=TRUE)
    }  
    ## Import the reference
    {
      ref_2nd <- read.csv(paste("data_output/pre_processed_2nd_period/",sensorunit,".csv",sep = ""), header = TRUE)
      
      ref_2nd$X <- seq(from = 1, to = length(ref_2nd$date) ,by = 1)
      ref_2nd$date <- ymd_hms(ref_2nd$date)
      
      ref_2nd <- as_tibble(ref_2nd)
      
      ref_2nd <- as_tbl_time(ref_2nd, index = date)
    }
    # Reorganize the output
    {
      ### NO_00
      {
        two_data_NO_00 <- as.data.frame(matrix(0,dim(data_2nd_NO_00)[1]*2,4))
        colnames(two_data_NO_00) <- c("Date","Obs","Ref","timestamp")
        two_data_NO_00$Date <- rep(as.POSIXct(data_2nd_NO_00$Date, origin="1970-01-01", tz='UTC'),2)
        two_data_NO_00$Obs <- as.numeric(c(data_2nd_NO_00$RLM,data_2nd_NO_00$RF))
        two_data_NO_00$Ref <- as.numeric(rep(data_2nd_NO_00$Reference,2))
        two_data_NO_00$timestamp <- as.numeric(rep(data_2nd_NO_00$timestamp,2))
      }
      ### NO_01
      {
        two_data_NO_01 <- as.data.frame(matrix(0,dim(data_2nd_NO_01)[1]*2,4))
        colnames(two_data_NO_01) <- c("Date","Obs","Ref","timestamp")
        two_data_NO_01$Date <- rep(as.POSIXct(data_2nd_NO_01$Date, origin="1970-01-01", tz='UTC'),2)
        two_data_NO_01$Obs <- as.numeric(c(data_2nd_NO_01$RLM,data_2nd_NO_01$RF))
        two_data_NO_01$Ref <- as.numeric(rep(data_2nd_NO_01$Reference,2))
        two_data_NO_01$timestamp <- as.numeric(rep(data_2nd_NO_01$timestamp,2))
      }
      ### NO2_00
      {
        two_data_NO2_00 <- as.data.frame(matrix(0,dim(data_2nd_NO2_00)[1]*2,4))
        colnames(two_data_NO2_00) <- c("Date","Obs","Ref","timestamp")
        two_data_NO2_00$Date <- rep(as.POSIXct(data_2nd_NO2_00$Date, origin="1970-01-01", tz='UTC'),2)
        two_data_NO2_00$Obs <- as.numeric(c(data_2nd_NO2_00$RLM,data_2nd_NO2_00$RF))
        two_data_NO2_00$Ref <- as.numeric(rep(data_2nd_NO2_00$Reference,2))
        two_data_NO2_00$timestamp <- as.numeric(rep(data_2nd_NO2_00$timestamp,2))
      }
      ### NO2_01
      {
        two_data_NO2_01 <- as.data.frame(matrix(0,dim(data_2nd_NO2_01)[1]*2,4))
        colnames(two_data_NO2_01) <- c("Date","Obs","Ref","timestamp")
        two_data_NO2_01$Date <- rep(as.POSIXct(data_2nd_NO2_01$Date, origin="1970-01-01", tz='UTC'),2)
        two_data_NO2_01$Obs <- as.numeric(c(data_2nd_NO2_01$RLM,data_2nd_NO2_01$RF))
        two_data_NO2_01$Ref <- as.numeric(rep(data_2nd_NO2_01$Reference,2))
        two_data_NO2_01$timestamp <- as.numeric(rep(data_2nd_NO2_01$timestamp,2))
      }
      ### Merging
      two_data_NO <- as.data.frame(rbind(two_data_NO_00, two_data_NO_01))
      two_data_NO2 <- as.data.frame(rbind(two_data_NO2_00, two_data_NO2_01))
    }
    
    r.list <- list(two_data_NO, two_data_NO2)
    
    
    return(r.list)
  }
  
  AC9_2nd_NO <- as.data.frame(import_2nd_com("AC9")[[1]])
  AC9_2nd_NO2 <- as.data.frame(import_2nd_com("AC9")[[2]])
  AC10_2nd_NO <- as.data.frame(import_2nd_com("AC10")[[1]])
  AC10_2nd_NO2 <- as.data.frame(import_2nd_com("AC10")[[2]])
  AC11_2nd_NO <- as.data.frame(import_2nd_com("AC11")[[1]])
  AC11_2nd_NO2 <- as.data.frame(import_2nd_com("AC11")[[2]])
  AC12_2nd_NO <- as.data.frame(import_2nd_com("AC12")[[1]])
  AC12_2nd_NO2 <- as.data.frame(import_2nd_com("AC12")[[2]])
}


# Averaging --------------------------------------------------------
## setting the indicator of interval
{
  biweekly <- as.numeric(1209600)
  weekly <- as.numeric(604800)
  daily <- as.numeric(86400)
  hourly <- as.numeric(3600)
}
## Function
averaging <- function(interval,sensorunit,period){
    if(period == "1st") {p <- 2}
    if(period == "2nd") {p <- 4}
  
    data_NO <- get(paste(sensorunit,"_",period,"_NO",sep=""))
    
    data_NO$intervalstamp<- (data_NO$timestamp)/interval
    data_NO$intervalstamp<- floor(data_NO$intervalstamp)
    avg_data_NO <- summaryBy(.~intervalstamp,data = data_NO, FUN = c(mean))
    avg_data_NO <- avg_data_NO[,-p]
    avg_data_NO$timestamp <- (avg_data_NO$intervalstamp+1)*interval
    avg_data_NO$Date <- as.POSIXct(avg_data_NO$timestamp, origin="1970-01-01", tz='UTC')
    avg_data_NO$months <- months(avg_data_NO$Date)
    
    data_NO2 <- get(paste(sensorunit,"_",period,"_NO2",sep=""))
    
    data_NO2$intervalstamp<- (data_NO2$timestamp)/interval
    data_NO2$intervalstamp<- floor(data_NO2$intervalstamp)
    avg_data_NO2 <- summaryBy(.~intervalstamp,data = data_NO2, FUN = c(mean))
    avg_data_NO2 <- avg_data_NO2[,-p]
    avg_data_NO2$timestamp <- (avg_data_NO2$intervalstamp+1)*interval
    avg_data_NO2$Date <- as.POSIXct(avg_data_NO2$timestamp, origin="1970-01-01", tz='UTC')
    avg_data_NO2$months <- months(avg_data_NO2$Date)
    
  ## return
  r.list <- list(avg_data_NO,
                 avg_data_NO2)
  
  return(r.list)
  
}
## get results
{
biweekly_avg_AC9_1st_NO <- as.data.frame(averaging(biweekly,"AC9","1st")[[1]])
biweekly_avg_AC9_1st_NO2 <- as.data.frame(averaging(biweekly,"AC9","1st")[[2]])
biweekly_avg_AC10_1st_NO <- as.data.frame(averaging(biweekly,"AC10","1st")[[1]])
biweekly_avg_AC10_1st_NO2 <- as.data.frame(averaging(biweekly,"AC10","1st")[[2]])
biweekly_avg_AC11_1st_NO <- as.data.frame(averaging(biweekly,"AC11","1st")[[1]])
biweekly_avg_AC11_1st_NO2 <- as.data.frame(averaging(biweekly,"AC11","1st")[[2]])
biweekly_avg_AC12_1st_NO <- as.data.frame(averaging(biweekly,"AC12","1st")[[1]])
biweekly_avg_AC12_1st_NO2 <- as.data.frame(averaging(biweekly,"AC12","1st")[[2]])

weekly_avg_AC9_1st_NO <- as.data.frame(averaging(weekly,"AC9","1st")[[1]])
weekly_avg_AC9_1st_NO2 <- as.data.frame(averaging(weekly,"AC9","1st")[[2]])
weekly_avg_AC10_1st_NO <- as.data.frame(averaging(weekly,"AC10","1st")[[1]])
weekly_avg_AC10_1st_NO2 <- as.data.frame(averaging(weekly,"AC10","1st")[[2]])
weekly_avg_AC11_1st_NO <- as.data.frame(averaging(weekly,"AC11","1st")[[1]])
weekly_avg_AC11_1st_NO2 <- as.data.frame(averaging(weekly,"AC11","1st")[[2]])
weekly_avg_AC12_1st_NO <- as.data.frame(averaging(weekly,"AC12","1st")[[1]])
weekly_avg_AC12_1st_NO2 <- as.data.frame(averaging(weekly,"AC12","1st")[[2]])

daily_avg_AC9_1st_NO <- as.data.frame(averaging(daily,"AC9","1st")[[1]])
daily_avg_AC9_1st_NO2 <- as.data.frame(averaging(daily,"AC9","1st")[[2]])
daily_avg_AC10_1st_NO <- as.data.frame(averaging(daily,"AC10","1st")[[1]])
daily_avg_AC10_1st_NO2 <- as.data.frame(averaging(daily,"AC10","1st")[[2]])
daily_avg_AC11_1st_NO <- as.data.frame(averaging(daily,"AC11","1st")[[1]])
daily_avg_AC11_1st_NO2 <- as.data.frame(averaging(daily,"AC11","1st")[[2]])
daily_avg_AC12_1st_NO <- as.data.frame(averaging(daily,"AC12","1st")[[1]])
daily_avg_AC12_1st_NO2 <- as.data.frame(averaging(daily,"AC12","1st")[[2]])

hourly_avg_AC9_1st_NO <- as.data.frame(averaging(hourly,"AC9","1st")[[1]])
hourly_avg_AC9_1st_NO2 <- as.data.frame(averaging(hourly,"AC9","1st")[[2]])
hourly_avg_AC10_1st_NO <- as.data.frame(averaging(hourly,"AC10","1st")[[1]])
hourly_avg_AC10_1st_NO2 <- as.data.frame(averaging(hourly,"AC10","1st")[[2]])
hourly_avg_AC11_1st_NO <- as.data.frame(averaging(hourly,"AC11","1st")[[1]])
hourly_avg_AC11_1st_NO2 <- as.data.frame(averaging(hourly,"AC11","1st")[[2]])
hourly_avg_AC12_1st_NO <- as.data.frame(averaging(hourly,"AC12","1st")[[1]])
hourly_avg_AC12_1st_NO2 <- as.data.frame(averaging(hourly,"AC12","1st")[[2]])

biweekly_avg_AC9_2nd_NO <- as.data.frame(averaging(biweekly,"AC9","2nd")[[1]])
biweekly_avg_AC9_2nd_NO2 <- as.data.frame(averaging(biweekly,"AC9","2nd")[[2]])
biweekly_avg_AC10_2nd_NO <- as.data.frame(averaging(biweekly,"AC10","2nd")[[1]])
biweekly_avg_AC10_2nd_NO2 <- as.data.frame(averaging(biweekly,"AC10","2nd")[[2]])
biweekly_avg_AC11_2nd_NO <- as.data.frame(averaging(biweekly,"AC11","2nd")[[1]])
biweekly_avg_AC11_2nd_NO2 <- as.data.frame(averaging(biweekly,"AC11","2nd")[[2]])
biweekly_avg_AC12_2nd_NO <- as.data.frame(averaging(biweekly,"AC12","2nd")[[1]])
biweekly_avg_AC12_2nd_NO2 <- as.data.frame(averaging(biweekly,"AC12","2nd")[[2]])

weekly_avg_AC9_2nd_NO <- as.data.frame(averaging(weekly,"AC9","2nd")[[1]])
weekly_avg_AC9_2nd_NO2 <- as.data.frame(averaging(weekly,"AC9","2nd")[[2]])
weekly_avg_AC10_2nd_NO <- as.data.frame(averaging(weekly,"AC10","2nd")[[1]])
weekly_avg_AC10_2nd_NO2 <- as.data.frame(averaging(weekly,"AC10","2nd")[[2]])
weekly_avg_AC11_2nd_NO <- as.data.frame(averaging(weekly,"AC11","2nd")[[1]])
weekly_avg_AC11_2nd_NO2 <- as.data.frame(averaging(weekly,"AC11","2nd")[[2]])
weekly_avg_AC12_2nd_NO <- as.data.frame(averaging(weekly,"AC12","2nd")[[1]])
weekly_avg_AC12_2nd_NO2 <- as.data.frame(averaging(weekly,"AC12","2nd")[[2]])

daily_avg_AC9_2nd_NO <- as.data.frame(averaging(daily,"AC9","2nd")[[1]])
daily_avg_AC9_2nd_NO2 <- as.data.frame(averaging(daily,"AC9","2nd")[[2]])
daily_avg_AC10_2nd_NO <- as.data.frame(averaging(daily,"AC10","2nd")[[1]])
daily_avg_AC10_2nd_NO2 <- as.data.frame(averaging(daily,"AC10","2nd")[[2]])
daily_avg_AC11_2nd_NO <- as.data.frame(averaging(daily,"AC11","2nd")[[1]])
daily_avg_AC11_2nd_NO2 <- as.data.frame(averaging(daily,"AC11","2nd")[[2]])
daily_avg_AC12_2nd_NO <- as.data.frame(averaging(daily,"AC12","2nd")[[1]])
daily_avg_AC12_2nd_NO2 <- as.data.frame(averaging(daily,"AC12","2nd")[[2]])

hourly_avg_AC9_2nd_NO <- as.data.frame(averaging(hourly,"AC9","2nd")[[1]])
hourly_avg_AC9_2nd_NO2 <- as.data.frame(averaging(hourly,"AC9","2nd")[[2]])
hourly_avg_AC10_2nd_NO <- as.data.frame(averaging(hourly,"AC10","2nd")[[1]])
hourly_avg_AC10_2nd_NO2 <- as.data.frame(averaging(hourly,"AC10","2nd")[[2]])
hourly_avg_AC11_2nd_NO <- as.data.frame(averaging(hourly,"AC11","2nd")[[1]])
hourly_avg_AC11_2nd_NO2 <- as.data.frame(averaging(hourly,"AC11","2nd")[[2]])
hourly_avg_AC12_2nd_NO <- as.data.frame(averaging(hourly,"AC12","2nd")[[1]])
hourly_avg_AC12_2nd_NO2 <- as.data.frame(averaging(hourly,"AC12","2nd")[[2]])
}

# Data from deployment period ----------------------------------------
{
  ## import passive sampler data
  PS_NO2 <- read.csv("data_input/PSdata.csv", header = TRUE)
  
  ## import the sensor data
  
  ## indicate the number of corresponding reference values
  sen <- c("AC9","AC10","AC11","AC12")
  num <- c(24,22,18,24)
  ind <- as.data.frame(cbind(sen,num))
  
  import_dp <- function(sensorunit) {
    # Import CSV file
    {
      data_NO2_00_RLM <- read.csv(paste("data_output/calibrated_deployment/",sensorunit,"_NO2_00_RLM.csv",sep = ""), header=TRUE)  
      data_NO2_00_RF <- read.csv(paste("data_output/calibrated_deployment/",sensorunit,"_NO2_00_RF.csv",sep = ""), header=TRUE) 
      data_NO2_01_RLM <- read.csv(paste("data_output/calibrated_deployment/",sensorunit,"_NO2_01_RLM.csv",sep = ""), header=TRUE)  
      data_NO2_01_RF <- read.csv(paste("data_output/calibrated_deployment/",sensorunit,"_NO2_01_RF.csv",sep = ""), header=TRUE) 
    }
    # bind the data of RLM and RF
    {
      ### NO2_00 --------------------
      {
        data_NO2_00 <- as.data.frame(cbind(data_NO2_00_RLM$NO2.ppb.,data_NO2_00_RF$NO2.ppb.))
        data_NO2_00 <- round(data_NO2_00, digits = 2)
        data_NO2_00 <- as.data.frame(cbind(data_NO2_00_RLM$date,data_NO2_00))
        data_NO2_00 <- rename(data_NO2_00,"Date[UTC]"="data_NO2_00_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO2_00$`Date[UTC]` <- ymd_hms(data_NO2_00$`Date[UTC]`)
      }
      ### NO2_01 --------------------
      {
        data_NO2_01 <- as.data.frame(cbind(data_NO2_01_RLM$NO2.ppb.,data_NO2_01_RF$NO2.ppb.))
        data_NO2_01 <- round(data_NO2_01, digits = 2)
        data_NO2_01 <- as.data.frame(cbind(data_NO2_01_RLM$date,data_NO2_01))
        data_NO2_01 <- rename(data_NO2_01,"Date[UTC]"="data_NO2_01_RLM$date" ,"RLM"="V1","RF"="V2")
        data_NO2_01$`Date[UTC]` <- ymd_hms(data_NO2_01$`Date[UTC]`)
      }
      
      
    }
    # timestamp insertion
    {
      data_NO2_00$timestamp <- as.numeric(difftime(time1=data_NO2_00$`Date[UTC]`,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
      data_NO2_01$timestamp <- as.numeric(difftime(time1=data_NO2_01$`Date[UTC]`,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
      
      data_NO2_00 <- na.omit(data_NO2_00)
      data_NO2_01 <- na.omit(data_NO2_01)
      
    }
    # biweekly averaging and merge with reference
    {
      for(i in 1:4){
        if(ind$sen[i] == sensorunit){k <- ind$num[i]}
      }
      
      if(sensorunit== "AC9") {REFER <- as.vector(PS_NO2$ZRIS.ppb.)}
      if(sensorunit== "AC10") {REFER <- as.vector(PS_NO2$ZBLG.ppb.)}
      if(sensorunit== "AC11") {REFER <- as.vector(PS_NO2$ZSBS.ppb.)}
      if(sensorunit== "AC12") {REFER <- as.vector(PS_NO2$ZMAN.ppb.)}
      ### NO2_00 -----------------------
      {
        data_NO2_00$labeling <- 0
        
        for (i in 1:nrow(data_NO2_00))
        {
          for (j in c(1:k))
          {
            if (data_NO2_00$timestamp[i] >= PS_NO2$start_ts[j] && data_NO2_00$timestamp[i] <= PS_NO2$end_ts[j])
            {
              data_NO2_00$labeling[i] <- str_c("Group_",letters[j])
            }
          }
        }
        
        data_NO2_00_PS_RLM <- summaryBy(RLM~labeling, data = data_NO2_00, FUN=c(mean))
        data_NO2_00_PS_RF <- summaryBy(RF~labeling, data = data_NO2_00, FUN=c(mean))
        
        data_NO2_00_PS_comparison <- data.table(PS_NO2$Start[1:k],PS_NO2$End[1:k],
                                                REFER[1:k],
                                                data_NO2_00_PS_RLM$RLM.mean,
                                                data_NO2_00_PS_RF$RF.mean)
        
        data_NO2_00_PS_comparison <- rename(data_NO2_00_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
      }
      ### NO2_01 -----------------------
      {
        data_NO2_01$labeling <- 0
        
        for (i in 1:nrow(data_NO2_01))
        {
          for (j in c(1:k))
          {
            if (data_NO2_01$timestamp[i] >= PS_NO2$start_ts[j] && data_NO2_01$timestamp[i] <= PS_NO2$end_ts[j])
            {
              data_NO2_01$labeling[i] <- str_c("Group_",letters[j])
            }
          }
        }
        
        data_NO2_01_PS_RLM <- summaryBy(RLM~labeling, data = data_NO2_01, FUN=c(mean))
        data_NO2_01_PS_RF <- summaryBy(RF~labeling, data = data_NO2_01, FUN=c(mean))
        
        data_NO2_01_PS_comparison <- data.table(PS_NO2$Start[1:k],PS_NO2$End[1:k],
                                                REFER[1:k],
                                                data_NO2_01_PS_RLM$RLM.mean,
                                                data_NO2_01_PS_RF$RF.mean)
        data_NO2_01_PS_comparison <- rename(data_NO2_01_PS_comparison, "Start" = "V1", "End" = "V2", "Reference"="V3", "RLM"="V4","RF"="V5")
        
      }
    }
    # reorganize the data
    {
      dep_data_RLM <- as.data.frame(matrix(0,dim(data_NO2_00_PS_comparison)[1]*2,2))
      colnames(dep_data_RLM) <- c("Obs","Ref")
      dep_data_RLM$Obs <- c(data_NO2_00_PS_comparison$RLM,
                            data_NO2_01_PS_comparison$RLM)
      
      dep_data_RLM$Ref <- c(data_NO2_00_PS_comparison$Reference,
                            data_NO2_01_PS_comparison$Reference)
      
      dep_data_RLM <- na.omit(dep_data_RLM)
      
      dep_data_RF <- as.data.frame(matrix(0,dim(data_NO2_00_PS_comparison)[1]*2,2))
      colnames(dep_data_RF) <- c("Obs","Ref")
      dep_data_RF$Obs <- c(data_NO2_00_PS_comparison$RF,
                           data_NO2_01_PS_comparison$RF)
      
      dep_data_RF$Ref <- c(data_NO2_00_PS_comparison$Reference,
                           data_NO2_01_PS_comparison$Reference)
      
      dep_data_RF <- na.omit(dep_data_RF)
    }
    # return
    r.list <- list(dep_data_RLM,dep_data_RF)
    
  }
      
  dep_AC9_RLM <- as.data.frame(import_dp("AC9")[[1]])
  dep_AC9_RF <- as.data.frame(import_dp("AC9")[[2]])
  dep_AC10_RLM <- as.data.frame(import_dp("AC10")[[1]])
  dep_AC10_RF <- as.data.frame(import_dp("AC10")[[2]])
  dep_AC11_RLM <- as.data.frame(import_dp("AC11")[[1]])
  dep_AC11_RF <- as.data.frame(import_dp("AC11")[[2]])
  dep_AC12_RLM <- as.data.frame(import_dp("AC12")[[1]])
  dep_AC12_RF <- as.data.frame(import_dp("AC12")[[2]])
}

# get statistical metrics -------------------------------------------
## merging
{
  tenminute_total_1st_NO <- as.data.frame(rbind(AC9_1st_NO,AC10_1st_NO,
                                                AC11_1st_NO,AC12_1st_NO))
  
  tenminute_total_1st_NO2 <- as.data.frame(rbind(AC9_1st_NO2,AC10_1st_NO2,
                                                 AC11_1st_NO2,AC12_1st_NO2))
  
  tenminute_total_2nd_NO <- as.data.frame(rbind(AC9_2nd_NO,AC10_2nd_NO,
                                                AC11_2nd_NO,AC12_2nd_NO))
  
  tenminute_total_2nd_NO2 <- as.data.frame(rbind(AC9_2nd_NO2,AC10_2nd_NO2,
                                                 AC11_2nd_NO2,AC12_2nd_NO2))  

biweekly_avg_total_1st_NO <- as.data.frame(rbind(biweekly_avg_AC9_1st_NO,
                                                 biweekly_avg_AC10_1st_NO,
                                                 biweekly_avg_AC11_1st_NO,
                                                 biweekly_avg_AC12_1st_NO))

biweekly_avg_total_1st_NO2 <- as.data.frame(rbind(biweekly_avg_AC9_1st_NO2,
                                                 biweekly_avg_AC10_1st_NO2,
                                                 biweekly_avg_AC11_1st_NO2,
                                                 biweekly_avg_AC12_1st_NO2))

biweekly_avg_total_2nd_NO <- as.data.frame(rbind(biweekly_avg_AC9_2nd_NO,
                                                 biweekly_avg_AC10_2nd_NO,
                                                 biweekly_avg_AC11_2nd_NO,
                                                 biweekly_avg_AC12_2nd_NO))

biweekly_avg_total_2nd_NO2 <- as.data.frame(rbind(biweekly_avg_AC9_2nd_NO2,
                                                  biweekly_avg_AC10_2nd_NO2,
                                                  biweekly_avg_AC11_2nd_NO2,
                                                  biweekly_avg_AC12_2nd_NO2))

weekly_avg_total_1st_NO <- as.data.frame(rbind(weekly_avg_AC9_1st_NO,
                                               weekly_avg_AC10_1st_NO,
                                               weekly_avg_AC11_1st_NO,
                                               weekly_avg_AC12_1st_NO))

weekly_avg_total_1st_NO2 <- as.data.frame(rbind(weekly_avg_AC9_1st_NO2,
                                                weekly_avg_AC10_1st_NO2,
                                                weekly_avg_AC11_1st_NO2,
                                                weekly_avg_AC12_1st_NO2))

weekly_avg_total_2nd_NO <- as.data.frame(rbind(weekly_avg_AC9_2nd_NO,
                                               weekly_avg_AC10_2nd_NO,
                                               weekly_avg_AC11_2nd_NO,
                                               weekly_avg_AC12_2nd_NO))

weekly_avg_total_2nd_NO2 <- as.data.frame(rbind(weekly_avg_AC9_2nd_NO2,
                                                weekly_avg_AC10_2nd_NO2,
                                                weekly_avg_AC11_2nd_NO2,
                                                weekly_avg_AC12_2nd_NO2))

hourly_avg_total_1st_NO <- as.data.frame(rbind(hourly_avg_AC9_1st_NO,
                                               hourly_avg_AC10_1st_NO,
                                               hourly_avg_AC11_1st_NO,
                                               hourly_avg_AC12_1st_NO))

hourly_avg_total_1st_NO2 <- as.data.frame(rbind(hourly_avg_AC9_1st_NO2,
                                                hourly_avg_AC10_1st_NO2,
                                                hourly_avg_AC11_1st_NO2,
                                                hourly_avg_AC12_1st_NO2))

hourly_avg_total_2nd_NO <- as.data.frame(rbind(hourly_avg_AC9_2nd_NO,
                                               hourly_avg_AC10_2nd_NO,
                                               hourly_avg_AC11_2nd_NO,
                                               hourly_avg_AC12_2nd_NO))

hourly_avg_total_2nd_NO2 <- as.data.frame(rbind(hourly_avg_AC9_2nd_NO2,
                                                hourly_avg_AC10_2nd_NO2,
                                                hourly_avg_AC11_2nd_NO2,
                                                hourly_avg_AC12_2nd_NO2))

daily_avg_total_1st_NO <- as.data.frame(rbind(daily_avg_AC9_1st_NO,
                                              daily_avg_AC10_1st_NO,
                                              daily_avg_AC11_1st_NO,
                                              daily_avg_AC12_1st_NO))

daily_avg_total_1st_NO2 <- as.data.frame(rbind(daily_avg_AC9_1st_NO2,
                                               daily_avg_AC10_1st_NO2,
                                               daily_avg_AC11_1st_NO2,
                                               daily_avg_AC12_1st_NO2))

daily_avg_total_2nd_NO <- as.data.frame(rbind(daily_avg_AC9_2nd_NO,
                                              daily_avg_AC10_2nd_NO,
                                              daily_avg_AC11_2nd_NO,
                                              daily_avg_AC12_2nd_NO))

daily_avg_total_2nd_NO2 <- as.data.frame(rbind(daily_avg_AC9_2nd_NO2,
                                               daily_avg_AC10_2nd_NO2,
                                               daily_avg_AC11_2nd_NO2,
                                               daily_avg_AC12_2nd_NO2))
}

col_metrics <- as.data.frame(matrix(0,10,5))
col_metrics <- rename(col_metrics, "Resolution"="V1", "Period" = "V2",
                      "RMSE [ppb]"="V3","MBE [ppb]"="V4", "CRMSE [ppb]"="V5" )
col_metrics$Resolution <- rep(c("10min","Hourly","Daily","Weekly","Biweekly"),2)
col_metrics$Period <- c(rep("1st",5),rep("2nd",5))
col_metrics[1,3:4] <- tdStats(tenminute_total_1st_NO2$Obs,tenminute_total_1st_NO2$Ref,functions = c("rmse","mbe") )
col_metrics[2,3:4] <- tdStats(hourly_avg_total_1st_NO2$Obs.mean,hourly_avg_total_1st_NO2$Ref.mean,functions = c("rmse","mbe") )
col_metrics[3,3:4] <- tdStats(daily_avg_total_1st_NO2$Obs.mean,daily_avg_total_1st_NO2$Ref.mean,functions = c("rmse","mbe") )
col_metrics[4,3:4] <- tdStats(weekly_avg_total_1st_NO2$Obs.mean,weekly_avg_total_1st_NO2$Ref.mean,functions = c("rmse","mbe") )
col_metrics[5,3:4] <- tdStats(biweekly_avg_total_1st_NO2$Obs.mean,biweekly_avg_total_1st_NO2$Ref.mean,functions = c("rmse","mbe") )
col_metrics[6,3:4] <- tdStats(tenminute_total_2nd_NO2$Obs,tenminute_total_2nd_NO2$Ref,functions = c("rmse","mbe") )
col_metrics[7,3:4] <- tdStats(hourly_avg_total_2nd_NO2$Obs.mean,hourly_avg_total_2nd_NO2$Ref.mean,functions = c("rmse","mbe") )
col_metrics[8,3:4] <- tdStats(daily_avg_total_2nd_NO2$Obs.mean,daily_avg_total_2nd_NO2$Ref.mean,functions = c("rmse","mbe") )
col_metrics[9,3:4] <- tdStats(weekly_avg_total_2nd_NO2$Obs.mean,weekly_avg_total_2nd_NO2$Ref.mean,functions = c("rmse","mbe") )
col_metrics[10,3:4] <- tdStats(biweekly_avg_total_2nd_NO2$Obs.mean,biweekly_avg_total_2nd_NO2$Ref.mean,functions = c("rmse","mbe") )
col_metrics$`CRMSE [ppb]` <- sqrt((col_metrics$`RMSE [ppb]`)^2-(col_metrics$`MBE [ppb]`)^2)

dep_metrics <- as.data.frame(matrix(0,8,5))
dep_metrics <- rename(dep_metrics, "Sensor"="V1", "Model" = "V2",
                      "RMSE [ppb]"="V3","MBE [ppb]"="V4", "CRMSE [ppb]"="V5" )

dep_metrics$Sensor <- c(rep("AC009",2),rep("AC010",2),
                        rep("AC011",2),rep("AC012",2))
dep_metrics$Model <- rep(c("RLM","RF"),4)
dep_metrics[1,3:4] <- tdStats(dep_AC9_RLM$Obs,dep_AC9_RLM$Ref,functions = c("rmse","mbe"))
dep_metrics[2,3:4] <- tdStats(dep_AC9_RF$Obs,dep_AC9_RF$Ref,functions = c("rmse","mbe"))
dep_metrics[3,3:4] <- tdStats(dep_AC10_RLM$Obs,dep_AC10_RLM$Ref,functions = c("rmse","mbe"))
dep_metrics[4,3:4] <- tdStats(dep_AC10_RF$Obs,dep_AC10_RF$Ref,functions = c("rmse","mbe"))
dep_metrics[5,3:4] <- tdStats(dep_AC11_RLM$Obs,dep_AC11_RLM$Ref,functions = c("rmse","mbe"))
dep_metrics[6,3:4] <- tdStats(dep_AC11_RF$Obs,dep_AC11_RF$Ref,functions = c("rmse","mbe"))
dep_metrics[7,3:4] <- tdStats(dep_AC12_RLM$Obs,dep_AC12_RLM$Ref,functions = c("rmse","mbe"))
dep_metrics[8,3:4] <- tdStats(dep_AC12_RF$Obs,dep_AC12_RF$Ref,functions = c("rmse","mbe"))
dep_metrics$`CRMSE [ppb]` <- sqrt((dep_metrics$`RMSE [ppb]`)^2-(dep_metrics$`MBE [ppb]`)^2)


res_NO2_total <- as.data.frame(matrix(0,10,4))

res_NO2_total <- rename(res_NO2_total, "Resolution"="V1","CRMSE [ppb]"="V2",
                        "RMSE [ppb]"="V3", "Period"="V4" )
res_NO2_total$Resolution <- rep(c("10min","Hourly","Daily","Weekly","Biweekly"),2)
res_NO2_total$Period <- c(rep("1st",5),rep("2nd",5))
res_NO2_total$`CRMSE [ppb]` <- col_metrics$`CRMSE [ppb]`
res_NO2_total$`RMSE [ppb]` <- col_metrics$`RMSE [ppb]`
res_NO2_total$Resolution <- factor(res_NO2_total$Resolution, levels = c("10min","Hourly","Daily","Weekly","Biweekly"))

res_final <- as.data.frame(rbind(0,0,0,0,dep_metrics,0,0,0,0))
res_final[1:4,1] <- "1st co-location"
res_final[13:16,1] <- "2nd co-location"
res_final[c(1,13),2] <- "AC009"
res_final[c(2,14),2] <- "AC010"
res_final[c(3,15),2] <- "AC011"
res_final[c(4,16),2] <- "AC012"

res_final[1,3:4] <- tdStats(biweekly_avg_AC9_1st_NO2$Obs.mean,
                            biweekly_avg_AC9_1st_NO2$Ref.mean,functions = c("rmse","mbe"))
res_final[2,3:4] <- tdStats(biweekly_avg_AC10_1st_NO2$Obs.mean,
                            biweekly_avg_AC10_1st_NO2$Ref.mean,functions = c("rmse","mbe"))
res_final[3,3:4] <- tdStats(biweekly_avg_AC11_1st_NO2$Obs.mean,
                            biweekly_avg_AC11_1st_NO2$Ref.mean,functions = c("rmse","mbe"))
res_final[4,3:4] <- tdStats(biweekly_avg_AC12_1st_NO2$Obs.mean,
                            biweekly_avg_AC12_1st_NO2$Ref.mean,functions = c("rmse","mbe"))
res_final[13,3:4] <- tdStats(biweekly_avg_AC9_2nd_NO2$Obs.mean,
                             biweekly_avg_AC9_2nd_NO2$Ref.mean,functions = c("rmse","mbe"))
res_final[14,3:4] <- tdStats(biweekly_avg_AC10_2nd_NO2$Obs.mean,
                             biweekly_avg_AC10_2nd_NO2$Ref.mean,functions = c("rmse","mbe"))
res_final[15,3:4] <- tdStats(biweekly_avg_AC11_2nd_NO2$Obs.mean,
                             biweekly_avg_AC11_2nd_NO2$Ref.mean,functions = c("rmse","mbe"))
res_final[16,3:4] <- tdStats(biweekly_avg_AC12_2nd_NO2$Obs.mean,
                             biweekly_avg_AC12_2nd_NO2$Ref.mean,functions = c("rmse","mbe"))

res_final$`CRMSE [ppb]` <- sqrt((res_final$`RMSE [ppb]`)^2-(res_final$`MBE [ppb]`)^2)

res_final$Sensor <- factor(res_final$Sensor, levels = c("1st co-location",
                                                        "AC009","AC010","AC011","AC012",
                                                        "2nd co-location"))
# plotting ----------------------------------------------------------

CO <- ggplot(res_NO2_total, aes(x=Resolution, y=`CRMSE [ppb]`, fill=Period)
)+geom_col(position=position_dodge(width=0.9)
)+xlab("Time interval of data")+ylab("CRMSE [ppb]"
)+theme_bw()+scale_fill_manual(values=c("#999999","#5e5e5e")
)+ylim(c(0,11))+geom_point(aes(x=Resolution, y=`RMSE [ppb]`),position=position_dodge(width = 0.9),size=3
)+scale_shape_manual(values=c(rep(16,16)))

jpeg("figure/sup_fig.jpeg", width =12, height = 14, units = 'cm', res = 600)
CO
dev.off()