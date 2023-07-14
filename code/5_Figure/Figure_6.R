# Figure 6 : Plotting concentration range-wise target plot with density plot.
# 
#
# Library ----------------------------------
library(data.table)
library(lubridate)
library(dplyr)
library(onlineforecast)
library(stringr)
library(doBy)
library(tdr)
library(ggplot2)
library(GGally)
library(ggforce)
library(ggrepel)
library(tibble)
library(tidyverse)
library(ggpubr)
library(plotrix)
library(jpeg)

setwd("D:/empa_intern/Paper/data_and_code/")

# 1st colocation period data --------------------------------------------------

import_1st <- function(sensorunit) {
  ## Data import
  {
    data_NO_00_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RLM.csv",sep=""), header=TRUE)
    data_NO_00_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RF.csv",sep=""), header=TRUE)
    data_NO_01_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RLM.csv",sep=""), header=TRUE)
    data_NO_01_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RF.csv",sep=""), header=TRUE)
    data_NO2_00_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RLM.csv",sep=""), header=TRUE)
    data_NO2_00_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RF.csv",sep=""), header=TRUE)
    data_NO2_01_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RLM.csv",sep=""), header=TRUE)
    data_NO2_01_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RF.csv",sep=""), header=TRUE)
  }
  ## Import the Reference
  ref <- read.csv(paste("data_output/pre_processed_1st_period/",sensorunit,".csv",sep=""), header = TRUE)
  ## Bind the data in each AC & Sensors
  {
    ### NO_00 --------------------
    {
      data_NO_00 <- as.data.frame(cbind(data_NO_00_RLM$NO.ppb.,data_NO_00_RF$NO.ppb.))
      data_NO_00 <- round(data_NO_00, digits = 2)
      data_NO_00 <- as.data.frame(cbind(data_NO_00_RLM$date,data_NO_00))
      data_NO_00 <- rename(data_NO_00,"Date[UTC]"="data_NO_00_RLM$date" ,"RLM"="V1","RF"="V2")
      data_NO_00$`Date[UTC]` <- ymd_hms(data_NO_00$`Date[UTC]`)
      data_NO_00$Reference <- ref$REFNO
    }
    ### NO_01 --------------------
    {
      data_NO_01 <- as.data.frame(cbind(data_NO_01_RLM$NO.ppb.,data_NO_01_RF$NO.ppb.))
      data_NO_01 <- round(data_NO_01, digits = 2)
      data_NO_01 <- as.data.frame(cbind(data_NO_01_RLM$date,data_NO_01))
      data_NO_01 <- rename(data_NO_01,"Date[UTC]"="data_NO_01_RLM$date" ,"RLM"="V1","RF"="V2")
      data_NO_01$`Date[UTC]` <- ymd_hms(data_NO_01$`Date[UTC]`)
      data_NO_01$Reference <- ref$REFNO
    }
    ### NO2_00 --------------------
    {
      data_NO2_00 <- as.data.frame(cbind(data_NO2_00_RLM$NO2.ppb.,data_NO2_00_RF$NO2.ppb.))
      data_NO2_00 <- round(data_NO2_00, digits = 2)
      data_NO2_00 <- as.data.frame(cbind(data_NO2_00_RLM$date,data_NO2_00))
      data_NO2_00 <- rename(data_NO2_00,"Date[UTC]"="data_NO2_00_RLM$date" ,"RLM"="V1","RF"="V2")
      data_NO2_00$`Date[UTC]` <- ymd_hms(data_NO2_00$`Date[UTC]`)
      data_NO2_00$Reference <- ref$REFNO2
    }
    ### NO2_01 --------------------
    {
      data_NO2_01 <- as.data.frame(cbind(data_NO2_01_RLM$NO2.ppb.,data_NO2_01_RF$NO2.ppb.))
      data_NO2_01 <- round(data_NO2_01, digits = 2)
      data_NO2_01 <- as.data.frame(cbind(data_NO2_01_RLM$date,data_NO2_01))
      data_NO2_01 <- rename(data_NO2_01,"Date[UTC]"="data_NO2_01_RLM$date" ,"RLM"="V1","RF"="V2")
      data_NO2_01$`Date[UTC]` <- ymd_hms(data_NO2_01$`Date[UTC]`)
      data_NO2_01$Reference <- ref$REFNO2
    }
  }
  ## merging data to NO and NO2 ---------------
  {
    ### data_NO_00
    one_data_NO_00 <- as.data.frame(cbind(data_NO_00$`Date[UTC]`,data_NO_00$RLM,data_NO_00$RF,data_NO_00$Reference))
    colnames(one_data_NO_00) <- c("Date","RLM","RF","Ref")
    one_data_NO_00$Date <- as.POSIXct(one_data_NO_00$Date, origin="1970-01-01", tz='UTC')
    
    ### data_NO_01
    one_data_NO_01 <- as.data.frame(cbind(data_NO_01$`Date[UTC]`,data_NO_01$RLM,data_NO_01$RF,data_NO_01$Reference))
    colnames(one_data_NO_01) <- c("Date","RLM","RF","Ref")
    one_data_NO_01$Date <- as.POSIXct(one_data_NO_01$Date, origin="1970-01-01", tz='UTC')
    
    ### data_NO2_00
    one_data_NO2_00 <- as.data.frame(cbind(data_NO2_00$`Date[UTC]`,data_NO2_00$RLM,data_NO2_00$RF,data_NO2_00$Reference))
    colnames(one_data_NO2_00) <- c("Date","RLM","RF","Ref")
    one_data_NO2_00$Date <- as.POSIXct(one_data_NO2_00$Date, origin="1970-01-01", tz='UTC')
    
    ### data_NO2_01
    one_data_NO2_01 <- as.data.frame(cbind(data_NO2_01$`Date[UTC]`,data_NO2_01$RLM,data_NO2_01$RF,data_NO2_01$Reference))
    colnames(one_data_NO2_01) <- c("Date","RLM","RF","Ref")
    one_data_NO2_01$Date <- as.POSIXct(one_data_NO2_01$Date, origin="1970-01-01", tz='UTC')
  }
  
  one_NO <- as.data.frame(rbind(one_data_NO_00,one_data_NO_01))
  one_NO2 <- as.data.frame(rbind(one_data_NO2_00,one_data_NO2_01))  
  
  r.list <- list(one_NO, one_NO2)
  
  return(r.list)
}

## with the function above, derive the statistical metrics in each concentration range.
{
  ## Take the data from the function 
  AC9_1st_NO <- as.data.frame(import_1st("AC9")[[1]])
  AC9_1st_NO2 <- as.data.frame(import_1st("AC9")[[2]])
  AC10_1st_NO <- as.data.frame(import_1st("AC10")[[1]])
  AC10_1st_NO2 <- as.data.frame(import_1st("AC10")[[2]])
  AC11_1st_NO <- as.data.frame(import_1st("AC11")[[1]])
  AC11_1st_NO2 <- as.data.frame(import_1st("AC11")[[2]])
  AC12_1st_NO <- as.data.frame(import_1st("AC12")[[1]])
  AC12_1st_NO2 <- as.data.frame(import_1st("AC12")[[2]])
  
  ## Merge datasets to each pollutant group
  one_NO <- as.data.frame(rbind(AC9_1st_NO,AC10_1st_NO,AC11_1st_NO,AC12_1st_NO))
  one_NO2 <- as.data.frame(rbind(AC9_1st_NO2,AC10_1st_NO2,AC11_1st_NO2,AC12_1st_NO2))
  
  ### Then grouping and derive statistical metrics
  ## one_NO --------
  {
    one_NO$labeling <- as.character(0)
    
    for (i in 1:dim(one_NO)[1]){
      if(one_NO$Ref[i] < 2){one_NO$labeling[i] <- as.character("Group_a")}
      if(one_NO$Ref[i] >= 2 && one_NO$Ref[i] < 10){one_NO$labeling[i] <- as.character("Group_b")}
      if(one_NO$Ref[i] >= 10 && one_NO$Ref[i] < 28){one_NO$labeling[i] <- as.character("Group_c")}
      if(one_NO$Ref[i] >= 28){one_NO$labeling[i] <- as.character("Group_d")}
    }
    
    one_NO_group_a <- filter(one_NO, one_NO$labeling == "Group_a")
    one_NO_group_b <- filter(one_NO, one_NO$labeling == "Group_b")
    one_NO_group_c <- filter(one_NO, one_NO$labeling == "Group_c")
    one_NO_group_d <- filter(one_NO, one_NO$labeling == "Group_d")
    
    
    res_one_NO_RLM_group_a <- tdStats(one_NO_group_a$RLM,one_NO_group_a$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO_RLM_group_b <- tdStats(one_NO_group_b$RLM,one_NO_group_b$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO_RLM_group_c <- tdStats(one_NO_group_c$RLM,one_NO_group_c$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO_RLM_group_d <- tdStats(one_NO_group_d$RLM,one_NO_group_d$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO_RLM_one     <- tdStats(one_NO$RLM,one_NO$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_one_NO_RLM <- as.data.frame(rbind(res_one_NO_RLM_group_a,
                                          res_one_NO_RLM_group_b,
                                          res_one_NO_RLM_group_c,
                                          res_one_NO_RLM_group_d,
                                          res_one_NO_RLM_one))
    
    res_one_NO_RLM$x <- sign((res_one_NO_RLM$sdm)-(res_one_NO_RLM$sdo))/(res_one_NO_RLM$sdo)*sqrt((res_one_NO_RLM$rmse)^2-(res_one_NO_RLM$mbe)^2)
    res_one_NO_RLM$y <- (res_one_NO_RLM$mbe)/(res_one_NO_RLM$sdo)
    res_one_NO_RLM$nrmse <- sqrt((res_one_NO_RLM$x)^2+(res_one_NO_RLM$y)^2)
    
    res_one_NO_RF_group_a <- tdStats(one_NO_group_a$RF,one_NO_group_a$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO_RF_group_b <- tdStats(one_NO_group_b$RF,one_NO_group_b$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO_RF_group_c <- tdStats(one_NO_group_c$RF,one_NO_group_c$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO_RF_group_d <- tdStats(one_NO_group_d$RF,one_NO_group_d$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO_RF_one     <- tdStats(one_NO$RF,one_NO$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_one_NO_RF <- as.data.frame(rbind(res_one_NO_RF_group_a,
                                         res_one_NO_RF_group_b,
                                         res_one_NO_RF_group_c,
                                         res_one_NO_RF_group_d,
                                         res_one_NO_RF_one))
    
    res_one_NO_RF$x <- sign((res_one_NO_RF$sdm)-(res_one_NO_RF$sdo))/(res_one_NO_RF$sdo)*sqrt((res_one_NO_RF$rmse)^2-(res_one_NO_RF$mbe)^2)
    res_one_NO_RF$y <- (res_one_NO_RF$mbe)/(res_one_NO_RF$sdo)
    res_one_NO_RF$nrmse <- sqrt((res_one_NO_RF$x)^2+(res_one_NO_RF$y)^2)
    
  }
  
  ## one_NO2 --------
  {
    one_NO2$labeling <- as.character(0)
    
    for (i in 1:dim(one_NO2)[1]){
      if(one_NO2$Ref[i] < 9){one_NO2$labeling[i] <- as.character("Group_a")}
      if(one_NO2$Ref[i] >= 9 && one_NO2$Ref[i] < 17){one_NO2$labeling[i] <- as.character("Group_b")}
      if(one_NO2$Ref[i] >= 17 && one_NO2$Ref[i] < 26){one_NO2$labeling[i] <- as.character("Group_c")}
      if(one_NO2$Ref[i] >= 26){one_NO2$labeling[i] <- as.character("Group_d")}
    }
    
    one_NO2_group_a <- filter(one_NO2, one_NO2$labeling == "Group_a")
    one_NO2_group_b <- filter(one_NO2, one_NO2$labeling == "Group_b")
    one_NO2_group_c <- filter(one_NO2, one_NO2$labeling == "Group_c")
    one_NO2_group_d <- filter(one_NO2, one_NO2$labeling == "Group_d")
    
    
    res_one_NO2_RLM_group_a <- tdStats(one_NO2_group_a$RLM,one_NO2_group_a$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO2_RLM_group_b <- tdStats(one_NO2_group_b$RLM,one_NO2_group_b$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO2_RLM_group_c <- tdStats(one_NO2_group_c$RLM,one_NO2_group_c$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO2_RLM_group_d <- tdStats(one_NO2_group_d$RLM,one_NO2_group_d$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO2_RLM_one     <- tdStats(one_NO2$RLM,one_NO2$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_one_NO2_RLM <- as.data.frame(rbind(res_one_NO2_RLM_group_a,
                                           res_one_NO2_RLM_group_b,
                                           res_one_NO2_RLM_group_c,
                                           res_one_NO2_RLM_group_d,
                                           res_one_NO2_RLM_one))
    
    res_one_NO2_RLM$x <- sign((res_one_NO2_RLM$sdm)-(res_one_NO2_RLM$sdo))/(res_one_NO2_RLM$sdo)*sqrt((res_one_NO2_RLM$rmse)^2-(res_one_NO2_RLM$mbe)^2)
    res_one_NO2_RLM$y <- (res_one_NO2_RLM$mbe)/(res_one_NO2_RLM$sdo)
    res_one_NO2_RLM$nrmse <- sqrt((res_one_NO2_RLM$x)^2+(res_one_NO2_RLM$y)^2)
    
    res_one_NO2_RF_group_a <- tdStats(one_NO2_group_a$RF,one_NO2_group_a$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO2_RF_group_b <- tdStats(one_NO2_group_b$RF,one_NO2_group_b$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO2_RF_group_c <- tdStats(one_NO2_group_c$RF,one_NO2_group_c$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO2_RF_group_d <- tdStats(one_NO2_group_d$RF,one_NO2_group_d$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_one_NO2_RF_one     <- tdStats(one_NO2$RF,one_NO2$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_one_NO2_RF <- as.data.frame(rbind(res_one_NO2_RF_group_a,
                                          res_one_NO2_RF_group_b,
                                          res_one_NO2_RF_group_c,
                                          res_one_NO2_RF_group_d,
                                          res_one_NO2_RF_one))
    
    res_one_NO2_RF$x <- sign((res_one_NO2_RF$sdm)-(res_one_NO2_RF$sdo))/(res_one_NO2_RF$sdo)*sqrt((res_one_NO2_RF$rmse)^2-(res_one_NO2_RF$mbe)^2)
    res_one_NO2_RF$y <- (res_one_NO2_RF$mbe)/(res_one_NO2_RF$sdo)
    res_one_NO2_RF$nrmse <- sqrt((res_one_NO2_RF$x)^2+(res_one_NO2_RF$y)^2)
    
  }
  
  res_one_NO_RLM <- round(res_one_NO_RLM , digits=2)
  res_one_NO_RF<- round(res_one_NO_RF , digits=2)
  res_one_NO2_RLM <- round(res_one_NO2_RLM , digits=2)
  res_one_NO2_RF<- round(res_one_NO2_RF , digits=2)
}


# 2nd colocation period data --------------------------------------------------

import_2nd<- function(sensorunit) {
  sensorunit <- "AC11"
  ## Data import
  {
    data_NO_00<- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO_00.csv",sep=""), header=TRUE)
    data_NO_01 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO_01.csv",sep=""), header=TRUE)
    data_NO2_00 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO2_00.csv",sep=""), header=TRUE)
    data_NO2_01 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO2_01.csv",sep=""), header=TRUE)
  }
  
  ## merging data to NO and NO2 
  {
    ### NO_00
    {
      two_data_NO_00 <- as.data.frame(cbind(data_NO_00$Date.UTC.,data_NO_00$RLM,data_NO_00$RF,data_NO_00$Reference))
      colnames(two_data_NO_00) <- c("Date","RLM","RF","Ref")
      two_data_NO_00$Date <- as.POSIXct(two_data_NO_00$Date, origin="1970-01-01", tz='UTC')
      two_data_NO_00$RLM <- as.numeric(two_data_NO_00$RLM)
      two_data_NO_00$RF <- as.numeric(two_data_NO_00$RF)
      two_data_NO_00$Ref <- as.numeric(two_data_NO_00$Ref)
    }
    ### NO_01
    {
      two_data_NO_01 <- as.data.frame(cbind(data_NO_01$Date.UTC.,data_NO_01$RLM,data_NO_01$RF,data_NO_01$Reference))
      colnames(two_data_NO_01) <- c("Date","RLM","RF","Ref")
      two_data_NO_01$Date <- as.POSIXct(two_data_NO_01$Date, origin="1970-01-01", tz='UTC')
      two_data_NO_01$RLM <- as.numeric(two_data_NO_01$RLM)
      two_data_NO_01$RF <- as.numeric(two_data_NO_01$RF)
      two_data_NO_01$Ref <- as.numeric(two_data_NO_01$Ref)
    }
    ### NO2_00
    {
      two_data_NO2_00 <- as.data.frame(cbind(data_NO2_00$Date.UTC.,data_NO2_00$RLM,data_NO2_00$RF,data_NO2_00$Reference))
      colnames(two_data_NO2_00) <- c("Date","RLM","RF","Ref")
      two_data_NO2_00$Date <- as.POSIXct(two_data_NO2_00$Date, origin="1970-01-01", tz='UTC')
      two_data_NO2_00$RLM <- as.numeric(two_data_NO2_00$RLM)
      two_data_NO2_00$RF <- as.numeric(two_data_NO2_00$RF)
      two_data_NO2_00$Ref <- as.numeric(two_data_NO2_00$Ref)
    }
    ### NO2_01
    {
      two_data_NO2_01 <- as.data.frame(cbind(data_NO2_01$Date.UTC.,data_NO2_01$RLM,data_NO2_01$RF,data_NO2_01$Reference))
      colnames(two_data_NO2_01) <- c("Date","RLM","RF","Ref")
      two_data_NO2_01$Date <- as.POSIXct(two_data_NO2_01$Date, origin="1970-01-01", tz='UTC')
      two_data_NO2_01$RLM <- as.numeric(two_data_NO2_01$RLM)
      two_data_NO2_01$RF <- as.numeric(two_data_NO2_01$RF)
      two_data_NO2_01$Ref <- as.numeric(two_data_NO2_01$Ref)
    }
  }
  
  two_NO <- as.data.frame(rbind(two_data_NO_00,two_data_NO_01))
  two_NO2 <- as.data.frame(rbind(two_data_NO2_00,two_data_NO2_01))  
  
  r.list <- list(two_NO, two_NO2)
  
  return(r.list)
}

## with the function above, derive the statistical metrics in each concentration range.
{
  ## Take the data from the function 
  AC9_2nd_NO <- as.data.frame(import_2nd("AC9")[[1]])
  AC9_2nd_NO2 <- as.data.frame(import_2nd("AC9")[[2]])
  AC10_2nd_NO <- as.data.frame(import_2nd("AC10")[[1]])
  AC10_2nd_NO2 <- as.data.frame(import_2nd("AC10")[[2]])
  AC11_2nd_NO <- as.data.frame(import_2nd("AC11")[[1]])
  AC11_2nd_NO2 <- as.data.frame(import_2nd("AC11")[[2]])
  AC12_2nd_NO <- as.data.frame(import_2nd("AC12")[[1]])
  AC12_2nd_NO2 <- as.data.frame(import_2nd("AC12")[[2]])
  
  ## Merge datasets to each pollutant group
  two_NO <- as.data.frame(rbind(AC9_2nd_NO,AC10_2nd_NO,AC11_2nd_NO,AC12_2nd_NO))
  two_NO2 <- as.data.frame(rbind(AC9_2nd_NO2,AC10_2nd_NO2,AC11_2nd_NO2,AC12_2nd_NO2))
  
  ### Then grouping and derive statistical metrics
  ## two_NO --------
  {
    two_NO$labeling <- as.character(0)
    
    for (i in 1:dim(two_NO)[1]){
      if(two_NO$Ref[i] < 2){two_NO$labeling[i] <- as.character("Group_a")}
      if(two_NO$Ref[i] >= 2 && two_NO$Ref[i] < 10){two_NO$labeling[i] <- as.character("Group_b")}
      if(two_NO$Ref[i] >= 10 && two_NO$Ref[i] < 28){two_NO$labeling[i] <- as.character("Group_c")}
      if(two_NO$Ref[i] >= 28){two_NO$labeling[i] <- as.character("Group_d")}
    }
    
    two_NO_group_a <- filter(two_NO, two_NO$labeling == "Group_a")
    two_NO_group_b <- filter(two_NO, two_NO$labeling == "Group_b")
    two_NO_group_c <- filter(two_NO, two_NO$labeling == "Group_c")
    two_NO_group_d <- filter(two_NO, two_NO$labeling == "Group_d")
    
    
    res_two_NO_RLM_group_a <- tdStats(two_NO_group_a$RLM,two_NO_group_a$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO_RLM_group_b <- tdStats(two_NO_group_b$RLM,two_NO_group_b$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO_RLM_group_c <- tdStats(two_NO_group_c$RLM,two_NO_group_c$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO_RLM_group_d <- tdStats(two_NO_group_d$RLM,two_NO_group_d$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO_RLM_one     <- tdStats(two_NO$RLM,two_NO$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_two_NO_RLM <- as.data.frame(rbind(res_two_NO_RLM_group_a,
                                          res_two_NO_RLM_group_b,
                                          res_two_NO_RLM_group_c,
                                          res_two_NO_RLM_group_d,
                                          res_two_NO_RLM_one))
    
    res_two_NO_RLM$x <- sign((res_two_NO_RLM$sdm)-(res_two_NO_RLM$sdo))/(res_two_NO_RLM$sdo)*sqrt((res_two_NO_RLM$rmse)^2-(res_two_NO_RLM$mbe)^2)
    res_two_NO_RLM$y <- (res_two_NO_RLM$mbe)/(res_two_NO_RLM$sdo)
    res_two_NO_RLM$nrmse <- sqrt((res_two_NO_RLM$x)^2+(res_two_NO_RLM$y)^2)
    
    res_two_NO_RF_group_a <- tdStats(two_NO_group_a$RF,two_NO_group_a$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO_RF_group_b <- tdStats(two_NO_group_b$RF,two_NO_group_b$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO_RF_group_c <- tdStats(two_NO_group_c$RF,two_NO_group_c$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO_RF_group_d <- tdStats(two_NO_group_d$RF,two_NO_group_d$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO_RF_one     <- tdStats(two_NO$RF,two_NO$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_two_NO_RF <- as.data.frame(rbind(res_two_NO_RF_group_a,
                                         res_two_NO_RF_group_b,
                                         res_two_NO_RF_group_c,
                                         res_two_NO_RF_group_d,
                                         res_two_NO_RF_one))
    
    res_two_NO_RF$x <- sign((res_two_NO_RF$sdm)-(res_two_NO_RF$sdo))/(res_two_NO_RF$sdo)*sqrt((res_two_NO_RF$rmse)^2-(res_two_NO_RF$mbe)^2)
    res_two_NO_RF$y <- (res_two_NO_RF$mbe)/(res_two_NO_RF$sdo)
    res_two_NO_RF$nrmse <- sqrt((res_two_NO_RF$x)^2+(res_two_NO_RF$y)^2)
    
  }
  
  ## two_NO2 --------
  {
    two_NO2$labeling <- as.character(0)
    
    for (i in 1:dim(two_NO2)[1]){
      if(two_NO2$Ref[i] < 9){two_NO2$labeling[i] <- as.character("Group_a")}
      if(two_NO2$Ref[i] >= 9 && two_NO2$Ref[i] < 17){two_NO2$labeling[i] <- as.character("Group_b")}
      if(two_NO2$Ref[i] >= 17 && two_NO2$Ref[i] < 26){two_NO2$labeling[i] <- as.character("Group_c")}
      if(two_NO2$Ref[i] >= 26){two_NO2$labeling[i] <- as.character("Group_d")}
    }
    
    two_NO2_group_a <- filter(two_NO2, two_NO2$labeling == "Group_a")
    two_NO2_group_b <- filter(two_NO2, two_NO2$labeling == "Group_b")
    two_NO2_group_c <- filter(two_NO2, two_NO2$labeling == "Group_c")
    two_NO2_group_d <- filter(two_NO2, two_NO2$labeling == "Group_d")
    
    
    res_two_NO2_RLM_group_a <- tdStats(two_NO2_group_a$RLM,two_NO2_group_a$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO2_RLM_group_b <- tdStats(two_NO2_group_b$RLM,two_NO2_group_b$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO2_RLM_group_c <- tdStats(two_NO2_group_c$RLM,two_NO2_group_c$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO2_RLM_group_d <- tdStats(two_NO2_group_d$RLM,two_NO2_group_d$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO2_RLM_one     <- tdStats(two_NO2$RLM,two_NO2$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_two_NO2_RLM <- as.data.frame(rbind(res_two_NO2_RLM_group_a,
                                           res_two_NO2_RLM_group_b,
                                           res_two_NO2_RLM_group_c,
                                           res_two_NO2_RLM_group_d,
                                           res_two_NO2_RLM_one))
    
    res_two_NO2_RLM$x <- sign((res_two_NO2_RLM$sdm)-(res_two_NO2_RLM$sdo))/(res_two_NO2_RLM$sdo)*sqrt((res_two_NO2_RLM$rmse)^2-(res_two_NO2_RLM$mbe)^2)
    res_two_NO2_RLM$y <- (res_two_NO2_RLM$mbe)/(res_two_NO2_RLM$sdo)
    res_two_NO2_RLM$nrmse <- sqrt((res_two_NO2_RLM$x)^2+(res_two_NO2_RLM$y)^2)
    
    res_two_NO2_RF_group_a <- tdStats(two_NO2_group_a$RF,two_NO2_group_a$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO2_RF_group_b <- tdStats(two_NO2_group_b$RF,two_NO2_group_b$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO2_RF_group_c <- tdStats(two_NO2_group_c$RF,two_NO2_group_c$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO2_RF_group_d <- tdStats(two_NO2_group_d$RF,two_NO2_group_d$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    res_two_NO2_RF_one     <- tdStats(two_NO2$RF,two_NO2$Ref, functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_two_NO2_RF <- as.data.frame(rbind(res_two_NO2_RF_group_a,
                                          res_two_NO2_RF_group_b,
                                          res_two_NO2_RF_group_c,
                                          res_two_NO2_RF_group_d,
                                          res_two_NO2_RF_one))
    
    res_two_NO2_RF$x <- sign((res_two_NO2_RF$sdm)-(res_two_NO2_RF$sdo))/(res_two_NO2_RF$sdo)*sqrt((res_two_NO2_RF$rmse)^2-(res_two_NO2_RF$mbe)^2)
    res_two_NO2_RF$y <- (res_two_NO2_RF$mbe)/(res_two_NO2_RF$sdo)
    res_two_NO2_RF$nrmse <- sqrt((res_two_NO2_RF$x)^2+(res_two_NO2_RF$y)^2)
    
  }
  
  res_two_NO_RLM <- round(res_two_NO_RLM , digits=2)
  res_two_NO_RF<- round(res_two_NO_RF , digits=2)
  res_two_NO2_RLM <- round(res_two_NO2_RLM , digits=2)
  res_two_NO2_RF<- round(res_two_NO2_RF , digits=2)
}

# 3. density plots
{
  HAEref <- read.csv2("data_input/HAE_reference/HAE.csv", header = FALSE)
  
  # Delete reluctant rows and store the header information
  
  HAEref <- rename(HAEref, "Date" = "V1","SO2_ppb" = "V2",
                   "NOx_ppb" = "V3", "NO2_ppb" = "V4",
                   "NO_ppb" = "V5", "O3_ppb" = "V6",
                   "CO_ppb" = "V7", "DRUCK_hPa" = "V8",
                   "TEMP_C" = "V9", "FEUCHTE_%" = "V10",
                   "WIRI_GRAD" = "V11", "WIGE_m/s" = "V12",
                   "WIGEM_m/s" = "V13", "REGEN_mm" = "V14",
                   "P_Anz_1/cm3" = "V15", "PM10_kon_ug/m3" = "V16",
                   "Russ_25_ug/m3" = "V17")
  
  HAEref <- HAEref[-c(1,2,3,4,5),]
  
  # Timestamp insertion to to synchronize date and time with the sensor data-----
  
  HAEref$Date <- dmy_hm(HAEref$Date) -3600 
  
  HAEref$timestamp <- as.numeric(difftime(time1=HAEref$Date,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
  
  for (i in 1:105264)
  {
    if(is.na(HAEref$timestamp[i])== TRUE)
    { HAEref$timestamp[i] <- HAEref$timestamp[i-1] + 600}
  }
  
  
  HAEref$Date <- as.POSIXct(HAEref$timestamp, origin="1970-01-01", tz='UTC')
  HAEref$Date <- ymd_hms(HAEref$Date)
  
  for (i in 2:17)
  {
    if (is.numeric(HAEref[,i])== FALSE)
    {
      HAEref[,i] <- as.numeric(HAEref[,i])
    }
  }
  
  rownames(HAEref) <- NULL
  
  HAEref_1st_period <- HAEref[4038:28085,]
  
  HAEref_2nd_period <- HAEref[80502:96485,]
  
  NO <- as.vector(rep("NO", 24048))
  NO2 <- as.vector(rep("NO2", 24048))
  Pollutant <- c(NO,NO2)
  `Concentration [ppb]` <- c(HAEref_1st_period$NO_ppb,HAEref_1st_period$NO2_ppb)
  
  NOx_1st_period <-as.data.frame(cbind(Pollutant,`Concentration [ppb]`))
  NOx_1st_period$`Concentration [ppb]` <- as.numeric(NOx_1st_period$`Concentration [ppb]`)
}

## plotting ----------------------------------------------------------------------
# aggregate the data

data_for_td <- as.data.frame(matrix(0,32,10))
colnames(data_for_td) <-c("Period","Pollutants","Model","Group","RMSE","MBE","r2","x","y","nRMSE")
data_for_td$Period <- c(rep("1st",16),rep("2nd",16))
data_for_td$Pollutants <- c(rep("NO",8),rep("NO2",8),rep("NO",8),rep("NO2",8))
data_for_td$Model <- c(rep("RLM",4),rep("RF",4),rep("RLM",4),rep("RF",4),rep("RLM",4),rep("RF",4),rep("RLM",4),rep("RF",4))
groupname <- c("a","b","c","d")
data_for_td$Group <- rep(groupname,8)
data_for_td[1:4,5:10] <- res_one_NO_RLM[1:4,c(1,2,5,6,7,8)]
data_for_td[5:8,5:10] <- res_one_NO_RF[1:4,c(1,2,5,6,7,8)]
data_for_td[9:12,5:10] <- res_one_NO2_RLM[1:4,c(1,2,5,6,7,8)]
data_for_td[13:16,5:10] <- res_one_NO2_RF[1:4,c(1,2,5,6,7,8)]
data_for_td[17:20,5:10] <- res_two_NO_RLM[1:4,c(1,2,5,6,7,8)]
data_for_td[21:24,5:10] <- res_two_NO_RF[1:4,c(1,2,5,6,7,8)]
data_for_td[25:28,5:10] <- res_two_NO2_RLM[1:4,c(1,2,5,6,7,8)]
data_for_td[29:32,5:10] <- res_two_NO2_RF[1:4,c(1,2,5,6,7,8)]

write.csv(res_one_NO_RLM, "data_output/results_colocation_metrics/res_one_NO_RLM.csv")
write.csv(res_one_NO_RF, "data_output/results_colocation_metrics/res_one_NO_RF.csv")
write.csv(res_one_NO2_RLM, "data_output/results_colocation_metrics/res_one_NO2_RLM.csv")
write.csv(res_one_NO2_RF, "data_output/results_colocation_metrics/res_one_NO2_RF.csv")
write.csv(res_two_NO_RLM, "data_output/results_colocation_metrics/res_two_NO_RLM.csv")
write.csv(res_two_NO_RF, "data_output/results_colocation_metrics/res_two_NO_RF.csv")
write.csv(res_two_NO2_RLM, "data_output/results_colocation_metrics/res_two_NO2_RLM.csv")
write.csv(res_two_NO2_RF, "data_output/results_colocation_metrics/res_two_NO2_RF.csv")


td_NO <- data_for_td %>% filter(Pollutants == "NO")
td_NO2 <- data_for_td %>% filter(Pollutants == "NO2")



for(i in 1:16)
{
  if(td_NO$Group[i] == "a"){td_NO$Group[i] <- "X < 2"}
  if(td_NO$Group[i] == "b"){td_NO$Group[i] <- "2 <= X < 10"}
  if(td_NO$Group[i] == "c"){td_NO$Group[i] <- "10 <= X < 28"}
  if(td_NO$Group[i] == "d"){td_NO$Group[i] <- "X >= 28"}
}

for(i in 1:16)
{
  if(td_NO2$Group[i] == "a"){td_NO2$Group[i] <- "X < 9"}
  if(td_NO2$Group[i] == "b"){td_NO2$Group[i] <- "9 <= X < 17"}
  if(td_NO2$Group[i] == "c"){td_NO2$Group[i] <- "17 <= X < 26"}
  if(td_NO2$Group[i] == "d"){td_NO2$Group[i] <- "X >= 26"}
}

td_NO$Group <- factor(td_NO$Group, levels = c("X < 2",
                                              "2 <= X < 10",
                                              "10 <= X < 28",
                                              "X >= 28"))

td_NO2$Group <- factor(td_NO2$Group, levels = c("X < 9",
                                                "9 <= X < 17",
                                                "17 <= X < 26",
                                                "X >= 26"))


colnames(td_NO)[4] <- "Ref. concentration [ppb]"
colnames(td_NO2)[4] <- "Ref. concentration [ppb]"


td_NO$newcolumn <- c(rep("1st & RLM",4),rep("1st & RF",4),
                     rep("2nd & RLM",4),rep("2nd & RF",4))

colnames(td_NO)[11] <- "Period & Model"

td_NO2$newcolumn <- c(rep("1st & RLM",4),rep("1st & RF",4),
                     rep("2nd & RLM",4),rep("2nd & RF",4))

colnames(td_NO2)[11] <- "Period & Model"

# 1) target plots for NO ----------------------------------------------------
pNO <- ggplot(td_NO, aes(x,y,color= `Ref. concentration [ppb]`, shape = `Period & Model`))+geom_point(size=1.8
)+ggtitle("Target diagram : NO"
)+labs(x=expression(paste("CRMSE/",sigma["y"])),y=expression(paste("MBE/",sigma["y"]))
)+xlim(c(-11,11))+ylim(c(-11,11)
)+geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE,col="black",size=0.2
)+geom_circle(aes(x0 = 0, y0 = 0, r = 5), inherit.aes = FALSE,col="black",size=0.2
)+geom_circle(aes(x0 = 0, y0 = 0, r = 10), inherit.aes = FALSE,col="black",size=0.2
)+geom_point(aes(0,0),col="black",shape=3, size=1
)+scale_shape_manual(values = c(17,15,4,8)
)+ theme(axis.text = element_text(size = 5)
)+ theme(axis.title = element_text(size = 3)
)+ theme(plot.title = element_text(size = 4)
)+ theme(legend.text = element_text(size = 5)
)+ theme(legend.title = element_text(size = 4)) +theme_bw(
)+guides(colour = guide_legend(override.aes = list(size=4),order=1)
)+guides(shape = guide_legend(override.aes = list(size=4),order=0)
)+scale_color_manual(values=c("red2","orange","forestgreen","deepskyblue")
)#+ theme(legend.position="none")

# 2) target plots for NO2 --------------------------------------------------------
pNO2<- ggplot(td_NO2, aes(x,y,color= `Ref. concentration [ppb]`, shape = `Period & Model`))+geom_point(size=1.8
)+ggtitle("Target diagram : NO2"
)+labs(x=expression(paste("CRMSE/",sigma["y"])),y=expression(paste("MBE/",sigma["y"]))
)+xlim(c(-5,5))+ylim(c(-5,5)
)+geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE,col="black",size=0.2
)+geom_circle(aes(x0 = 0, y0 = 0, r = 5), inherit.aes = FALSE,col="black",size=0.2
)+geom_point(aes(0,0),col="black",shape=3, size=1
)+scale_shape_manual(values = c(17,15,4,8)
)+ theme(axis.text = element_text(size = 5)
)+ theme(axis.title = element_text(size = 3)
)+ theme(plot.title = element_text(size = 4)
)+ theme(legend.text = element_text(size = 5)
)+ theme(legend.title = element_text(size = 4)) +theme_bw(
)+guides(colour = guide_legend(override.aes = list(size=4),order = 1)
)+guides(shape = guide_legend(override.aes = list(size=4),order=0)
)+scale_color_manual(values=c("red2","orange","forestgreen","deepskyblue")
)#+ theme(legend.position="none")

# + theme(legend.position="none")
# 3) density plots ------------------------------------------------------------
NOx_1st_period <- na.omit(NOx_1st_period)

DP <- ggplot(NOx_1st_period, aes(x=`Concentration [ppb]`,y=Pollutant)
)+geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+theme_bw(
)+scale_color_manual(values=c("#2a9d8f","#f4a261")
)+scale_fill_manual(values=c("#2a9d8f","#f4a261"))+ggtitle("Distribution of reference NO and NO2 concentration in the first colocation period"
)

# Figure 6 ------------------------------------------------------------------
jpeg("figure/fig_6_DP.jpeg", width =20, height = 6, units = 'cm', res = 600)
ggarrange(DP, nrow = 1,labels = "(a)") 
dev.off()

jpeg("figure/fig_6_target_diagram.jpeg", width = 7.6, height = 16, units = 'cm', res = 600)
ggarrange(pNO, pNO2,nrow = 2, labels = c("(b)","(d)"))
dev.off()
