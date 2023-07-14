# Figure 6 : Plotting concentration range-wise taylor diagram.
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
    
  }

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
    
   
  }
  
}


## plotting ----------------------------------------------------------------------
# Taylor diagram --------------------
{
  ## NO
  jpeg("figure/fig_6_taylor_diagram_NO.jpeg", width =11, height = 11, units = 'cm', res = 1200)
  {
    ## 1st
    ## group_a
    {
    taylor.diagram(one_NO_group_a$Ref, # makes a vector
                   one_NO_group_a$RLM,
                   add=FALSE,
                   col="red2",
                   pch=17,
                   pos.cor=TRUE,
                   xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                   ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                   main="Taylor Diagram : NO",
                   show.gamma=TRUE,
                   ngamma=3,
                   sd.arcs=1,
                   ref.sd=TRUE,
                   grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                   pcex=1,cex.axis=0.8,
                   normalize=TRUE,
                   mar=c(4,4,4,4),
                   lwd=10,
                   font=7,
                   lty=3)
      
      taylor.diagram(one_NO_group_a$Ref, # makes a vector
                     one_NO_group_a$RF,
                     add=TRUE,
                     col="red2",
                     pch=15,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
    
    }
    ## group_b
    {
      taylor.diagram(one_NO_group_b$Ref, # makes a vector
                     one_NO_group_b$RLM,
                     add=TRUE,
                     col="orange",
                     pch=17,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(one_NO_group_b$Ref, # makes a vector
                     one_NO_group_b$RF,
                     add=TRUE,
                     col="orange",
                     pch=15,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_c
    {
      taylor.diagram(one_NO_group_c$Ref, # makes a vector
                     one_NO_group_c$RLM,
                     add=TRUE,
                     col="forestgreen",
                     pch=17,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(one_NO_group_c$Ref, # makes a vector
                     one_NO_group_c$RF,
                     add=TRUE,
                     col="forestgreen",
                     pch=15,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_d
    {
      taylor.diagram(one_NO_group_d$Ref, # makes a vector
                     one_NO_group_d$RLM,
                     add=TRUE,
                     col="deepskyblue",
                     pch=17,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(one_NO_group_d$Ref, # makes a vector
                     one_NO_group_d$RF,
                     add=TRUE,
                     col="deepskyblue",
                     pch=15,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    
    ## 2nd
    ## group_a
    {
      taylor.diagram(two_NO_group_a$Ref, # makes a vector
                     two_NO_group_a$RLM,
                     add=TRUE,
                     col="red2",
                     pch=4,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(two_NO_group_a$Ref, # makes a vector
                     two_NO_group_a$RF,
                     add=TRUE,
                     col="red2",
                     pch=8,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_b
    {
      taylor.diagram(two_NO_group_b$Ref, # makes a vector
                     two_NO_group_b$RLM,
                     add=TRUE,
                     col="orange",
                     pch=4,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(two_NO_group_b$Ref, # makes a vector
                     two_NO_group_b$RF,
                     add=TRUE,
                     col="orange",
                     pch=8,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_c
    {
      taylor.diagram(two_NO_group_c$Ref, # makes a vector
                     two_NO_group_c$RLM,
                     add=TRUE,
                     col="forestgreen",
                     pch=4,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(two_NO_group_c$Ref, # makes a vector
                     two_NO_group_c$RF,
                     add=TRUE,
                     col="forestgreen",
                     pch=8,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_d
    {
      taylor.diagram(two_NO_group_d$Ref, # makes a vector
                     two_NO_group_d$RLM,
                     add=TRUE,
                     col="deepskyblue",
                     pch=4,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(two_NO_group_d$Ref, # makes a vector
                     two_NO_group_d$RF,
                     add=TRUE,
                     col="deepskyblue",
                     pch=8,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    
  }
  dev.off()
  
  ## NO2
  jpeg("figure/fig_6_taylor_diagram_NO2.jpeg", width =11, height = 11, units = 'cm', res = 1200)
  {
    ## 2nd
    ## group_a
    {
      taylor.diagram(two_NO2_group_a$Ref, # makes a vector
                     two_NO2_group_a$RLM,
                     add=FALSE,
                     col="red2",
                     pch=4,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(two_NO2_group_a$Ref, # makes a vector
                     two_NO2_group_a$RF,
                     add=TRUE,
                     col="red2",
                     pch=8,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    
    ## 1st
    ## group_a
    {
      taylor.diagram(one_NO2_group_a$Ref, # makes a vector
                     one_NO2_group_a$RLM,
                     add=TRUE,
                     col="red2",
                     pch=17,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(one_NO2_group_a$Ref, # makes a vector
                     one_NO2_group_a$RF,
                     add=TRUE,
                     col="red2",
                     pch=15,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_b
    {
      taylor.diagram(one_NO2_group_b$Ref, # makes a vector
                     one_NO2_group_b$RLM,
                     add=TRUE,
                     col="orange",
                     pch=17,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(one_NO2_group_b$Ref, # makes a vector
                     one_NO2_group_b$RF,
                     add=TRUE,
                     col="orange",
                     pch=15,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_c
    {
      taylor.diagram(one_NO2_group_c$Ref, # makes a vector
                     one_NO2_group_c$RLM,
                     add=TRUE,
                     col="forestgreen",
                     pch=17,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(one_NO2_group_c$Ref, # makes a vector
                     one_NO2_group_c$RF,
                     add=TRUE,
                     col="forestgreen",
                     pch=15,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_d
    {
      taylor.diagram(one_NO2_group_d$Ref, # makes a vector
                     one_NO2_group_d$RLM,
                     add=TRUE,
                     col="deepskyblue",
                     pch=17,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(one_NO2_group_d$Ref, # makes a vector
                     one_NO2_group_d$RF,
                     add=TRUE,
                     col="deepskyblue",
                     pch=15,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    
    ## 2nd
    ## group_b
    {
      taylor.diagram(two_NO2_group_b$Ref, # makes a vector
                     two_NO2_group_b$RLM,
                     add=TRUE,
                     col="orange",
                     pch=4,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(two_NO2_group_b$Ref, # makes a vector
                     two_NO2_group_b$RF,
                     add=TRUE,
                     col="orange",
                     pch=8,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_c
    {
      taylor.diagram(two_NO2_group_c$Ref, # makes a vector
                     two_NO2_group_c$RLM,
                     add=TRUE,
                     col="forestgreen",
                     pch=4,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(two_NO2_group_c$Ref, # makes a vector
                     two_NO2_group_c$RF,
                     add=TRUE,
                     col="forestgreen",
                     pch=8,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    ## group_d
    {
      taylor.diagram(two_NO2_group_d$Ref, # makes a vector
                     two_NO2_group_d$RLM,
                     add=TRUE,
                     col="deepskyblue",
                     pch=4,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
      taylor.diagram(two_NO2_group_d$Ref, # makes a vector
                     two_NO2_group_d$RF,
                     add=TRUE,
                     col="deepskyblue",
                     pch=8,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO",
                     show.gamma=TRUE,
                     ngamma=3,
                     sd.arcs=1,
                     ref.sd=TRUE,
                     grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9),
                     pcex=1,cex.axis=0.8,
                     normalize=TRUE,
                     mar=c(4,4,4,4),
                     lwd=10,
                     font=7,
                     lty=3)
      
    }
    
  }
  dev.off()

}