# Figure 5 (Target Diagram) : Target diagrams and taylor diagrams
#
#
# Library ----------------------------------
## libraries
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

setwd("D:/empa_intern/Paper/data_and_code/")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Data from 1st colocation period ---------------------------------
{
  ## import the calibrated data, merge with reference, and get metrics
  metrics_1st <- function(sensorunit){
    # calibrated data ----------------------------------------------------
    {
    RE_M_data_1st_NO_00_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RLM.csv",sep = ""), header=TRUE)
    RE_M_data_1st_NO_00_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RF.csv",sep = ""), header=TRUE)
    RE_M_data_1st_NO_01_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RLM.csv",sep = ""), header=TRUE)
    RE_M_data_1st_NO_01_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RF.csv",sep = ""), header=TRUE)
    RE_M_data_1st_NO2_00_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RLM.csv",sep = ""), header=TRUE)
    RE_M_data_1st_NO2_00_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RF.csv",sep = ""), header=TRUE)
    RE_M_data_1st_NO2_01_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RLM.csv",sep = ""), header=TRUE)
    RE_M_data_1st_NO2_01_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RF.csv",sep = ""), header=TRUE)
    }
    ## Import the reference
    {
    ref_1st <- read.csv(paste("data_output/pre_processed_1st_period/",sensorunit,".csv",sep = ""), header = TRUE)
    
    ref_1st$X <- seq(from = 1, to = length(ref_1st$date) ,by = 1)
    ref_1st$date <- ymd_hms(ref_1st$date)
    
    ref_1st <- as_tibble(ref_1st)
    
    ref_1st <- as_tbl_time(ref_1st, index = date)
    }
    # tdStats ------------------------------------------
    {
    
    ## NO_00
    {
      td_1st_data_NO_00_RLM <- tdStats(RE_M_data_1st_NO_00_RLM$NO.ppb., ref_1st$REFNO, functions = c("rmse", "mbe", "sdm", "sdo"))
      td_1st_data_NO_00_RF <- tdStats(RE_M_data_1st_NO_00_RF$NO.ppb., ref_1st$REFNO, functions = c("rmse", "mbe", "sdm", "sdo"))
      
      td_1st_data_NO_00 <- data.frame(rbind(td_1st_data_NO_00_RLM,
                                            td_1st_data_NO_00_RF))
      
      td_1st_data_NO_00$model<- c("RLM","RF")
      td_1st_data_NO_00$pollutants <- c("NO","NO")
      td_1st_data_NO_00$sensor<- c(sensorunit,sensorunit)
      td_1st_data_NO_00$period <- c("1st","1st")
      
    }
    ## NO_01
    {
      td_1st_data_NO_01_RLM <- tdStats(RE_M_data_1st_NO_01_RLM$NO.ppb., ref_1st$REFNO, functions = c("rmse", "mbe", "sdm", "sdo"))
      td_1st_data_NO_01_RF <- tdStats(RE_M_data_1st_NO_01_RF$NO.ppb., ref_1st$REFNO, functions = c("rmse", "mbe", "sdm", "sdo"))
      
      td_1st_data_NO_01 <- data.frame(rbind(td_1st_data_NO_01_RLM,
                                            td_1st_data_NO_01_RF))
      
      td_1st_data_NO_01$model<- c("RLM","RF")
      td_1st_data_NO_01$pollutants <- c("NO","NO")
      td_1st_data_NO_01$sensor<- c(sensorunit,sensorunit)
      td_1st_data_NO_01$period <- c("1st","1st")
      
    }
    ## NO2_00
    {
      td_1st_data_NO2_00_RLM <- tdStats(RE_M_data_1st_NO2_00_RLM$NO2.ppb., ref_1st$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo"))
      td_1st_data_NO2_00_RF <- tdStats(RE_M_data_1st_NO2_00_RF$NO2.ppb., ref_1st$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo"))
      
      td_1st_data_NO2_00 <- data.frame(rbind(td_1st_data_NO2_00_RLM,
                                             td_1st_data_NO2_00_RF))
      
      td_1st_data_NO2_00$model <- c("RLM","RF")
      td_1st_data_NO2_00$pollutants <- c("NO2","NO2")
      td_1st_data_NO2_00$sensor<- c(sensorunit,sensorunit)
      td_1st_data_NO2_00$period <- c("1st","1st")
    }
    ## NO2_01
    {
      td_1st_data_NO2_01_RLM <- tdStats(RE_M_data_1st_NO2_01_RLM$NO2.ppb., ref_1st$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo"))
      td_1st_data_NO2_01_RF <- tdStats(RE_M_data_1st_NO2_01_RF$NO2.ppb., ref_1st$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo"))
      
      td_1st_data_NO2_01 <- data.frame(rbind(td_1st_data_NO2_01_RLM,
                                             td_1st_data_NO2_01_RF))
      
      td_1st_data_NO2_01$model <- c("RLM","RF")
      td_1st_data_NO2_01$pollutants <- c("NO2","NO2")
      td_1st_data_NO2_01$sensor<- c(sensorunit,sensorunit)
      td_1st_data_NO2_01$period <- c("1st","1st")
    }
    
    td_1st_data_NO_00$x <- sign((td_1st_data_NO_00$sdm)-(td_1st_data_NO_00$sdo))/(td_1st_data_NO_00$sdo)*sqrt((td_1st_data_NO_00$rmse)^2-(td_1st_data_NO_00$mbe)^2)
    td_1st_data_NO_00$y <- (td_1st_data_NO_00$mbe)/(td_1st_data_NO_00$sdo)
    td_1st_data_NO_00$nrmse <- sqrt((td_1st_data_NO_00$x)^2+(td_1st_data_NO_00$y)^2)
    
    td_1st_data_NO_01$x <- sign((td_1st_data_NO_01$sdm)-(td_1st_data_NO_01$sdo))/(td_1st_data_NO_01$sdo)*sqrt((td_1st_data_NO_01$rmse)^2-(td_1st_data_NO_01$mbe)^2)
    td_1st_data_NO_01$y <- (td_1st_data_NO_01$mbe)/(td_1st_data_NO_01$sdo)
    td_1st_data_NO_01$nrmse <- sqrt((td_1st_data_NO_01$x)^2+(td_1st_data_NO_01$y)^2)
    
    td_1st_data_NO2_00$x <- sign((td_1st_data_NO2_00$sdm)-(td_1st_data_NO2_00$sdo))/(td_1st_data_NO2_00$sdo)*sqrt((td_1st_data_NO2_00$rmse)^2-(td_1st_data_NO2_00$mbe)^2)
    td_1st_data_NO2_00$y <- (td_1st_data_NO2_00$mbe)/(td_1st_data_NO2_00$sdo)
    td_1st_data_NO2_00$nrmse <- sqrt((td_1st_data_NO2_00$x)^2+(td_1st_data_NO2_00$y)^2)
    
    td_1st_data_NO2_01$x <- sign((td_1st_data_NO2_01$sdm)-(td_1st_data_NO2_01$sdo))/(td_1st_data_NO2_01$sdo)*sqrt((td_1st_data_NO2_01$rmse)^2-(td_1st_data_NO2_01$mbe)^2)
    td_1st_data_NO2_01$y <- (td_1st_data_NO2_01$mbe)/(td_1st_data_NO2_01$sdo)
    td_1st_data_NO2_01$nrmse <- sqrt((td_1st_data_NO2_01$x)^2+(td_1st_data_NO2_01$y)^2)
    }
    
    r.list <- list(td_1st_data_NO_00,
                   td_1st_data_NO_01,
                   td_1st_data_NO2_00,
                   td_1st_data_NO2_01)
    
    return(r.list)
  
  }
  
  td_1st_AC9_NO_00 <- as.data.frame(metrics_1st("AC9")[[1]])
  td_1st_AC9_NO_01 <- as.data.frame(metrics_1st("AC9")[[2]])
  td_1st_AC9_NO2_00 <- as.data.frame(metrics_1st("AC9")[[3]])
  td_1st_AC9_NO2_01 <- as.data.frame(metrics_1st("AC9")[[4]])
  td_1st_AC10_NO_00 <- as.data.frame(metrics_1st("AC10")[[1]])
  td_1st_AC10_NO_01 <- as.data.frame(metrics_1st("AC10")[[2]])
  td_1st_AC10_NO2_00 <- as.data.frame(metrics_1st("AC10")[[3]])
  td_1st_AC10_NO2_01 <- as.data.frame(metrics_1st("AC10")[[4]])
  td_1st_AC11_NO_00 <- as.data.frame(metrics_1st("AC11")[[1]])
  td_1st_AC11_NO_01 <- as.data.frame(metrics_1st("AC11")[[2]])
  td_1st_AC11_NO2_00 <- as.data.frame(metrics_1st("AC11")[[3]])
  td_1st_AC11_NO2_01 <- as.data.frame(metrics_1st("AC11")[[4]])
  td_1st_AC12_NO_00 <- as.data.frame(metrics_1st("AC12")[[1]])
  td_1st_AC12_NO_01 <- as.data.frame(metrics_1st("AC12")[[2]])
  td_1st_AC12_NO2_00 <- as.data.frame(metrics_1st("AC12")[[3]])
  td_1st_AC12_NO2_01 <- as.data.frame(metrics_1st("AC12")[[4]])
  
}

# Data from 2nd colocation period ---------------------------------
{ 
  ## import the calibrated data and merge with reference
  metrics_2nd <- function(sensorunit){
    # calibrated data ----------------------------------------------------
    {
      data_2nd_NO_00 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO_00.csv",sep = ""), header=TRUE)
      data_2nd_NO_01 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO_01.csv",sep = ""), header=TRUE)
      data_2nd_NO2_00 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO2_00.csv",sep = ""), header=TRUE)
      data_2nd_NO2_01 <- read.csv(paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO2_01.csv",sep = ""), header=TRUE)
    }  
    # tdStats ------------------------------------------
    {
      
      ## NO_00
      {
        td_2nd_data_NO_00_RLM <- tdStats(data_2nd_NO_00$RLM, data_2nd_NO_00$Reference, functions = c("rmse", "mbe", "sdm", "sdo"))
        td_2nd_data_NO_00_RF <- tdStats(data_2nd_NO_00$RF, data_2nd_NO_00$Reference, functions = c("rmse", "mbe", "sdm", "sdo"))
        
        td_2nd_data_NO_00 <- data.frame(rbind(td_2nd_data_NO_00_RLM,
                                              td_2nd_data_NO_00_RF))
        
        td_2nd_data_NO_00$model<- c("RLM","RF")
        td_2nd_data_NO_00$pollutants <- c("NO","NO")
        td_2nd_data_NO_00$sensor<- c(sensorunit,sensorunit)
        td_2nd_data_NO_00$period <- c("2nd","2nd")
        
        td_2nd_data_NO_00$x <- sign((td_2nd_data_NO_00$sdm)-(td_2nd_data_NO_00$sdo))/(td_2nd_data_NO_00$sdo)*sqrt((td_2nd_data_NO_00$rmse)^2-(td_2nd_data_NO_00$mbe)^2)
        td_2nd_data_NO_00$y <- (td_2nd_data_NO_00$mbe)/(td_2nd_data_NO_00$sdo)
        td_2nd_data_NO_00$nrmse <- sqrt((td_2nd_data_NO_00$x)^2+(td_2nd_data_NO_00$y)^2)
      }
      ## NO_01
      {
        td_2nd_data_NO_01_RLM <- tdStats(data_2nd_NO_01$RLM, data_2nd_NO_01$Reference, functions = c("rmse", "mbe", "sdm", "sdo"))
        td_2nd_data_NO_01_RF <- tdStats(data_2nd_NO_01$RF, data_2nd_NO_01$Reference, functions = c("rmse", "mbe", "sdm", "sdo"))
        
        td_2nd_data_NO_01 <- data.frame(rbind(td_2nd_data_NO_01_RLM,
                                              td_2nd_data_NO_01_RF))
        
        td_2nd_data_NO_01$model<- c("RLM","RF")
        td_2nd_data_NO_01$pollutants <- c("NO","NO")
        td_2nd_data_NO_01$sensor<- c(sensorunit,sensorunit)
        td_2nd_data_NO_01$period <- c("2nd","2nd")
        
        td_2nd_data_NO_01$x <- sign((td_2nd_data_NO_01$sdm)-(td_2nd_data_NO_01$sdo))/(td_2nd_data_NO_01$sdo)*sqrt((td_2nd_data_NO_01$rmse)^2-(td_2nd_data_NO_01$mbe)^2)
        td_2nd_data_NO_01$y <- (td_2nd_data_NO_01$mbe)/(td_2nd_data_NO_01$sdo)
        td_2nd_data_NO_01$nrmse <- sqrt((td_2nd_data_NO_01$x)^2+(td_2nd_data_NO_01$y)^2)
      }
      ## NO2_00
      {
        td_2nd_data_NO2_00_RLM <- tdStats(data_2nd_NO2_00$RLM, data_2nd_NO2_00$Reference, functions = c("rmse", "mbe", "sdm", "sdo"))
        td_2nd_data_NO2_00_RF <- tdStats(data_2nd_NO2_00$RF, data_2nd_NO2_00$Reference, functions = c("rmse", "mbe", "sdm", "sdo"))
        
        td_2nd_data_NO2_00 <- data.frame(rbind(td_2nd_data_NO2_00_RLM,
                                               td_2nd_data_NO2_00_RF))
        
        td_2nd_data_NO2_00$model<- c("RLM","RF")
        td_2nd_data_NO2_00$pollutants <- c("NO2","NO2")
        td_2nd_data_NO2_00$sensor<- c(sensorunit,sensorunit)
        td_2nd_data_NO2_00$period <- c("2nd","2nd")
        
        td_2nd_data_NO2_00$x <- sign((td_2nd_data_NO2_00$sdm)-(td_2nd_data_NO2_00$sdo))/(td_2nd_data_NO2_00$sdo)*sqrt((td_2nd_data_NO2_00$rmse)^2-(td_2nd_data_NO2_00$mbe)^2)
        td_2nd_data_NO2_00$y <- (td_2nd_data_NO2_00$mbe)/(td_2nd_data_NO2_00$sdo)
        td_2nd_data_NO2_00$nrmse <- sqrt((td_2nd_data_NO2_00$x)^2+(td_2nd_data_NO2_00$y)^2)
      }
      ## NO2_01
      {
        td_2nd_data_NO2_01_RLM <- tdStats(data_2nd_NO2_01$RLM, data_2nd_NO2_01$Reference, functions = c("rmse", "mbe", "sdm", "sdo"))
        td_2nd_data_NO2_01_RF <- tdStats(data_2nd_NO2_01$RF, data_2nd_NO2_01$Reference, functions = c("rmse", "mbe", "sdm", "sdo"))
        
        td_2nd_data_NO2_01 <- data.frame(rbind(td_2nd_data_NO2_01_RLM,
                                               td_2nd_data_NO2_01_RF))
        
        td_2nd_data_NO2_01$model<- c("RLM","RF")
        td_2nd_data_NO2_01$pollutants <- c("NO2","NO2")
        td_2nd_data_NO2_01$sensor<- c(sensorunit,sensorunit)
        td_2nd_data_NO2_01$period <- c("2nd","2nd")
        
        td_2nd_data_NO2_01$x <- sign((td_2nd_data_NO2_01$sdm)-(td_2nd_data_NO2_01$sdo))/(td_2nd_data_NO2_01$sdo)*sqrt((td_2nd_data_NO2_01$rmse)^2-(td_2nd_data_NO2_01$mbe)^2)
        td_2nd_data_NO2_01$y <- (td_2nd_data_NO2_01$mbe)/(td_2nd_data_NO2_01$sdo)
        td_2nd_data_NO2_01$nrmse <- sqrt((td_2nd_data_NO2_01$x)^2+(td_2nd_data_NO2_01$y)^2)
      }
    }
    r.list <- list(td_2nd_data_NO_00,
                   td_2nd_data_NO_01,
                   td_2nd_data_NO2_00,
                   td_2nd_data_NO2_01)
    
    return(r.list)
    
  }
  
  td_2nd_AC9_NO_00 <- as.data.frame(metrics_2nd("AC9")[[1]])
  td_2nd_AC9_NO_01 <- as.data.frame(metrics_2nd("AC9")[[2]])
  td_2nd_AC9_NO2_00 <- as.data.frame(metrics_2nd("AC9")[[3]])
  td_2nd_AC9_NO2_01 <- as.data.frame(metrics_2nd("AC9")[[4]])
  td_2nd_AC10_NO_00 <- as.data.frame(metrics_2nd("AC10")[[1]])
  td_2nd_AC10_NO_01 <- as.data.frame(metrics_2nd("AC10")[[2]])
  td_2nd_AC10_NO2_00 <- as.data.frame(metrics_2nd("AC10")[[3]])
  td_2nd_AC10_NO2_01 <- as.data.frame(metrics_2nd("AC10")[[4]])
  td_2nd_AC11_NO_00 <- as.data.frame(metrics_2nd("AC11")[[1]])
  td_2nd_AC11_NO_01 <- as.data.frame(metrics_2nd("AC11")[[2]])
  td_2nd_AC11_NO2_00 <- as.data.frame(metrics_2nd("AC11")[[3]])
  td_2nd_AC11_NO2_01 <- as.data.frame(metrics_2nd("AC11")[[4]])
  td_2nd_AC12_NO_00 <- as.data.frame(metrics_2nd("AC12")[[1]])
  td_2nd_AC12_NO_01 <- as.data.frame(metrics_2nd("AC12")[[2]])
  td_2nd_AC12_NO2_00 <- as.data.frame(metrics_2nd("AC12")[[3]])
  td_2nd_AC12_NO2_01 <- as.data.frame(metrics_2nd("AC12")[[4]])
}

# Target diagrams
{
  total_data_frame <- as.data.frame(matrix(0,64,7))
  total_data_frame <- rename(total_data_frame, "Model" = "V1", "Pollutants" = "V2", "Sensor" = "V3", "Period" = "V4", "x" = "V5","y" = "V6","nrmse" = "V7")
  total_data_frame[1:2,] <- td_1st_AC9_NO_00[,5:11]
  total_data_frame[3:4,] <- td_1st_AC9_NO_01[,5:11]
  total_data_frame[5:6,] <- td_1st_AC9_NO2_00[,5:11]
  total_data_frame[7:8,] <- td_1st_AC9_NO2_01[,5:11]
  total_data_frame[9:10,] <- td_1st_AC10_NO_00[,5:11]
  total_data_frame[11:12,] <- td_1st_AC10_NO_01[,5:11]
  total_data_frame[13:14,] <- td_1st_AC10_NO2_00[,5:11]
  total_data_frame[15:16,] <- td_1st_AC10_NO2_01[,5:11]
  total_data_frame[17:18,] <- td_1st_AC11_NO_00[,5:11]
  total_data_frame[19:20,] <- td_1st_AC11_NO_01[,5:11]
  total_data_frame[21:22,] <- td_1st_AC11_NO2_00[,5:11]
  total_data_frame[23:24,] <- td_1st_AC11_NO2_01[,5:11]
  total_data_frame[25:26,] <- td_1st_AC12_NO_00[,5:11]
  total_data_frame[27:28,] <- td_1st_AC12_NO_01[,5:11]
  total_data_frame[29:30,] <- td_1st_AC12_NO2_00[,5:11]
  total_data_frame[31:32,] <- td_1st_AC12_NO2_01[,5:11]
  total_data_frame[33:34,] <- td_2nd_AC9_NO_00[,5:11]
  total_data_frame[35:36,] <- td_2nd_AC9_NO_01[,5:11]
  total_data_frame[37:38,] <- td_2nd_AC9_NO2_00[,5:11]
  total_data_frame[39:40,] <- td_2nd_AC9_NO2_01[,5:11]
  total_data_frame[41:42,] <- td_2nd_AC10_NO_00[,5:11]
  total_data_frame[43:44,] <- td_2nd_AC10_NO_01[,5:11]
  total_data_frame[45:46,] <- td_2nd_AC10_NO2_00[,5:11]
  total_data_frame[47:48,] <- td_2nd_AC10_NO2_01[,5:11]
  total_data_frame[49:50,] <- td_2nd_AC11_NO_00[,5:11]
  total_data_frame[51:52,] <- td_2nd_AC11_NO_01[,5:11]
  total_data_frame[53:54,] <- td_2nd_AC11_NO2_00[,5:11]
  total_data_frame[55:56,] <- td_2nd_AC11_NO2_01[,5:11]
  total_data_frame[57:58,] <- td_2nd_AC12_NO_00[,5:11]
  total_data_frame[59:60,] <- td_2nd_AC12_NO_01[,5:11]
  total_data_frame[61:62,] <- td_2nd_AC12_NO2_00[,5:11]
  total_data_frame[63:64,] <- td_2nd_AC12_NO2_01[,5:11]
  
  total_data_frame_NO <- total_data_frame %>% filter(total_data_frame$Pollutants == "NO")
  total_data_frame_NO2 <- total_data_frame %>% filter(total_data_frame$Pollutants == "NO2")
  
  pNO <- ggplot(total_data_frame_NO, aes(x,y,color= Period, shape = Model))+geom_point(size=1.8
  )+ggtitle("Target diagram : NO "
  )+labs(x=expression(paste("CRMSE/",sigma["y"])),y=expression(paste("MBE/",sigma["y"]))
  )+lims(x=c(-1,1),y=c(-1,1)
  )+geom_circle(aes(x0 = 0, y0 = 0, r = .5), inherit.aes = FALSE,col="black",size=0.2
  )+geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE,col="black",size=0.2
  )+geom_point(aes(0,0),col="black",shape=16
  )+scale_color_manual(values=c("#008BF8","#DC0073")
  )+scale_shape_manual(values = c(21,3)
  )+ theme(axis.text = element_text(size = 5)
  )+ theme(axis.title = element_text(size = 3)
  )+ theme(plot.title = element_text(size = 4)
  )+ theme(legend.text = element_text(size = 5)
  )+ theme(legend.title = element_text(size = 4)) +theme_bw()+ guides(colour = guide_legend(override.aes = list(size=4)))
  
  pNO2 <- ggplot(total_data_frame_NO2, aes(x,y,color= Period, shape=Model))+geom_point(size=1.8
  )+ggtitle("Target diagram : NO2 "
  )+labs(x=expression(paste("CRMSE/",sigma["y"])),y=expression(paste("MBE/",sigma["y"]))
  )+lims(x=c(-1,1),y=c(-1,1)
  )+geom_circle(aes(x0 = 0, y0 = 0, r = .5), inherit.aes = FALSE,col="black",size=0.2
  )+geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE,col="black",size=0.2
  )+geom_point(aes(0,0),col="black",shape=16
  )+scale_color_manual(values=c("#008BF8","#DC0073")
  )+scale_shape_manual(values = c(21,3)
  )+ theme(axis.text = element_text(size = 5)
  )+ theme(axis.title = element_text(size = 3)
  )+ theme(plot.title = element_text(size = 4)
  )+ theme(legend.text = element_text(size = 5)
  )+ theme(legend.title = element_text(size = 4)) +theme_bw()+ guides(colour = guide_legend(override.aes = list(size=4)))
}

  jpeg("figure/fig_5_target_diagram.jpeg", width =10, height = 8.4*2, units = 'cm', res = 600)
  multiplot(pNO, pNO2,cols=1)
  dev.off()

