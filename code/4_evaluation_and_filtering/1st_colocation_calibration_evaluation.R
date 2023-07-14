# 1st_colocation_calibration_evaluation.R
#
# Evaluate the calibration data from the first colocation campaign.
# Derive the statistical metrics
#
# Author : Horim Kim
#

# Library----------------------------------
require(chron)
library(data.table)
library(dplyr)
library(lubridate)
library(tdr)
library(tibbletime)

setwd("C:/") # change the directory

source("data_input/Timeseries_plotting.r")

# -------------------------------------------

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

cal.eval <- function(sensorunit){
  # Data import
  {data_NO_00_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RLM.csv",sep=""), header=TRUE)  
    data_NO_00_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RF.csv",sep=""), header=TRUE)
    
    data_NO_01_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RLM.csv",sep=""), header=TRUE)  
    data_NO_01_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RF.csv",sep=""), header=TRUE)
    
    data_NO2_00_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RLM.csv",sep=""), header=TRUE)  
    data_NO2_00_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RF.csv",sep=""), header=TRUE)
    
    data_NO2_01_RLM <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RLM.csv",sep=""), header=TRUE)  
    data_NO2_01_RF <- read.csv(paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RF.csv",sep=""), header=TRUE)
  }
  # Import the Reference
  {
  ref <- read.csv(paste("data_output/pre_processed_1st_period/",sensorunit,".csv", sep=""), header = TRUE)
  ref$X <- seq(from = 1, to = length(ref$date) ,by = 1)
  ref$date <- ymd_hms(ref$date)
  
  ref <- as_tibble(ref)
  
  ref <- as_tbl_time(ref, index = date)
  }
  # Bind the data in each AC & Sensor 
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
  # td_stats : derive the statistical metrics and save it in data_output/calibrated_1st/td_res/
  {
    ### NO_00
    {
      data_NO_00_RLM <- tdStats(data_NO_00$RLM, data_NO_00$Reference,functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      data_NO_00_RF <- tdStats(data_NO_00$RF, data_NO_00$Reference,functions = c("rmse", "mbe", "sdm", "sdo","r2"))

      res_data_NO_00 <- data.frame(rbind(data_NO_00_RLM,data_NO_00_RF))

      res_data_NO_00$x <- sign((res_data_NO_00$sdm)-(res_data_NO_00$sdo))/(res_data_NO_00$sdo)*sqrt((res_data_NO_00$rmse)^2-(res_data_NO_00$mbe)^2)
      res_data_NO_00$y <- (res_data_NO_00$mbe)/(res_data_NO_00$sdo)
      res_data_NO_00$nrmse <- sqrt((res_data_NO_00$x)^2+(res_data_NO_00$y)^2)
      
    }
    ### NO_01
    {
      data_NO_01_RLM <- tdStats(data_NO_01$RLM, data_NO_01$Reference,functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      data_NO_01_RF <- tdStats(data_NO_01$RF, data_NO_01$Reference,functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO_01 <- data.frame(rbind(data_NO_01_RLM,data_NO_01_RF))
      
      res_data_NO_01$x <- sign((res_data_NO_01$sdm)-(res_data_NO_01$sdo))/(res_data_NO_01$sdo)*sqrt((res_data_NO_01$rmse)^2-(res_data_NO_01$mbe)^2)
      res_data_NO_01$y <- (res_data_NO_01$mbe)/(res_data_NO_01$sdo)
      res_data_NO_01$nrmse <- sqrt((res_data_NO_01$x)^2+(res_data_NO_01$y)^2)
      
    }
    ### NO2_00
    {
      data_NO2_00_RLM <- tdStats(data_NO2_00$RLM, data_NO2_00$Reference,functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      data_NO2_00_RF <- tdStats(data_NO2_00$RF, data_NO2_00$Reference,functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO2_00 <- data.frame(rbind(data_NO2_00_RLM,data_NO2_00_RF))
      
      res_data_NO2_00$x <- sign((res_data_NO2_00$sdm)-(res_data_NO2_00$sdo))/(res_data_NO2_00$sdo)*sqrt((res_data_NO2_00$rmse)^2-(res_data_NO2_00$mbe)^2)
      res_data_NO2_00$y <- (res_data_NO2_00$mbe)/(res_data_NO2_00$sdo)
      res_data_NO2_00$nrmse <- sqrt((res_data_NO2_00$x)^2+(res_data_NO2_00$y)^2)
      
    }
    ### NO2_01
    {
      data_NO2_01_RLM <- tdStats(data_NO2_01$RLM, data_NO2_01$Reference,functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      data_NO2_01_RF <- tdStats(data_NO2_01$RF, data_NO2_01$Reference,functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO2_01 <- data.frame(rbind(data_NO2_01_RLM,data_NO2_01_RF))
      
      res_data_NO2_01$x <- sign((res_data_NO2_01$sdm)-(res_data_NO2_01$sdo))/(res_data_NO2_01$sdo)*sqrt((res_data_NO2_01$rmse)^2-(res_data_NO2_01$mbe)^2)
      res_data_NO2_01$y <- (res_data_NO2_01$mbe)/(res_data_NO2_01$sdo)
      res_data_NO2_01$nrmse <- sqrt((res_data_NO2_01$x)^2+(res_data_NO2_01$y)^2)
      
    }
    

    
    ### save the file
    write.csv(res_data_NO_00, paste("data_output/calibrated_1st/td_res/",sensorunit,"_NO_00.csv", sep = ""))
    write.csv(res_data_NO_01, paste("data_output/calibrated_1st/td_res/",sensorunit,"_NO_01.csv", sep = ""))
    write.csv(res_data_NO2_00, paste("data_output/calibrated_1st/td_res/",sensorunit,"_NO2_00.csv", sep = ""))
    write.csv(res_data_NO2_01, paste("data_output/calibrated_1st/td_res/",sensorunit,"_NO2_01.csv", sep = ""))
    
  }
  # scatterplot in figure/1st_colocation/scatterplot/
  {
   ## NO_00
    {
      data_NO_00_RLM <- ggplot(data_NO_00, aes(Reference,RLM)
      )+geom_point(shape=1,size=1.2
      )+geom_abline(intercept = 0, slope = 1, col='red'
      )+xlab('Reference'
      )+ylab('Calibrated value'
      )+xlim(c(-20,250))+ylim(c(-20,250)) +theme_bw() +ggtitle(paste(sensorunit,", NO_A, RLM",sep = "")) 
      
      data_NO_00_RF <- ggplot(data_NO_00, aes(Reference,RF)
      )+geom_point(shape=1,size=1.2
      )+geom_abline(intercept = 0, slope = 1, col='red'
      )+xlab('Reference'
      )+ylab('Calibrated value'
      )+xlim(c(-20,250))+ylim(c(-20,250)) +theme_bw() +ggtitle(paste(sensorunit,", NO_A, RF",sep = ""))
      
    }
   ## NO_01
    {
      data_NO_01_RLM <- ggplot(data_NO_01, aes(Reference,RLM)
      )+geom_point(shape=1,size=1.2
      )+geom_abline(intercept = 0, slope = 1, col='red'
      )+xlab('Reference'
      )+ylab('Calibrated value'
      )+xlim(c(-20,250))+ylim(c(-20,250)) +theme_bw() +ggtitle(paste(sensorunit,", NO_B, RLM",sep = "")) 
      
      data_NO_01_RF<- ggplot(data_NO_01, aes(Reference,RF)
      )+geom_point(shape=1,size=1.2
      )+geom_abline(intercept = 0, slope = 1, col='red'
      )+xlab('Reference'
      )+ylab('Calibrated value'
      )+xlim(c(-20,250))+ylim(c(-20,250)) +theme_bw() +ggtitle(paste(sensorunit,", NO_B, RF",sep = ""))
      
    }
    ## NO2_00
    {
      data_NO2_00_RLM <- ggplot(data_NO2_00, aes(Reference,RLM)
      )+geom_point(shape=1,size=1.2
      )+geom_abline(intercept = 0, slope = 1, col='red'
      )+xlab('Reference'
      )+ylab('Calibrated value'
      )+xlim(c(-20,100))+ylim(c(-20,100)) +theme_bw() +ggtitle(paste(sensorunit,", NO2_A, RLM",sep = ""))
      
      data_NO2_00_RF<- ggplot(data_NO2_00, aes(Reference,RF)
      )+geom_point(shape=1,size=1.2
      )+geom_abline(intercept = 0, slope = 1, col='red'
      )+xlab('Reference'
      )+ylab('Calibrated value'
      )+xlim(c(-20,100))+ylim(c(-20,100)) +theme_bw() +ggtitle(paste(sensorunit,", NO2_A, RF",sep = ""))
      
    }
    ## NO2_01
    {
      data_NO2_01_RLM <- ggplot(data_NO2_01, aes(Reference,RLM)
      )+geom_point(shape=1,size=1.2
      )+geom_abline(intercept = 0, slope = 1, col='red'
      )+xlab('Reference'
      )+ylab('Calibrated value'
      )+xlim(c(-20,100))+ylim(c(-20,100)) +theme_bw() +ggtitle(paste(sensorunit,", NO2_B, RLM",sep = "")) 
      
      data_NO2_01_RF<- ggplot(data_NO2_01, aes(Reference,RF)
      )+geom_point(shape=1,size=1.2
      )+geom_abline(intercept = 0, slope = 1, col='red'
      )+xlab('Reference'
      )+ylab('Calibrated value'
      )+xlim(c(-20,100))+ylim(c(-20,100)) +theme_bw() +ggtitle(paste(sensorunit,", NO2_B, RF",sep = ""))
      
    }
  
  jpeg(paste("figure/1st_colocation/scatterplot/",sensorunit,".jpeg",sep = ""), width =28, height = 14, units = 'cm', res = 600)
  multiplot(data_NO_00_RLM,data_NO2_00_RLM,
            data_NO_00_RF,data_NO2_00_RF,
            data_NO_01_RLM,data_NO2_01_RLM,
            data_NO_01_RF,data_NO2_01_RF,
            cols=4)
  dev.off() 
    
  }
  # time-series plotting in figure/1st_colocation/timeseries/
  {
    ### NO_00
    {
      save_loc <- "figure/1st_colocation/timeseries/"
      
      figname <- paste(save_loc,sensorunit,"_NO_00.pdf",sep="")
      
      yyy     <- as.matrix(data_NO_00[,2:dim(data_NO_00)[2]])
      
      xlabString <- "Date [UTC]"
      ylabString <- paste("NO [ppb]")
      legend_str <- c(colnames(data_NO_00)[2:dim(data_NO_00)[2]])
      plot_ts(figname,data_NO_00$`Date[UTC]`,yyy,"day",NULL,c(-20,250),xlabString,ylabString,legend_str)
      dev.off()
    }
    ### NO_01
    {
      save_loc <- "figure/1st_colocation/timeseries/"
      
      figname <- paste(save_loc,sensorunit,"_NO_01.pdf",sep="")
      
      yyy     <- as.matrix(data_NO_01[,2:dim(data_NO_01)[2]])
      
      xlabString <- "Date [UTC]"
      ylabString <- paste("NO [ppb]")
      legend_str <- c(colnames(data_NO_01)[2:dim(data_NO_01)[2]])
      plot_ts(figname,data_NO_01$`Date[UTC]`,yyy,"day",NULL,c(-20,250),xlabString,ylabString,legend_str)
      dev.off()
    }
    ### NO2_00
    {
      save_loc <- "figure/1st_colocation/timeseries/"
      
      figname <- paste(save_loc,sensorunit,"_NO2_00.pdf",sep="")
      
      yyy     <- as.matrix(data_NO2_00[,2:dim(data_NO2_00)[2]])
      
      xlabString <- "Date [UTC]"
      ylabString <- paste("NO2 [ppb]")
      legend_str <- c(colnames(data_NO2_00)[2:dim(data_NO2_00)[2]])
      plot_ts(figname,data_NO2_00$`Date[UTC]`,yyy,"day",NULL,c(-20,100),xlabString,ylabString,legend_str)
      dev.off()
    }
    ### NO2_01
    {
      save_loc <- "figure/1st_colocation/timeseries/"
      
      figname <- paste(save_loc,sensorunit,"_NO2_01.pdf",sep="")
      
      yyy     <- as.matrix(data_NO2_01[,2:dim(data_NO2_01)[2]])
      
      xlabString <- "Date [UTC]"
      ylabString <- paste("NO2 [ppb]")
      legend_str <- c(colnames(data_NO2_01)[2:dim(data_NO2_01)[2]])
      plot_ts(figname,data_NO2_01$`Date[UTC]`,yyy,"day",NULL,c(-20,100),xlabString,ylabString,legend_str)
      dev.off()
    }
  }
  
  
  
  
}

cal.eval("AC9")
cal.eval("AC10")
cal.eval("AC11")
cal.eval("AC12")

## There would be 25-27 warnings during the function implementation,
## that's just I did not indicate all variables for plot_ts()
## However, time-series are getting well even though we ignore the warnings!
