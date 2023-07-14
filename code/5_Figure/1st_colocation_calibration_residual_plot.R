# 1st_colocation_calibration_residual_plot.R
#
# Evaluate the calibration data from the first colocation campaign.
# Derive the residual plots.
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
library(ggpubr)

setwd("D:/empa_intern/Paper/data_and_code/")

source("data_input/Carbosense/CarboSenseFunctions.r")
source("data_input/Carbosense/api-v1.3.r")

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

cal.eval.residual.plot <- function(sensorunit){
  # Data import
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
      data_NO_00$resid_RLM <-  data_NO_00$RLM - data_NO_00$Reference
      data_NO_00$resid_RF <- data_NO_00$RF - data_NO_00$Reference
    }
    ### NO_01 --------------------
    {
      data_NO_01 <- as.data.frame(cbind(data_NO_01_RLM$NO.ppb.,data_NO_01_RF$NO.ppb.))
      data_NO_01 <- round(data_NO_01, digits = 2)
      data_NO_01 <- as.data.frame(cbind(data_NO_01_RLM$date,data_NO_01))
      data_NO_01 <- rename(data_NO_01,"Date[UTC]"="data_NO_01_RLM$date" ,"RLM"="V1","RF"="V2")
      data_NO_01$`Date[UTC]` <- ymd_hms(data_NO_01$`Date[UTC]`)
      data_NO_01$Reference <- ref$REFNO
      data_NO_01$resid_RLM <- data_NO_01$RLM - data_NO_01$Reference
      data_NO_01$resid_RF <- data_NO_01$RF - data_NO_01$Reference
    }
    ### NO2_00 --------------------
    {
      data_NO2_00 <- as.data.frame(cbind(data_NO2_00_RLM$NO2.ppb.,data_NO2_00_RF$NO2.ppb.))
      data_NO2_00 <- round(data_NO2_00, digits = 2)
      data_NO2_00 <- as.data.frame(cbind(data_NO2_00_RLM$date,data_NO2_00))
      data_NO2_00 <- rename(data_NO2_00,"Date[UTC]"="data_NO2_00_RLM$date" ,"RLM"="V1","RF"="V2")
      data_NO2_00$`Date[UTC]` <- ymd_hms(data_NO2_00$`Date[UTC]`)
      data_NO2_00$Reference <- ref$REFNO2
      data_NO2_00$resid_RLM <- data_NO2_00$RLM - data_NO2_00$Reference
      data_NO2_00$resid_RF <-  data_NO2_00$RF  - data_NO2_00$Reference
    }
    ### NO2_01 --------------------
    {
      data_NO2_01 <- as.data.frame(cbind(data_NO2_01_RLM$NO2.ppb.,data_NO2_01_RF$NO2.ppb.))
      data_NO2_01 <- round(data_NO2_01, digits = 2)
      data_NO2_01 <- as.data.frame(cbind(data_NO2_01_RLM$date,data_NO2_01))
      data_NO2_01 <- rename(data_NO2_01,"Date[UTC]"="data_NO2_01_RLM$date" ,"RLM"="V1","RF"="V2")
      data_NO2_01$`Date[UTC]` <- ymd_hms(data_NO2_01$`Date[UTC]`)
      data_NO2_01$Reference <- ref$REFNO2
      data_NO2_01$resid_RLM <- data_NO2_01$RLM - data_NO2_01$Reference
      data_NO2_01$resid_RF <- data_NO2_01$RF - data_NO2_01$Reference
      
    }
    
    
    
  }
  # residualplot in figure/1st_colocation/residualplot/
  {
    ## NO_00
    {
      data_NO_00_RLM_upper <- ggplot(data_NO_00, aes(RLM,resid_RLM)
      )+geom_point(shape=1,size=1.2
      )+geom_smooth(color='cyan1',span=5,level=0.95
      )+ylab('residual (Calibrated - Reference) [ppb]'
      )+xlab('Calibrated concentration [ppb]'
      )+xlim(c(-20,250))+ylim(c(-30,30)) +theme_bw() +ggtitle(paste(sensorunit,", NO_A, RLM",sep = "")
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+geom_hline(aes(yintercept=0), colour='orange')+ theme(legend.position="none")
      
      data_NO_00_RLM_lower <- ggplot(data_NO_00, aes(x=RLM)
      )+geom_density(
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+xlim(c(-20,250))+ylim(c(0,0.08))+theme_bw() + theme(legend.position="none"
      )+xlab('Calibrated concentration [ppb]'
      )+ylab('Density'
      )
      
      data_NO_00_RLM <- ggarrange(data_NO_00_RLM_upper, data_NO_00_RLM_lower,
                                  ncol = 1, nrow = 2, heights = c(3,1.2))
      
      data_NO_00_RF_upper <- ggplot(data_NO_00, aes(RF,resid_RF)
      )+geom_point(shape=1,size=1.2
      )+geom_smooth(color='cyan1',span=5,level=0.95
      )+ylab('residual (Calibrated - Reference) [ppb]'
      )+xlab('Calibrated concentration [ppb]'
      )+xlim(c(-20,250))+ylim(c(-30,30)) +theme_bw() +ggtitle(paste(sensorunit,", NO_A, RF",sep = "")
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+geom_hline(aes(yintercept=0), colour='orange')+ theme(legend.position="none")
      
      data_NO_00_RF_lower <- ggplot(data_NO_00, aes(x=RF)
      )+geom_density(
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+xlim(c(-20,250))+ylim(c(0,0.08))+theme_bw() + theme(legend.position="none"
      )+xlab('Calibrated concentration [ppb]'
      )+ylab('Density'
      )
      
      data_NO_00_RF <- ggarrange(data_NO_00_RF_upper, data_NO_00_RF_lower,
                                 ncol = 1, nrow = 2, heights = c(3,1.2))
      
      
    }
    ## NO_01
    {
      data_NO_01_RLM_upper <- ggplot(data_NO_01, aes(RLM,resid_RLM)
      )+geom_point(shape=1,size=1.2
      )+geom_smooth(color='cyan1',span=5,level=0.95
      )+ylab('residual (Calibrated - Reference) [ppb]'
      )+xlab('Calibrated concentration [ppb]'
      )+xlim(c(-20,250))+ylim(c(-30,30)) +theme_bw() +ggtitle(paste(sensorunit,", NO_B, RLM",sep = "")
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+geom_hline(aes(yintercept=0), colour='orange')+ theme(legend.position="none")
      
      data_NO_01_RLM_lower <- ggplot(data_NO_01, aes(x=RLM)
      )+geom_density(
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+xlim(c(-20,250))+ylim(c(0,0.08))+theme_bw() + theme(legend.position="none"
      )+xlab('Calibrated concentration [ppb]'
      )+ylab('Density'
      )
      
      data_NO_01_RLM <- ggarrange(data_NO_01_RLM_upper, data_NO_01_RLM_lower,
                                  ncol = 1, nrow = 2, heights = c(3,1.2))
      
      data_NO_01_RF_upper <- ggplot(data_NO_01, aes(RF,resid_RF)
      )+geom_point(shape=1,size=1.2
      )+geom_smooth(color='cyan1',span=5,level=0.95
      )+ylab('residual (Calibrated - Reference) [ppb]'
      )+xlab('Calibrated concentration [ppb]'
      )+xlim(c(-20,250))+ylim(c(-30,30)) +theme_bw() +ggtitle(paste(sensorunit,", NO_B, RF",sep = "")
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+geom_hline(aes(yintercept=0), colour='orange')+ theme(legend.position="none")
      
      data_NO_01_RF_lower <- ggplot(data_NO_01, aes(x=RF)
      )+geom_density(
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+xlim(c(-20,250))+ylim(c(0,0.08))+theme_bw() + theme(legend.position="none"
      )+xlab('Calibrated concentration [ppb]'
      )+ylab('Density'
      )
      
      data_NO_01_RF <- ggarrange(data_NO_01_RF_upper, data_NO_01_RF_lower,
                                 ncol = 1, nrow = 2, heights = c(3,1.2))
      
      
    }
    ## NO2_00
    {
      data_NO2_00_RLM_upper <- ggplot(data_NO2_00, aes(RLM,resid_RLM)
      )+geom_point(shape=1,size=1.2
      )+geom_smooth(color='cyan1',span=5,level=0.95
      )+ylab('residual (Calibrated - Reference) [ppb]'
      )+xlab('Calibrated concentration [ppb]'
      )+xlim(c(-20,100))+ylim(c(-15,15)) +theme_bw() +ggtitle(paste(sensorunit,", NO2_A, RLM",sep = "")
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+geom_hline(aes(yintercept=0), colour='orange')+ theme(legend.position="none")
      
      data_NO2_00_RLM_lower <- ggplot(data_NO2_00, aes(x=RLM)
      )+geom_density(
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+xlim(c(-20,100))+ylim(c(0,0.08))+theme_bw() + theme(legend.position="none"
      )+xlab('Calibrated concentration [ppb]'
      )+ylab('Density'
      )
      
      data_NO2_00_RLM <- ggarrange(data_NO2_00_RLM_upper, data_NO2_00_RLM_lower,
                                   ncol = 1, nrow = 2, heights = c(3,1.2))
      
      data_NO2_00_RF_upper <- ggplot(data_NO2_00, aes(RF,resid_RF)
      )+geom_point(shape=1,size=1.2
      )+geom_smooth(color='cyan1',span=5,level=0.95
      )+ylab('residual (Calibrated - Reference) [ppb]'
      )+xlab('Calibrated concentration [ppb]'
      )+xlim(c(-20,100))+ylim(c(-15,15)) +theme_bw() +ggtitle(paste(sensorunit,", NO2_A, RF",sep = "")
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+geom_hline(aes(yintercept=0), colour='orange')+ theme(legend.position="none")
      
      data_NO2_00_RF_lower <- ggplot(data_NO2_00, aes(x=RF)
      )+geom_density(
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+xlim(c(-20,100))+ylim(c(0,0.08))+theme_bw() + theme(legend.position="none"
      )+xlab('Calibrated concentration [ppb]'
      )+ylab('Density'
      )
      
      data_NO2_00_RF <- ggarrange(data_NO2_00_RF_upper, data_NO2_00_RF_lower,
                                  ncol = 1, nrow = 2, heights = c(3,1.2))
      
      
    }
    ## NO2_01
    {
      data_NO2_01_RLM_upper <- ggplot(data_NO2_01, aes(RLM,resid_RLM)
      )+geom_point(shape=1,size=1.2
      )+geom_smooth(color='cyan1',span=5,level=0.95
      )+ylab('residual (Calibrated - Reference) [ppb]'
      )+xlab('Calibrated concentration [ppb]'
      )+xlim(c(-20,100))+ylim(c(-15,15)) +theme_bw() +ggtitle(paste(sensorunit,", NO2_B, RLM",sep = "")
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+geom_hline(aes(yintercept=0), colour='orange')+ theme(legend.position="none")
      
      data_NO2_01_RLM_lower <- ggplot(data_NO2_01, aes(x=RLM)
      )+geom_density(
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+xlim(c(-20,100))+ylim(c(0,0.08))+theme_bw() + theme(legend.position="none"
      )+xlab('Calibrated concentration [ppb]'
      )+ylab('Density'
      )
      
      data_NO2_01_RLM <- ggarrange(data_NO2_01_RLM_upper, data_NO2_01_RLM_lower,
                                   ncol = 1, nrow = 2, heights = c(3,1.2))
      
      data_NO2_01_RF_upper <- ggplot(data_NO2_01, aes(RF,resid_RF)
      )+geom_point(shape=1,size=1.2
      )+geom_smooth(color='cyan1',span=5,level=0.95
      )+ylab('residual (Calibrated - Reference) [ppb]'
      )+xlab('Calibrated concentration [ppb]'
      )+xlim(c(-20,100))+ylim(c(-15,15)) +theme_bw() +ggtitle(paste(sensorunit,", NO2_B, RF",sep = "")
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+geom_hline(aes(yintercept=0), colour='orange')+ theme(legend.position="none")
      
      data_NO2_01_RF_lower <- ggplot(data_NO2_01, aes(x=RF)
      )+geom_density(
      )+geom_vline(aes(xintercept=2), colour='red'
      )+geom_vline(aes(xintercept=10), colour='red'
      )+geom_vline(aes(xintercept=28), colour='red'
      )+xlim(c(-20,100))+ylim(c(0,0.08))+theme_bw() + theme(legend.position="none"
      )+xlab('Calibrated concentration [ppb]'
      )+ylab('Density'
      )
      
      data_NO2_01_RF <- ggarrange(data_NO2_01_RF_upper, data_NO2_01_RF_lower,
                                  ncol = 1, nrow = 2, heights = c(3,1.2))
      
      
    }
    
    jpeg(paste("figure/1st_colocation/residualplot/resid_",sensorunit,".jpeg",sep = ""), width =28, height = 21, units = 'cm', res = 600)
    multiplot(data_NO_00_RLM,data_NO2_00_RLM,
              data_NO_00_RF,data_NO2_00_RF,
              data_NO_01_RLM,data_NO2_01_RLM,
              data_NO_01_RF,data_NO2_01_RF,
              cols=4)
    dev.off() 
    
  }
  
  
  
  
}

cal.eval.residual.plot("AC9")
cal.eval.residual.plot("AC10")
cal.eval.residual.plot("AC11")
cal.eval.residual.plot("AC12")

## There would be 25-27 warnings during the function implementation,
## that's just I did not indicate all variables for plot_ts()
## However, time-series are getting well even though we ignore the warnings!
