# Figure 5 : Target diagrams and taylor diagrams
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
library(plotrix)

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

    r.list <- list(RE_M_data_1st_NO_00_RLM,
                   RE_M_data_1st_NO_00_RF,
                   RE_M_data_1st_NO_01_RLM,
                   RE_M_data_1st_NO_01_RF,
                   RE_M_data_1st_NO2_00_RLM,
                   RE_M_data_1st_NO2_00_RF,
                   RE_M_data_1st_NO2_01_RLM,
                   RE_M_data_1st_NO2_01_RF,
                   ref_1st)
    
    return(r.list)
    
  }
  
  # AC9
  {
    RE_M_AC9_1st_NO_00_RLM <- as.data.frame(metrics_1st("AC9")[[1]])
    RE_M_AC9_1st_NO_00_RF <- as.data.frame(metrics_1st("AC9")[[2]])
    RE_M_AC9_1st_NO_01_RLM <- as.data.frame(metrics_1st("AC9")[[3]])
    RE_M_AC9_1st_NO_01_RF <- as.data.frame(metrics_1st("AC9")[[4]])
    RE_M_AC9_1st_NO2_00_RLM <- as.data.frame(metrics_1st("AC9")[[5]])
    RE_M_AC9_1st_NO2_00_RF <- as.data.frame(metrics_1st("AC9")[[6]])
    RE_M_AC9_1st_NO2_01_RLM <- as.data.frame(metrics_1st("AC9")[[7]])
    RE_M_AC9_1st_NO2_01_RF <- as.data.frame(metrics_1st("AC9")[[8]])
    AC9_1st <- as.data.frame(metrics_1st("AC9")[[9]])
  }
  # AC10
  {
    RE_M_AC10_1st_NO_00_RLM <- as.data.frame(metrics_1st("AC10")[[1]])
    RE_M_AC10_1st_NO_00_RF <- as.data.frame(metrics_1st("AC10")[[2]])
    RE_M_AC10_1st_NO_01_RLM <- as.data.frame(metrics_1st("AC10")[[3]])
    RE_M_AC10_1st_NO_01_RF <- as.data.frame(metrics_1st("AC10")[[4]])
    RE_M_AC10_1st_NO2_00_RLM <- as.data.frame(metrics_1st("AC10")[[5]])
    RE_M_AC10_1st_NO2_00_RF <- as.data.frame(metrics_1st("AC10")[[6]])
    RE_M_AC10_1st_NO2_01_RLM <- as.data.frame(metrics_1st("AC10")[[7]])
    RE_M_AC10_1st_NO2_01_RF <- as.data.frame(metrics_1st("AC10")[[8]])
    AC10_1st <- as.data.frame(metrics_1st("AC10")[[9]])
  }
  # AC11
  {
    RE_M_AC11_1st_NO_00_RLM <- as.data.frame(metrics_1st("AC11")[[1]])
    RE_M_AC11_1st_NO_00_RF <- as.data.frame(metrics_1st("AC11")[[2]])
    RE_M_AC11_1st_NO_01_RLM <- as.data.frame(metrics_1st("AC11")[[3]])
    RE_M_AC11_1st_NO_01_RF <- as.data.frame(metrics_1st("AC11")[[4]])
    RE_M_AC11_1st_NO2_00_RLM <- as.data.frame(metrics_1st("AC11")[[5]])
    RE_M_AC11_1st_NO2_00_RF <- as.data.frame(metrics_1st("AC11")[[6]])
    RE_M_AC11_1st_NO2_01_RLM <- as.data.frame(metrics_1st("AC11")[[7]])
    RE_M_AC11_1st_NO2_01_RF <- as.data.frame(metrics_1st("AC11")[[8]])
    AC11_1st <- as.data.frame(metrics_1st("AC11")[[9]])
  }
  # AC12
  {
    RE_M_AC12_1st_NO_00_RLM <- as.data.frame(metrics_1st("AC12")[[1]])
    RE_M_AC12_1st_NO_00_RF <- as.data.frame(metrics_1st("AC12")[[2]])
    RE_M_AC12_1st_NO_01_RLM <- as.data.frame(metrics_1st("AC12")[[3]])
    RE_M_AC12_1st_NO_01_RF <- as.data.frame(metrics_1st("AC12")[[4]])
    RE_M_AC12_1st_NO2_00_RLM <- as.data.frame(metrics_1st("AC12")[[5]])
    RE_M_AC12_1st_NO2_00_RF <- as.data.frame(metrics_1st("AC12")[[6]])
    RE_M_AC12_1st_NO2_01_RLM <- as.data.frame(metrics_1st("AC12")[[7]])
    RE_M_AC12_1st_NO2_01_RF <- as.data.frame(metrics_1st("AC12")[[8]])
    AC12_1st <- as.data.frame(metrics_1st("AC12")[[9]])
  }
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
    
    r.list <- list(data_2nd_NO_00,
                   data_2nd_NO_01,
                   data_2nd_NO2_00,
                   data_2nd_NO2_01)
    
    return(r.list)
    
  }
  
  ty_2nd_AC9_NO_00 <- as.data.frame(metrics_2nd("AC9")[[1]])
  ty_2nd_AC9_NO_01 <- as.data.frame(metrics_2nd("AC9")[[2]])
  ty_2nd_AC9_NO2_00 <- as.data.frame(metrics_2nd("AC9")[[3]])
  ty_2nd_AC9_NO2_01 <- as.data.frame(metrics_2nd("AC9")[[4]])
  ty_2nd_AC10_NO_00 <- as.data.frame(metrics_2nd("AC10")[[1]])
  ty_2nd_AC10_NO_01 <- as.data.frame(metrics_2nd("AC10")[[2]])
  ty_2nd_AC10_NO2_00 <- as.data.frame(metrics_2nd("AC10")[[3]])
  ty_2nd_AC10_NO2_01 <- as.data.frame(metrics_2nd("AC10")[[4]])
  ty_2nd_AC11_NO_00 <- as.data.frame(metrics_2nd("AC11")[[1]])
  ty_2nd_AC11_NO_01 <- as.data.frame(metrics_2nd("AC11")[[2]])
  ty_2nd_AC11_NO2_00 <- as.data.frame(metrics_2nd("AC11")[[3]])
  ty_2nd_AC11_NO2_01 <- as.data.frame(metrics_2nd("AC11")[[4]])
  ty_2nd_AC12_NO_00 <- as.data.frame(metrics_2nd("AC12")[[1]])
  ty_2nd_AC12_NO_01 <- as.data.frame(metrics_2nd("AC12")[[2]])
  ty_2nd_AC12_NO2_00 <- as.data.frame(metrics_2nd("AC12")[[3]])
  ty_2nd_AC12_NO2_01 <- as.data.frame(metrics_2nd("AC12")[[4]])
}

# Taylor diagram --------------------
{
  ## NO
  jpeg("figure/fig_5_taylor_diagram_NO.jpeg", width =11, height = 11, units = 'cm', res = 1200)
  {
    ## 1st
    ## AC9 
    {
      taylor.diagram(AC9_1st$REFNO, # makes a vector
                     RE_M_AC9_1st_NO_00_RLM$NO.ppb.,
                     add=FALSE,
                     col="#008BF8",
                     pch=3,
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
      
      taylor.diagram(AC9_1st$REFNO, # makes a vector
                     RE_M_AC9_1st_NO_00_RF$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
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
      
      
      
      taylor.diagram(AC9_1st$REFNO, # makes a vector
                     RE_M_AC9_1st_NO_01_RLM$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
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
      
      
      
      taylor.diagram(AC9_1st$REFNO, # makes a vector
                     RE_M_AC9_1st_NO_01_RF$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
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
    ## AC10 
    {
      taylor.diagram(AC10_1st$REFNO, # makes a vector
                     RE_M_AC10_1st_NO_00_RLM$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
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
      
      
      
      taylor.diagram(AC10_1st$REFNO, # makes a vector
                     RE_M_AC10_1st_NO_00_RF$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
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
      
      
      
      taylor.diagram(AC10_1st$REFNO, # makes a vector
                     RE_M_AC10_1st_NO_01_RLM$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
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
      
      
      
      taylor.diagram(AC10_1st$REFNO, # makes a vector
                     RE_M_AC10_1st_NO_01_RF$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
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
    ## AC11 
    {
      taylor.diagram(AC11_1st$REFNO, # makes a vector
                     RE_M_AC11_1st_NO_00_RLM$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
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
      
      
      
      taylor.diagram(AC11_1st$REFNO, # makes a vector
                     RE_M_AC11_1st_NO_00_RF$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
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
      
      
      
      taylor.diagram(AC11_1st$REFNO, # makes a vector
                     RE_M_AC11_1st_NO_01_RLM$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
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
      
      
      
      taylor.diagram(AC11_1st$REFNO, # makes a vector
                     RE_M_AC11_1st_NO_01_RF$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
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
    ## AC12 
    {
      taylor.diagram(AC12_1st$REFNO, # makes a vector
                     RE_M_AC12_1st_NO_00_RLM$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
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
      
      
      
      taylor.diagram(AC12_1st$REFNO, # makes a vector
                     RE_M_AC12_1st_NO_00_RF$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
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
      
      
      
      taylor.diagram(AC12_1st$REFNO, # makes a vector
                     RE_M_AC12_1st_NO_01_RLM$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
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
      
      
      
      taylor.diagram(AC12_1st$REFNO, # makes a vector
                     RE_M_AC12_1st_NO_01_RF$NO.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
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
    ## AC9 
    {
      taylor.diagram(ty_2nd_AC9_NO_00$Reference, # makes a vector
                     ty_2nd_AC9_NO_00$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
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
      
      
      taylor.diagram(ty_2nd_AC9_NO_00$Reference, # makes a vector
                     ty_2nd_AC9_NO_00$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
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
      
      
      taylor.diagram(ty_2nd_AC9_NO_01$Reference, # makes a vector
                     ty_2nd_AC9_NO_01$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
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
      
      
      
      taylor.diagram(ty_2nd_AC9_NO_01$Reference, # makes a vector
                     ty_2nd_AC9_NO_01$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
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
    ## AC10 
    {
      taylor.diagram(ty_2nd_AC10_NO_00$Reference, # makes a vector
                     ty_2nd_AC10_NO_00$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
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
      
      
      taylor.diagram(ty_2nd_AC10_NO_00$Reference, # makes a vector
                     ty_2nd_AC10_NO_00$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
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
      
      
      taylor.diagram(ty_2nd_AC10_NO_01$Reference, # makes a vector
                     ty_2nd_AC10_NO_01$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
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
      
      
      
      taylor.diagram(ty_2nd_AC10_NO_01$Reference, # makes a vector
                     ty_2nd_AC10_NO_01$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
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
    ## AC11 
    {
      taylor.diagram(ty_2nd_AC11_NO_00$Reference, # makes a vector
                     ty_2nd_AC11_NO_00$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
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
      
      
      taylor.diagram(ty_2nd_AC11_NO_00$Reference, # makes a vector
                     ty_2nd_AC11_NO_00$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
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
      
      
      taylor.diagram(ty_2nd_AC11_NO_01$Reference, # makes a vector
                     ty_2nd_AC11_NO_01$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
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
      
      
      
      taylor.diagram(ty_2nd_AC11_NO_01$Reference, # makes a vector
                     ty_2nd_AC11_NO_01$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
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
    ## AC12 
    {
      taylor.diagram(ty_2nd_AC12_NO_00$Reference, # makes a vector
                     ty_2nd_AC12_NO_00$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
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
      
      
      taylor.diagram(ty_2nd_AC12_NO_00$Reference, # makes a vector
                     ty_2nd_AC12_NO_00$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
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
      
      
      taylor.diagram(ty_2nd_AC12_NO_01$Reference, # makes a vector
                     ty_2nd_AC12_NO_01$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
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
      
      
      
      taylor.diagram(ty_2nd_AC12_NO_01$Reference, # makes a vector
                     ty_2nd_AC12_NO_01$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
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
  jpeg("figure/fig_5_taylor_diagram_NO2.jpeg", width =11, height = 11, units = 'cm', res = 1200)
  {
    ## 1st
    ## AC9 
    {
      taylor.diagram(AC9_1st$REFNO2, # makes a vector
                     RE_M_AC9_1st_NO2_00_RLM$NO2.ppb.,
                     add=FALSE,
                     col="#008BF8",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      taylor.diagram(AC9_1st$REFNO2, # makes a vector
                     RE_M_AC9_1st_NO2_00_RF$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC9_1st$REFNO2, # makes a vector
                     RE_M_AC9_1st_NO2_01_RLM$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC9_1st$REFNO2, # makes a vector
                     RE_M_AC9_1st_NO2_01_RF$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
    ## AC10 
    {
      taylor.diagram(AC10_1st$REFNO2, # makes a vector
                     RE_M_AC10_1st_NO2_00_RLM$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC10_1st$REFNO2, # makes a vector
                     RE_M_AC10_1st_NO2_00_RF$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC10_1st$REFNO2, # makes a vector
                     RE_M_AC10_1st_NO2_01_RLM$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC10_1st$REFNO2, # makes a vector
                     RE_M_AC10_1st_NO2_01_RF$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
    ## AC11 
    {
      taylor.diagram(AC11_1st$REFNO2, # makes a vector
                     RE_M_AC11_1st_NO2_00_RLM$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC11_1st$REFNO2, # makes a vector
                     RE_M_AC11_1st_NO2_00_RF$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC11_1st$REFNO2, # makes a vector
                     RE_M_AC11_1st_NO2_01_RLM$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC11_1st$REFNO2, # makes a vector
                     RE_M_AC11_1st_NO2_01_RF$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
    ## AC12 
    {
      taylor.diagram(AC12_1st$REFNO2, # makes a vector
                     RE_M_AC12_1st_NO2_00_RLM$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC12_1st$REFNO2, # makes a vector
                     RE_M_AC12_1st_NO2_00_RF$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC12_1st$REFNO2, # makes a vector
                     RE_M_AC12_1st_NO2_01_RLM$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(AC12_1st$REFNO2, # makes a vector
                     RE_M_AC12_1st_NO2_01_RF$NO2.ppb.,
                     add=TRUE,
                     col="#008BF8",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
    ## AC9 
    {
      taylor.diagram(ty_2nd_AC9_NO2_00$Reference, # makes a vector
                     ty_2nd_AC9_NO2_00$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      taylor.diagram(ty_2nd_AC9_NO2_00$Reference, # makes a vector
                     ty_2nd_AC9_NO2_00$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      taylor.diagram(ty_2nd_AC9_NO2_01$Reference, # makes a vector
                     ty_2nd_AC9_NO2_01$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(ty_2nd_AC9_NO2_01$Reference, # makes a vector
                     ty_2nd_AC9_NO2_01$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
    ## AC10 
    {
      taylor.diagram(ty_2nd_AC10_NO2_00$Reference, # makes a vector
                     ty_2nd_AC10_NO2_00$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      taylor.diagram(ty_2nd_AC10_NO2_00$Reference, # makes a vector
                     ty_2nd_AC10_NO2_00$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      taylor.diagram(ty_2nd_AC10_NO2_01$Reference, # makes a vector
                     ty_2nd_AC10_NO2_01$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(ty_2nd_AC10_NO2_01$Reference, # makes a vector
                     ty_2nd_AC10_NO2_01$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
    ## AC11 
    {
      taylor.diagram(ty_2nd_AC11_NO2_00$Reference, # makes a vector
                     ty_2nd_AC11_NO2_00$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      taylor.diagram(ty_2nd_AC11_NO2_00$Reference, # makes a vector
                     ty_2nd_AC11_NO2_00$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      taylor.diagram(ty_2nd_AC11_NO2_01$Reference, # makes a vector
                     ty_2nd_AC11_NO2_01$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(ty_2nd_AC11_NO2_01$Reference, # makes a vector
                     ty_2nd_AC11_NO2_01$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
    ## AC12 
    {
      taylor.diagram(ty_2nd_AC12_NO2_00$Reference, # makes a vector
                     ty_2nd_AC12_NO2_00$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      taylor.diagram(ty_2nd_AC12_NO2_00$Reference, # makes a vector
                     ty_2nd_AC12_NO2_00$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      taylor.diagram(ty_2nd_AC12_NO2_01$Reference, # makes a vector
                     ty_2nd_AC12_NO2_01$RLM,
                     add=TRUE,
                     col="#DC0073",
                     pch=3,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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
      
      
      
      taylor.diagram(ty_2nd_AC12_NO2_01$Reference, # makes a vector
                     ty_2nd_AC12_NO2_01$RF,
                     add=TRUE,
                     col="#DC0073",
                     pch=21,
                     pos.cor=TRUE,
                     xlab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     ylab=expression(paste(sigma[hat("y")]/sigma["y"])),
                     main="Taylor Diagram : NO2",
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