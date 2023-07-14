# 2nd_colocation_Malfunction_filtering.R
#
# Objective: Filtering the malfunction periods in the 2nd co-location campaign
#            and plot the scatter plot
#
# Library----------------------------------
require(chron)
library(data.table)
library(dplyr)
library(lubridate)
library(tdr)
library(ggpubr)

setwd("D:/empa_intern/Paper/data_and_code")


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


# import Filtering period csv file
{
filtering_period <- read.csv("data_input/filtering_2nd_period.csv",header = TRUE)
filtering_period$start <- mdy_hm(filtering_period$start)
filtering_period$end <- mdy_hm(filtering_period$end)
filtering_period$start_timestamp <- as.numeric(difftime(time1=filtering_period$start,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
filtering_period$end_timestamp <- as.numeric(difftime(time1=filtering_period$end,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
}

# function -------------------------------
malfunc.filter <- function(sensorunit){
  ## Data import -------------------------------
  {
    data_NO_00_RLM <- read.csv(paste("data_output/calibrated_2nd/",sensorunit,"_NO_00_RLM.csv",sep = ""),header = TRUE)
    data_NO_00_RF <- read.csv(paste("data_output/calibrated_2nd/",sensorunit,"_NO_00_RF.csv",sep = ""),header = TRUE)
    data_NO_01_RLM <- read.csv(paste("data_output/calibrated_2nd/",sensorunit,"_NO_01_RLM.csv",sep = ""),header = TRUE)
    data_NO_01_RF <- read.csv(paste("data_output/calibrated_2nd/",sensorunit,"_NO_01_RF.csv",sep = ""),header = TRUE)
    data_NO2_00_RLM <- read.csv(paste("data_output/calibrated_2nd/",sensorunit,"_NO2_00_RLM.csv",sep = ""),header = TRUE)
    data_NO2_00_RF <- read.csv(paste("data_output/calibrated_2nd/",sensorunit,"_NO2_00_RF.csv",sep = ""),header = TRUE)
    data_NO2_01_RLM <- read.csv(paste("data_output/calibrated_2nd/",sensorunit,"_NO2_01_RLM.csv",sep = ""),header = TRUE)
    data_NO2_01_RF <- read.csv(paste("data_output/calibrated_2nd/",sensorunit,"_NO2_01_RF.csv",sep = ""),header = TRUE)
    
    
  }
  ## Import the reference -------------------------------
  {
    ref <- read.csv(paste("data_output/pre_processed_2nd_period/",sensorunit,".csv",sep = ""), header = TRUE)
  }
  ## Merge the calibrated data with the reference ----------------------------
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
    ### Insert the timestamp
    {
      data_NO_00$timestamp <- as.numeric(difftime(time1=data_NO_00$`Date[UTC]`,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
      data_NO_01$timestamp <- as.numeric(difftime(time1=data_NO_01$`Date[UTC]`,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
      data_NO2_00$timestamp <- as.numeric(difftime(time1=data_NO2_00$`Date[UTC]`,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
      data_NO2_01$timestamp <- as.numeric(difftime(time1=data_NO2_01$`Date[UTC]`,time2=strptime("01.01.1970 00:00:00","%d.%m.%Y %H:%M:%S",tz="UTC"),tz="UTC",units="secs"))
      
    }
    
  }
  ## Filtering -------------------------------
  {
    ### indicate period for each sensor unit
    {
      period_for_sensor <- which(filtering_period$Sensor == sensorunit)
      period_for_sensor <- c(as.numeric(period_for_sensor))
    }
    ### NO_00
    {
      data_NO_00$Status <- "Unfiltered"
      for (i in period_for_sensor)
      {
        for(j in 1:nrow(data_NO_00))
        {
          if(data_NO_00$timestamp[j] >= filtering_period$start_timestamp[i] && data_NO_00$timestamp[j] <= filtering_period$end_timestamp[i])
          {
            data_NO_00$Status[j] <- "Filtered"
          }
        }
      }
      data_NO_00_nonfilter <- data_NO_00 %>% filter(Status != "Filtered")
      data_NO_00_filter <- data_NO_00 %>% filter(Status == "Filtered")
      data_NO_00$Status <- as.character(data_NO_00$Status)
    }
    ### NO_01
    {
      data_NO_01$Status <- "Unfiltered"
      for (i in period_for_sensor)
      {
        for(j in 1:nrow(data_NO_01))
        {
          if(data_NO_01$timestamp[j] >= filtering_period$start_timestamp[i] && data_NO_01$timestamp[j] <= filtering_period$end_timestamp[i])
          {
            data_NO_01$Status[j] <-"Filtered"
          }
        }
      }
      data_NO_01_nonfilter <- data_NO_01 %>% filter(Status != "Filtered")
      data_NO_01_filter <- data_NO_01 %>% filter(Status == "Filtered")
      data_NO_01$Status <- as.character(data_NO_01$Status)
    }
    ### NO2_00
    {
      data_NO2_00$Status <- "Unfiltered"
      for (i in period_for_sensor)
      {
        for(j in 1:nrow(data_NO2_00))
        {
          if(data_NO2_00$timestamp[j] >= filtering_period$start_timestamp[i] && data_NO2_00$timestamp[j] <= filtering_period$end_timestamp[i])
          {
            data_NO2_00$Status[j] <- "Filtered"
          }
        }
      }
      data_NO2_00_nonfilter <- data_NO2_00 %>% filter(Status != "Filtered")
      data_NO2_00_filter <- data_NO2_00 %>% filter(Status == "Filtered")
      data_NO2_00$Status <- as.character(data_NO2_00$Status)
    }
    ### NO2_01
    {
      data_NO2_01$Status <- "Unfiltered"
      for (i in period_for_sensor)
      {
        for(j in 1:nrow(data_NO2_01))
        {
          if(data_NO2_01$timestamp[j] >= filtering_period$start_timestamp[i] && data_NO2_01$timestamp[j] <= filtering_period$end_timestamp[i])
          {
            data_NO2_01$Status[j] <-"Filtered"
          }
        }
      }
      data_NO2_01_nonfilter <- data_NO2_01 %>% filter(Status != "Filtered")
      data_NO2_01_filter <- data_NO2_01 %>% filter(Status == "Filtered")
      data_NO2_01$Status <- as.character(data_NO2_01$Status)
    }
    
  }
  ## Store the filtered data -------------------------------
  {
    write.csv(data_NO_00_nonfilter, paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO_00.csv",sep = ""))
    write.csv(data_NO_01_nonfilter, paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO_01.csv",sep = ""))
    write.csv(data_NO2_00_nonfilter, paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO2_00.csv",sep = ""))
    write.csv(data_NO2_01_nonfilter, paste("data_output/calibrated_2nd_filtered/",sensorunit,"_NO2_01.csv",sep = ""))
    
  }
  ## tdStats :  derive the statistical metrics of filtered 2nd colocation data -
  { 
    ### NO_00
    {
    res_data_NO_00_RLM <- tdStats(data_NO_00_nonfilter$RLM,
                                  data_NO_00_nonfilter$Reference,
                                  functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_data_NO_00_RF <- tdStats(data_NO_00_nonfilter$RF,
                                  data_NO_00_nonfilter$Reference,
                                  functions = c("rmse", "mbe", "sdm", "sdo","r2"))
    
    res_data_NO_00 <- as.data.frame(rbind(res_data_NO_00_RLM,res_data_NO_00_RF))
    
    res_data_NO_00$x <- sign((res_data_NO_00$sdm)-(res_data_NO_00$sdo))/(res_data_NO_00$sdo)*sqrt((res_data_NO_00$rmse)^2-(res_data_NO_00$mbe)^2)
    res_data_NO_00$y <- (res_data_NO_00$mbe)/(res_data_NO_00$sdo)
    res_data_NO_00$nrmse <- sqrt((res_data_NO_00$x)^2+(res_data_NO_00$y)^2)
    }
    ### NO_01
    {
      res_data_NO_01_RLM <- tdStats(data_NO_01_nonfilter$RLM,
                                    data_NO_01_nonfilter$Reference,
                                    functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO_01_RF <- tdStats(data_NO_01_nonfilter$RF,
                                   data_NO_01_nonfilter$Reference,
                                   functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO_01 <- as.data.frame(rbind(res_data_NO_01_RLM,res_data_NO_01_RF))
      
      res_data_NO_01$x <- sign((res_data_NO_01$sdm)-(res_data_NO_01$sdo))/(res_data_NO_01$sdo)*sqrt((res_data_NO_01$rmse)^2-(res_data_NO_01$mbe)^2)
      res_data_NO_01$y <- (res_data_NO_01$mbe)/(res_data_NO_01$sdo)
      res_data_NO_01$nrmse <- sqrt((res_data_NO_01$x)^2+(res_data_NO_01$y)^2)
    }
    ### NO2_00
    {
      res_data_NO2_00_RLM <- tdStats(data_NO2_00_nonfilter$RLM,
                                     data_NO2_00_nonfilter$Reference,
                                     functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO2_00_RF <- tdStats(data_NO2_00_nonfilter$RF,
                                    data_NO2_00_nonfilter$Reference,
                                    functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO2_00 <- as.data.frame(rbind(res_data_NO2_00_RLM,res_data_NO2_00_RF))
      
      res_data_NO2_00$x <- sign((res_data_NO2_00$sdm)-(res_data_NO2_00$sdo))/(res_data_NO2_00$sdo)*sqrt((res_data_NO2_00$rmse)^2-(res_data_NO2_00$mbe)^2)
      res_data_NO2_00$y <- (res_data_NO2_00$mbe)/(res_data_NO2_00$sdo)
      res_data_NO2_00$nrmse <- sqrt((res_data_NO2_00$x)^2+(res_data_NO2_00$y)^2)
    }
    ### NO2_01
    {
      res_data_NO2_01_RLM <- tdStats(data_NO2_01_nonfilter$RLM,
                                     data_NO2_01_nonfilter$Reference,
                                     functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO2_01_RF <- tdStats(data_NO2_01_nonfilter$RF,
                                    data_NO2_01_nonfilter$Reference,
                                    functions = c("rmse", "mbe", "sdm", "sdo","r2"))
      
      res_data_NO2_01 <- as.data.frame(rbind(res_data_NO2_01_RLM,res_data_NO2_01_RF))
      
      res_data_NO2_01$x <- sign((res_data_NO2_01$sdm)-(res_data_NO2_01$sdo))/(res_data_NO2_01$sdo)*sqrt((res_data_NO2_01$rmse)^2-(res_data_NO2_01$mbe)^2)
      res_data_NO2_01$y <- (res_data_NO2_01$mbe)/(res_data_NO2_01$sdo)
      res_data_NO2_01$nrmse <- sqrt((res_data_NO2_01$x)^2+(res_data_NO2_01$y)^2)
    }
    ### store the metrics
    {
      write.csv(res_data_NO_00, paste("data_output/calibrated_2nd_filtered/td_res/",sensorunit,"_NO_00.csv",sep = ""))
      write.csv(res_data_NO_01, paste("data_output/calibrated_2nd_filtered/td_res/",sensorunit,"_NO_01.csv",sep = ""))
      write.csv(res_data_NO2_00, paste("data_output/calibrated_2nd_filtered/td_res/",sensorunit,"_NO2_00.csv",sep = ""))
      write.csv(res_data_NO2_01, paste("data_output/calibrated_2nd_filtered/td_res/",sensorunit,"_NO2_01.csv",sep = ""))
      
    }
  }
  ## Plotting scatter plot which involved in the supplementary materials ------
  {
     pdata_NO_A_RLM <- ggplot(data_NO_00, aes(Reference,RLM,color=Status)
      )+geom_point(size=0.3
      )+geom_abline(slope=1,intercept = 0,col="#089099",linetype=2
      )+xlim(c(-20,250))+ylim(c(-20,250))+theme_bw(
      )+xlab("Reference [ppb]")+ylab("Sensor concentration [ppb]"
      )+ggtitle(paste(sensorunit,", NO_A, RLM model",sep = "")
      )+scale_color_manual(values=c("#ed553b","#000000")
      )+theme(axis.text = element_text(size = 8)
      )+theme(axis.title = element_text(size = 9)
      )+theme(plot.title = element_text(size = 10)
      )+guides(colour = guide_legend(override.aes = list(size=4)))
     
      
     pdata_NO_A_RF <- ggplot(data_NO_00, aes(Reference,RF,color=Status)
     )+geom_point(size=0.3
     )+geom_abline(slope=1,intercept = 0,col="#089099",linetype=2
     )+xlim(c(-20,250))+ylim(c(-20,250))+theme_bw(
     )+xlab("Reference [ppb]")+ylab("Sensor concentration [ppb]"
     )+ggtitle(paste(sensorunit,", NO_A, RF model",sep = "")
     )+scale_color_manual(values=c("#ed553b","#000000")
     )+theme(axis.text = element_text(size = 8)
     )+theme(axis.title = element_text(size = 9)
     )+theme(plot.title = element_text(size = 10)
     )+guides(colour = guide_legend(override.aes = list(size=4)))
     
     
     pdata_NO_B_RLM <- ggplot(data_NO_01, aes(Reference,RLM,color=Status)
     )+geom_point(size=0.3
     )+geom_abline(slope=1,intercept = 0,col="#089099",linetype=2
     )+xlim(c(-20,250))+ylim(c(-20,250))+theme_bw(
     )+xlab("Reference [ppb]")+ylab("Sensor concentration [ppb]"
     )+ggtitle(paste(sensorunit,", NO_B, RLM model",sep = "")
     )+scale_color_manual(values=c("#ed553b","#000000")
     )+theme(axis.text = element_text(size = 8)
     )+theme(axis.title = element_text(size = 9)
     )+theme(plot.title = element_text(size = 10)
     )+guides(colour = guide_legend(override.aes = list(size=4)))
     
     
     pdata_NO_B_RF <- ggplot(data_NO_01, aes(Reference,RF,color=Status)
     )+geom_point(size=0.3
     )+geom_abline(slope=1,intercept = 0,col="#089099",linetype=2
     )+xlim(c(-20,250))+ylim(c(-20,250))+theme_bw(
     )+xlab("Reference [ppb]")+ylab("Sensor concentration [ppb]"
     )+ggtitle(paste(sensorunit,", NO_B, RF model",sep = "")
     )+scale_color_manual(values=c("#ed553b","#000000")
     )+theme(axis.text = element_text(size = 8)
     )+theme(axis.title = element_text(size = 9)
     )+theme(plot.title = element_text(size = 10)
     )+guides(colour = guide_legend(override.aes = list(size=4)))
     
     
     pdata_NO2_A_RLM <- ggplot(data_NO2_00, aes(Reference,RLM,color=Status)
     )+geom_point(size=0.3
     )+geom_abline(slope=1,intercept = 0,col="#089099",linetype=2
     )+xlim(c(-20,100))+ylim(c(-20,100))+theme_bw(
     )+xlab("Reference [ppb]")+ylab("Sensor concentration [ppb]"
     )+ggtitle(paste(sensorunit,", NO2_A, RLM model",sep = "")
     )+scale_color_manual(values=c("#ed553b","#000000")
     )+theme(axis.text = element_text(size = 8)
     )+theme(axis.title = element_text(size = 9)
     )+theme(plot.title = element_text(size = 10)
     )+guides(colour = guide_legend(override.aes = list(size=4)))
     
     
     pdata_NO2_A_RF <- ggplot(data_NO2_00, aes(Reference,RF,color=Status)
     )+geom_point(size=0.3
     )+geom_abline(slope=1,intercept = 0,col="#089099",linetype=2
     )+xlim(c(-20,100))+ylim(c(-20,100))+theme_bw(
     )+xlab("Reference [ppb]")+ylab("Sensor concentration [ppb]"
     )+ggtitle(paste(sensorunit,", NO2_A, RF model",sep = "")
     )+scale_color_manual(values=c("#ed553b","#000000")
     )+theme(axis.text = element_text(size = 8)
     )+theme(axis.title = element_text(size = 9)
     )+theme(plot.title = element_text(size = 10)
     )+guides(colour = guide_legend(override.aes = list(size=4)))
     
     
     pdata_NO2_B_RLM <- ggplot(data_NO2_01, aes(Reference,RLM,color=Status)
     )+geom_point(size=0.3
     )+geom_abline(slope=1,intercept = 0,col="#089099",linetype=2
     )+xlim(c(-20,100))+ylim(c(-20,100))+theme_bw(
     )+xlab("Reference [ppb]")+ylab("Sensor concentration [ppb]"
     )+ggtitle(paste(sensorunit,", NO2_B, RLM model",sep = "")
     )+scale_color_manual(values=c("#ed553b","#000000")
     )+theme(axis.text = element_text(size = 8)
     )+theme(axis.title = element_text(size = 9)
     )+theme(plot.title = element_text(size = 10)
     )+guides(colour = guide_legend(override.aes = list(size=4)))
     
     
     pdata_NO2_B_RF <- ggplot(data_NO2_01, aes(Reference,RF,color=Status)
     )+geom_point(size=0.3
     )+geom_abline(slope=1,intercept = 0,col="#089099",linetype=2
     )+xlim(c(-20,100))+ylim(c(-20,100))+theme_bw(
     )+xlab("Reference [ppb]")+ylab("Sensor concentration [ppb]"
     )+ggtitle(paste(sensorunit,", NO2_B, RF model",sep = "")
     )+scale_color_manual(values=c("#ed553b","#000000")
     )+theme(axis.text = element_text(size = 8)
     )+theme(axis.title = element_text(size = 9)
     )+theme(plot.title = element_text(size = 10)
     )+guides(colour = guide_legend(override.aes = list(size=4)))
     
     fig <- ggarrange(pdata_NO_A_RLM, pdata_NO2_A_RLM, ncol = 2, nrow = 1, labels = c("(a)","(b)"))
     
     return(fig)
  }
}

p <- malfunc.filter("AC10")

jpeg("figure/fig_9.jpeg", width =21, height = 8, units = 'cm', res = 800)
p
dev.off()
