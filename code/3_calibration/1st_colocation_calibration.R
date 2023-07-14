# 1st_colocation_calibration.R
#
# Objectives : This script do the calibration with training & test dataset
#              during first colocation campaign.
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

# Function -----------------------

kfoldcv <- function(sensorunit){
  
  data <- read.csv(paste("data_output/pre_processed_1st_period/",sensorunit,".csv",sep = ""), header = TRUE)
  
  data$X <- seq(from = 1, to = length(data$date) ,by = 1)
  data$date <- ymd_hms(data$date)
  
  data <- as_tibble(data)
  
  data <- as_tbl_time(data, index = date)
  
  
  # Calibration parameter -----------------------------------------------------
  
  ## Robust Regression
  RLM_K_NO  = 1.345
  RLM_K_NO2 = 1.345
  RLM_MAXIT = 1e3
  
  ##Random Forest
  nodesize <- 100
  ntree <- 1000
  
  # split the training and testing data --------------------------------------
  {
    ## My methods --------------
    set.seed(1234)
    rs1 <- createDataPartition(data$timestamp, p = 4/5, list = FALSE)
    TR_data_1 <- data[rs1,]
    VA_data_1 <- data[-rs1,]
    
    set.seed(1234)
    rs2 <- createDataPartition(TR_data_1$timestamp, p = 3/4, list = FALSE)
    TR_data_2 <- TR_data_1[rs2,]
    VA_data_2 <- TR_data_1[-rs2,]
    
    set.seed(1234)
    rs3 <- createDataPartition(TR_data_2$timestamp, p = 2/3, list = FALSE)
    TR_data_3 <- TR_data_2[rs3,]
    VA_data_3 <- TR_data_2[-rs3,]
    
    set.seed(1234)
    rs4 <- createDataPartition(TR_data_3$timestamp, p = 1/2, list = FALSE)
    VA_data_5 <- TR_data_3[rs4,]
    VA_data_4 <- TR_data_3[-rs4,]
    
    rm(rs1,rs2,rs3,rs4,TR_data_1,TR_data_2,TR_data_3)
  }
  
  # First round ---------------------------------------
  {
    TR <- rbind(VA_data_2,VA_data_3,VA_data_4,VA_data_5)
    VA <- VA_data_1
    
    TR <- TR[order(TR$X),]
    
    # Calibration models 
    ## 
    ### NO_00
    {
      #### RLM
      M_data_NO_00_RLM_C <- rlm(REFNO~I(NO_00)+I(T_01)+I(NO_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_00_RF_A <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO_01
    {
      #### RLM
      M_data_NO_01_RLM_C <- rlm(REFNO~I(NO_01)+I(T_01)+I(NO_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_01_RF_A <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO2_00
    {
      #### RLM
      M_data_NO2_00_RLM_C <- rlm(REFNO2~I(NO2_00)+I(T_01)+I(NO2_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_00_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    ### NO2_01
    {
      #### RLM
      M_data_NO2_01_RLM_C <- rlm(REFNO2~I(NO2_01)+I(T_01)+I(NO2_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_01_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    
    # Prediction --------------------------------------------------
    ## 
    ### NO_00
    {
      #### RLM
      PR_M_data_NO_00_RLM_C <- predict(M_data_NO_00_RLM_C, VA)
      #### RF
      PR_M_data_NO_00_RF_A <- predict(M_data_NO_00_RF_A, VA)
    }
    ### NO_01
    {
      #### RLM
      PR_M_data_NO_01_RLM_C <- predict(M_data_NO_01_RLM_C, VA)
      #### RF
      PR_M_data_NO_01_RF_A <- predict(M_data_NO_01_RF_A, VA)
    }
    ### NO2_00
    {
      #### RLM
      PR_M_data_NO2_00_RLM_C <- predict(M_data_NO2_00_RLM_C, VA)
      #### RF
      PR_M_data_NO2_00_RF_A <- predict(M_data_NO2_00_RF_A, VA)
    }
    ### NO2_01
    {
      #### RLM
      PR_M_data_NO2_01_RLM_C <- predict(M_data_NO2_01_RLM_C, VA)
      #### RF
      PR_M_data_NO2_01_RF_A <- predict(M_data_NO2_01_RF_A, VA)
    }
    
    # store the results in withe index -----------------------------------------------
    ### NO_00
    {
      RE_PR_M_data_NO_00_RLM_C_1 <- as.data.frame(PR_M_data_NO_00_RLM_C)
      RE_PR_M_data_NO_00_RLM_C_1 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM_C_1$PR_M_data_NO_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO_00_RF_A_1 <- as.data.frame(PR_M_data_NO_00_RF_A)
      RE_PR_M_data_NO_00_RF_A_1 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RF_A_1$PR_M_data_NO_00_RF_A,VA$X))
    }
    ### NO_01
    {
      RE_PR_M_data_NO_01_RLM_C_1 <- as.data.frame(PR_M_data_NO_01_RLM_C)
      RE_PR_M_data_NO_01_RLM_C_1 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RLM_C_1$PR_M_data_NO_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO_01_RF_A_1 <- as.data.frame(PR_M_data_NO_01_RF_A)
      RE_PR_M_data_NO_01_RF_A_1 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RF_A_1$PR_M_data_NO_01_RF_A,VA$X))
    }
    ### NO2_00
    {
      RE_PR_M_data_NO2_00_RLM_C_1 <- as.data.frame(PR_M_data_NO2_00_RLM_C)
      RE_PR_M_data_NO2_00_RLM_C_1 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM_C_1$PR_M_data_NO2_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_00_RF_A_1 <- as.data.frame(PR_M_data_NO2_00_RF_A)
      RE_PR_M_data_NO2_00_RF_A_1 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RF_A_1$PR_M_data_NO2_00_RF_A,VA$X))
    }
    ### NO2_01
    {
      RE_PR_M_data_NO2_01_RLM_C_1 <- as.data.frame(PR_M_data_NO2_01_RLM_C)
      RE_PR_M_data_NO2_01_RLM_C_1 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RLM_C_1$PR_M_data_NO2_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_01_RF_A_1 <- as.data.frame(PR_M_data_NO2_01_RF_A)
      RE_PR_M_data_NO2_01_RF_A_1 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RF_A_1$PR_M_data_NO2_01_RF_A,VA$X))
    }
  }
  # Second round ---------------------------------------
  {
    TR <- rbind(VA_data_1,VA_data_3,VA_data_4,VA_data_5)
    VA <- VA_data_2
    
    TR <- TR[order(TR$X),]
    
    # Calibration models 
    ## 
    ### NO_00
    {
      #### RLM
      M_data_NO_00_RLM_C <- rlm(REFNO~I(NO_00)+I(T_01)+I(NO_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_00_RF_A <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO_01
    {
      #### RLM
      M_data_NO_01_RLM_C <- rlm(REFNO~I(NO_01)+I(T_01)+I(NO_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_01_RF_A <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO2_00
    {
      #### RLM
      M_data_NO2_00_RLM_C <- rlm(REFNO2~I(NO2_00)+I(T_01)+I(NO2_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_00_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    ### NO2_01
    {
      #### RLM
      M_data_NO2_01_RLM_C <- rlm(REFNO2~I(NO2_01)+I(T_01)+I(NO2_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_01_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    
    # Prediction --------------------------------------------------
    ## 
    ### NO_00
    {
      #### RLM
      PR_M_data_NO_00_RLM_C <- predict(M_data_NO_00_RLM_C, VA)
      #### RF
      PR_M_data_NO_00_RF_A <- predict(M_data_NO_00_RF_A, VA)
    }
    ### NO_01
    {
      #### RLM
      PR_M_data_NO_01_RLM_C <- predict(M_data_NO_01_RLM_C, VA)
      #### RF
      PR_M_data_NO_01_RF_A <- predict(M_data_NO_01_RF_A, VA)
    }
    ### NO2_00
    {
      #### RLM
      PR_M_data_NO2_00_RLM_C <- predict(M_data_NO2_00_RLM_C, VA)
      #### RF
      PR_M_data_NO2_00_RF_A <- predict(M_data_NO2_00_RF_A, VA)
    }
    ### NO2_01
    {
      #### RLM
      PR_M_data_NO2_01_RLM_C <- predict(M_data_NO2_01_RLM_C, VA)
      #### RF
      PR_M_data_NO2_01_RF_A <- predict(M_data_NO2_01_RF_A, VA)
    }
    
    # store the results in withe index -----------------------------------------------
    ### NO_00
    {
      RE_PR_M_data_NO_00_RLM_C_2 <- as.data.frame(PR_M_data_NO_00_RLM_C)
      RE_PR_M_data_NO_00_RLM_C_2 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM_C_2$PR_M_data_NO_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO_00_RF_A_2 <- as.data.frame(PR_M_data_NO_00_RF_A)
      RE_PR_M_data_NO_00_RF_A_2 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RF_A_2$PR_M_data_NO_00_RF_A,VA$X))
    }
    ### NO_01
    {
      RE_PR_M_data_NO_01_RLM_C_2 <- as.data.frame(PR_M_data_NO_01_RLM_C)
      RE_PR_M_data_NO_01_RLM_C_2 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RLM_C_2$PR_M_data_NO_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO_01_RF_A_2 <- as.data.frame(PR_M_data_NO_01_RF_A)
      RE_PR_M_data_NO_01_RF_A_2 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RF_A_2$PR_M_data_NO_01_RF_A,VA$X))
    }
    ### NO2_00
    {
      RE_PR_M_data_NO2_00_RLM_C_2 <- as.data.frame(PR_M_data_NO2_00_RLM_C)
      RE_PR_M_data_NO2_00_RLM_C_2 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM_C_2$PR_M_data_NO2_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_00_RF_A_2 <- as.data.frame(PR_M_data_NO2_00_RF_A)
      RE_PR_M_data_NO2_00_RF_A_2 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RF_A_2$PR_M_data_NO2_00_RF_A,VA$X))
    }
    ### NO2_01
    {
      RE_PR_M_data_NO2_01_RLM_C_2 <- as.data.frame(PR_M_data_NO2_01_RLM_C)
      RE_PR_M_data_NO2_01_RLM_C_2 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RLM_C_2$PR_M_data_NO2_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_01_RF_A_2 <- as.data.frame(PR_M_data_NO2_01_RF_A)
      RE_PR_M_data_NO2_01_RF_A_2 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RF_A_2$PR_M_data_NO2_01_RF_A,VA$X))
    }
  }
  # Third round ---------------------------------------
  {
    TR <- rbind(VA_data_1,VA_data_2,VA_data_4,VA_data_5)
    VA <- VA_data_3
    
    TR <- TR[order(TR$X),]
    
    # Calibration models 
    ## 
    ### NO_00
    {
      #### RLM
      M_data_NO_00_RLM_C <- rlm(REFNO~I(NO_00)+I(T_01)+I(NO_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_00_RF_A <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO_01
    {
      #### RLM
      M_data_NO_01_RLM_C <- rlm(REFNO~I(NO_01)+I(T_01)+I(NO_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_01_RF_A <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO2_00
    {
      #### RLM
      M_data_NO2_00_RLM_C <- rlm(REFNO2~I(NO2_00)+I(T_01)+I(NO2_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_00_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    ### NO2_01
    {
      #### RLM
      M_data_NO2_01_RLM_C <- rlm(REFNO2~I(NO2_01)+I(T_01)+I(NO2_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_01_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    
    # Prediction --------------------------------------------------
    ## 
    ### NO_00
    {
      #### RLM
      PR_M_data_NO_00_RLM_C <- predict(M_data_NO_00_RLM_C, VA)
      #### RF
      PR_M_data_NO_00_RF_A <- predict(M_data_NO_00_RF_A, VA)
    }
    ### NO_01
    {
      #### RLM
      PR_M_data_NO_01_RLM_C <- predict(M_data_NO_01_RLM_C, VA)
      #### RF
      PR_M_data_NO_01_RF_A <- predict(M_data_NO_01_RF_A, VA)
    }
    ### NO2_00
    {
      #### RLM
      PR_M_data_NO2_00_RLM_C <- predict(M_data_NO2_00_RLM_C, VA)
      #### RF
      PR_M_data_NO2_00_RF_A <- predict(M_data_NO2_00_RF_A, VA)
    }
    ### NO2_01
    {
      #### RLM
      PR_M_data_NO2_01_RLM_C <- predict(M_data_NO2_01_RLM_C, VA)
      #### RF
      PR_M_data_NO2_01_RF_A <- predict(M_data_NO2_01_RF_A, VA)
    }
    
    # store the results in withe index -----------------------------------------------
    ### NO_00
    {
      RE_PR_M_data_NO_00_RLM_C_3 <- as.data.frame(PR_M_data_NO_00_RLM_C)
      RE_PR_M_data_NO_00_RLM_C_3 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM_C_3$PR_M_data_NO_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO_00_RF_A_3 <- as.data.frame(PR_M_data_NO_00_RF_A)
      RE_PR_M_data_NO_00_RF_A_3 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RF_A_3$PR_M_data_NO_00_RF_A,VA$X))
    }
    ### NO_01
    {
      RE_PR_M_data_NO_01_RLM_C_3 <- as.data.frame(PR_M_data_NO_01_RLM_C)
      RE_PR_M_data_NO_01_RLM_C_3 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RLM_C_3$PR_M_data_NO_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO_01_RF_A_3 <- as.data.frame(PR_M_data_NO_01_RF_A)
      RE_PR_M_data_NO_01_RF_A_3 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RF_A_3$PR_M_data_NO_01_RF_A,VA$X))
    }
    ### NO2_00
    {
      RE_PR_M_data_NO2_00_RLM_C_3 <- as.data.frame(PR_M_data_NO2_00_RLM_C)
      RE_PR_M_data_NO2_00_RLM_C_3 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM_C_3$PR_M_data_NO2_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_00_RF_A_3 <- as.data.frame(PR_M_data_NO2_00_RF_A)
      RE_PR_M_data_NO2_00_RF_A_3 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RF_A_3$PR_M_data_NO2_00_RF_A,VA$X))
    }
    ### NO2_01
    {
      RE_PR_M_data_NO2_01_RLM_C_3 <- as.data.frame(PR_M_data_NO2_01_RLM_C)
      RE_PR_M_data_NO2_01_RLM_C_3 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RLM_C_3$PR_M_data_NO2_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_01_RF_A_3 <- as.data.frame(PR_M_data_NO2_01_RF_A)
      RE_PR_M_data_NO2_01_RF_A_3 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RF_A_3$PR_M_data_NO2_01_RF_A,VA$X))
    }
  }
  # Fourth round ---------------------------------------
  {
    TR <- rbind(VA_data_1,VA_data_2,VA_data_3,VA_data_5)
    VA <- VA_data_4
    
    TR <- TR[order(TR$X),]
    
    # Calibration models 
    ## 
    ### NO_00
    {
      #### RLM
      M_data_NO_00_RLM_C <- rlm(REFNO~I(NO_00)+I(T_01)+I(NO_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_00_RF_A <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO_01
    {
      #### RLM
      M_data_NO_01_RLM_C <- rlm(REFNO~I(NO_01)+I(T_01)+I(NO_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_01_RF_A <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO2_00
    {
      #### RLM
      M_data_NO2_00_RLM_C <- rlm(REFNO2~I(NO2_00)+I(T_01)+I(NO2_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_00_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    ### NO2_01
    {
      #### RLM
      M_data_NO2_01_RLM_C <- rlm(REFNO2~I(NO2_01)+I(T_01)+I(NO2_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_01_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    
    # Prediction --------------------------------------------------
    ## 
    ### NO_00
    {
      #### RLM
      PR_M_data_NO_00_RLM_C <- predict(M_data_NO_00_RLM_C, VA)
      #### RF
      PR_M_data_NO_00_RF_A <- predict(M_data_NO_00_RF_A, VA)
    }
    ### NO_01
    {
      #### RLM
      PR_M_data_NO_01_RLM_C <- predict(M_data_NO_01_RLM_C, VA)
      #### RF
      PR_M_data_NO_01_RF_A <- predict(M_data_NO_01_RF_A, VA)
    }
    ### NO2_00
    {
      #### RLM
      PR_M_data_NO2_00_RLM_C <- predict(M_data_NO2_00_RLM_C, VA)
      #### RF
      PR_M_data_NO2_00_RF_A <- predict(M_data_NO2_00_RF_A, VA)
    }
    ### NO2_01
    {
      #### RLM
      PR_M_data_NO2_01_RLM_C <- predict(M_data_NO2_01_RLM_C, VA)
      #### RF
      PR_M_data_NO2_01_RF_A <- predict(M_data_NO2_01_RF_A, VA)
    }
    
    # store the results in withe index -----------------------------------------------
    ### NO_00
    {
      RE_PR_M_data_NO_00_RLM_C_4 <- as.data.frame(PR_M_data_NO_00_RLM_C)
      RE_PR_M_data_NO_00_RLM_C_4 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM_C_4$PR_M_data_NO_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO_00_RF_A_4 <- as.data.frame(PR_M_data_NO_00_RF_A)
      RE_PR_M_data_NO_00_RF_A_4 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RF_A_4$PR_M_data_NO_00_RF_A,VA$X))
    }
    ### NO_01
    {
      RE_PR_M_data_NO_01_RLM_C_4 <- as.data.frame(PR_M_data_NO_01_RLM_C)
      RE_PR_M_data_NO_01_RLM_C_4 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RLM_C_4$PR_M_data_NO_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO_01_RF_A_4 <- as.data.frame(PR_M_data_NO_01_RF_A)
      RE_PR_M_data_NO_01_RF_A_4 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RF_A_4$PR_M_data_NO_01_RF_A,VA$X))
    }
    ### NO2_00
    {
      RE_PR_M_data_NO2_00_RLM_C_4 <- as.data.frame(PR_M_data_NO2_00_RLM_C)
      RE_PR_M_data_NO2_00_RLM_C_4 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM_C_4$PR_M_data_NO2_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_00_RF_A_4 <- as.data.frame(PR_M_data_NO2_00_RF_A)
      RE_PR_M_data_NO2_00_RF_A_4 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RF_A_4$PR_M_data_NO2_00_RF_A,VA$X))
    }
    ### NO2_01
    {
      RE_PR_M_data_NO2_01_RLM_C_4 <- as.data.frame(PR_M_data_NO2_01_RLM_C)
      RE_PR_M_data_NO2_01_RLM_C_4 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RLM_C_4$PR_M_data_NO2_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_01_RF_A_4 <- as.data.frame(PR_M_data_NO2_01_RF_A)
      RE_PR_M_data_NO2_01_RF_A_4 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RF_A_4$PR_M_data_NO2_01_RF_A,VA$X))
    }
  }
  # Fifth round ---------------------------------------
  {
    TR <- rbind(VA_data_1,VA_data_2,VA_data_3,VA_data_4)
    VA <- VA_data_5
    
    TR <- TR[order(TR$X),]
    
    # Calibration models 
    ## 
    ### NO_00
    {
      #### RLM
      M_data_NO_00_RLM_C <- rlm(REFNO~I(NO_00)+I(T_01)+I(NO_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_00_RF_A <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO_01
    {
      #### RLM
      M_data_NO_01_RLM_C <- rlm(REFNO~I(NO_01)+I(T_01)+I(NO_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      #### RF
      M_data_NO_01_RF_A <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,nodesize=nodesize)
    }
    ### NO2_00
    {
      #### RLM
      M_data_NO2_00_RLM_C <- rlm(REFNO2~I(NO2_00)+I(T_01)+I(NO2_00*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_00_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    ### NO2_01
    {
      #### RLM
      M_data_NO2_01_RLM_C <- rlm(REFNO2~I(NO2_01)+I(T_01)+I(NO2_01*T_01)+I(dRH_01_T0_150),TR,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      #### RF
      M_data_NO2_01_RF_A <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_150"),data=TR,ntree=ntree,NO2desize=NO2desize)
    }
    
    # Prediction --------------------------------------------------
    ## 
    ### NO_00
    {
      #### RLM
      PR_M_data_NO_00_RLM_C <- predict(M_data_NO_00_RLM_C, VA)
      #### RF
      PR_M_data_NO_00_RF_A <- predict(M_data_NO_00_RF_A, VA)
    }
    ### NO_01
    {
      #### RLM
      PR_M_data_NO_01_RLM_C <- predict(M_data_NO_01_RLM_C, VA)
      #### RF
      PR_M_data_NO_01_RF_A <- predict(M_data_NO_01_RF_A, VA)
    }
    ### NO2_00
    {
      #### RLM
      PR_M_data_NO2_00_RLM_C <- predict(M_data_NO2_00_RLM_C, VA)
      #### RF
      PR_M_data_NO2_00_RF_A <- predict(M_data_NO2_00_RF_A, VA)
    }
    ### NO2_01
    {
      #### RLM
      PR_M_data_NO2_01_RLM_C <- predict(M_data_NO2_01_RLM_C, VA)
      #### RF
      PR_M_data_NO2_01_RF_A <- predict(M_data_NO2_01_RF_A, VA)
    }
    
    # store the results in withe index -----------------------------------------------
    ### NO_00
    {
      RE_PR_M_data_NO_00_RLM_C_5 <- as.data.frame(PR_M_data_NO_00_RLM_C)
      RE_PR_M_data_NO_00_RLM_C_5 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RLM_C_5$PR_M_data_NO_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO_00_RF_A_5 <- as.data.frame(PR_M_data_NO_00_RF_A)
      RE_PR_M_data_NO_00_RF_A_5 <- as.data.frame(cbind(RE_PR_M_data_NO_00_RF_A_5$PR_M_data_NO_00_RF_A,VA$X))
    }
    ### NO_01
    {
      RE_PR_M_data_NO_01_RLM_C_5 <- as.data.frame(PR_M_data_NO_01_RLM_C)
      RE_PR_M_data_NO_01_RLM_C_5 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RLM_C_5$PR_M_data_NO_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO_01_RF_A_5 <- as.data.frame(PR_M_data_NO_01_RF_A)
      RE_PR_M_data_NO_01_RF_A_5 <- as.data.frame(cbind(RE_PR_M_data_NO_01_RF_A_5$PR_M_data_NO_01_RF_A,VA$X))
    }
    ### NO2_00
    {
      RE_PR_M_data_NO2_00_RLM_C_5 <- as.data.frame(PR_M_data_NO2_00_RLM_C)
      RE_PR_M_data_NO2_00_RLM_C_5 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RLM_C_5$PR_M_data_NO2_00_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_00_RF_A_5 <- as.data.frame(PR_M_data_NO2_00_RF_A)
      RE_PR_M_data_NO2_00_RF_A_5 <- as.data.frame(cbind(RE_PR_M_data_NO2_00_RF_A_5$PR_M_data_NO2_00_RF_A,VA$X))
    }
    ### NO2_01
    {
      RE_PR_M_data_NO2_01_RLM_C_5 <- as.data.frame(PR_M_data_NO2_01_RLM_C)
      RE_PR_M_data_NO2_01_RLM_C_5 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RLM_C_5$PR_M_data_NO2_01_RLM_C,VA$X))
      
      RE_PR_M_data_NO2_01_RF_A_5 <- as.data.frame(PR_M_data_NO2_01_RF_A)
      RE_PR_M_data_NO2_01_RF_A_5 <- as.data.frame(cbind(RE_PR_M_data_NO2_01_RF_A_5$PR_M_data_NO2_01_RF_A,VA$X))
    }
  }
  
  
  # Calibration Evaluation ----------------------------------------------------
  ## Merging
  ### NO_00
  {RE_M_data_NO_00_RLM_C <- rbind(RE_PR_M_data_NO_00_RLM_C_1,
                                  RE_PR_M_data_NO_00_RLM_C_2,
                                  RE_PR_M_data_NO_00_RLM_C_3,
                                  RE_PR_M_data_NO_00_RLM_C_4,
                                  RE_PR_M_data_NO_00_RLM_C_5)
  
  RE_M_data_NO_00_RF_A <- rbind(RE_PR_M_data_NO_00_RF_A_1,
                                RE_PR_M_data_NO_00_RF_A_2,
                                RE_PR_M_data_NO_00_RF_A_3,
                                RE_PR_M_data_NO_00_RF_A_4,
                                RE_PR_M_data_NO_00_RF_A_5)
  }
  ### NO_01
  { RE_M_data_NO_01_RLM_C <- rbind(RE_PR_M_data_NO_01_RLM_C_1,
                                   RE_PR_M_data_NO_01_RLM_C_2,
                                   RE_PR_M_data_NO_01_RLM_C_3,
                                   RE_PR_M_data_NO_01_RLM_C_4,
                                   RE_PR_M_data_NO_01_RLM_C_5)
    
    RE_M_data_NO_01_RF_A <- rbind(RE_PR_M_data_NO_01_RF_A_1,
                                  RE_PR_M_data_NO_01_RF_A_2,
                                  RE_PR_M_data_NO_01_RF_A_3,
                                  RE_PR_M_data_NO_01_RF_A_4,
                                  RE_PR_M_data_NO_01_RF_A_5)
  }
  ### NO2_00
  { RE_M_data_NO2_00_RLM_C <- rbind(RE_PR_M_data_NO2_00_RLM_C_1,
                                    RE_PR_M_data_NO2_00_RLM_C_2,
                                    RE_PR_M_data_NO2_00_RLM_C_3,
                                    RE_PR_M_data_NO2_00_RLM_C_4,
                                    RE_PR_M_data_NO2_00_RLM_C_5)
    
    RE_M_data_NO2_00_RF_A <- rbind(RE_PR_M_data_NO2_00_RF_A_1,
                                   RE_PR_M_data_NO2_00_RF_A_2,
                                   RE_PR_M_data_NO2_00_RF_A_3,
                                   RE_PR_M_data_NO2_00_RF_A_4,
                                   RE_PR_M_data_NO2_00_RF_A_5)
  }
  ### NO2_01
  { RE_M_data_NO2_01_RLM_C <- rbind(RE_PR_M_data_NO2_01_RLM_C_1,
                                    RE_PR_M_data_NO2_01_RLM_C_2,
                                    RE_PR_M_data_NO2_01_RLM_C_3,
                                    RE_PR_M_data_NO2_01_RLM_C_4,
                                    RE_PR_M_data_NO2_01_RLM_C_5)
    
    RE_M_data_NO2_01_RF_A <- rbind(RE_PR_M_data_NO2_01_RF_A_1,
                                   RE_PR_M_data_NO2_01_RF_A_2,
                                   RE_PR_M_data_NO2_01_RF_A_3,
                                   RE_PR_M_data_NO2_01_RF_A_4,
                                   RE_PR_M_data_NO2_01_RF_A_5)
  }
  
  
  ## Sorting
  ### NO_00
  { RE_M_data_NO_00_RLM_C <- RE_M_data_NO_00_RLM_C[order(RE_M_data_NO_00_RLM_C$V2),]
    RE_M_data_NO_00_RF_A <- RE_M_data_NO_00_RF_A[order(RE_M_data_NO_00_RF_A$V2),]
    
    RE_M_data_NO_00_RLM_C$date <- data$date
    RE_M_data_NO_00_RF_A$date <- data$date
    
    RE_M_data_NO_00_RLM_C <- rename(RE_M_data_NO_00_RLM_C, "NO[ppb]"="V1")
    RE_M_data_NO_00_RF_A <- rename(RE_M_data_NO_00_RF_A, "NO[ppb]"="V1")
    
    setcolorder(RE_M_data_NO_00_RLM_C, neworder = c("date","NO[ppb]","V2"))
    setcolorder(RE_M_data_NO_00_RF_A, neworder = c("date","NO[ppb]","V2"))
    
    RE_M_data_NO_00_RLM_C <- as_tibble(RE_M_data_NO_00_RLM_C)
    RE_M_data_NO_00_RLM_C <- as_tbl_time(RE_M_data_NO_00_RLM_C, index = date)
    
    RE_M_data_NO_00_RF_A <- as_tibble(RE_M_data_NO_00_RF_A)
    RE_M_data_NO_00_RF_A <- as_tbl_time(RE_M_data_NO_00_RF_A, index = date)
  }
  ### NO_01
  { RE_M_data_NO_01_RLM_C <- RE_M_data_NO_01_RLM_C[order(RE_M_data_NO_01_RLM_C$V2),]
    RE_M_data_NO_01_RF_A <- RE_M_data_NO_01_RF_A[order(RE_M_data_NO_01_RF_A$V2),]
    
    RE_M_data_NO_01_RLM_C$date <- data$date
    RE_M_data_NO_01_RF_A$date <- data$date
    
    RE_M_data_NO_01_RLM_C <- rename(RE_M_data_NO_01_RLM_C, "NO[ppb]"="V1")
    RE_M_data_NO_01_RF_A <- rename(RE_M_data_NO_01_RF_A, "NO[ppb]"="V1")
    
    setcolorder(RE_M_data_NO_01_RLM_C, neworder = c("date","NO[ppb]","V2"))
    setcolorder(RE_M_data_NO_01_RF_A, neworder = c("date","NO[ppb]","V2"))
    
    RE_M_data_NO_01_RLM_C <- as_tibble(RE_M_data_NO_01_RLM_C)
    RE_M_data_NO_01_RLM_C <- as_tbl_time(RE_M_data_NO_01_RLM_C, index = date)
    
    RE_M_data_NO_01_RF_A <- as_tibble(RE_M_data_NO_01_RF_A)
    RE_M_data_NO_01_RF_A <- as_tbl_time(RE_M_data_NO_01_RF_A, index = date)
  }
  ### NO2_00
  { RE_M_data_NO2_00_RLM_C <- RE_M_data_NO2_00_RLM_C[order(RE_M_data_NO2_00_RLM_C$V2),]
    RE_M_data_NO2_00_RF_A <- RE_M_data_NO2_00_RF_A[order(RE_M_data_NO2_00_RF_A$V2),]
    
    RE_M_data_NO2_00_RLM_C$date <- data$date
    RE_M_data_NO2_00_RF_A$date <- data$date
    
    RE_M_data_NO2_00_RLM_C <- rename(RE_M_data_NO2_00_RLM_C, "NO2[ppb]"="V1")
    RE_M_data_NO2_00_RF_A <- rename(RE_M_data_NO2_00_RF_A, "NO2[ppb]"="V1")
    
    setcolorder(RE_M_data_NO2_00_RLM_C, neworder = c("date","NO2[ppb]","V2"))
    setcolorder(RE_M_data_NO2_00_RF_A, neworder = c("date","NO2[ppb]","V2"))
    
    RE_M_data_NO2_00_RLM_C <- as_tibble(RE_M_data_NO2_00_RLM_C)
    RE_M_data_NO2_00_RLM_C <- as_tbl_time(RE_M_data_NO2_00_RLM_C, index = date)
    
    RE_M_data_NO2_00_RF_A <- as_tibble(RE_M_data_NO2_00_RF_A)
    RE_M_data_NO2_00_RF_A <- as_tbl_time(RE_M_data_NO2_00_RF_A, index = date)
  }
  ### NO2_01
  { RE_M_data_NO2_01_RLM_C <- RE_M_data_NO2_01_RLM_C[order(RE_M_data_NO2_01_RLM_C$V2),]
    RE_M_data_NO2_01_RF_A <- RE_M_data_NO2_01_RF_A[order(RE_M_data_NO2_01_RF_A$V2),]
    
    RE_M_data_NO2_01_RLM_C$date <- data$date
    RE_M_data_NO2_01_RF_A$date <- data$date
    
    RE_M_data_NO2_01_RLM_C <- rename(RE_M_data_NO2_01_RLM_C, "NO2[ppb]"="V1")
    RE_M_data_NO2_01_RF_A <- rename(RE_M_data_NO2_01_RF_A, "NO2[ppb]"="V1")
    
    setcolorder(RE_M_data_NO2_01_RLM_C, neworder = c("date","NO2[ppb]","V2"))
    setcolorder(RE_M_data_NO2_01_RF_A, neworder = c("date","NO2[ppb]","V2"))
    
    RE_M_data_NO2_01_RLM_C <- as_tibble(RE_M_data_NO2_01_RLM_C)
    RE_M_data_NO2_01_RLM_C <- as_tbl_time(RE_M_data_NO2_01_RLM_C, index = date)
    
    RE_M_data_NO2_01_RF_A <- as_tibble(RE_M_data_NO2_01_RF_A)
    RE_M_data_NO2_01_RF_A <- as_tbl_time(RE_M_data_NO2_01_RF_A, index = date)
  }
  
  data_NO_00_RLM <- RE_M_data_NO_00_RLM_C
  data_NO_00_RF <- RE_M_data_NO_00_RF_A
  
  data_NO_01_RLM <- RE_M_data_NO_01_RLM_C
  data_NO_01_RF <- RE_M_data_NO_01_RF_A
  
  data_NO2_00_RLM <- RE_M_data_NO2_00_RLM_C
  data_NO2_00_RF <- RE_M_data_NO2_00_RF_A
  
  data_NO2_01_RLM <- RE_M_data_NO2_01_RLM_C
  data_NO2_01_RF <- RE_M_data_NO2_01_RF_A
  
  # save the data
  write.csv(data_NO_00_RLM, paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RLM.csv",sep = ""))
  write.csv(data_NO_00_RF, paste("data_output/calibrated_1st/",sensorunit,"_NO_00_RF.csv",sep = ""))
  write.csv(data_NO_01_RLM, paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RLM.csv",sep = ""))
  write.csv(data_NO_01_RF, paste("data_output/calibrated_1st/",sensorunit,"_NO_01_RF.csv",sep = ""))
  write.csv(data_NO2_00_RLM, paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RLM.csv",sep = ""))
  write.csv(data_NO2_00_RF, paste("data_output/calibrated_1st/",sensorunit,"_NO2_00_RF.csv",sep = ""))
  write.csv(data_NO2_01_RLM, paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RLM.csv",sep = ""))
  write.csv(data_NO2_01_RF, paste("data_output/calibrated_1st/",sensorunit,"_NO2_01_RF.csv",sep = ""))
}

kfoldcv("AC9")
kfoldcv("AC10")
kfoldcv("AC11")
kfoldcv("AC12")