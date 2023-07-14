# model_selection.R
#
# Objectives : This script evaluate the model candidates for sensor calibration.
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

# Calibration parameter -----------------------------------------------------

## Robust Regression
RLM_K_NO  = 1.345
RLM_K_NO2 = 1.345
RLM_MAXIT = 1e3

##Random Forest
nodesize <- 100
ntree <- 1000

# Calibration for model selection -------------------------------------------

AC9 <- "data_output/pre_processed_1st_period/AC9.csv"
AC10 <- "data_output/pre_processed_1st_period/AC10.csv"
AC11 <- "data_output/pre_processed_1st_period/AC11.csv"
AC12 <- "data_output/pre_processed_1st_period/AC12.csv"

model_selection <- function(data){
  
  ## Import the AC data -------------------------------------------------------
  data.im <- read.csv(data, header = TRUE)
  
  data.im$X <- seq(from = 1, to = length(data.im$date) ,by = 1)
  data.im$date <- ymd_hms(data.im$date)
  
  data.im <- as_tibble(data.im)
  
  data.im <- as_tbl_time(data.im, index = date)
  
  ## spit the training and testing data --------------------------------------
  set.seed(1234)
  rs <- createDataPartition(data.im$timestamp, p = 0.8, list = FALSE)
  TR_data.im <- data.im[rs,]
  VA_data.im <- data.im[-rs,]
  
  # Calibration models ------------------------------------------
  {
    ### NO_00 (35 models)
    {
      #### RLM (26 models)
      M_NO_00_RLM_1 <- rlm(REFNO~1+offset(NO_00),TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_2 <- rlm(REFNO~NO_00,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_3 <- rlm(REFNO~NO_00+T_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_4 <- rlm(REFNO~NO_00+RH_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_5 <- rlm(REFNO~NO_00+T_01+RH_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_6 <- rlm(REFNO~NO_00+T_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_7 <- rlm(REFNO~NO_00+T_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_8 <- rlm(REFNO~NO_00+T_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_9 <- rlm(REFNO~NO_00+T_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_10 <- rlm(REFNO~NO_00+RH_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_11 <- rlm(REFNO~NO_00+RH_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_12 <- rlm(REFNO~NO_00+RH_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_13 <- rlm(REFNO~NO_00+RH_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_14 <- rlm(REFNO~NO_00+T_01+RH_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_15 <- rlm(REFNO~NO_00+T_01+RH_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_16 <- rlm(REFNO~NO_00+T_01+RH_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_17 <- rlm(REFNO~NO_00+T_01+RH_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_18 <- rlm(REFNO~NO_00+T_01+NO_00*T_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_19 <- rlm(REFNO~NO_00+T_01+NO_00*T_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_20 <- rlm(REFNO~NO_00+T_01+NO_00*T_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_21 <- rlm(REFNO~NO_00+T_01+NO_00*T_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_22 <- rlm(REFNO~NO_00+T_01+NO_00*T_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_23 <- rlm(REFNO~NO_00+T_01+NO_00*T_01+dRH_01_T0_060+T_01^2+NO_00*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_24 <- rlm(REFNO~NO_00+T_01+NO_00*T_01+dRH_01_T0_090+T_01^2+NO_00*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_25 <- rlm(REFNO~NO_00+T_01+NO_00*T_01+dRH_01_T0_120+T_01^2+NO_00*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_00_RLM_26 <- rlm(REFNO~NO_00+T_01+NO_00*T_01+dRH_01_T0_150+T_01^2+NO_00*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      
      #### RF (9 models)
      M_NO_00_RF_1 <- randomForest(formula=as.formula("REFNO~NO_00"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_00_RF_2 <- randomForest(formula=as.formula("REFNO~NO_00+T_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_00_RF_3 <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_060"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_00_RF_4 <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_090"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_00_RF_5 <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_120"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_00_RF_6 <- randomForest(formula=as.formula("REFNO~NO_00+T_01+dRH_01_T0_150"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_00_RF_7 <- randomForest(formula=as.formula("REFNO~NO_00+RH_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_00_RF_8 <- randomForest(formula=as.formula("REFNO~NO_00+T_01+RH_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_00_RF_9 <- randomForest(formula=as.formula("REFNO~NO_00+T_01+NO_00*T_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
    }
    ### NO_01 (35 models)
    {
      #### RLM (26 models)
      M_NO_01_RLM_1 <- rlm(REFNO~1+offset(NO_01),TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_2 <- rlm(REFNO~NO_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_3 <- rlm(REFNO~NO_01+T_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_4 <- rlm(REFNO~NO_01+RH_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_5 <- rlm(REFNO~NO_01+T_01+RH_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_6 <- rlm(REFNO~NO_01+T_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_7 <- rlm(REFNO~NO_01+T_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_8 <- rlm(REFNO~NO_01+T_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_9 <- rlm(REFNO~NO_01+T_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_10 <- rlm(REFNO~NO_01+RH_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_11 <- rlm(REFNO~NO_01+RH_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_12 <- rlm(REFNO~NO_01+RH_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_13 <- rlm(REFNO~NO_01+RH_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_14 <- rlm(REFNO~NO_01+T_01+RH_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_15 <- rlm(REFNO~NO_01+T_01+RH_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_16 <- rlm(REFNO~NO_01+T_01+RH_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_17 <- rlm(REFNO~NO_01+T_01+RH_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_18 <- rlm(REFNO~NO_01+T_01+NO_01*T_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_19 <- rlm(REFNO~NO_01+T_01+NO_01*T_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_20 <- rlm(REFNO~NO_01+T_01+NO_01*T_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_21 <- rlm(REFNO~NO_01+T_01+NO_01*T_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_22 <- rlm(REFNO~NO_01+T_01+NO_01*T_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_23 <- rlm(REFNO~NO_01+T_01+NO_01*T_01+dRH_01_T0_060+T_01^2+NO_01*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_24 <- rlm(REFNO~NO_01+T_01+NO_01*T_01+dRH_01_T0_090+T_01^2+NO_01*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_25 <- rlm(REFNO~NO_01+T_01+NO_01*T_01+dRH_01_T0_120+T_01^2+NO_01*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      M_NO_01_RLM_26 <- rlm(REFNO~NO_01+T_01+NO_01*T_01+dRH_01_T0_150+T_01^2+NO_01*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO,maxit=RLM_MAXIT)
      
      #### RF (9 models)
      M_NO_01_RF_1 <- randomForest(formula=as.formula("REFNO~NO_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_01_RF_2 <- randomForest(formula=as.formula("REFNO~NO_01+T_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_01_RF_3 <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_060"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_01_RF_4 <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_090"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_01_RF_5 <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_120"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_01_RF_6 <- randomForest(formula=as.formula("REFNO~NO_01+T_01+dRH_01_T0_150"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_01_RF_7 <- randomForest(formula=as.formula("REFNO~NO_01+RH_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_01_RF_8 <- randomForest(formula=as.formula("REFNO~NO_01+T_01+RH_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO_01_RF_9 <- randomForest(formula=as.formula("REFNO~NO_01+T_01+NO_01*T_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      
    }
    ### NO2_00 (35 models)
    {
      #### RLM (26 models)
      M_NO2_00_RLM_1 <- rlm(REFNO2~1+offset(NO2_00),TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_2 <- rlm(REFNO2~NO2_00,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_3 <- rlm(REFNO2~NO2_00+T_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_4 <- rlm(REFNO2~NO2_00+RH_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_5 <- rlm(REFNO2~NO2_00+T_01+RH_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_6 <- rlm(REFNO2~NO2_00+T_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_7 <- rlm(REFNO2~NO2_00+T_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_8 <- rlm(REFNO2~NO2_00+T_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_9 <- rlm(REFNO2~NO2_00+T_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_10 <- rlm(REFNO2~NO2_00+RH_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_11 <- rlm(REFNO2~NO2_00+RH_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_12 <- rlm(REFNO2~NO2_00+RH_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_13 <- rlm(REFNO2~NO2_00+RH_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_14 <- rlm(REFNO2~NO2_00+T_01+RH_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_15 <- rlm(REFNO2~NO2_00+T_01+RH_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_16 <- rlm(REFNO2~NO2_00+T_01+RH_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_17 <- rlm(REFNO2~NO2_00+T_01+RH_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_18 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_19 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_20 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_21 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_22 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_23 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01+dRH_01_T0_060+T_01^2+NO2_00*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_24 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01+dRH_01_T0_090+T_01^2+NO2_00*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_25 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01+dRH_01_T0_120+T_01^2+NO2_00*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_00_RLM_26 <- rlm(REFNO2~NO2_00+T_01+NO2_00*T_01+dRH_01_T0_150+T_01^2+NO2_00*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      
      #### RF (9 models)
      M_NO2_00_RF_1 <- randomForest(formula=as.formula("REFNO2~NO2_00"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_00_RF_2 <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_00_RF_3 <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_060"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_00_RF_4 <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_090"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_00_RF_5 <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_120"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_00_RF_6 <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+dRH_01_T0_150"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_00_RF_7 <- randomForest(formula=as.formula("REFNO2~NO2_00+RH_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_00_RF_8 <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+RH_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_00_RF_9 <- randomForest(formula=as.formula("REFNO2~NO2_00+T_01+NO2_00*T_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
    }
    ### NO2_01 (35 models)
    {
      #### RLM (26 models)
      M_NO2_01_RLM_1 <- rlm(REFNO2~1+offset(NO2_01),TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_2 <- rlm(REFNO2~NO2_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_3 <- rlm(REFNO2~NO2_01+T_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_4 <- rlm(REFNO2~NO2_01+RH_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_5 <- rlm(REFNO2~NO2_01+T_01+RH_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_6 <- rlm(REFNO2~NO2_01+T_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_7 <- rlm(REFNO2~NO2_01+T_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_8 <- rlm(REFNO2~NO2_01+T_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_9 <- rlm(REFNO2~NO2_01+T_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_10 <- rlm(REFNO2~NO2_01+RH_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_11 <- rlm(REFNO2~NO2_01+RH_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_12 <- rlm(REFNO2~NO2_01+RH_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_13 <- rlm(REFNO2~NO2_01+RH_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_14 <- rlm(REFNO2~NO2_01+T_01+RH_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_15 <- rlm(REFNO2~NO2_01+T_01+RH_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_16 <- rlm(REFNO2~NO2_01+T_01+RH_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_17 <- rlm(REFNO2~NO2_01+T_01+RH_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_18 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_19 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01+dRH_01_T0_060,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_20 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01+dRH_01_T0_090,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_21 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01+dRH_01_T0_120,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_22 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01+dRH_01_T0_150,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_23 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01+dRH_01_T0_060+T_01^2+NO2_01*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_24 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01+dRH_01_T0_090+T_01^2+NO2_01*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_25 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01+dRH_01_T0_120+T_01^2+NO2_01*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      M_NO2_01_RLM_26 <- rlm(REFNO2~NO2_01+T_01+NO2_01*T_01+dRH_01_T0_150+T_01^2+NO2_01*T_01^2,TR_data.im,model=F,psi=psi.huber,k=RLM_K_NO2,maxit=RLM_MAXIT)
      
      #### RF (9 models)
      M_NO2_01_RF_1 <- randomForest(formula=as.formula("REFNO2~NO2_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_01_RF_2 <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_01_RF_3 <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_060"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_01_RF_4 <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_090"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_01_RF_5 <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_120"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_01_RF_6 <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+dRH_01_T0_150"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_01_RF_7 <- randomForest(formula=as.formula("REFNO2~NO2_01+RH_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_01_RF_8 <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+RH_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      M_NO2_01_RF_9 <- randomForest(formula=as.formula("REFNO2~NO2_01+T_01+NO2_01*T_01"),data=TR_data.im,ntree=ntree,nodesize=nodesize)
      
    }
  }
  # Prediction --------------------------------------------------
  {
    ### NO_00 (35 models)
    {
      #### RLM (26 models)
      PR_M_NO_00_RLM_1 <- predict(M_NO_00_RLM_1, VA_data.im)
      PR_M_NO_00_RLM_2 <- predict(M_NO_00_RLM_2, VA_data.im)
      PR_M_NO_00_RLM_3 <- predict(M_NO_00_RLM_3, VA_data.im)
      PR_M_NO_00_RLM_4 <- predict(M_NO_00_RLM_4, VA_data.im)
      PR_M_NO_00_RLM_5 <- predict(M_NO_00_RLM_5, VA_data.im)
      PR_M_NO_00_RLM_6 <- predict(M_NO_00_RLM_6, VA_data.im)
      PR_M_NO_00_RLM_7 <- predict(M_NO_00_RLM_7, VA_data.im)
      PR_M_NO_00_RLM_8 <- predict(M_NO_00_RLM_8, VA_data.im)
      PR_M_NO_00_RLM_9 <- predict(M_NO_00_RLM_9, VA_data.im)
      PR_M_NO_00_RLM_10 <- predict(M_NO_00_RLM_10, VA_data.im)
      PR_M_NO_00_RLM_11 <- predict(M_NO_00_RLM_11, VA_data.im)
      PR_M_NO_00_RLM_12 <- predict(M_NO_00_RLM_12, VA_data.im)
      PR_M_NO_00_RLM_13 <- predict(M_NO_00_RLM_13, VA_data.im)
      PR_M_NO_00_RLM_14 <- predict(M_NO_00_RLM_14, VA_data.im)
      PR_M_NO_00_RLM_15 <- predict(M_NO_00_RLM_15, VA_data.im)
      PR_M_NO_00_RLM_16 <- predict(M_NO_00_RLM_16, VA_data.im)
      PR_M_NO_00_RLM_17 <- predict(M_NO_00_RLM_17, VA_data.im)
      PR_M_NO_00_RLM_18 <- predict(M_NO_00_RLM_18, VA_data.im)
      PR_M_NO_00_RLM_19 <- predict(M_NO_00_RLM_19, VA_data.im)
      PR_M_NO_00_RLM_20 <- predict(M_NO_00_RLM_20, VA_data.im)
      PR_M_NO_00_RLM_21 <- predict(M_NO_00_RLM_21, VA_data.im)
      PR_M_NO_00_RLM_22 <- predict(M_NO_00_RLM_22, VA_data.im)
      PR_M_NO_00_RLM_23 <- predict(M_NO_00_RLM_23, VA_data.im)
      PR_M_NO_00_RLM_24 <- predict(M_NO_00_RLM_24, VA_data.im)
      PR_M_NO_00_RLM_25 <- predict(M_NO_00_RLM_25, VA_data.im)
      PR_M_NO_00_RLM_26 <- predict(M_NO_00_RLM_26, VA_data.im)
      
      #### RF (9 models)
      PR_M_NO_00_RF_1 <- predict(M_NO_00_RF_1, VA_data.im)
      PR_M_NO_00_RF_2 <- predict(M_NO_00_RF_2, VA_data.im)
      PR_M_NO_00_RF_3 <- predict(M_NO_00_RF_3, VA_data.im)
      PR_M_NO_00_RF_4 <- predict(M_NO_00_RF_4, VA_data.im)
      PR_M_NO_00_RF_5 <- predict(M_NO_00_RF_5, VA_data.im)
      PR_M_NO_00_RF_6 <- predict(M_NO_00_RF_6, VA_data.im)
      PR_M_NO_00_RF_7 <- predict(M_NO_00_RF_7, VA_data.im)
      PR_M_NO_00_RF_8 <- predict(M_NO_00_RF_8, VA_data.im)
      PR_M_NO_00_RF_9 <- predict(M_NO_00_RF_9, VA_data.im)
    }
    ### NO_01 (35 models)
    {
      #### RLM (26 models)
      PR_M_NO_01_RLM_1 <- predict(M_NO_01_RLM_1, VA_data.im)
      PR_M_NO_01_RLM_2 <- predict(M_NO_01_RLM_2, VA_data.im)
      PR_M_NO_01_RLM_3 <- predict(M_NO_01_RLM_3, VA_data.im)
      PR_M_NO_01_RLM_4 <- predict(M_NO_01_RLM_4, VA_data.im)
      PR_M_NO_01_RLM_5 <- predict(M_NO_01_RLM_5, VA_data.im)
      PR_M_NO_01_RLM_6 <- predict(M_NO_01_RLM_6, VA_data.im)
      PR_M_NO_01_RLM_7 <- predict(M_NO_01_RLM_7, VA_data.im)
      PR_M_NO_01_RLM_8 <- predict(M_NO_01_RLM_8, VA_data.im)
      PR_M_NO_01_RLM_9 <- predict(M_NO_01_RLM_9, VA_data.im)
      PR_M_NO_01_RLM_10 <- predict(M_NO_01_RLM_10, VA_data.im)
      PR_M_NO_01_RLM_11 <- predict(M_NO_01_RLM_11, VA_data.im)
      PR_M_NO_01_RLM_12 <- predict(M_NO_01_RLM_12, VA_data.im)
      PR_M_NO_01_RLM_13 <- predict(M_NO_01_RLM_13, VA_data.im)
      PR_M_NO_01_RLM_14 <- predict(M_NO_01_RLM_14, VA_data.im)
      PR_M_NO_01_RLM_15 <- predict(M_NO_01_RLM_15, VA_data.im)
      PR_M_NO_01_RLM_16 <- predict(M_NO_01_RLM_16, VA_data.im)
      PR_M_NO_01_RLM_17 <- predict(M_NO_01_RLM_17, VA_data.im)
      PR_M_NO_01_RLM_18 <- predict(M_NO_01_RLM_18, VA_data.im)
      PR_M_NO_01_RLM_19 <- predict(M_NO_01_RLM_19, VA_data.im)
      PR_M_NO_01_RLM_20 <- predict(M_NO_01_RLM_20, VA_data.im)
      PR_M_NO_01_RLM_21 <- predict(M_NO_01_RLM_21, VA_data.im)
      PR_M_NO_01_RLM_22 <- predict(M_NO_01_RLM_22, VA_data.im)
      PR_M_NO_01_RLM_23 <- predict(M_NO_01_RLM_23, VA_data.im)
      PR_M_NO_01_RLM_24 <- predict(M_NO_01_RLM_24, VA_data.im)
      PR_M_NO_01_RLM_25 <- predict(M_NO_01_RLM_25, VA_data.im)
      PR_M_NO_01_RLM_26 <- predict(M_NO_01_RLM_26, VA_data.im)
      
      #### RF (9 models)
      PR_M_NO_01_RF_1 <- predict(M_NO_01_RF_1, VA_data.im)
      PR_M_NO_01_RF_2 <- predict(M_NO_01_RF_2, VA_data.im)
      PR_M_NO_01_RF_3 <- predict(M_NO_01_RF_3, VA_data.im)
      PR_M_NO_01_RF_4 <- predict(M_NO_01_RF_4, VA_data.im)
      PR_M_NO_01_RF_5 <- predict(M_NO_01_RF_5, VA_data.im)
      PR_M_NO_01_RF_6 <- predict(M_NO_01_RF_6, VA_data.im)
      PR_M_NO_01_RF_7 <- predict(M_NO_01_RF_7, VA_data.im)
      PR_M_NO_01_RF_8 <- predict(M_NO_01_RF_8, VA_data.im)
      PR_M_NO_01_RF_9 <- predict(M_NO_01_RF_9, VA_data.im)
    }
    ### NO2_00 (35 models)
    {
      #### RLM (26 models)
      PR_M_NO2_00_RLM_1 <- predict(M_NO2_00_RLM_1, VA_data.im)
      PR_M_NO2_00_RLM_2 <- predict(M_NO2_00_RLM_2, VA_data.im)
      PR_M_NO2_00_RLM_3 <- predict(M_NO2_00_RLM_3, VA_data.im)
      PR_M_NO2_00_RLM_4 <- predict(M_NO2_00_RLM_4, VA_data.im)
      PR_M_NO2_00_RLM_5 <- predict(M_NO2_00_RLM_5, VA_data.im)
      PR_M_NO2_00_RLM_6 <- predict(M_NO2_00_RLM_6, VA_data.im)
      PR_M_NO2_00_RLM_7 <- predict(M_NO2_00_RLM_7, VA_data.im)
      PR_M_NO2_00_RLM_8 <- predict(M_NO2_00_RLM_8, VA_data.im)
      PR_M_NO2_00_RLM_9 <- predict(M_NO2_00_RLM_9, VA_data.im)
      PR_M_NO2_00_RLM_10 <- predict(M_NO2_00_RLM_10, VA_data.im)
      PR_M_NO2_00_RLM_11 <- predict(M_NO2_00_RLM_11, VA_data.im)
      PR_M_NO2_00_RLM_12 <- predict(M_NO2_00_RLM_12, VA_data.im)
      PR_M_NO2_00_RLM_13 <- predict(M_NO2_00_RLM_13, VA_data.im)
      PR_M_NO2_00_RLM_14 <- predict(M_NO2_00_RLM_14, VA_data.im)
      PR_M_NO2_00_RLM_15 <- predict(M_NO2_00_RLM_15, VA_data.im)
      PR_M_NO2_00_RLM_16 <- predict(M_NO2_00_RLM_16, VA_data.im)
      PR_M_NO2_00_RLM_17 <- predict(M_NO2_00_RLM_17, VA_data.im)
      PR_M_NO2_00_RLM_18 <- predict(M_NO2_00_RLM_18, VA_data.im)
      PR_M_NO2_00_RLM_19 <- predict(M_NO2_00_RLM_19, VA_data.im)
      PR_M_NO2_00_RLM_20 <- predict(M_NO2_00_RLM_20, VA_data.im)
      PR_M_NO2_00_RLM_21 <- predict(M_NO2_00_RLM_21, VA_data.im)
      PR_M_NO2_00_RLM_22 <- predict(M_NO2_00_RLM_22, VA_data.im)
      PR_M_NO2_00_RLM_23 <- predict(M_NO2_00_RLM_23, VA_data.im)
      PR_M_NO2_00_RLM_24 <- predict(M_NO2_00_RLM_24, VA_data.im)
      PR_M_NO2_00_RLM_25 <- predict(M_NO2_00_RLM_25, VA_data.im)
      PR_M_NO2_00_RLM_26 <- predict(M_NO2_00_RLM_26, VA_data.im)
      
      #### RF (9 models)
      PR_M_NO2_00_RF_1 <- predict(M_NO2_00_RF_1, VA_data.im)
      PR_M_NO2_00_RF_2 <- predict(M_NO2_00_RF_2, VA_data.im)
      PR_M_NO2_00_RF_3 <- predict(M_NO2_00_RF_3, VA_data.im)
      PR_M_NO2_00_RF_4 <- predict(M_NO2_00_RF_4, VA_data.im)
      PR_M_NO2_00_RF_5 <- predict(M_NO2_00_RF_5, VA_data.im)
      PR_M_NO2_00_RF_6 <- predict(M_NO2_00_RF_6, VA_data.im)
      PR_M_NO2_00_RF_7 <- predict(M_NO2_00_RF_7, VA_data.im)
      PR_M_NO2_00_RF_8 <- predict(M_NO2_00_RF_8, VA_data.im)
      PR_M_NO2_00_RF_9 <- predict(M_NO2_00_RF_9, VA_data.im)
    }
    ### NO2_01 (35 models)
    {
      #### RLM (26 models)
      PR_M_NO2_01_RLM_1 <- predict(M_NO2_01_RLM_1, VA_data.im)
      PR_M_NO2_01_RLM_2 <- predict(M_NO2_01_RLM_2, VA_data.im)
      PR_M_NO2_01_RLM_3 <- predict(M_NO2_01_RLM_3, VA_data.im)
      PR_M_NO2_01_RLM_4 <- predict(M_NO2_01_RLM_4, VA_data.im)
      PR_M_NO2_01_RLM_5 <- predict(M_NO2_01_RLM_5, VA_data.im)
      PR_M_NO2_01_RLM_6 <- predict(M_NO2_01_RLM_6, VA_data.im)
      PR_M_NO2_01_RLM_7 <- predict(M_NO2_01_RLM_7, VA_data.im)
      PR_M_NO2_01_RLM_8 <- predict(M_NO2_01_RLM_8, VA_data.im)
      PR_M_NO2_01_RLM_9 <- predict(M_NO2_01_RLM_9, VA_data.im)
      PR_M_NO2_01_RLM_10 <- predict(M_NO2_01_RLM_10, VA_data.im)
      PR_M_NO2_01_RLM_11 <- predict(M_NO2_01_RLM_11, VA_data.im)
      PR_M_NO2_01_RLM_12 <- predict(M_NO2_01_RLM_12, VA_data.im)
      PR_M_NO2_01_RLM_13 <- predict(M_NO2_01_RLM_13, VA_data.im)
      PR_M_NO2_01_RLM_14 <- predict(M_NO2_01_RLM_14, VA_data.im)
      PR_M_NO2_01_RLM_15 <- predict(M_NO2_01_RLM_15, VA_data.im)
      PR_M_NO2_01_RLM_16 <- predict(M_NO2_01_RLM_16, VA_data.im)
      PR_M_NO2_01_RLM_17 <- predict(M_NO2_01_RLM_17, VA_data.im)
      PR_M_NO2_01_RLM_18 <- predict(M_NO2_01_RLM_18, VA_data.im)
      PR_M_NO2_01_RLM_19 <- predict(M_NO2_01_RLM_19, VA_data.im)
      PR_M_NO2_01_RLM_20 <- predict(M_NO2_01_RLM_20, VA_data.im)
      PR_M_NO2_01_RLM_21 <- predict(M_NO2_01_RLM_21, VA_data.im)
      PR_M_NO2_01_RLM_22 <- predict(M_NO2_01_RLM_22, VA_data.im)
      PR_M_NO2_01_RLM_23 <- predict(M_NO2_01_RLM_23, VA_data.im)
      PR_M_NO2_01_RLM_24 <- predict(M_NO2_01_RLM_24, VA_data.im)
      PR_M_NO2_01_RLM_25 <- predict(M_NO2_01_RLM_25, VA_data.im)
      PR_M_NO2_01_RLM_26 <- predict(M_NO2_01_RLM_26, VA_data.im)
      
      #### RF (9 models)
      PR_M_NO2_01_RF_1 <- predict(M_NO2_01_RF_1, VA_data.im)
      PR_M_NO2_01_RF_2 <- predict(M_NO2_01_RF_2, VA_data.im)
      PR_M_NO2_01_RF_3 <- predict(M_NO2_01_RF_3, VA_data.im)
      PR_M_NO2_01_RF_4 <- predict(M_NO2_01_RF_4, VA_data.im)
      PR_M_NO2_01_RF_5 <- predict(M_NO2_01_RF_5, VA_data.im)
      PR_M_NO2_01_RF_6 <- predict(M_NO2_01_RF_6, VA_data.im)
      PR_M_NO2_01_RF_7 <- predict(M_NO2_01_RF_7, VA_data.im)
      PR_M_NO2_01_RF_8 <- predict(M_NO2_01_RF_8, VA_data.im)
      PR_M_NO2_01_RF_9 <- predict(M_NO2_01_RF_9, VA_data.im)
    }
  }
  # Calibration Evaluation ----------------------------------------------------
  ## NO_00
  {
    ### RLM
    td_NO_00_RLM_1 <- tdStats(PR_M_NO_00_RLM_1, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_2 <- tdStats(PR_M_NO_00_RLM_2, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_3 <- tdStats(PR_M_NO_00_RLM_3, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_4 <- tdStats(PR_M_NO_00_RLM_4, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_5 <- tdStats(PR_M_NO_00_RLM_5, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_6 <- tdStats(PR_M_NO_00_RLM_6, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_7 <- tdStats(PR_M_NO_00_RLM_7, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_8 <- tdStats(PR_M_NO_00_RLM_8, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_9 <- tdStats(PR_M_NO_00_RLM_9, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_10 <- tdStats(PR_M_NO_00_RLM_10, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_11 <- tdStats(PR_M_NO_00_RLM_11, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_12 <- tdStats(PR_M_NO_00_RLM_12, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_13 <- tdStats(PR_M_NO_00_RLM_13, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_14 <- tdStats(PR_M_NO_00_RLM_14, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_15 <- tdStats(PR_M_NO_00_RLM_15, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_16 <- tdStats(PR_M_NO_00_RLM_16, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_17 <- tdStats(PR_M_NO_00_RLM_17, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_18 <- tdStats(PR_M_NO_00_RLM_18, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_19 <- tdStats(PR_M_NO_00_RLM_19, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_20 <- tdStats(PR_M_NO_00_RLM_20, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_21 <- tdStats(PR_M_NO_00_RLM_21, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_22 <- tdStats(PR_M_NO_00_RLM_22, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_23 <- tdStats(PR_M_NO_00_RLM_23, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_24 <- tdStats(PR_M_NO_00_RLM_24, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_25 <- tdStats(PR_M_NO_00_RLM_25, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RLM_26 <- tdStats(PR_M_NO_00_RLM_26, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    ###RF
    td_NO_00_RF_1 <- tdStats(PR_M_NO_00_RF_1, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RF_2 <- tdStats(PR_M_NO_00_RF_2, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RF_3 <- tdStats(PR_M_NO_00_RF_3, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RF_4 <- tdStats(PR_M_NO_00_RF_4, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RF_5 <- tdStats(PR_M_NO_00_RF_5, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RF_6 <- tdStats(PR_M_NO_00_RF_6, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RF_7 <- tdStats(PR_M_NO_00_RF_7, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RF_8 <- tdStats(PR_M_NO_00_RF_8, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_00_RF_9 <- tdStats(PR_M_NO_00_RF_9, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    
    
    td_NO_00 <- data.frame(rbind(td_NO_00_RLM_1,
                                 td_NO_00_RLM_2,
                                 td_NO_00_RLM_3,
                                 td_NO_00_RLM_4,
                                 td_NO_00_RLM_5,
                                 td_NO_00_RLM_6,
                                 td_NO_00_RLM_7,
                                 td_NO_00_RLM_8,
                                 td_NO_00_RLM_9,
                                 td_NO_00_RLM_10,
                                 td_NO_00_RLM_11,
                                 td_NO_00_RLM_12,
                                 td_NO_00_RLM_13,
                                 td_NO_00_RLM_14,
                                 td_NO_00_RLM_15,
                                 td_NO_00_RLM_16,
                                 td_NO_00_RLM_17,
                                 td_NO_00_RLM_18,
                                 td_NO_00_RLM_19,
                                 td_NO_00_RLM_20,
                                 td_NO_00_RLM_21,
                                 td_NO_00_RLM_22,
                                 td_NO_00_RLM_23,
                                 td_NO_00_RLM_24,
                                 td_NO_00_RLM_25,
                                 td_NO_00_RLM_26,
                                 td_NO_00_RF_1,
                                 td_NO_00_RF_2,
                                 td_NO_00_RF_3,
                                 td_NO_00_RF_4,
                                 td_NO_00_RF_5,
                                 td_NO_00_RF_6,
                                 td_NO_00_RF_7,
                                 td_NO_00_RF_8,
                                 td_NO_00_RF_9
    ))
    
  }
  ## NO_01
  {
    ### RLM
    td_NO_01_RLM_1 <- tdStats(PR_M_NO_01_RLM_1, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_2 <- tdStats(PR_M_NO_01_RLM_2, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_3 <- tdStats(PR_M_NO_01_RLM_3, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_4 <- tdStats(PR_M_NO_01_RLM_4, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_5 <- tdStats(PR_M_NO_01_RLM_5, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_6 <- tdStats(PR_M_NO_01_RLM_6, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_7 <- tdStats(PR_M_NO_01_RLM_7, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_8 <- tdStats(PR_M_NO_01_RLM_8, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_9 <- tdStats(PR_M_NO_01_RLM_9, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_10 <- tdStats(PR_M_NO_01_RLM_10, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_11 <- tdStats(PR_M_NO_01_RLM_11, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_12 <- tdStats(PR_M_NO_01_RLM_12, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_13 <- tdStats(PR_M_NO_01_RLM_13, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_14 <- tdStats(PR_M_NO_01_RLM_14, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_15 <- tdStats(PR_M_NO_01_RLM_15, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_16 <- tdStats(PR_M_NO_01_RLM_16, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_17 <- tdStats(PR_M_NO_01_RLM_17, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_18 <- tdStats(PR_M_NO_01_RLM_18, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_19 <- tdStats(PR_M_NO_01_RLM_19, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_20 <- tdStats(PR_M_NO_01_RLM_20, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_21 <- tdStats(PR_M_NO_01_RLM_21, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_22 <- tdStats(PR_M_NO_01_RLM_22, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_23 <- tdStats(PR_M_NO_01_RLM_23, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_24 <- tdStats(PR_M_NO_01_RLM_24, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_25 <- tdStats(PR_M_NO_01_RLM_25, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RLM_26 <- tdStats(PR_M_NO_01_RLM_26, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    ###RF
    td_NO_01_RF_1 <- tdStats(PR_M_NO_01_RF_1, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RF_2 <- tdStats(PR_M_NO_01_RF_2, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RF_3 <- tdStats(PR_M_NO_01_RF_3, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RF_4 <- tdStats(PR_M_NO_01_RF_4, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RF_5 <- tdStats(PR_M_NO_01_RF_5, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RF_6 <- tdStats(PR_M_NO_01_RF_6, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RF_7 <- tdStats(PR_M_NO_01_RF_7, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RF_8 <- tdStats(PR_M_NO_01_RF_8, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO_01_RF_9 <- tdStats(PR_M_NO_01_RF_9, VA_data.im$REFNO, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    
    
    td_NO_01 <- data.frame(rbind(td_NO_01_RLM_1,
                                 td_NO_01_RLM_2,
                                 td_NO_01_RLM_3,
                                 td_NO_01_RLM_4,
                                 td_NO_01_RLM_5,
                                 td_NO_01_RLM_6,
                                 td_NO_01_RLM_7,
                                 td_NO_01_RLM_8,
                                 td_NO_01_RLM_9,
                                 td_NO_01_RLM_10,
                                 td_NO_01_RLM_11,
                                 td_NO_01_RLM_12,
                                 td_NO_01_RLM_13,
                                 td_NO_01_RLM_14,
                                 td_NO_01_RLM_15,
                                 td_NO_01_RLM_16,
                                 td_NO_01_RLM_17,
                                 td_NO_01_RLM_18,
                                 td_NO_01_RLM_19,
                                 td_NO_01_RLM_20,
                                 td_NO_01_RLM_21,
                                 td_NO_01_RLM_22,
                                 td_NO_01_RLM_23,
                                 td_NO_01_RLM_24,
                                 td_NO_01_RLM_25,
                                 td_NO_01_RLM_26,
                                 td_NO_01_RF_1,
                                 td_NO_01_RF_2,
                                 td_NO_01_RF_3,
                                 td_NO_01_RF_4,
                                 td_NO_01_RF_5,
                                 td_NO_01_RF_6,
                                 td_NO_01_RF_7,
                                 td_NO_01_RF_8,
                                 td_NO_01_RF_9
    ))
    
  }
  ## NO2_00
  {
    ### RLM
    td_NO2_00_RLM_1 <- tdStats(PR_M_NO2_00_RLM_1, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_2 <- tdStats(PR_M_NO2_00_RLM_2, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_3 <- tdStats(PR_M_NO2_00_RLM_3, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_4 <- tdStats(PR_M_NO2_00_RLM_4, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_5 <- tdStats(PR_M_NO2_00_RLM_5, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_6 <- tdStats(PR_M_NO2_00_RLM_6, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_7 <- tdStats(PR_M_NO2_00_RLM_7, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_8 <- tdStats(PR_M_NO2_00_RLM_8, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_9 <- tdStats(PR_M_NO2_00_RLM_9, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_10 <- tdStats(PR_M_NO2_00_RLM_10, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_11 <- tdStats(PR_M_NO2_00_RLM_11, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_12 <- tdStats(PR_M_NO2_00_RLM_12, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_13 <- tdStats(PR_M_NO2_00_RLM_13, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_14 <- tdStats(PR_M_NO2_00_RLM_14, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_15 <- tdStats(PR_M_NO2_00_RLM_15, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_16 <- tdStats(PR_M_NO2_00_RLM_16, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_17 <- tdStats(PR_M_NO2_00_RLM_17, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_18 <- tdStats(PR_M_NO2_00_RLM_18, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_19 <- tdStats(PR_M_NO2_00_RLM_19, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_20 <- tdStats(PR_M_NO2_00_RLM_20, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_21 <- tdStats(PR_M_NO2_00_RLM_21, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_22 <- tdStats(PR_M_NO2_00_RLM_22, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_23 <- tdStats(PR_M_NO2_00_RLM_23, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_24 <- tdStats(PR_M_NO2_00_RLM_24, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_25 <- tdStats(PR_M_NO2_00_RLM_25, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RLM_26 <- tdStats(PR_M_NO2_00_RLM_26, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    ###RF
    td_NO2_00_RF_1 <- tdStats(PR_M_NO2_00_RF_1, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RF_2 <- tdStats(PR_M_NO2_00_RF_2, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RF_3 <- tdStats(PR_M_NO2_00_RF_3, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RF_4 <- tdStats(PR_M_NO2_00_RF_4, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RF_5 <- tdStats(PR_M_NO2_00_RF_5, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RF_6 <- tdStats(PR_M_NO2_00_RF_6, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RF_7 <- tdStats(PR_M_NO2_00_RF_7, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RF_8 <- tdStats(PR_M_NO2_00_RF_8, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_00_RF_9 <- tdStats(PR_M_NO2_00_RF_9, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    
    
    td_NO2_00 <- data.frame(rbind(td_NO2_00_RLM_1,
                                  td_NO2_00_RLM_2,
                                  td_NO2_00_RLM_3,
                                  td_NO2_00_RLM_4,
                                  td_NO2_00_RLM_5,
                                  td_NO2_00_RLM_6,
                                  td_NO2_00_RLM_7,
                                  td_NO2_00_RLM_8,
                                  td_NO2_00_RLM_9,
                                  td_NO2_00_RLM_10,
                                  td_NO2_00_RLM_11,
                                  td_NO2_00_RLM_12,
                                  td_NO2_00_RLM_13,
                                  td_NO2_00_RLM_14,
                                  td_NO2_00_RLM_15,
                                  td_NO2_00_RLM_16,
                                  td_NO2_00_RLM_17,
                                  td_NO2_00_RLM_18,
                                  td_NO2_00_RLM_19,
                                  td_NO2_00_RLM_20,
                                  td_NO2_00_RLM_21,
                                  td_NO2_00_RLM_22,
                                  td_NO2_00_RLM_23,
                                  td_NO2_00_RLM_24,
                                  td_NO2_00_RLM_25,
                                  td_NO2_00_RLM_26,
                                  td_NO2_00_RF_1,
                                  td_NO2_00_RF_2,
                                  td_NO2_00_RF_3,
                                  td_NO2_00_RF_4,
                                  td_NO2_00_RF_5,
                                  td_NO2_00_RF_6,
                                  td_NO2_00_RF_7,
                                  td_NO2_00_RF_8,
                                  td_NO2_00_RF_9
    ))
    
  }
  ## NO2_01
  {
    ### RLM
    td_NO2_01_RLM_1 <- tdStats(PR_M_NO2_01_RLM_1, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_2 <- tdStats(PR_M_NO2_01_RLM_2, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_3 <- tdStats(PR_M_NO2_01_RLM_3, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_4 <- tdStats(PR_M_NO2_01_RLM_4, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_5 <- tdStats(PR_M_NO2_01_RLM_5, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_6 <- tdStats(PR_M_NO2_01_RLM_6, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_7 <- tdStats(PR_M_NO2_01_RLM_7, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_8 <- tdStats(PR_M_NO2_01_RLM_8, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_9 <- tdStats(PR_M_NO2_01_RLM_9, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_10 <- tdStats(PR_M_NO2_01_RLM_10, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_11 <- tdStats(PR_M_NO2_01_RLM_11, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_12 <- tdStats(PR_M_NO2_01_RLM_12, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_13 <- tdStats(PR_M_NO2_01_RLM_13, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_14 <- tdStats(PR_M_NO2_01_RLM_14, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_15 <- tdStats(PR_M_NO2_01_RLM_15, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_16 <- tdStats(PR_M_NO2_01_RLM_16, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_17 <- tdStats(PR_M_NO2_01_RLM_17, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_18 <- tdStats(PR_M_NO2_01_RLM_18, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_19 <- tdStats(PR_M_NO2_01_RLM_19, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_20 <- tdStats(PR_M_NO2_01_RLM_20, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_21 <- tdStats(PR_M_NO2_01_RLM_21, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_22 <- tdStats(PR_M_NO2_01_RLM_22, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_23 <- tdStats(PR_M_NO2_01_RLM_23, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_24 <- tdStats(PR_M_NO2_01_RLM_24, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_25 <- tdStats(PR_M_NO2_01_RLM_25, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RLM_26 <- tdStats(PR_M_NO2_01_RLM_26, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    ###RF
    td_NO2_01_RF_1 <- tdStats(PR_M_NO2_01_RF_1, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RF_2 <- tdStats(PR_M_NO2_01_RF_2, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RF_3 <- tdStats(PR_M_NO2_01_RF_3, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RF_4 <- tdStats(PR_M_NO2_01_RF_4, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RF_5 <- tdStats(PR_M_NO2_01_RF_5, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RF_6 <- tdStats(PR_M_NO2_01_RF_6, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RF_7 <- tdStats(PR_M_NO2_01_RF_7, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RF_8 <- tdStats(PR_M_NO2_01_RF_8, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    td_NO2_01_RF_9 <- tdStats(PR_M_NO2_01_RF_9, VA_data.im$REFNO2, functions = c("rmse", "mbe", "sdm", "sdo","r2","nrmse"))
    
    
    td_NO2_01 <- data.frame(rbind(td_NO2_01_RLM_1,
                                  td_NO2_01_RLM_2,
                                  td_NO2_01_RLM_3,
                                  td_NO2_01_RLM_4,
                                  td_NO2_01_RLM_5,
                                  td_NO2_01_RLM_6,
                                  td_NO2_01_RLM_7,
                                  td_NO2_01_RLM_8,
                                  td_NO2_01_RLM_9,
                                  td_NO2_01_RLM_10,
                                  td_NO2_01_RLM_11,
                                  td_NO2_01_RLM_12,
                                  td_NO2_01_RLM_13,
                                  td_NO2_01_RLM_14,
                                  td_NO2_01_RLM_15,
                                  td_NO2_01_RLM_16,
                                  td_NO2_01_RLM_17,
                                  td_NO2_01_RLM_18,
                                  td_NO2_01_RLM_19,
                                  td_NO2_01_RLM_20,
                                  td_NO2_01_RLM_21,
                                  td_NO2_01_RLM_22,
                                  td_NO2_01_RLM_23,
                                  td_NO2_01_RLM_24,
                                  td_NO2_01_RLM_25,
                                  td_NO2_01_RLM_26,
                                  td_NO2_01_RF_1,
                                  td_NO2_01_RF_2,
                                  td_NO2_01_RF_3,
                                  td_NO2_01_RF_4,
                                  td_NO2_01_RF_5,
                                  td_NO2_01_RF_6,
                                  td_NO2_01_RF_7,
                                  td_NO2_01_RF_8,
                                  td_NO2_01_RF_9
    ))
    
  }
  
  # nRMSE -------------------------------------------------------------------
  td_NO_00$x <- sign((td_NO_00$sdm)-(td_NO_00$sdo))/(td_NO_00$sdo)*sqrt((td_NO_00$rmse)^2-(td_NO_00$mbe)^2)
  td_NO_00$y <- (td_NO_00$mbe)/(td_NO_00$sdo)
  td_NO_00$nrmse <- sqrt((td_NO_00$x)^2+(td_NO_00$y)^2)
  
  td_NO_01$x <- sign((td_NO_01$sdm)-(td_NO_01$sdo))/(td_NO_01$sdo)*sqrt((td_NO_01$rmse)^2-(td_NO_01$mbe)^2)
  td_NO_01$y <- (td_NO_01$mbe)/(td_NO_01$sdo)
  td_NO_01$nrmse <- sqrt((td_NO_01$x)^2+(td_NO_01$y)^2)
  
  td_NO2_00$x <- sign((td_NO2_00$sdm)-(td_NO2_00$sdo))/(td_NO2_00$sdo)*sqrt((td_NO2_00$rmse)^2-(td_NO2_00$mbe)^2)
  td_NO2_00$y <- (td_NO2_00$mbe)/(td_NO2_00$sdo)
  td_NO2_00$nrmse <- sqrt((td_NO2_00$x)^2+(td_NO2_00$y)^2)
  
  td_NO2_01$x <- sign((td_NO2_01$sdm)-(td_NO2_01$sdo))/(td_NO2_01$sdo)*sqrt((td_NO2_01$rmse)^2-(td_NO2_01$mbe)^2)
  td_NO2_01$y <- (td_NO2_01$mbe)/(td_NO2_01$sdo)
  td_NO2_01$nrmse <- sqrt((td_NO2_01$x)^2+(td_NO2_01$y)^2)
  
  td_all <- as.data.frame(rbind(td_NO_00,td_NO_01,td_NO2_00,td_NO2_01))
  
  return(td_all)
}

metrics_AC9 <- as.data.frame(model_selection(AC9))

metrics_AC10<- as.data.frame(model_selection(AC10))

metrics_AC11 <- as.data.frame(model_selection(AC11))

metrics_AC12<- as.data.frame(model_selection(AC12))


# merged nRMSE and R2 -------------------------------
## nRMSE
nrmse_total <- data.frame(matrix(0,35,17))
nrmse_total <- rename(nrmse_total, "Model"="X1","AC9_NO_00" = "X2", "AC9_NO_01" = "X3","AC9_NO2_00"="X4", "AC9_NO2_01"="X5",
       "AC10_NO_00" = "X6", "AC10_NO_01" = "X7","AC10_NO2_00"="X8", "AC10_NO2_01"="X9",
       "AC11_NO_00" = "X10", "AC11_NO_01" = "X11","AC11_NO2_00"="X12", "AC11_NO2_01"="X13",
       "AC12_NO_00" = "X14", "AC12_NO_01" = "X15","AC12_NO2_00"="X16", "AC12_NO2_01"="X17")
for (i in 1:26)
{
  nrmse_total$Model[i] <- paste("RLM_",i)
}

for (j in 27:35)
{
  nrmse_total$Model[j] <- paste("RF_",j-26)
}

nrmse_total$AC9_NO_00 <- round(metrics_AC9$nrmse[1:35], 3)
nrmse_total$AC9_NO_01 <- round(metrics_AC9$nrmse[36:70], 3)
nrmse_total$AC9_NO2_00 <- round(metrics_AC9$nrmse[71:105], 3)
nrmse_total$AC9_NO2_01 <- round(metrics_AC9$nrmse[106:140], 3)
nrmse_total$AC10_NO_00 <- round(metrics_AC10$nrmse[1:35], 3)
nrmse_total$AC10_NO_01 <- round(metrics_AC10$nrmse[36:70], 3)
nrmse_total$AC10_NO2_00 <- round(metrics_AC10$nrmse[71:105], 3)
nrmse_total$AC10_NO2_01 <- round(metrics_AC10$nrmse[106:140], 3)
nrmse_total$AC11_NO_00 <- round(metrics_AC11$nrmse[1:35], 3)
nrmse_total$AC11_NO_01 <- round(metrics_AC11$nrmse[36:70], 3)
nrmse_total$AC11_NO2_00 <- round(metrics_AC11$nrmse[71:105], 3)
nrmse_total$AC11_NO2_01 <- round(metrics_AC11$nrmse[106:140], 3)
nrmse_total$AC12_NO_00 <- round(metrics_AC12$nrmse[1:35], 3)
nrmse_total$AC12_NO_01 <- round(metrics_AC12$nrmse[36:70], 3)
nrmse_total$AC12_NO2_00 <- round(metrics_AC12$nrmse[71:105], 3)
nrmse_total$AC12_NO2_01 <- round(metrics_AC12$nrmse[106:140], 3)

# R2
r2_total <- data.frame(matrix(0,35,17))
r2_total <- rename(r2_total, "Model"="X1","AC9_NO_00" = "X2", "AC9_NO_01" = "X3","AC9_NO2_00"="X4", "AC9_NO2_01"="X5",
                      "AC10_NO_00" = "X6", "AC10_NO_01" = "X7","AC10_NO2_00"="X8", "AC10_NO2_01"="X9",
                      "AC11_NO_00" = "X10", "AC11_NO_01" = "X11","AC11_NO2_00"="X12", "AC11_NO2_01"="X13",
                      "AC12_NO_00" = "X14", "AC12_NO_01" = "X15","AC12_NO2_00"="X16", "AC12_NO2_01"="X17")
for (i in 1:26)
{
  r2_total$Model[i] <- paste("RLM_",i)
}

for (j in 27:35)
{
  r2_total$Model[j] <- paste("RF_",j-26)
}

r2_total$AC9_NO_00 <- round(metrics_AC9$r2[1:35], 3)
r2_total$AC9_NO_01 <- round(metrics_AC9$r2[36:70], 3)
r2_total$AC9_NO2_00 <- round(metrics_AC9$r2[71:105], 3)
r2_total$AC9_NO2_01 <- round(metrics_AC9$r2[106:140], 3)
r2_total$AC10_NO_00 <- round(metrics_AC10$r2[1:35], 3)
r2_total$AC10_NO_01 <- round(metrics_AC10$r2[36:70], 3)
r2_total$AC10_NO2_00 <- round(metrics_AC10$r2[71:105], 3)
r2_total$AC10_NO2_01 <- round(metrics_AC10$r2[106:140], 3)
r2_total$AC11_NO_00 <- round(metrics_AC11$r2[1:35], 3)
r2_total$AC11_NO_01 <- round(metrics_AC11$r2[36:70], 3)
r2_total$AC11_NO2_00 <- round(metrics_AC11$r2[71:105], 3)
r2_total$AC11_NO2_01 <- round(metrics_AC11$r2[106:140], 3)
r2_total$AC12_NO_00 <- round(metrics_AC12$r2[1:35], 3)
r2_total$AC12_NO_01 <- round(metrics_AC12$r2[36:70], 3)
r2_total$AC12_NO2_00 <- round(metrics_AC12$r2[71:105], 3)
r2_total$AC12_NO2_01 <- round(metrics_AC12$r2[106:140], 3)

# plotting ------------------------------------------------------------
rownames(nrmse_total) <- nrmse_total$Model
nrmse_total <- subset(nrmse_total, select = -c(Model))


nrmse_total_new <- as.data.frame(t(nrmse_total))

nrmse_total_new <-  rename(nrmse_total_new, 
                           "RLM_01" = "RLM_ 1",
                           "RLM_02" = "RLM_ 2",
                           "RLM_03" = "RLM_ 3",
                           "RLM_04" = "RLM_ 4",
                           "RLM_05" = "RLM_ 5",
                           "RLM_06" = "RLM_ 6",
                           "RLM_07" = "RLM_ 7",
                           "RLM_08" = "RLM_ 8",
                           "RLM_09" = "RLM_ 9",
                           "RLM_10" = "RLM_ 10",
                           "RLM_11" = "RLM_ 11",
                           "RLM_12" = "RLM_ 12",
                           "RLM_13" = "RLM_ 13",
                           "RLM_14" = "RLM_ 14",
                           "RLM_15" = "RLM_ 15",
                           "RLM_16" = "RLM_ 16",
                           "RLM_17" = "RLM_ 17",
                           "RLM_18" = "RLM_ 18",
                           "RLM_19" = "RLM_ 19",
                           "RLM_20" = "RLM_ 20",
                           "RLM_21" = "RLM_ 21",
                           "RLM_22" = "RLM_ 22",
                           "RLM_23" = "RLM_ 23",
                           "RLM_24" = "RLM_ 24",
                           "RLM_25" = "RLM_ 25",
                           "RLM_26" = "RLM_ 26",
                           "RF_01" = "RF_ 1",
                           "RF_02" = "RF_ 2",
                           "RF_03" = "RF_ 3",
                           "RF_04" = "RF_ 4",
                           "RF_05" = "RF_ 5",
                           "RF_06" = "RF_ 6",
                           "RF_07" = "RF_ 7",
                           "RF_08" = "RF_ 8",
                           "RF_09" = "RF_ 9")



Aircube <- vector(mode = "character", length = 16)

for (i in 1:4) {Aircube[i] <- "AC009"}
for (i in 5:8) {Aircube[i] <- "AC010"}
for (i in 9:12) {Aircube[i] <- "AC011"}
for (i in 13:16) {Aircube[i] <- "AC012"}

Sensor <- vector(mode = "character", length = 16)

for (i in 1:16) {if(i%%4 == 1){Sensor[i] <- "NO_00"}}
for (i in 1:16) {if(i%%4 == 2){Sensor[i] <- "NO_01"}}
for (i in 1:16) {if(i%%4 == 3){Sensor[i] <- "NO2_00"}}
for (i in 1:16) {if(i%%4 == 0){Sensor[i] <- "NO2_01"}}

Pollutants <- vector(mode = "character", length = 16)

for (i in 1:16) {if(i%%4 == 1){Pollutants[i] <- "NO"}}
for (i in 1:16) {if(i%%4 == 2){Pollutants[i] <- "NO"}}
for (i in 1:16) {if(i%%4 == 3){Pollutants[i] <- "NO2"}}
for (i in 1:16) {if(i%%4 == 0){Pollutants[i] <- "NO2"}}

nrmse_total_new <- cbind.data.frame(Aircube,Sensor,Pollutants,nrmse_total_new)

nrmse_drh_060 <- cbind.data.frame(Aircube,Sensor,Pollutants,nrmse_total_new[,c(9,13,17,22,32)])
nrmse_drh_090 <- cbind.data.frame(Aircube,Sensor,Pollutants,nrmse_total_new[,c(10,14,18,23,33)])
nrmse_drh_120 <- cbind.data.frame(Aircube,Sensor,Pollutants,nrmse_total_new[,c(11,15,19,24,34)])
nrmse_drh_150 <- cbind.data.frame(Aircube,Sensor,Pollutants,nrmse_total_new[,c(12,16,20,25,35)])

{
  forplot_nrmse_drh_060 <- as.data.frame(matrix(0,80,5))
  forplot_nrmse_drh_060 <-  rename(forplot_nrmse_drh_060, "Aircube" = "V1", "Sensor" = "V2", "Pollutants" = "V3", "Model" = "V4", "Value" = "V5")
  forplot_nrmse_drh_060$Aircube <- nrmse_drh_060$Aircube
  forplot_nrmse_drh_060$Sensor <- nrmse_drh_060$Sensor
  forplot_nrmse_drh_060$Pollutants <- nrmse_drh_060$Pollutants
  forplot_nrmse_drh_060$Model[1:16] <- "RLM_06"
  forplot_nrmse_drh_060$Model[17:32] <- "RLM_10"
  forplot_nrmse_drh_060$Model[33:48] <- "RLM_14"
  forplot_nrmse_drh_060$Model[49:64] <- "RLM_19"
  forplot_nrmse_drh_060$Model[65:80] <- "RF_03"
  forplot_nrmse_drh_060$Value[1:16] <- nrmse_drh_060$RLM_06
  forplot_nrmse_drh_060$Value[17:32] <- nrmse_drh_060$RLM_10
  forplot_nrmse_drh_060$Value[33:48] <- nrmse_drh_060$RLM_14
  forplot_nrmse_drh_060$Value[49:64] <- nrmse_drh_060$RLM_19
  forplot_nrmse_drh_060$Value[65:80] <- nrmse_drh_060$RF_03
  forplot_nrmse_drh_060$drh[1:80] <- "060"
}
{
  forplot_nrmse_drh_090 <- as.data.frame(matrix(0,80,5))
  forplot_nrmse_drh_090 <-  rename(forplot_nrmse_drh_090, "Aircube" = "V1", "Sensor" = "V2", "Pollutants" = "V3", "Model" = "V4", "Value" = "V5")
  forplot_nrmse_drh_090$Aircube <- nrmse_drh_090$Aircube
  forplot_nrmse_drh_090$Sensor <- nrmse_drh_090$Sensor
  forplot_nrmse_drh_090$Pollutants <- nrmse_drh_090$Pollutants
  forplot_nrmse_drh_090$Model[1:16] <- "RLM_07"
  forplot_nrmse_drh_090$Model[17:32] <- "RLM_11"
  forplot_nrmse_drh_090$Model[33:48] <- "RLM_15"
  forplot_nrmse_drh_090$Model[49:64] <- "RLM_20"
  forplot_nrmse_drh_090$Model[65:80] <- "RF_04"
  forplot_nrmse_drh_090$Value[1:16] <- nrmse_drh_090$RLM_07
  forplot_nrmse_drh_090$Value[17:32] <- nrmse_drh_090$RLM_11
  forplot_nrmse_drh_090$Value[33:48] <- nrmse_drh_090$RLM_15
  forplot_nrmse_drh_090$Value[49:64] <- nrmse_drh_090$RLM_20
  forplot_nrmse_drh_090$Value[65:80] <- nrmse_drh_090$RF_04
  forplot_nrmse_drh_090$drh[1:80] <- "090"
}
{
  forplot_nrmse_drh_120 <- as.data.frame(matrix(0,80,5))
  forplot_nrmse_drh_120 <-  rename(forplot_nrmse_drh_120, "Aircube" = "V1", "Sensor" = "V2", "Pollutants" = "V3", "Model" = "V4", "Value" = "V5")
  forplot_nrmse_drh_120$Aircube <- nrmse_drh_120$Aircube
  forplot_nrmse_drh_120$Sensor <- nrmse_drh_120$Sensor
  forplot_nrmse_drh_120$Pollutants <- nrmse_drh_120$Pollutants
  forplot_nrmse_drh_120$Model[1:16] <- "RLM_08"
  forplot_nrmse_drh_120$Model[17:32] <- "RLM_12"
  forplot_nrmse_drh_120$Model[33:48] <- "RLM_16"
  forplot_nrmse_drh_120$Model[49:64] <- "RLM_21"
  forplot_nrmse_drh_120$Model[65:80] <- "RF_05"
  forplot_nrmse_drh_120$Value[1:16] <- nrmse_drh_120$RLM_08
  forplot_nrmse_drh_120$Value[17:32] <- nrmse_drh_120$RLM_12
  forplot_nrmse_drh_120$Value[33:48] <- nrmse_drh_120$RLM_16
  forplot_nrmse_drh_120$Value[49:64] <- nrmse_drh_120$RLM_21
  forplot_nrmse_drh_120$Value[65:80] <- nrmse_drh_120$RF_05
  forplot_nrmse_drh_120$drh[1:80] <- "120"
}
{
  forplot_nrmse_drh_150 <- as.data.frame(matrix(0,80,5))
  forplot_nrmse_drh_150 <-  rename(forplot_nrmse_drh_150, "Aircube" = "V1", "Sensor" = "V2", "Pollutants" = "V3", "Model" = "V4", "Value" = "V5")
  forplot_nrmse_drh_150$Aircube <- nrmse_drh_150$Aircube
  forplot_nrmse_drh_150$Sensor <- nrmse_drh_150$Sensor
  forplot_nrmse_drh_150$Pollutants <- nrmse_drh_150$Pollutants
  forplot_nrmse_drh_150$Model[1:16] <- "RLM_09"
  forplot_nrmse_drh_150$Model[17:32] <- "RLM_13"
  forplot_nrmse_drh_150$Model[33:48] <- "RLM_17"
  forplot_nrmse_drh_150$Model[49:64] <- "RLM_22"
  forplot_nrmse_drh_150$Model[65:80] <- "RF_06"
  forplot_nrmse_drh_150$Value[1:16] <- nrmse_drh_150$RLM_09
  forplot_nrmse_drh_150$Value[17:32] <- nrmse_drh_150$RLM_13
  forplot_nrmse_drh_150$Value[33:48] <- nrmse_drh_150$RLM_17
  forplot_nrmse_drh_150$Value[49:64] <- nrmse_drh_150$RLM_22
  forplot_nrmse_drh_150$Value[65:80] <- nrmse_drh_150$RF_06
  forplot_nrmse_drh_150$drh[1:80] <- "150"
}

forplot_nrmse_total <- as.data.frame(matrix(0,320,6))

forplot_nrmse_total[1:80,] <- forplot_nrmse_drh_060
forplot_nrmse_total[81:160,] <- forplot_nrmse_drh_090
forplot_nrmse_total[161:240,] <- forplot_nrmse_drh_120
forplot_nrmse_total[241:320,] <- forplot_nrmse_drh_150

forplot_nrmse_total <-  rename(forplot_nrmse_total, "Aircube" = "V1", "Sensor" = "V2", "Pollutants" = "V3", "Model" = "V4", "Value" = "V5", "drh_delta_t"="V6")

colnames(forplot_nrmse_total)[3] <- "Pollutant"
for (i in 1:320){
  if(forplot_nrmse_total$drh_delta_t[i] == "060"){forplot_nrmse_total$drh_delta_t[i] <- "60"}
  if(forplot_nrmse_total$drh_delta_t[i] == "090"){forplot_nrmse_total$drh_delta_t[i] <- "90"}
}

forplot_nrmse_total$drh_delta_t <- ordered(forplot_nrmse_total$drh_delta_t, levels=c("60","90","120","150"))

## Figure 3 --------------------------

jpeg("figure/fig_3.jpeg", width = 8.7, height = 7, units = 'cm', res = 1000)
ggplot(forplot_nrmse_total, aes(drh_delta_t, Value, color=Pollutant)
)+geom_boxplot(
)+xlab(expression(paste(Delta,t[0]," of ",D[RH]," [sec]"))
)+ylab("nRMSE"
)+scale_color_manual(values=c("#2a9d8f","#f4a261")
)+ theme(axis.text = element_text(size = 5, color = "#FFFFFF")
)+ theme(axis.title = element_text(size = 3)
)+ theme(plot.title = element_text(size = 4)
)+ theme(legend.text = element_text(size = 5)
)+ theme(legend.title = element_text(size = 4)) +theme_bw()
dev.off()

nrmse_new_new <- subset(nrmse_total_new[,c(1,2,3,4,5,6,7,8,12,16,20,21,25,30,31,35,36,37,38)])

{
  forplot_nrmse_new_new <- as.data.frame(matrix(0,256,5))
  forplot_nrmse_new_new <-  rename(forplot_nrmse_new_new, "Aircube" = "V1", "Sensor" = "V2", "Pollutants" = "V3", "Model" = "V4", "Value" = "V5")
  forplot_nrmse_new_new$Aircube <- nrmse_new_new$Aircube
  forplot_nrmse_new_new$Sensor <- nrmse_new_new$Sensor
  forplot_nrmse_new_new$Pollutants <- nrmse_new_new$Pollutants
  for (i in 1:16)
  {
    forplot_nrmse_new_new$Model[(16*i-15):(16*i)] <- colnames(nrmse_new_new)[i+3]
  }
  for (j in 1:16)
  {
    forplot_nrmse_new_new$Value[(16*j-15):(16*j)] <- nrmse_new_new[,j+3]
  }
}

nrmse.mean <- forplot_nrmse_new_new %>% group_by(Model) %>% summarise(mean = mean(Value))
colnames(forplot_nrmse_new_new)[3] <- "Pollutant"

## Figure 4 --------------------------

jpeg("figure/fig_4.jpeg", width = 20, height = 12, units = 'cm', res = 800)
ggplot(forplot_nrmse_new_new, aes(Model, Value, color=Pollutant))+
  geom_point(size=1.5,
  )+
  stat_summary(
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "point",  size = 4,
    position = position_dodge(0)
  )+xlab("Model")+ylab("nRMSE")+ylim(0.1,0.9
  )+scale_color_manual(values=c("#2a9d8f","#f4a261")
  )+ theme(axis.text = element_text(size = 9)
  )+ scale_x_discrete(guide = guide_axis(angle = 90)
  )+ theme(axis.title = element_text(size = 6)
  )+ theme(plot.title = element_text(size = 7)
  )+ theme(legend.text = element_text(size = 7)
  )+ theme(legend.title = element_text(size = 6)) +theme_bw() + theme(axis.title.x = element_text(vjust = -1))
dev.off()

write.csv(nrmse_total, "data_output/model_selection/nrmse_total.csv")
write.csv(r2_total, "data_output/model_selection/r2_total.csv")
