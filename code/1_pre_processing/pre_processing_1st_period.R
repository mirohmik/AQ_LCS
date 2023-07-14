# Pre_processing_1st_period.R
# Script introduction --------------------------------
#
# Objectives : This script merged the raw signal and reference data for 
#              1st co-location period before its calibration process.
#
#
# Library ------------------------------------------------------

require(chron)
library(lubridate)
library(dplyr)
library(data.table)
library(doBy)
library(tibbletime)

setwd("C:/") # change the directory

# import the raw data --------------------------------------------------

AC_Raw_9 <- read.csv('data_input/raw_signal_1st_colocation_period/AIRCUBES_ID_9.csv',header=TRUE)
AC_Raw_10 <- read.csv('data_input/raw_signal_1st_colocation_period/AIRCUBES_ID_10.csv',header=TRUE)
AC_Raw_11 <- read.csv('data_input/raw_signal_1st_colocation_period/AIRCUBES_ID_11.csv',header=TRUE)
AC_Raw_12 <- read.csv('data_input/raw_signal_1st_colocation_period/AIRCUBES_ID_12.csv',header=TRUE)

AC_Raw_9 <-subset(AC_Raw_9, select = -X)
AC_Raw_10 <-subset(AC_Raw_10, select = -X)
AC_Raw_11 <-subset(AC_Raw_11, select = -X)
AC_Raw_12 <-subset(AC_Raw_12, select = -X)

# Import reference data  -----------------------------------------------------

REF <- read.csv('data_output/reference_HAE/HAEref_1st_period.csv',header=TRUE)

REF <- subset(REF, select = -X)

REF_9 <- REF[3812:23947,]
REF_10 <- REF[86:23947,]
REF_11 <- REF[86:23947,]
REF_12 <- REF[86:23947,]

# merge raw data and reference data in one data.frame by each sensor ---------

AC9 <- AC_Raw_9
AC10 <- AC_Raw_10
AC11 <- AC_Raw_11
AC12 <- AC_Raw_12

AC9$REFNO <- REF_9$NO_ppb
AC10$REFNO <- REF_10$NO_ppb
AC11$REFNO <- REF_11$NO_ppb
AC12$REFNO <- REF_12$NO_ppb

AC9$REFNO2 <- REF_9$NO2_ppb
AC10$REFNO2 <- REF_10$NO2_ppb
AC11$REFNO2 <- REF_11$NO2_ppb
AC12$REFNO2 <- REF_12$NO2_ppb


# Remove the NA data in all merged data.frames --------------------------

AC9 <- na.omit(AC9)
AC10 <- na.omit(AC10)
AC11 <- na.omit(AC11)
AC12<- na.omit(AC12)

# Export the AC(9,10,11,12) ----------------------------------------------

write.csv(AC9, "data_output/pre_processed_1st_period/AC9.csv")
write.csv(AC10, "data_output/pre_processed_1st_period/AC10.csv")
write.csv(AC11, "data_output/pre_processed_1st_period/AC11.csv")
write.csv(AC12, "data_output/pre_processed_1st_period/AC12.csv")







