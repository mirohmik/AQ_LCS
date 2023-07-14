# Reference.R
# Script introduction --------------------------------
#
# Objectives : This script import the data from reference instruments,
#              which is Haerkingen station in NABEL network.
#
# Library and reference source -------------------------

require(chron)
library(lubridate)
library(dplyr)
library(data.table)
library(doBy)
library(tibbletime)

setwd("C:/") # change the directory

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


write.csv(HAEref_1st_period,"data_output/reference_HAE/HAEref_1st_period.csv")
write.csv(HAEref_2nd_period,"data_output/reference_HAE/HAEref_2nd_period.csv")

