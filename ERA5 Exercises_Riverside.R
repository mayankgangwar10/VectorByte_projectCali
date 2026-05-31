install.packages("ecmwfr")
install.packages("assertthat")
install.packages("sf")

library(assertthat)
library(ecmwfr)
library(terra)
library(dplyr)
library(sf)
library(readxl)
library(ncdf4)
library(geodata)
library(stringr)
library(ggplot2)
library(lubridate)

wf_set_key(key = "02dd6b32-445d-4ac0-81d6-db657a98da86")

#-------------------------------------------
#----------------Temperature----------------
#-------------------------------------------
pullFromERA5 = function(variableName, dailyStatistic)
{
  #This loop pulls temperature data from the ERA5 database
  for(i in 2014:2023)
  {
    request <- list(
      dataset_short_name = "derived-era5-single-levels-daily-statistics",
      product_type = "reanalysis",
      variable = variableName,
      year = paste0(i),
      month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
      day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
      daily_statistic = dailyStatistic,
      data_format = "netcdf",
      download_format = "unarchived",
      area = c(34.2, -114.4,33.36, -117.73),
      target = paste0("era5_", variableName,"_",dailyStatistic,"_",i,"_RiversideCounty.nc")
    )
    
    file <- wf_request(
      request  = request,  # the request
      transfer = TRUE,     # download the file
      path     = "C:\\Users\\saxce\\OneDrive\\Desktop\\Research\\Training Resources\\VectorByte 2026\\ERA5Downloads"
    )
    print(paste0("Completed: ",i))
  }
}
#Means
pullFromERA5("2m_temperature","daily_mean")
pullFromERA5("2m_dewpoint_temperature","daily_mean")
pullFromERA5("soil_temperature_level_1","daily_mean")
pullFromERA5("volumetric_soil_water_layer_1","daily_mean")

#Mins
pullFromERA5("2m_temperature","daily_minimum")
pullFromERA5("2m_dewpoint_temperature","daily_minimum")

#Maxima
pullFromERA5("2m_temperature","daily_maximum")
pullFromERA5("2m_dewpoint_temperature","daily_maximum")