#############
# Code to extract data from EPA PM2.5 sites (FRM)
# Data downloaded from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily Parameter code 88101
# Metadata available at https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_daily_summary_files
#############

library(tidyverse)


setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research/Data/CSN Data")
pm25.files <- list.files(path = "./daily_88101/", pattern = "daily_88101_20*", full.names = TRUE)

# Extract DAILY data Parameter Code 88101 (not speciation PM2.5)
pm25.list <- vector('list', length(pm25.files))

for(i in 1:length(pm25.files)) { 
  dat.pm25 <- read_csv(pm25.files[i])
  dat.pm25 <- dat.pm25 %>%
    select(PM25 = `Arithmetic Mean`, `POC`, Date = `Date Local`, `Latitude`, `Longitude`,
           State = `State Name`, County = `County Name`, State.Code = `State Code`,
           County.Code = `County Code`, Site.Num = `Site Num`)

  pm25.list[[i]] <- dat.pm25
}  
