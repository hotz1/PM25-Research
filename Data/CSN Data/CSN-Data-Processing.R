#############
# Code to extract data from EPA PM2.5 sites (Speciation)
# Data downloaded from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily - PM2.5 Speciation
# Metadata available at https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_daily_summary_files
#############

library(tidyverse)

# Get names and file locations of all the required CSV files
setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research/Data/CSN Data")
pm25.spec.files <- list.files(path = "./daily_SPEC/", pattern = "daily_SPEC_20*", full.names = TRUE)

# Create an empty list to store data from each file
pm25.spec.list <- vector('list', length(pm25.spec.files))

# Clean downloaded data files (one for each year 2000 - 2021)
for(i in 1:length(pm25.spec.files)) { 
  pm25.spec.data <- read_csv(pm25.spec.files[i])

  # Creates a table of daily mean temperatures and the site info 
  mean_temp <- pm25.spec.data %>%
    filter(`Parameter Code` == 68105) %>%
    select(`Mean.Temp` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()

  # Creates a table of daily min temperatures and the site info   
  min_temp <- pm25.spec.data %>%
    filter(`Parameter Code` == 68103) %>%
    select(`Min.Temp` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily max temperatures and the site info 
  max_temp <- pm25.spec.data %>%
    filter(`Parameter Code` == 68104) %>%
    select(`Max.Temp` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily atmospheric pressures and the site info 
  atm_pres <- pm25.spec.data %>%
    filter(`Parameter Code` == 68108) %>%
    select(`Atm.Press` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Join all the newly-created tables together
  joined.table <- Reduce(function(...) merge(..., all = TRUE),
                         list(mean_temp, min_temp, max_temp, atm_pres))
  
  
  pm25.spec.list[[i]] <- joined.table
}

# Merge tables together for all 22 years
pm25.spec.all <- do.call("rbind", pm25.spec.list)