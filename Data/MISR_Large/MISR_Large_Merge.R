#############
# An R script which spatially matches MISR Level 2 Aerosol satellite data to PM2.5 and Speciation data 
# from the AQS, CSN, and IMPROVE datasets (not CASTNET since CASTNET is weekly) which were collected
# on the same day as the MISR satellite data, within 2.2 km of the MISR satellite data location.
# Last updated: May 8, 2023
#############

library(sf)
library(plyr)
library(tidyverse)
library(data.table)
library(readxl)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")


# Get local file directory names 
misr.datasets.dir = paste0(getwd(), '/Data/MISR_Large/MISR_datasets/') # Sub-folder containing yearly MISR datasets from the CMAQ area
merged.data.dir = paste0(getwd(), '/Data/MISR_Large/MISR_merged_data/') # Sub-folder which will store merged MISR datasets


# Read in PM2.5 data collected at AQS data sites across the USA
cat('- Reading in AQS Data......')
start = Sys.time()
AQS.PM25.USA <- read_csv(paste0(getwd(), '/Data/AQS Data/AQS_PM25_2000_2021_USA.csv'), show_col_types = FALSE) %>%
  mutate(Site.Code = paste0("AQS_", Site.Code)) %>%
  rename(Site.Longitude = Longitude, Site.Latitude = Latitude) %>%
  select(-c("State", "County", "City", "State.Code", "County.Code", "Site.Num"))
cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')


# Read in Speciation data collected at CSN data sites across the USA
cat('- Reading in CSN Data......')
start = Sys.time()
CSN.SPEC.USA <- read_csv(paste0(getwd(), '/Data/CSN Data/CSN_PM25_SPEC_2000_2021_USA.csv'), show_col_types = FALSE) %>%
  mutate(Site.Code = paste0("CSN_", Site.Code)) %>%
  rename(Site.Longitude = Longitude, Site.Latitude = Latitude) %>%
  select(-c("State", "County", "City", "State.Code", "County.Code", "Site.Num"))
cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')