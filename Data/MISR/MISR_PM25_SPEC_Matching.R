#############
# An R script which spatially matches previously collected and cleaned MISR Level 2 Aerosol data to
# PM2.5 and Speciation data which were collected by the EPA within 24 hours of the MISR satellite.
# Last updated: August 3, 2022
#############

library(sf)
library(plyr)
library(tidyverse)
library(data.table)
library(readxl)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Read in MISR datasets and merge them into one large table
misr.datasets.dir = paste0(getwd(), '/Data/MISR/MISR_datasets/') # Sub-folder containing yearly MISR datasets
misr.annual.filenames <- list.files(misr.datasets.dir, pattern = ".csv", full.names = T)
misr.annual.files <- vector("list", length = length(misr.annual.filenames))
for(i in 1:length(misr.annual.filenames)){
  misr.annual.files[[i]] <- read_csv(misr.annual.filenames[i], guess_max = Inf)
}
misr.cali <- do.call("rbind", misr.annual.files)

# Read in PM2.5 data collected at AQS data sites in California
AQS.PM25.cali <- read_csv(paste0(getwd(), '/Data/AQS Data/AQS_PM25_2000_2021_Cali.csv'))

# Read in speciation data collected at CSN data sites in California
CSN.SPEC.cali <- read_csv(paste0(getwd(), '/Data/CSN Data/CSN_PM25_SPEC_2000_2021_Cali.csv'))

# Read in speciation data from the IMPROVE dataset and select only the observations which are in California
IMPROVE.SPEC.cali <- readxl::read_excel(paste0(getwd(), '/Data/IMPROVE Data/IMPROVE_Raw_Data_2000_2021.xlsx'), sheet = 1) %>%
  filter(State == "CA")


# Create pixel_ID values for different pixels from the MISR dataset.
# These will be used to match MISR pixels with local AQS/CSN/IMPROVE data sites.
misr.pixels <- misr.cali %>%
  select(path, longitude, latitude) %>%
  unique() %>%
  group_by(path) %>%
  mutate(pixel_id = paste0(path, '_', sprintf('%06d', 1:n()))) %>%
  ungroup()

# Add pixel ID values to the main MISR dataset
misr.cali <- merge.data.frame(misr.cali, misr.pixels) %>%
  relocate(pixel_id, .before = path)


# Create a table containing info about AQS data collection sites
AQS.sites <- AQS.PM25.cali %>%
  select(Longitude, Latitude, Site.Code) %>%
  unique() %>%
  mutate(Site.Code = paste0("AQS_", Site.Code))

# Create a table containing info about CSN data collection sites
CSN.sites <- CSN.SPEC.cali %>%
  select(Longitude, Latitude, Site.Code) %>%
  unique() %>%
  mutate(Site.Code = paste0("CSN_", Site.Code))

# Create a table containing info about CSN data collection sites
IMPROVE.sites <- IMPROVE.SPEC.cali %>%
  select(Longitude, Latitude, SiteCode) %>%
  unique() %>%
  mutate(Site.Code = paste0("IMPROVE_", SiteCode)) %>%
  select(Longitude, Latitude, Site.Code)

# Create a table of all data sites
data.sites <- rbind(AQS.sites, CSN.sites, IMPROVE.sites)
