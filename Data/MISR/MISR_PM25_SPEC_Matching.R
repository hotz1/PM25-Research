#############
# An R script which spatially matches previously collected and cleaned MISR Level 2 Aerosol data to
# PM2.5 and Speciation data which were collected by the EPA within 24 hours of the MISR satellite.
# Last updated: August 8, 2022
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
AQS.PM25.cali <- read_csv(paste0(getwd(), '/Data/AQS Data/AQS_PM25_2000_2021_Cali.csv')) %>%
  mutate(Site.Code = paste0("AQS_", Site.Code))

# Read in speciation data collected at CSN data sites in California
CSN.SPEC.cali <- read_csv(paste0(getwd(), '/Data/CSN Data/CSN_PM25_SPEC_2000_2021_Cali.csv')) %>%
  mutate(Site.Code = paste0("CSN_", Site.Code))

# Read in speciation data from the IMPROVE dataset and select only the observations which are in California
IMPROVE.SPEC.cali <- readxl::read_excel(paste0(getwd(), '/Data/IMPROVE Data/IMPROVE_Raw_Data_2000_2021.xlsx'), sheet = 1) %>%
  filter(State == "CA") %>%
  rename(Site.Code = SiteCode) %>%
  mutate(Site.Code = paste0("IMPROVE_", Site.Code))


# Create pixel.id values for different pixels from the MISR dataset.
# These will be used to match MISR pixels with local AQS/CSN/IMPROVE data sites.
misr.pixels <- misr.cali %>%
  select(path, longitude, latitude) %>%
  unique() %>%
  group_by(path) %>%
  mutate(pixel.id = paste0(path, '_', sprintf('%06d', 1:n()))) %>%
  ungroup() %>%
  select(pixel.id, longitude, latitude)

# Merge MISR dataset with the newly-created MISR pixel IDs
misr.cali <- merge(misr.cali, misr.pixels) %>%
  relocate(pixel.id, .after = latitude)

# Create a table containing info about AQS data collection sites
AQS.sites <- AQS.PM25.cali %>%
  select(Longitude, Latitude, Site.Code) %>%
  unique() %>%
  rename(Site.Longitude = Longitude, Site.Latitude = Latitude) %>%
  select(Site.Code, Site.Longitude, Site.Latitude)

# Create a table containing info about CSN data collection sites
CSN.sites <- CSN.SPEC.cali %>%
  select(Longitude, Latitude, Site.Code) %>%
  unique() %>%
  rename(Site.Longitude = Longitude, Site.Latitude = Latitude) %>%
  select(Site.Code, Site.Longitude, Site.Latitude)

# Create a table containing info about CSN data collection sites
IMPROVE.sites <- IMPROVE.SPEC.cali %>%
  select(Longitude, Latitude, Site.Code) %>%
  unique() %>%
  rename(Site.Longitude = Longitude, Site.Latitude = Latitude) %>%
  select(Site.Code, Site.Longitude, Site.Latitude)

# Convert the tables above into sf objects
misr.pixels_sf <- sf::st_as_sf(misr.pixels, coords = c(2:3), crs = 4326)
AQS.sites_sf <- sf::st_as_sf(AQS.sites, coords = c(2:3), crs = 4326)
CSN.sites_sf <- sf::st_as_sf(CSN.sites, coords = c(2:3), crs = 4326)
IMPROVE.sites_sf <- sf::st_as_sf(IMPROVE.sites, coords = c(2:3), crs = 4326)

# Collect pairs of MISR pixels and AQS data collection sites within 4.4 km of each other
MISR.near.AQS <- st_join(misr.pixels_sf, AQS.sites_sf, join = st_is_within_distance, dist = units::set_units(4.4, km), left = FALSE)
MISR.near.AQS <- data.frame(MISR.near.AQS) %>%
  select(pixel.id, Site.Code)

# Merge MISR data with pairs of pixels which are near AQS data collection sites
MISR.cali.near.AQS <- merge(misr.cali, MISR.near.AQS)
