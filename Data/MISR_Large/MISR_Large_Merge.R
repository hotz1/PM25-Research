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



# Create a table containing info about AQS data collection sites
AQS.sites <- AQS.PM25.USA %>%
  select(Site.Code, Site.Longitude, Site.Latitude) %>%
  unique()

# Create a table containing info about CSN data collection sites
CSN.sites <- CSN.SPEC.USA %>%
  select(Site.Code, Site.Longitude, Site.Latitude) %>%
  unique()

# Convert the tables above into sf objects
AQS.sites_sf <- sf::st_as_sf(AQS.sites, coords = c(2:3), crs = 4326)
CSN.sites_sf <- sf::st_as_sf(CSN.sites, coords = c(2:3), crs = 4326)


# Read in MISR pixel ID values (generated in a different R script)
cat('- Reading in MISR Pixels......')
start = Sys.time()
misr.pixels <- read_csv(paste0(getwd(), '/Data/MISR_Large/MISR_CMAQ_pixels.csv'), show_col_types = FALSE)

# Convert MISR pixel ID values to an sf object
misr.pixels_sf <- sf::st_as_sf(misr.pixels, coords = c(2:3), crs = 4326)
cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')


# Collect pairs of MISR pixels and AQS data collection sites within 2.2 km of the MISR pixels
cat('- Finding AQS Data Sites near MISR pixels......')
start = Sys.time()
MISR.near.AQS <- st_join(misr.pixels_sf, AQS.sites_sf, join = st_is_within_distance, dist = units::set_units(2.2, km), left = FALSE)
MISR.near.AQS <- data.frame(MISR.near.AQS) %>%
  select(pixel.id, Site.Code)
cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')

# Collect pairs of MISR pixels and CSN data collection sites within 2.2 km of the MISR pixels
cat('- Finding CSN Data Sites near MISR pixels......')
start = Sys.time()
MISR.near.CSN <- st_join(misr.pixels_sf, CSN.sites_sf, join = st_is_within_distance, dist = units::set_units(2.2, km), left = FALSE)
MISR.near.CSN <- data.frame(MISR.near.CSN) %>%
  select(pixel.id, Site.Code)
cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')

# Collect MISR filenames
misr.annual.filenames <- list.files(misr.datasets.dir, pattern = ".csv", full.names = T)

# Create empty (for now) lists which will store annual merged MISR datasets 
aqs.misr.annual <- vector("list", length = length(misr.annual.filenames))
csn.misr.annual <- vector("list", length = length(misr.annual.filenames))

# For each MISR file, load in the MISR dataset, and merge with the AQS/CSN/IMPROVE datasets based on spatial matching and time matching
for(i in 1:length(misr.annual.filenames)){
  year <- substr(misr.annual.filenames[i], nchar(misr.annual.filenames[i]) - 7, nchar(misr.annual.filenames[i]) - 4)
  cat(misr.annual.filenames[i], ':', rep(' ', 20), year, sep = '')
}
