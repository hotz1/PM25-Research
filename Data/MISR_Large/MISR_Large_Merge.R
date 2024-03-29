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
  cat('\n', 'Loading MISR Data from ', year, '......', sep = '')
  start = Sys.time()
  misr.annual <- read_csv(misr.annual.filenames[i], guess_max = Inf, show_col_types = FALSE)
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  # Merge MISR dataset with the MISR pixel IDs
  cat('- Merging MISR data with pixel IDs......')
  start = Sys.time()
  misr.annual <- merge(misr.annual, misr.pixels) %>%
    relocate(pixel.id, .after = latitude)
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  
  # Merge MISR data with any AQS data sites which are within 2.2 km of a MISR pixel
  cat('- Merging MISR data with AQS data......')
  start = Sys.time()
  MISR.AQS.match <- merge(misr.annual, MISR.near.AQS)
  
  # Select observations which are on the same date and have the same site code
  MISR.AQS.match <- merge(AQS.PM25.USA, MISR.AQS.match, by.x = c("Date", "Site.Code"), by.y = c("date", "Site.Code")) %>%
    rename(pixel.longitude = longitude, pixel.latitude = latitude) %>%
    select(-c(time)) %>%
    relocate(PM25, .after = Date)
  aqs.misr.annual[[i]] <- MISR.AQS.match
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  
  # Merge MISR data with any CSN data sites which are within 2.2 km of a MISR pixel
  cat('- Merging MISR data with CSN data......')
  start = Sys.time()
  MISR.CSN.match <- merge(misr.annual, MISR.near.CSN)
  
  # Select observations which are on the same date and have the same site code
  MISR.CSN.match <- merge(CSN.SPEC.USA, MISR.CSN.match, by.x = c("Date", "Site.Code"), by.y = c("date", "Site.Code")) %>%
    rename(pixel.longitude = longitude, pixel.latitude = latitude) %>%
    select(-c(time))
  csn.misr.annual[[i]] <- MISR.CSN.match
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
}


# Bind together all of the AQS/CSN/IMPROVE datasets which have been matched with corresponding MISR data
aqs.misr.merged <- do.call("rbind", aqs.misr.annual)
csn.misr.merged <- do.call("rbind", csn.misr.annual)

# Save the merged datasets to the merged dataset directory.
write.csv(aqs.misr.merged, paste0(merged.data.dir, '/MISR_AQS_Matched.csv'), row.names = F)
write.csv(csn.misr.merged, paste0(merged.data.dir, '/MISR_CSN_Matched.csv'), row.names = F)
