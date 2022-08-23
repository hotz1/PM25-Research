#############
# An R script which spatially matches MISR Level 2 Aerosol satellite data to PM2.5 and Speciation data 
# from the AQS, CSN, and IMPROVE datasets (not CASTNET since CASTNET is weekly) which were collected
# on the same day as the MISR satellite data, within 2.2 km of the MISR satellite data location.
# Last updated: August 23, 2022
#############

library(sf)
library(plyr)
library(tidyverse)
library(data.table)
library(readxl)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Get local file directory names
misr.datasets.dir = paste0(getwd(), '/Data/MISR/MISR_datasets/') # Sub-folder containing yearly MISR datasets
merged.data.dir = paste0(getwd(), '/Data/MISR/MISR_merged_data/') # Sub-folder which will store merged MISR datasets



# Read in PM2.5 data collected at AQS data sites in California
cat('- Reading in AQS Data......')
start = Sys.time()
AQS.PM25.cali <- read_csv(paste0(getwd(), '/Data/AQS Data/AQS_PM25_2000_2021_Cali.csv')) %>%
  mutate(Site.Code = paste0("AQS_", Site.Code)) %>%
  rename(Site.Longitude = Longitude, Site.Latitude = Latitude) %>%
  select(-c("State", "County", "City", "State.Code", "County.Code", "Site.Num"))
cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')

# Read in Speciation data collected at CSN data sites in California
cat('- Reading in CSN Data......')
start = Sys.time()
CSN.SPEC.cali <- read_csv(paste0(getwd(), '/Data/CSN Data/CSN_PM25_SPEC_2000_2021_Cali.csv')) %>%
  mutate(Site.Code = paste0("CSN_", Site.Code)) %>%
  rename(Site.Longitude = Longitude, Site.Latitude = Latitude) %>%
  select(-c("State", "County", "City", "State.Code", "County.Code", "Site.Num"))
cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')

# Read in Speciation data from the IMPROVE dataset and select only the observations which are in California
cat('- Reading in IMPROVE Data......')
start = Sys.time()
IMPROVE.SPEC.cali <- readxl::read_excel(paste0(getwd(), '/Data/IMPROVE Data/IMPROVE_Raw_Data_2000_2021.xlsx'), sheet = 1) %>%
  filter(State == "CA") %>%
  rename(Site.Name = SiteName, Site.ID = SiteCode, Site.Code = EPACode, Site.Longitude = Longitude, Site.Latitude = Latitude) %>%
  mutate(Site.Code = paste0("IMPROVE_", paste(substr(Site.Code, 0, 2), substr(Site.Code, 3, 5),
                                              substr(Site.Code, 6, 9), sep = "-"))) %>%
  select(-c("Dataset", "AuxID", "State", "CountyFIPS"))
cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')




# Create a table containing info about AQS data collection sites
AQS.sites <- AQS.PM25.cali %>%
  select(Site.Code, Site.Longitude, Site.Latitude) %>%
  unique()

# Create a table containing info about CSN data collection sites
CSN.sites <- CSN.SPEC.cali %>%
  select(Site.Code, Site.Longitude, Site.Latitude) %>%
  unique()

# Create a table containing info about IMPROVE data collection sites
IMPROVE.sites <- IMPROVE.SPEC.cali %>%
  select(Site.Code, Site.Longitude, Site.Latitude) %>%
  unique()

# Convert the tables above into sf objects
AQS.sites_sf <- sf::st_as_sf(AQS.sites, coords = c(2:3), crs = 4326)
CSN.sites_sf <- sf::st_as_sf(CSN.sites, coords = c(2:3), crs = 4326)
IMPROVE.sites_sf <- sf::st_as_sf(IMPROVE.sites, coords = c(2:3), crs = 4326)

# Read in MISR pixel ID values (generated in a different R script)
misr.pixels <- read_csv(paste0(getwd(), '/Data/MISR/misr_california_pixels.csv'))

# Convert MISR pixel ID values to an sf object
misr.pixels_sf <- sf::st_as_sf(misr.pixels, coords = c(2:3), crs = 4326)


# Collect pairs of MISR pixels and AQS data collection sites within 2.2 km of the MISR pixels
MISR.near.AQS <- st_join(misr.pixels_sf, AQS.sites_sf, join = st_is_within_distance, dist = units::set_units(2.2, km), left = FALSE)
MISR.near.AQS <- data.frame(MISR.near.AQS) %>%
  select(pixel.id, Site.Code)




# Collect MISR filenames
misr.annual.filenames <- list.files(misr.datasets.dir, pattern = ".csv", full.names = T)

# Create empty (for now) lists which will store annual merged MISR datasets 
aqs.misr.annual <- vector("list", length = length(misr.annual.filenames))
csn.misr.annual <- vector("list", length = length(misr.annual.filenames))
#improve.misr.annual <- vector("list", length = length(misr.annual.filenames))



# For each MISR file, load in the MISR dataset, and merge with the AQS/CSN/IMPROVE datasets based on spatial matching and time matching
for(i in 1:length(misr.annual.filenames)){
  misr.annual <- read_csv(misr.annual.filenames[i], guess_max = Inf)
  
  # Merge MISR dataset with the MISR pixel IDs
  misr.annual <- merge(misr.annual, misr.pixels) %>%
    relocate(pixel.id, .after = latitude)
  
  # Merge MISR data with any AQS data sites which are within 2.2 km of a MISR pixel
  MISR.AQS.match <- merge(misr.annual, MISR.near.AQS)
  
  # Select observations which are on the same date and have the same site code
  MISR.AQS.match <- merge(AQS.PM25.cali, MISR.AQS.match, by.x = c("Date", "Site.Code"), by.y = c("date", "Site.Code")) %>%
    rename(pixel.longitude = longitude, pixel.latitude = latitude) %>%
    select(-c(time)) %>%
    relocate(PM25, .after = Date)
  
  aqs.misr.annual[[i]] <- MISR.AQS.match
}

# Bind together all of the AQS/CSN/IMPROVE datasets which have been matched with corresponding MISR data
aqs.misr.merged <- do.call("rbind", aqs.misr.annual)

# Save the merged datasets to the merged dataset directory.
write.csv(aqs.misr.merged, paste0(merged.data.dir, '/MISR_AQS_Matched.csv'), row.names = F)
