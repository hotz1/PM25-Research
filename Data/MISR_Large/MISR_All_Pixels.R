#############
# An R script which creates a list of all unique MISR pixel locations (latitude + longitude) 
# in the CMAQ dataset and assigns a unique pixel ID to each of these locations based on the
# MISR satellite flightpath corresponding to the pixel.
# Last updated: January 25, 2022
#############

library(tidyverse)
library(data.table)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Get the list of the filenames and filepaths for cleaned MISR datasets 
misr.datasets.dir = paste0(getwd(), '/Data/MISR_Large/MISR_datasets/') # Sub-folder containing yearly MISR datasets
misr.annual.filenames <- list.files(misr.datasets.dir, pattern = ".csv", full.names = T)

# Create an empty list which will store the data for MISR pixels in each year
misr.annual.pixels <- vector("list", length = length(misr.annual.filenames))

# Populate the empty list by selecting all unique path/latitude/longitude combinations for each year
for(i in 1:length(misr.annual.filenames)){
  misr.annual <- read_csv(misr.annual.filenames[i], guess_max = Inf)
  misr.annual.pixels[[i]] <- misr.annual %>% 
    select(path, longitude, latitude) %>%
    unique()
}
misr.pixels.all <- do.call("rbind", misr.annual.pixels)

# Create pixel.id values for different pixels from the MISR datasets. 
# The first 4 characters will represent the corresponding flightpath, 
# and the digits after will be the pixel's unique "code"
misr.pixels <- misr.pixels.all %>%
  select(path, longitude, latitude) %>%
  unique() %>%
  group_by(path) %>%
  mutate(pixel.id = paste0(path, '_', sprintf('%06d', 1:n()))) %>%
  ungroup()

# Save to a locally stored csv file
write.csv(misr.pixels, paste0(getwd(), '/Data/MISR_Large/MISR_CMAQ_pixels.csv'), row.names = F)