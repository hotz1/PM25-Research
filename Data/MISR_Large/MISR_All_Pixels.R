#############
# An R script which creates a list of all unique MISR pixel locations (latitude + longitude) 
# in the CMAQ dataset and assigns a unique pixel ID to each of these locations based on the
# MISR satellite flightpath corresponding to the pixel.
# Last updated: January 26, 2022
#############

library(tidyverse)
library(data.table)
library(vroom)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")
misr.datasets.dir = paste0(getwd(), '/Data/MISR_Large/MISR_datasets/') # Sub-folder containing yearly MISR datasets
misr.pixels.dir = paste0(getwd(), '/Data/MISR_Large/MISR_annual_pixels/')

#### STEP 1: Read in MISR data in the CMAQ location for each year (2000-2022) and select unique pixels

# Select all unique path/latitude/longitude combinations for each year and write them to csv files
for(i in 2000:2022){
  annual.filename <- paste0(misr.datasets.dir, 'MISR_Data_', i, '.csv')
  misr.annual <- vroom(annual.filename, col_select = c(path, longitude, latitude))
  misr.annual <- misr.annual %>%
    unique()
  write.csv(misr.annual, paste0(misr.pixels.dir, 'MISR_Pixels_', i, '.csv'))
}


# #### STEP 2: Read in the unique pixels from each year, and merge these together to get all pixels 
# annual.pixel.filenames <- list.files(misr.pixels.dir, pattern = ".csv", full.names = T)
# misr.annual.pixels <- vector("list", length = length(annual.pixel.filenames))
# 
# for(j in 1:length(annual.pixel.filenames)){
#   misr.annual.pixels[[j]] <- read_csv(annual.pixel.filenames[j], guess_max = Inf)
# }
# misr.pixels.all <- do.call("rbind", misr.annual.pixels) %>%
#   unique()
# 
# 
# #### STEP 3: Assign unique pixel_id values to each pixel based on its corresponding MISR flightpath 
# 
# # Create pixel.id values for different pixels from the MISR datasets. 
# # The first 4 characters will represent the corresponding flightpath, 
# # and the digits after will be the pixel's unique "code"
# misr.pixels <- misr.pixels.all %>%
#   group_by(path) %>%
#   mutate(pixel.id = paste0(path, '_', sprintf('%07d', 1:n()))) %>%
#   ungroup()
# 
# # Save to a locally stored csv file
# write.csv(misr.pixels, paste0(getwd(), '/Data/MISR_Large/MISR_CMAQ_pixels.csv'), row.names = F)