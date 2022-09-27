#############
# An R script to create a "correlation heatmap" among the 74 AOD mixture models in the MISR data wihch was 
# collected in California from 2000-2022.
# Last updated: September 27, 2022
#############

library(tidyverse)
library(data.table)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Get local file directory names
misr.datasets.dir = paste0(getwd(), '/Data/MISR/MISR_datasets/') # Sub-folder containing yearly MISR datasets
merged.data.dir = paste0(getwd(), '/Data/MISR/MISR_merged_data/') # Sub-folder which will store merged MISR datasets

# Collect local MISR filenames
misr.annual.filenames <- list.files(misr.datasets.dir, pattern = ".csv", full.names = T)

# Create empty (for now) lists which will store annual merged MISR datasets 
misr.annual.mixtures <- vector("list", length = length(misr.annual.filenames))


# For each MISR file, load in the MISR dataset, and merge with the AQS/CSN/IMPROVE datasets based on spatial matching and time matching
for(i in 1:length(misr.annual.filenames)){
  # Select year from the filename
  year <- substr(misr.annual.filenames[i], nchar(misr.annual.filenames[i]) - 7, nchar(misr.annual.filenames[i]) - 4)
  cat('- Loading MISR Data from ', year, '......', sep = '')
  start = Sys.time()
  
  # Read in the csv file containing MISR data
  misr.annual <- read_csv(misr.annual.filenames[i], guess_max = Inf, show_col_types = FALSE)
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  # Select 74 AOD mixtures
  mixtures.annual <- misr.annual %>% select(c(32:105))
  misr.annual.mixtures[[i]] <- mixtures.annual
}

# Combine 74 mixtures for each year
misr.all.mixtures <- do.call("rbind", misr.annual.mixtures)
remove(misr.annual.mixtures)

# cor(misr.all.mixtures, use = "pairwise.complete.obs")
