#############
# Defining functions to read data from downloaded MISR Level 2 Aerosol NetCDF files into R
# and write these data to locally stored csv files
# Last updated: August 10, 2022
#############

library(sf)
library(plyr)
library(dplyr)
library(ncdf4)
library(data.table)
library(stringr)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Increase amount of time before a file download times out to 5 minutes
options(timeout = max(300, getOption("timeout")))

# Get directory names 
misr_urls.dir = paste0(getwd(), '/Data/MISR/MISR_urls/') # Folder containing urls for the NetCDF files to download
ncdf.dir = paste0(getwd(), '/Data/MISR/NetCDF_files/') # Folder to download NetCDF files into
datasets.dir = paste0(getwd(), '/Data/MISR/MISR_datasets/') # Folder to write datasets into

# Load in California shapefile
california <- st_read(paste0(getwd(), '/Data/ca-state-boundary/CA_State_TIGER2016.shp')) %>%
  st_transform(crs = 4326)

#### Function to extract relevant layers from MISR NetCDF files ####
extract.ncdf = function(filename, region, var.list, filter.data = T, filter.region = T){
  # Function Inputs: 
  # 
  # filename       : The location of a NetCDF file which is downloaded in the working directory, to be read into R (use full filepath) 
  # region         : An sf polygon representing a particular geographic region. Used for spatial filtering.
  # var.list       : vector of NetCDF layers' variable names
  # filter.data    : Defaults to true. When true, remove pixels with at least a certain number of columns which are missing
  # filter.region  : Defaults to true. When true, filter to exclusively select pixels within the provided region
  #
  #
  # Function Outputs:
  #
  # misr.data       : A data table containing the information which was stored in the MISR NetCDF files, and the MISR flight path
  
  # Open the chosen file
  mycdf = nc_open(filename)
  
  # Find the MISR flight path from the NetCDF filename. Paths are 'stored' in the MISR NetCDF filename as 
  # 'P' followed by some (usually 3) numeric characters, representing the path number
  path = str_extract(filename, pattern = "P[0-9]+")
  
  #### Get Aerosol Optical Depth (AOD) per mixture data from the NetCDF ####
  cat('- Aerosol Optical Depth Mixtures......')
  start = Sys.time()
  # Transform 74 AOD mixtures data from a 3-D array into a data.table with 74 columns
  aod.74 = setDT(lapply(alply(ncvar_get(mycdf, var.list[38]), 1), as.vector))
  # Give the data table's columns names
  names(aod.74) = paste0('aod_mix_', sprintf('%02d', 1:74))
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  
  #### Get minimum chisq per mixture data from the NetCDF ####
  cat('- Minimum Chi-Square Mixtures......')
  start = Sys.time()
  # Transform 74 minimum chisq of mixtures data from a 3-D array into a data.table with 74 columns
  chisq.74 = setDT(lapply(alply(ncvar_get(mycdf, var.list[39]), 1), as.vector))
  # Give the data table's columns names
  names(chisq.74) = paste0('min_chisq_', sprintf('%02d', 1:74))
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  
  #### Get retrieval flags for mixture data from the NetCDF ####
  cat('- Success Flags Mixtures......')
  start = Sys.time()
  # Transform retrieval flag data from a 3-D array into a data.table with 74 columns
  flags.74 = setDT(lapply(alply(ncvar_get(mycdf, var.list[40]), 1), as.vector))
  # Give the data table's columns names
  names(flags.74) = paste0('flag_retrieve_', sprintf('%02d', 1:74))
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  
  #### Create a data table containing raw data and MISR product data
  cat('- Products & Raws......')
  start = Sys.time()
  # create data.table combining all relevant layers as columns
  tmp = data.table(
      path = path,
      # Coordinates + Elevation
      longitude = as.vector(ncvar_get(mycdf, var.list[5])),
      latitude = as.vector(ncvar_get(mycdf, var.list[4])),
      elevation = as.vector(ncvar_get(mycdf, var.list[6])),
      # Date and time
      year = as.vector(ncvar_get(mycdf, var.list[7])),
      month = as.vector(ncvar_get(mycdf, var.list[9])),
      day = as.vector(ncvar_get(mycdf, var.list[10])),
      hour = as.vector(ncvar_get(mycdf, var.list[11])),
      min = as.vector(ncvar_get(mycdf, var.list[12])),
      ## Layers for 4.4 km products
      AOD = as.vector(ncvar_get(mycdf, var.list[14])),
      AOD_uncertainty = as.vector(ncvar_get(mycdf, var.list[15])),
      angstrom_exp_550_860 = as.vector(ncvar_get(mycdf, var.list[16])),
      AOD_absorption = as.vector(ncvar_get(mycdf, var.list[18])),
      AOD_nonspherical = as.vector(ncvar_get(mycdf, var.list[19])),
      small_mode_AOD = as.vector(ncvar_get(mycdf, var.list[20])),
      medium_mode_AOD = as.vector(ncvar_get(mycdf, var.list[21])),
      large_mode_AOD = as.vector(ncvar_get(mycdf, var.list[22])),
      ## Layers from the auxiliary 4.4 km products. Primarily raw data
      AOD_raw = as.vector(ncvar_get(mycdf, var.list[24])),
      AOD_uncertainty_raw = as.vector(ncvar_get(mycdf, var.list[25])),
      angstrom_exp_550_860_raw = as.vector(ncvar_get(mycdf, var.list[26])),
      AOD_absorption_raw = as.vector(ncvar_get(mycdf, var.list[28])),
      AOD_nonspherical_raw = as.vector(ncvar_get(mycdf, var.list[29])),
      small_mode_AOD_raw = as.vector(ncvar_get(mycdf, var.list[30])),
      medium_mode_AOD_raw = as.vector(ncvar_get(mycdf, var.list[31])),
      large_mode_AOD_raw = as.vector(ncvar_get(mycdf, var.list[32])),
      aerosol_retrieval_conf_index = as.vector(ncvar_get(mycdf, var.list[37])),
      cloud_screen_parameter = as.vector(ncvar_get(mycdf, var.list[41])),
      cloud_screen_parameter_neighbour_3x3 = as.vector(ncvar_get(mycdf, var.list[42])),
      aerosol_retrieval_flag = as.vector(ncvar_get(mycdf, var.list[43])),
      column_ozone_climatology = as.vector(ncvar_get(mycdf, var.list[44])),
      ocean_surface_windspeed_climatology = as.vector(ncvar_get(mycdf, var.list[45])),
      ocean_surface_windspeed_retrieved = as.vector(ncvar_get(mycdf, var.list[46])),
      rayleigh_optical_depth = as.vector(ncvar_get(mycdf, var.list[47])),
      lowest_residual_mixture = as.vector(ncvar_get(mycdf, var.list[48])),
      ## Previously extracted layers containing 74 mixtures (AOD, minimum chisq, retrieval flags)
      aod.74, 
      chisq.74,
      flags.74
    )
  
  # Remove any rows with missing latitude/longitude values
  tmp = na.omit(tmp, cols = c('longitude', 'latitude'))
  
  # Close the opened NetCDF file, remove NetCDF file and tables of 74 mixtures to free up some memory
  nc_close(mycdf)
  rm(mycdf, aod.74, chisq.74, flags.74)
  gc()
  
  # Create a single date column from the year/month/day columns, and then remove those 3 columns
  tmp <- tmp %>% 
    mutate(date = paste(year, sprintf('%02d', month), sprintf('%02d', day), sep = '-')) %>%
    select(-c('year', 'month', 'day')) %>%
    relocate(date, .after = elevation)
  
  # Create a single time of day column from the hour+minute columns, and then remove those 2 columns
  tmp <- tmp %>% 
    mutate(time = paste(sprintf('%02d', hour), sprintf('%02d', min), '00', sep = ':')) %>%
    select(-c('hour', 'min')) %>%
    relocate(time, .after = date)
  
  
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  # If the data filtering setting is true, remove observations for any pixels with too much missing data
  if(filter.data){
    cat('- Filtering Missing Data......')
    start = Sys.time()
    # Count number of NA values per row, and remove any row with fewer than 10 non-NA values.
    tmp <- tmp %>% mutate(num_obs = rowSums(!is.na(tmp))) %>%
      filter(num_obs > 11) %>%
      select(-num_obs)
    cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  }
  
  # If the region filtering setting is true, check if each pixel is in the designated geographic region
  if(filter.region){
    cat('- Filtering pixels in selected region......')
    start = Sys.time()
    
    in.region <- tmp %>% 
      st_as_sf(coords = c('longitude','latitude'), crs = 4326, remove = F) %>%
      st_contains(x = region, y = .) %>% 
      unlist

    tmp <- tmp %>% filter(row_number() %in% in.region)
    cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  }

  # Return the dataset derived from the NetCDF file and the set of pixels, including new pixels added with this .nc file
  return(tmp)
}

# Read in a list of the NetCDF variable names (these are the same for all MISR NetCDF files)
varlist <- readRDS(file = paste0(getwd(), '/Data/MISR/NetCDF_variables.rds'))

#### Code to extract all relevant NetCDF files for each year (2000-2021) and combine them into one dataset for the whole year ####
for(year in 2006:2007){
  # Read in list of urls for MISR files to download from the OpenDAP server
  misr_urls <- readRDS(list.files(path = misr_urls.dir, pattern = paste0(year, '.rds'), full.names = T))
  
  misr_extracted <- vector("list", length = length(misr_urls))
  
  for(i in 1:length(misr_urls)){
    start = Sys.time()
    
    # Attempt to download the file, with a catch clause to avoid breaking the loop in case of error
    cat('Attempting to download file ', i, '.\n', sep = '')
    tryCatch({
      # Attempt to download the NetCDF file from the OpenDAP server
      new_filename = paste0(ncdf.dir, substr(misr_urls[i], 74, nchar(misr_urls[i])))
      download.file(misr_urls[i], new_filename, quiet = TRUE, method = "libcurl", mode = "wb")
      
      cat("File", i, "downloaded!\n")
      
      # Extract all pixels in the file which are located in California
      misr_extracted[[i]] = extract.ncdf(filename = new_filename, region = california, var.list = varlist, filter.data = F)
      
      # Remove the file when we're done with it
      file.remove(new_filename)
      cat("File", i, "deleted!\n")
    },
    error = function(cond){
      message(paste0('WARNING: File', i, 'failed to download.\n'))
    })
    
    cat("Total time taken:", round(difftime(Sys.time(), start, units = 'secs'), 2), 'seconds.\n\n')
  }
  
  misr_yearly <- do.call("rbind", misr_extracted)
  write.csv(misr_yearly, paste0(datasets.dir, 'MISR_Data_', year, '.csv'), row.names = F)
  cat("Year:", year, "\n")
  cat("Total Observations:", nrow(misr_yearly), "\n\n")
}
