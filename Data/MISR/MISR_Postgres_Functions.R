#############
# Defining functions which can copy MISR datasets from R into a SQL database using Postgres and the RPostgreSQL package
# Last updated: July 4, 2022
#############

require(sf)
require(dplyr)
require(ncdf4)
require(data.table)
require(stringr)
require(RPostgreSQL)


#### Function to extract relevant layers from MISR NetCDF files ####
extract.ncdf = function(files, file.index, region, var.list, pixels.list,
                        filter.data = T, filter.region = T){
  # Function Inputs: 
  # 
  # files          : Vector of file names
  # file.index     : Index of chosen file in the vector above
  # region         : An sf polygon representing a particular geographic region. Used for spatial filtering.
  # var.list       : vector of NetCDF layers' variable names
  # pixels.list    : data.table containing a current list of unique pixels' xy coordinates
  # filter.data    : Defaults to true. When true, remove pixels with at least a certain number of columns which are missing
  # filter.region  : Defaults to true. When true, filter to exclusively select pixels within the provided region
  
  
  # Open the chosen file
  mycdf = nc_open(files[file.index])
  
  # Find the MISR flight path from the NetCDF filename. Paths are 'stored' in the MISR NetCDF filename as 
  # 'P' followed by some (usually 3) numeric characters, representing the path number
  path = str_extract(files[file.index], pattern = "P[0-9]*")
  
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
  
  # Create a single datetime column from the year/month/day/hour/minute columns, and then remove those 5 columns
  tmp <- tmp %>% mutate(datetime = paste0(paste(year, sprintf('%02d', month), sprintf('%02d', day), sep = '-'), " ",
                                          paste(sprintf('%02d', hour), sprintf('%02d', min), '00', sep = ':'))) %>%
    select(-c('year', 'month', 'day', 'hour', 'min'))
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
    cat('- Filtering pixels in selected region...')
    start = Sys.time()
    pixels.sf = st_as_sf(tmp[, c('longitude', 'latitude')], coords = c('longitude', 'latitude'), crs = 4326)
    over.region = suppressMessages(st_intersects(pixels.sf, region, sparse = TRUE))
    tmp = tmp[sapply(over.region, length) > 0]
    cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  }
}