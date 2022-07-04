#############
# Code to read, process, and reformat data stored in NetCDF files downloaded from NASA's MISR Level 2 Aerosol dataset
# The files are downloaded from NASA's OPeNDAP Hyrax server (https://opendap.larc.nasa.gov/opendap/jsp/index.jsp)
# Last updated: June 30, 2022
#############

library(ncdf4)
library(data.table)
library(dplyr) 
library(proj4) 

# set the working directory
setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

#### Geographic projection for California applied to lat/lon ####
proj_ca <- "+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"


#### Read MISR NetCDF (.nc) files ####

# Create list of NetCDF filenames
netcdf_list = list.files(path = './Data/MISR/NetCDF_Files', pattern = '.nc', full.names = TRUE)

# Create an empty list which will be populated with datasets created from each NetCDF file
misr_data_list <- vector('list', length(netcdf_list))

# For-loop to populate the empty list above by extracting data from the NetCDF files
for(i in 1:length(netcdf_list)){
  # Open the NetCDF file
  netcdf <- nc_open(netcdf_list[i])
  varlist <- names(netcdf$var)
  
  # Extract individual variables (columns) from the NetCDF as vectors, and create a table from these values
  misr_dataset <- data.table(
    latitude = as.vector(ncvar_get(netcdf, varlist[4])),
    longitude = as.vector(ncvar_get(netcdf, varlist[5])),
    year = as.vector(ncvar_get(netcdf, varlist[7])),
    month = as.vector(ncvar_get(netcdf, varlist[9])),
    day = as.vector(ncvar_get(netcdf, varlist[10])),
    hour = as.vector(ncvar_get(netcdf, varlist[11])),
    minute = as.vector(ncvar_get(netcdf, varlist[12])),
    AOD = as.vector(ncvar_get(netcdf, varlist[14])),
    AOD_uncertainty = as.vector(ncvar_get(netcdf, varlist[15])),
    AOD_absorption = as.vector(ncvar_get(netcdf, varlist[18])),
    AOD_nonspherical = as.vector(ncvar_get(netcdf, varlist[19])),
    small_mode_AOD = as.vector(ncvar_get(netcdf, varlist[20])),
    medium_mode_AOD = as.vector(ncvar_get(netcdf, varlist[21])),
    large_mode_AOD = as.vector(ncvar_get(netcdf, varlist[22])),
    path = as.numeric(substr(netcdf_list[i], 47, 49))
  )
  
  # Remove any rows which contain missing latitudes, longitudes or time variables, to ensure full spatiotemporal accuracy
  misr_dataset <- na.omit(misr_dataset, cols = c(1:7))
  
  misr_data_list[[i]] <- misr_dataset
}

# Bind all of the individual datasets together to create a larger dataset 
misr_all <- do.call("rbind", misr_data_list)




#### Collect data from the MISR 74 variable (real-valued part of the Spectral Refractive Index) ####
# This data is stored in a multidimensional array
# NOTE: This also includes AOD_raw (use with caution)

# Create an empty list which will be populated with MISR 74 data extracted from each NetCDF file
misr_74_list <- vector('list', length(netcdf_list))

# For-loop to populate the empty list above by extracting data from the NetCDF files
for(i in 1:length(netcdf_list)){
  # Open the NetCDF file
  netcdf <- nc_open(netcdf_list[i])
  varlist <- names(netcdf$var)
  
  # Get the MISR flight path corresponding to this file (part of the filename)
  path = as.numeric(substr(netcdf_list[i], 47, 49))
  
  # Get the date corresponding to this file
  year = unique(na.omit(as.vector(ncvar_get(netcdf, varlist[7]))))
  month = unique(na.omit(as.vector(ncvar_get(netcdf, varlist[9]))))
  day = unique(na.omit(as.vector(ncvar_get(netcdf, varlist[10]))))
  date = as.Date(paste(year, month, day, sep = "-"))
  
  # Get longitudes and latitudes for measured data
  longitude = ncvar_get(netcdf, varlist[5])
  latitude = ncvar_get(netcdf, varlist[4])
  lon_lat = data.table(lon = as.vector(longitude),
                       lat = as.vector(latitude))
  
  # Project latitude and longitude to an x-y grid
  proj_xy = data.table(project(as.matrix(lon_lat), proj = proj_ca))
  names(proj_xy) = c('x','y')
  
  # Get the Aerosol Optical Depth per Mixture data
  netcdf_aod_74 = ncvar_get(netcdf, varlist[38]) # extract 3-dim array w/ 74 mix
  
  # Extract individual mixture AOD values for each of the different 74 mixtures
  aod_mix_list = lapply(1:74, function(x) data.table(as.vector(netcdf_aod_74[x,,])))
  
  # Bind the columns together to create a 74-column dataframe
  aod_74 = bind_cols(aod_mix_list)
  names(aod_74) = paste0('aod_mix_',sprintf('%02d',1:74))
  
  # Add x-y projection, date, latitude, longitude, and general AOD to the new dataframe
  aod_74 = cbind(lon_lat, proj_xy, date = date, path = path,
                 aod = as.vector(ncvar_get(netcdf, varlist[14])),
                 aod_raw = as.vector(ncvar_get(netcdf, varlist[24])),
                 aod_74)
  
  # Remove any values with missing x-y projection data
  aod_74 = na.omit(aod_74, cols = c('x','y'))
  
  misr_74_list[[i]] <- aod_74
}

# Bind all of the individual datasets together to create a larger dataset 
misr_74_all <- do.call("rbind", misr_74_list)
