#############
# Code to read, process, and reformat data stored in NetCDF files downloaded from NASA's MISR Level 2 Aerosol dataset
# The files are downloaded from NASA's OPeNDAP Hyrax server (https://opendap.larc.nasa.gov/opendap/jsp/index.jsp)
# Last updated: June 27, 2022
#############

library(ncdf4)
library(data.table)
library(dplyr) 
library(proj4) 

# set the working directory
setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

#### Geographic projection for California applied to lat/lon ####
proj_ca<- "+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"


#### Read MISR NetCDF (.nc) files ####

# Create list of NetCDF filenames
netcdf_list = list.files(path = './Data/MISR/NetCDF_Files', pattern = '.nc', full.names = TRUE)

# Create an empty list which will be populated with data extracted from each NetCDF file
misr_data_list <- vector('list', length(netcdf_list))

