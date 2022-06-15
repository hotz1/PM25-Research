#############
# Code to download NetCDF files from the MISR Level 2 Aerosol dataset
# The files are downloaded from NASA's OPeNDAP server (https://opendap.larc.nasa.gov/opendap/jsp/index.jsp)
# The dataset used is the MIL2ASAE_3 dataset (https://opendap.larc.nasa.gov/opendap/MISR/MIL2ASAE.003/contents.html)
# This dataset contains MISR Level 2 Aerosol parameters, and the dates range from March 1, 2000 until present.
#############

require(XML) 
require(ncdf4)
require(data.table)


setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# FUNCTIONS ---------------------------------------------------------------
# FUNCTION to download MISR pages for specified date range