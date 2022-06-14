###################################################################################
# Author:      Ken Chau
# Description: Generate download URLs for MISR Level 2 Version 23 NetCDF files 
#              from their OpenDAP server.
#              This requires the accompanying data set ncdf_dpo.csv
# 13-Feb-19:   Current ncdf_dpo.csv is up to 2018-09-01, the latest available.
#              No new updates has been posted on OpenDAP since October 2018.
###################################################################################
require(data.table)
# character vector of repeated segments of download URLs
url.supp = c('https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL2ASAE.003/',
             '/MISR_AM1_AS_AEROSOL_P', '_O', '_F13_0023.nc')
# read in MISR date-path-orbit data
misr.dpo = fread('C:/Users/khang/OneDrive/USC/misr/data/misr_dpo.csv')
# cast date as Date class
misr.dpo$date = as.Date(misr.dpo$date)
# generate table of subset of interest based on dates & paths
target.dpo = misr.dpo[date >= '2012-01-01' & date <= '2014-01-01' &
                        path %in% c(40:45)]
# generate URLs for download
ncdf.urls = paste0(url.supp[1],  gsub('-','.',target.dpo$date),
                   url.supp[2], sprintf('%03d', target.dpo$path),
                   url.supp[3], sprintf('%06d', target.dpo$orbit),
                   url.supp[4])