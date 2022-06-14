######################################
# MISR local-mode pre-processed netcdf 
# variable extract and processing (California)
# Data create for Franklin et al RSE 2017
######################################

library(ncdf4)
library(data.table)
library(dplyr) 
library(proj4) 

#### Geographic projection for California applied to lat/lon ####
proj_ca<-"+proj=aea +lat_1=34.0 +lat_2=40.5 +lon_0=-120.0 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=km"

#### Read MISR .nc ####

# Create list of filenames in directory
nc_list = list.files(path='/Volumes/Projects/Satellite/MISR/ncdf', pattern='.nc', full.names = TRUE)

misr_data_list<-vector('list',length(nc_list))
for(i in 1:1) { #length(nc_list)
  mycdf = nc_open(nc_list[i])
  varlist = names(mycdf$var)
  
# create dataset for nc file with 4.4 KM PRODUCTS  
  misr.dat = na.omit(data.table(
    year = as.vector(ncvar_get(mycdf, varlist[7])),
    month = as.vector(ncvar_get(mycdf, varlist[9])),
    day = as.vector(ncvar_get(mycdf, varlist[10])),
    hour = as.vector(ncvar_get(mycdf, varlist[11])),
    minute = as.vector(ncvar_get(mycdf, varlist[12])),
    lon = as.vector(ncvar_get(mycdf, varlist[5])),
    lat = as.vector(ncvar_get(mycdf, varlist[4])),
    path = as.numeric(substr(nc.file, 27, 29)),
    aod = as.vector(ncvar_get(mycdf, varlist[14])),
    aod_uncertainty = as.vector(ncvar_get(mycdf, varlist[15])),
    absorption_aod = as.vector(ncvar_get(mycdf, varlist[18])),
    nonspherical_aod = as.vector(ncvar_get(mycdf, varlist[19])),
    small_mode_aod = as.vector(ncvar_get(mycdf, varlist[20])),
    medium_mode_aod = as.vector(ncvar_get(mycdf, varlist[21])),
    large_mode_aod = as.vector(ncvar_get(mycdf, varlist[22]))
  ), cols=1:7)
  
  misr_data_list[[i]]<-misr.dat
}  

misr_all<-do.call("rbind", misr_data_list)

# MISR 74 mixtures in multidimensional array
# NOTE this also includes AOD_raw (use with caution)
# this is the AOD retrieval that does not pass strict MISR cloud screen

misr_74_list<-vector('list',length(nc_list))

for(i in 1:1) { #length(nc_list)
  mycdf = nc_open(nc_list[i])
  varlist = names(mycdf$var)
  path = substr(nc_list[i], 
                nchar("/Volumes/Projects/Satellite/MISR/ncdf")+23,
                nchar("/Volumes/Projects/Satellite/MISR/ncdf")+25)
  origin_date = paste0(
    unique(na.omit(as.vector(ncvar_get(mycdf, varlist[7]))))-1,'-12-31')
  
  date = as.Date(unique(na.omit(as.vector(ncvar_get(mycdf, varlist[8])))),
                 origin=origin_date)
  
  # get lon-lat
  lon = ncvar_get(mycdf, varlist[5])
  lat = ncvar_get(mycdf, varlist[4])
  lon_lat = data.table(lon = as.vector(lon),
                       lat = as.vector(lat))
  # project to x-y
  proj_xy = data.table(project(as.matrix(lon_lat), proj=proj_ca))
  names(proj_xy) = c('x','y')
  
  # cbind to 74 mixtures + AOD
  nc_aod_74 = ncvar_get(mycdf, varlist[38]) # extract 3-dim array w/ 74 mix
  aod_mix_list = lapply(1:74, function(x) data.table(as.vector(nc_aod_74[x,,])))
  aod_74 = bind_cols(aod_mix_list)
  names(aod_74) = paste0('aod_mix_',sprintf('%02d',1:74))
  aod_74 = cbind(lon_lat, proj_xy, date=date, path=path,
                 aod = as.vector(ncvar_get(mycdf, varlist[14])),
                 aod_raw = as.vector(ncvar_get(mycdf, varlist[24])),
                 aod_74)
  aod_74 = na.omit(aod_74, cols=c('x','y'))
  
  misr_74_list[[i]] <- aod_74
               
}
misr_74_all<-do.call("rbind", misr_74_list)
  