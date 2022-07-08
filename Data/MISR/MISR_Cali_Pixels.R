#############
# Code to create a list of pixels in the MISR dataset which are located in California.
# A 'pixel' consists of three components; a unique pixel ID, and the real-world latitude and longitude which the pixel represents.
# The pixel ID is a string of the form "P###_#######", where the first three numerals represent the MISR flightpath of the pixel
# and the following 7 numerals represent the individual pixel's ID along that flight path.
# Last updated: July 8, 2022
#############

require(data.table)
require(downloader)
require(dplyr)
require(leaflet)
require(magrittr)
require(ncdf4)
require(sf)
require(stringr)

# Load in California shapefile
california <- st_read(paste0(getwd(), '/Data/ca-state-boundary/CA_State_TIGER2016.shp'))

misr_urls.dir = paste0(getwd(), '/Data/MISR/MISR_urls/') # Folder containing urls for the NetCDF files to download
ncdf.dir = paste0(getwd(), '/Data/MISR/NetCDF_files/') # Folder to download NetCDF files into

# Get lists of NetCDF urls to download from the OpenDAP server
ncdf_urls_list <- list.files(misr_urls.dir, pattern = "*.rds", full.names = T)

# ncdf.filenames = list.files(ncdf.dir, pattern = '.nc', full.names = F)


pixels.list = list()

#### Function to get the list of pixels in a NetCDF file which are in a certain given region ####
get_pixels_in_region <- function(ncdf.dir, ncdf.file, region){
  # Function Inputs: 
  # 
  # ncdf.dir       : Location of a folder within the working directory which contains NetCDF files
  # ncdf.file      : File name of a NetCDF file located in the ncdf.dir folder
  # region         : An sf polygon representing a particular geographic region. Used for spatial filtering.
  #
  #
  # Function Outputs:
  #
  # pixels.dt       : Data table of all unique pixels in the selected NetCDF file, containing pixel ids, latitudes and longitudes.
  
  
  mycdf = nc_open(paste0(ncdf.dir, ncdf.file)) # Open the NetCDF
  var.list = names(mycdf$var)
  
  # Extract the MISR flight path from the NetCDF filename
  path = str_extract(ncdf.file, pattern = "P[0-9]*") 
  cat("Path:", path, "\n")
  
  # Create a table of pixels (unique latitudes + longitudes)
  pixels = data.table(
    longitude = ncvar_get(mycdf, var.list[5]) %>% as.vector(),
    latitude = ncvar_get(mycdf, var.list[4]) %>% as.vector()) %>%
    na.omit
  
  # Get a list of pixels in the given region
  in.region <- pixels %>% 
    st_as_sf(coords = c('longitude','latitude'), crs = st_crs(region), remove = F) %>%
    st_contains(x = region, y = .) %>% 
    unlist
  
  cat(length(in.region), 'pixels in the given region.\n')
  
  if(length(in.region) > 0){
    pixels <- pixels %>%
      filter(row_number() %in% in.region) %>%
      mutate(pixel_id = paste0(path, '_', sprintf('%07d', 1:n()))) %>%
      select(pixel_id, longitude, latitude)
  }
  else{
    pixels <- data.table(longitude = numeric(), 
                         latitude = numeric(),
                         pixel_id = character())
  }
  
  # Close and remove the NetCDF to prevent memory leakage and free up working memory
  nc_close(mycdf)
  rm(mycdf)
  gc()
  
  return(pixels.dt = pixels)
}
