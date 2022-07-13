#############
# Code to create a list of pixels in the MISR dataset which are located in California.
# A 'pixel' consists of three components; a unique pixel ID, and the real-world latitude and longitude which the pixel represents.
# The pixel ID is a string of the form "P###_#######", where the first three numerals represent the MISR flightpath of the pixel
# and the following 7 numerals represent the individual pixel's ID along that flight path.
# Last updated: July 13, 2022
#############

install.packages(c("data.table", "downloader", "dplyr", "leaflet", "magrittr", "ncdf4", "sf", "stringr"))
require(data.table)
require(downloader)
require(dplyr)
require(leaflet)
require(magrittr)
require(ncdf4)
require(sf)
require(stringr)

# Increase amount of time before a file download times out to 5 minutes
options(timeout = max(300, getOption("timeout")))


# Get directory names 
misr_urls.dir = paste0(getwd(), '/Data/MISR/MISR_urls/') # Folder containing urls for the NetCDF files to download
ncdf.dir = paste0(getwd(), '/Data/MISR/NetCDF_files/') # Folder to download NetCDF files into

# Load in California shapefile
california <- st_read(paste0(getwd(), '/Data/ca-state-boundary/CA_State_TIGER2016.shp')) %>%
  st_transform(crs = 4326)


#### Function to get the list of pixels within a NetCDF file which are located inside of the given region ####
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
  
  # Create a table of pixels (path number + unique latitudes/longitudes)
  pixels = data.table(
    path = path,
    longitude = ncvar_get(mycdf, var.list[5]) %>% as.vector(),
    latitude = ncvar_get(mycdf, var.list[4]) %>% as.vector()) %>%
    na.omit
  
  # Get a list of pixels in the given region
  in.region <- pixels %>% 
    st_as_sf(coords = c('longitude','latitude'), crs = 4326, remove = F) %>%
    st_contains(x = region, y = .) %>% 
    unlist
  
  # Select only the pixels which are located in the given region
  pixels <- pixels %>% filter(row_number() %in% in.region)
  cat(length(in.region), 'pixels in the given region.\n')
  
  # Close and remove the NetCDF to prevent memory leakage and free up working memory
  nc_close(mycdf)
  rm(mycdf)
  gc()
  
  return(pixels.dt = pixels)
}


# Get lists (one for each year) of urls for the NetCDF files to download from the OpenDAP server
ncdf_urls_list <- list.files(misr_urls.dir, pattern = "*.rds", full.names = T)
ncdf.urls <- readRDS(ncdf_urls_list[1])


# An empty list which will be populated with individual lists of pixels in a region
pixels.list = vector("list", length = length(ncdf.urls))

for(i in 1:length(ncdf.urls)){
  start = Sys.time()
  
  # Attempt to download the file, with a catch clause to avoid breaking the loop in case of error
  cat('Attempting to download file ', i, '.\n', sep = '')
  tryCatch({
    # Attempt to download the NetCDF file from the OpenDAP server
    download.file(ncdf.urls[i], paste0(ncdf.dir, substr(ncdf.urls[i], 74, nchar(ncdf.urls[i]))),
                  quiet = TRUE, method = "libcurl", mode = "wb")
    cat("File", i, "downloaded!\n")
    
    # Extract all pixels in the file which are located in California
    pixels.list[[i]] = get_pixels_in_region(ncdf.dir, ncdf.file = substr(ncdf.urls[i], 74, nchar(ncdf.urls[i])),
                                            region = california)
    
    # Remove the file when we're done with it
    file.remove(paste0(ncdf.dir, substr(ncdf.urls[i], 74, nchar(ncdf.urls[i]))))
    cat("File", i, "deleted!\n")
  },
  error = function(cond){
      message(paste0('WARNING: File', i, 'failed to download.\n'))
  })
  
  cat("Time taken:", round(difftime(Sys.time(), start, units = 'secs'), 2), 'seconds.\n')
}

# Bind all the individual rows together into one larger dataset
all.pixels = do.call("rbind", pixels.list)

# Remove duplicate rows and then create unique pixel_id values for each lat-lon pair
all.pixels <- all.pixels %>% 
  unique() %>%
  group_by(path) %>%
  mutate(pixel_id = paste0(path, '_', sprintf('%06d', 1:n()))) %>%
  ungroup() %>%
  select(longitude, latitude, pixel_id)

# Save the list of pixels as a csv file
write.csv(all.pixels, paste0(getwd(), '/Data/MISR/cali_pixels.csv'), row.names = F)

# Create a map in leaflet (and save it) to verify that the pixels cover the desired region
cali_pixel_map <- leaflet(data = all.pixels) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude, opacity = 0.5, radius = 0.1, 
                   popup = paste("Pixel ID:", all.pixels$pixel_id, "<br>",
                                 "Longitude", all.pixels$longitude, "<br>",
                                 "Latitude:", all.pixels$latitude))
htmlwidgets::saveWidget(cali_pixel_map, paste0(getwd(), '/Data/MISR/cali_pixels_map.html'))
