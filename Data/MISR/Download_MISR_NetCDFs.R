#############
# Code to download NetCDF files from NASA's MISR Level 2 Aerosol dataset
# The files are downloaded from NASA's OPeNDAP Hyrax server (https://opendap.larc.nasa.gov/opendap/jsp/index.jsp)
# The dataset used is the MIL2ASAE_3 dataset (https://opendap.larc.nasa.gov/opendap/hyrax/MISR/MIL2ASAE.003/contents.html)
# The dataset contains Level 2 Aerosol parameters, with dates ranging from March 1, 2000 until November 30, 2021 (as of June 20, 2022)
# Paths are chosen to encompass California based on the MISR paths site (https://l0dup05.larc.nasa.gov/cgi-bin/DUE/misr_loc/misr_loc.cgi)
# Last updated: June 20, 2022
#############

require(XML) 
require(ncdf4)
require(data.table)

setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# FUNCTIONS ---------------------------------------------------------------
# FUNCTION to download MISR pages for specified date range to a specific subfolder of the working directory
download.misr.urls = function(start, end, url.folder = ''){
  # INPUTS
  # start     : character for beginning date
  # end       : character for ending date
  # url.folder: character for folder name where pages will be downloaded
  #             default is current folder
  # OUTPUT nothing
  
  require(downloader) # download()
  
  # create sequence of dates between start and end dates (inclusive) and replace '-' with '.' to generate HTML links
  url.dates = gsub('-', '.', seq.Date(as.Date(start), as.Date(end), 'days'))
  
  # create vector of sequential URL links by date
  OPeNDAP.urls = paste0('https://opendap.larc.nasa.gov/opendap/hyrax/MISR/MIL2ASAE.003/',
                        url.dates, '/contents.html')
  
  # set the url folder to the full filepath where files will be downloaded into
  if(nchar(url.folder) > 0){
    url.folder = paste0(getwd(), '/', url.folder, '/')
  }
  else{
    url.folder = paste0(getwd(), '/')
  }
  
  # loop to download OPeNDAP webpages (for scraping later on)
  # For each day between the beginning and end dates, download files (if possible) from the NASA data site
  cat('Downloading:\n')
  downloaded = 0
  for(i in 1:length(OPeNDAP.urls)){
    tryCatch(
      {
        # download a file for a given day to the chosen subfolder in the directory
        cat(substr(OPeNDAP.urls[i], 63, 72), '...', sep = ''); start = Sys.time()
        download.file(OPeNDAP.urls[i], paste0(url.folder, substr(OPeNDAP.urls[i], 63, 72), '.contents.html'),
                      quiet = TRUE)
        downloaded = downloaded + 1
        # print total time taken to download the file (in seconds) 
        cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
      },
      # print a warning if the page could not be found
      error = function(cond) {
        # in case the page does not exist, print date
        message('WARNING: Page does not exist!')
      }
    )
  }
  # Summarize number of successful and failed downloads
  cat('Finished:\n',
      '- Downloaded.......', downloaded, ' page(s)\n',
      '- Does not exist...', length(url.dates) - downloaded, ' page(s)\n', sep = '')
}



# FUNCTION TO COLLECT .nc URLs
grab.nc.url = function(url, paths = integer()){
  require(XML) # readHTMLTable(), htmlParse()
  ## INPUTS
  # url    : string of MISR page url
  # paths  : integer vector of paths of interest
  
  ## OUTPUT
  # nc.list: character vector of URLs for relevant .nc files
  
  # collect the dates from the html file names
  date = substr(url, 34, 43)
  
  # reads html table of the OPeNDAP page, collects the first column (file names)
  nc.list = readHTMLTable(htmlParse(url), stringsAsFactors = FALSE)[[1]][,1]
  
  ## OPTIONAL: subset for .nc files from specified paths
  if(length(paths) > 0){
    nc.list = nc.list[substr(nc.list, 22, 24) %in% sprintf('%03d', paths) & nchar(nc.list) == 44]
  }
  else{ 
    # otherwise collects .nc files from all paths
    nc.list = nc.list[nchar(nc.list) == 44]
  }
  
  if(length(nc.list) == 0){
    # if vector is empty, returns it
    return(nc.list)
  } 
  else{
    # otherwise, add the URL heading to complete URL for each NC file.
    return(paste0('https://opendap.larc.nasa.gov/opendap/hyrax/MISR/MIL2ASAE.003/', 
                  date, '/', nc.list))
  }
}



# For loop to download and scrape html files for each year in order to collect the NetCDF file links for that year
# This loop downloads files into a temporary file folder, reads data from these files, and deletes them after they are used.

for(year in 2000:2021){
  # Part 1 - Download MISR url pages from NASA site ----------------------------------------
  download.misr.urls(start = paste0(year, '-01-01'), end = paste0(year, '-12-31'),
                     url.folder = 'Data/MISR/MISR_urls/temp_files')
  
  # Part 2 - Collect .nc links to download ----------------------------------
  # Collect .nc links
  # replace the file path which we call list.files on with the url folder set in Part 1 above
  misr.urls = paste0('./Data/MISR/MISR_urls/temp_files/', list.files('./Data/MISR/MISR_urls/temp_files'))
  
  # Scrape NetCDF file links from MISR urls and dates
  system.time({
    ncdf.urls = unlist(sapply(misr.urls[substr(misr.urls, 34, 37) == year],
                              function(x) grab.nc.url(x, paths = c(36:48))),
                       use.names = F)
  })
  
  # Export vector of NetCDF links to RDS object to keep the download links for usage at a later time
  saveRDS(ncdf.urls, paste0('./Data/MISR/MISR_urls/ncdf_urls_', year, '.rds'))
  
  # Find the file paths of the downloaded files and remove them from the temporary file folder
  misr.filepaths = paste0(getwd(), '/Data/MISR/MISR_urls/temp_files')
  misr.filepaths = paste0(misr.filepaths, '/', list.files(misr.filepaths))
  file.remove(misr.filepaths)
  
  # Summarize number of NetCDF files found for a given year
  cat('Year: ', year, '\n',
      'Total Dates with Data: ', length(misr.filepaths), '\n',
      'Total NetCDF Files Found: ', length(ncdf.urls), '\n', sep = '')
}



# Part 3 - Download .nc files ---------------------------------------------

# Make sure you have created a directory named ndcf in your working path (to store ncdf files)

ncdf.folder = 'Data/MISR/NetCDF_files'
ncdf.folder = paste0(getwd(), '/', ncdf.folder, '/')

for(year in 2000:2021){
  # Read vector from RDS object to resume downloading
  ncdf.urls = readRDS(paste0('./Data/MISR/MISR_urls/ncdf_urls_', year, '.rds'))
  
  indices = 1:length(ncdf.urls)
  for(i in indices){
    cat('Downloading [', sprintf('%04d', i), '/',
        sprintf('%04d', max(indices)), ']: ',
        substr(ncdf.urls[i], 63, 72), '...', sep = '')
    
    start = Sys.time()
    tryCatch(
      {
        # download the NetCDF file (if possible)
        download.file(ncdf.urls[i], paste0(ncdf.folder, substr(ncdf.urls[i], 74, nchar(ncdf.urls[i]))),
                      quiet = TRUE)
        
        # print total time taken to download the file (in seconds) 
        cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
      },
      error = function(cond){
        # gives warning if failed, including file name
        message(paste0('\nWARNING: ', substr(ncdf.urls[i], 74, nchar(ncdf.urls[i])),
                       ' failed to download\n'))
      }
    )
  }
}
