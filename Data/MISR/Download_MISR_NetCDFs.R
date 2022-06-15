#############
# Code to download NetCDF files from NASA's MISR Level 2 Aerosol dataset
# The files are downloaded from NASA's OPeNDAP Hyrax server (https://opendap.larc.nasa.gov/opendap/jsp/index.jsp)
# The dataset used is the MIL2ASAE_3 dataset (https://opendap.larc.nasa.gov/opendap/hyrax/MISR/MIL2ASAE.003/contents.html)
# The dataset contains Level 2 Aerosol parameters, with dates ranging from March 1, 2000 until November 20, 2021 (as of June 15, 2022)
# Last updated: June 15, 2022
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
  date = substr(url, 23, 32)
  
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



# Part 1 - Download MISR url pages from NASA site ----------------------------------------
download.misr.urls(start = '2001-02-10', end = '2001-02-15', url.folder = 'Data/MISR/MISR_urls')



# Part 2 - Collect .nc links to download ----------------------------------
# Collect .nc links
# replace the file path which we call list.files on with the url folder set in Part 1 above
misr.urls = paste0('./Data/MISR/MISR_urls/', list.files('./Data/MISR/MISR_urls'))

# Scrape NetCDF file links from MISR urls and dates
system.time({
  ncdf.urls = unlist(sapply(misr.urls[substr(misr.urls, 23, 26) == '2001'],
                            function(x) grab.nc.url(x, paths = c(8:55))),
                     use.names = F)
})

# optional: export vector to RDS object if not downloading all at the same time
# saveRDS(ncdf.urls, 'ncdf_urls.rds')



# Part 3 - Download .nc files ---------------------------------------------
# optional: read vector from RDS object to resume downloading
# ncdf.urls = readRDS('ncdf_urls.rds')


# make sure you have created a directory named ndcf in your working path (to store ncdf files)
# indices of url vector

ncdf.folder = 'Data/MISR/NetCDF_files'
ncdf.folder = paste0(getwd(), '/', ncdf.folder, '/')

indices = 1:length(misr.urls)
for(i in indices){
  cat('Downloading [', sprintf('%05d', i), '/',
      sprintf('%05d', max(indices)), ']: ',
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
