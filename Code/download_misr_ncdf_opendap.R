require(XML) 
require(ncdf4)
require(data.table)


setwd('/Users/meredith/Dropbox/MISR/MISR/Data/new')

# FUNCTIONS ---------------------------------------------------------------
# FUNCTION to download MISR pages for specified date range
dl.misr.urls = function(start, end, url.folder=''){
  # INPUTS
  # start     : character for beginning date
  # end       : character for ending date
  # url.folder: character for folder name where pages will be downloaded
  #             default is current folder
  # OUTPUT nothing
  
  require(downloader) # download()
  # create sequence of dates to generate HTML links (replace '-' with '.')
  url.dates = gsub('-', '.',
                   seq.Date(as.Date(start), as.Date(end), 'days'))
  # create vector of sequential URL links by date
  OPeNDAP.urls = paste0('https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL2ASAE.003/',
                        url.dates, '/contents.html')
  # loop to download OPeNDAP webpages (for scraping later on)
  if(nchar(url.folder) > 0){
    url.folder = paste0(url.folder, '/')
  }
  cat('Downloading:\n')
  downloaded = 0
  for(i in 1:length(OPeNDAP.urls)){
    tryCatch({
      cat(substr(OPeNDAP.urls[i],66,75), '...', sep=''); start = Sys.time()
      download(OPeNDAP.urls[i], 
               paste0(url.folder, 
                      substr(OPeNDAP.urls[i],66,75), '.contents.html'), quiet=T)
      downloaded = downloaded + 1
      cat(round(difftime(Sys.time(), start, units='secs'), 2), ' secs\n', sep='')
    }, error = function(cond){
      # in case the page does not exist, print date
      message('WARNING: Page does not exist!')
    })
  }
  cat('Finished:\n- Downloaded.......', downloaded,
      ' page(s)\n- Does not exist...', length(url.dates) - downloaded, 
      ' page(s)\n', sep='')
}

# FUNCTION TO COLLECT .nc URLs
grab.nc.url = function(url, paths = integer()){
  require(XML) # readHTMLTable(), htmlParse()
  ## INPUTS
  # url    : string of MISR page url
  # paths  : integer vector of paths of interest
  ## OUTPUT
  # nc.list: character vector of URLs for relevant .nc files
  
  # the html file names have dates in them, we want that
  date = substr(url, 11, 20)
  # reads html table of the OPeNDAP page, collects the first column (file names)
  nc.list = readHTMLTable(htmlParse(url),
                          stringsAsFactors=F)[[1]][,1]
  ## OPTIONAL: subset for .nc files from specified paths
  if(length(paths) > 0){
    nc.list = nc.list[substr(nc.list, 22, 24) %in% 
                        sprintf('%03d', paths) &
                        nchar(nc.list) == 44]
  } else{ # otherwise collects .nc files from all paths
    nc.list = nc.list[nchar(nc.list) == 44]
  }
  if(length(nc.list) == 0){
    # if vector is empty, returns it
    return(nc.list)
  } else{
    # otherwise, add the URL heading to complete URL for each NC file.
    return(paste0(
      'https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL2ASAE.003/',
      date, '/', nc.list))
  }
}

# Part 1 - Download MISR url pages ----------------------------------------
# make sure you have created a directory named misr_urls in your working path (to store urls)
dl.misr.urls(start='2019-01-01', end='2019-03-01', url.folder='misr_urls')

# Part 2 - Collect .nc links to download ----------------------------------
# Collect .nc links
# can change paths here
misr.urls = paste0('misr_urls/', list.files('misr_urls'))

# scrape links from MISR urls
system.time({
  ncdf.urls = 
    unlist(
      sapply(misr.urls[substr(misr.urls, 11, 14) == '2019'], 
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
indices = 1:length(misr.urls)
for(i in indices){
  cat('Downloading [', sprintf('%04d',i), '/', 
      sprintf('%04d', max(indices)), ']: ',
      substr(ncdf.urls[i],66,75),'...',sep='')
  start = Sys.time()
  tryCatch({
    # download .nc file
    download.file(ncdf.urls[i], 
                  paste0('ncdf/', 
                         substr(ncdf.urls[i], 77, nchar(ncdf.urls[i]))),
                  mode='wb', quiet=T)
    cat(round(difftime(Sys.time(), start, units = 'secs'), 2),
        ' secs\n', sep='')
  }, error = function(cond){
    # gives warning if failed, including file name
    message(paste0('\nWARNING: ', 
                   substr(ncdf.urls[i],77,nchar(ncdf.urls[i])), 
                   ' failed to download\n'))
  })
}
