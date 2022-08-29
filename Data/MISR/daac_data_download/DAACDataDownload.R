# ------------------------------------------------------------------------------------------------ #
# How to Access the LP DAAC Data Pool with R
# The following R code example demonstrates how to configure a connection to download data from an
# Earthdata Login enabled server, specifically the LP DAAC Data Pool.
# ------------------------------------------------------------------------------------------------ #
# Author: LP DAAC
# Last Updated: 08/29/2022 (for local use)
# ------------------------------------------------------------------------------------------------ #
# Check for required packages, install if not previously installed
if ("sys" %in% rownames(installed.packages()) == FALSE) {install.packages("sys")}
if ("httr" %in% rownames(installed.packages()) == FALSE) { install.packages("httr")}

# Load necessary packages into R
library(sys)
library(httr)
# ---------------------------------SET UP ENVIRONMENT--------------------------------------------- #

# Increase amount of time before a file download times out to 15 minutes
options(timeout = max(900, getOption("timeout")))

# Get directory names 
misr.dir = paste0(getwd(), '/Data/MISR/')
misr_urls.dir = paste0(misr.dir, '/MISR_urls/') # Folder containing urls for the NetCDF files to download
ncdf.dir = paste0(misr.dir, '/NetCDF_files/') # Folder to download NetCDF files into
daac.dir = paste0(misr.dir, '/daac_data_download/') # Folder containing DAAC downloader files

# IMPORTANT: Update the line below if you want to download to a different directory (ex: "c:/data/")
dl_dir <- ncdf.dir                                                # Set dir to download files to
setwd(dl_dir)                                                     # Set the working dir to the dl_dir
#usr <- file.path(Sys.getenv("USERPROFILE"))                      # Retrieve home dir (for netrc file)
#if (usr == "") {usr = Sys.getenv("HOME")}                        # If no user profile exists, use home
netrc <- file.path(daac.dir,'.netrc', fsep = .Platform$file.sep)  # Path to netrc file

# ------------------------------------CREATE .NETRC FILE------------------------------------------ #
# If you already have a .netrc file with your Earthdata Login credentials stored in your home
# directory, this portion will be skipped. Otherwise you will be prompted for your NASA Earthdata
# Login Username/Password and a netrc file will be created to store your credentials (in home dir)
if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) = FALSE) {
  netrc_conn <- file(netrc)

  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
  }

# ---------------------------CONNECT TO DATA POOL AND DOWNLOAD FILES------------------------------ #
# Below, define either a single link to a file for download, a list of links, or a text file
# containing links to the desired files to download. For a text file, there should be 1 file link
# listed per line. Here we show examples of each of the three ways to download files.
# **IMPORTANT: be sure to update the links for the specific files you are interested in downloading.

# 1. Single file (this is just an example link, replace with your desired file to download):
# files <- "https://e4ftl01.cr.usgs.gov/MOLA/MYD09GA.061/2002.07.06/MYD09GA.A2002187.h10v04.061.2020071193416.hdf"

# 2. List of files (these are just example links, replace with your desired files to download:
#files <- c("https://e4ftl01.cr.usgs.gov/MOLA/MYD09GA.061/2002.07.06/MYD09GA.A2002187.h10v04.061.2020071193416.hdf",
#           "https://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.061/2000.03.09/MOD11A1.A2000069.h00v08.061.2020043121122.hdf")

# 3. Textfile containing links (just an example, replace with your text file location):
#files <- readLines("C:/datapool_downloads/URL_file_list.txt", warn = FALSE)


# Read in list of urls for MISR files to download from the OpenDAP server

cat("Select a year to extract MISR data for.\n")
year = readLines(con = "stdin", n = 1)
year = as.integer(year)

files <- readRDS(list.files(path = misr_urls.dir, pattern = paste0(year, '.rds'), full.names = T))

success_counter = 0 # Count successful file downloads

# Loop through all files and download them
for (i in 1:length(files)){
  start = Sys.time()
  
  filename <-  tail(strsplit(files[i], '/')[[1]], n = 1) # Keep original filename

  # Write file to disk (authenticating with netrc) using the current directory/filename
  response <- GET(files[i], write_disk(filename, overwrite = TRUE), progress(),
                  config(netrc = TRUE, netrc_file = netrc), set_cookies("LC" = "cookies"))

  download.time <- round(difftime(Sys.time(), start, units = 'secs'), 2)
  
  # Check to see if file downloaded correctly
  if (response$status_code == 200) {
    success_counter <- success_counter + 1
    cat("\nFile", i, "successfully downloaded!\n")
    cat("Time taken:", round(difftime(Sys.time(), start, units = 'secs'), 2), 'seconds\n\n')
  } 
  else {
    cat("\nFile", i, "failed to download.\n\n")
  }
}

cat("Year:", year, "\n")
cat("Successful File Downloads:", success_counter, "\n")
cat("Unsuccessful File Downloads:", files - success_counter, "\n")
