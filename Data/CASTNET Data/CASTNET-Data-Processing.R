#############
# Code to extract data from EPA PM2.5 sites (FRM/FEM)
# Data downloaded from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily - PM2.5 FRM/FEM Mass (88101)
# Metadata available at https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_daily_summary_files
#############

library(tidyverse)
library(sf) # Library needed for reading in KML files

setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research/Data/CASTNET Data")

my<- sf::st_read("./Active CASTNET Sites.kml")
plot(my[1])
