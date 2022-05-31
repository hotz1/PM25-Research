#############
# Code to extract data from IMPROVE dataset
# Data was queried on http://views.cira.colostate.edu/fed/QueryWizard/Default.aspx
# The dataset (downloaded as an Excel workbook) contains the data and the metadata
#############

library(tidyverse)
library(readxl)

setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research/Data/IMPROVE Data")

# Load in IMPROVE data
IMPROVE_raw <- readxl::read_excel(path = "./IMPROVE_Raw_Data_2000_2021.xlsx", sheet = 1)

# Load in IMPROVE site metadata
IMPROVE_metadata <- readxl::read_excel(path = "./IMPROVE_Raw_Data_2000_2021.xlsx", sheet = 3)

# List all of the individual data sites for the IMPROVE data
IMPROVE.Data.Sites <- IMPROVE_raw %>% 
  group_by(Latitude, Longitude, POC, SiteName, SiteCode, EPACode) %>%
  tally()

# Split the EPA code into state, county, site codes
IMPROVE.Data.Sites <- IMPROVE.Data.Sites %>%
  mutate(State.Code = substr(EPACode, 0, 2),
         County.Code = substr(EPACode, 3, 5),
         Site.Num = substr(EPACode, 6, 9))

write_csv(IMPROVE.Data.Sites, "./IMPROVE_Data_Sites_2000_2021.csv")

library(sf)
library(leaflet)
library(htmlwidgets)

# Create an interactive map of IMPROVE data sites
IMPROVE.sites.map <- leaflet(data = IMPROVE.Data.Sites) %>% 
  addTiles() %>%
  addCircleMarkers(data = IMPROVE.Data.Sites, lng = ~Longitude, lat = ~Latitude, color = "red", opacity = 1) %>%
  addCircleMarkers(~Longitude, ~Latitude, color="blue", opacity = 0.5,
                   radius = 2, popup = paste("Site Name:", IMPROVE.Data.Sites$SiteName, "<br>", 
                                             "Site Code:", IMPROVE.Data.Sites$SiteCode, "<br>",
                                             "EPA Site Code:", paste(IMPROVE.Data.Sites$State.Code,
                                                                     IMPROVE.Data.Sites$County.Code,
                                                                     IMPROVE.Data.Sites$Site.Num,
                                                                     sep = '-'), "<br>",
                                             "POC:", IMPROVE.Data.Sites$POC, "<br>",
                                             "Total Observations:", IMPROVE.Data.Sites$n))

saveWidget(IMPROVE.sites.map, file="./IMPROVE-Sites-Map.html")
