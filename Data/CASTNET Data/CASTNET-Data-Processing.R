#############
# Code to compile CASTNET air quality data from https://java.epa.gov/castnet/clearsession.do
# Air Quality Data retrieved as a custom report: "Measurement (Raw Data)", "Filter Pack Concentrations (Weekly)"
# Site Metadata retrieved as a custom report: "Factual Data", "Site"
#############

library(tidyverse)

# Set working directory
setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Load in the necessary data
CASTNET_raw_data <- read_csv("./Data/CASTNET Data/CASTNET Weekly Concentrations/Filter Pack Concentrations - Weekly.csv")
CASTNET_sites <- read_csv("./Data/CASTNET Data/CASTNET Sites/Site.csv")

# Join together collected CASTNET data and the corresponding site info
# We renamed the UPDATE_DATE parameter for recorded weekly data to "OBS_UPDATE_DATE" and
# UPDATE_DATE parameter for the site metadata to "SITE_UPDATE_DATE"
CASTNET_full <- merge(CASTNET_raw_data, CASTNET_sites, by = c('SITE_ID')) %>%
  rename(`OBS_UPDATE_DATE` = `UPDATE_DATE.x`, `SITE_UPDATE_DATE` = `UPDATE_DATE.y`)

# Save the new table as a CSV file
write_csv(CASTNET_full, "./Data/CASTNET Data/CASTNET_Air_Concentrations_2000_2022.csv")

# List all of the individual data sites for the CSN data
CASTNET_data_sites <- CASTNET_full %>% 
  group_by(SITE_ID, SITE_NUM, SITE_NAME, LATITUDE, LONGITUDE, STATE, COUNTY, AGENCY) %>%
  tally()

# Save the list of "used" CASTNET sites as CSV file
write_csv(CASTNET_data_sites, "./Data/CASTNET Data/CASTNET_Data_Sites_2000_2022.csv")

library(sf)
library(leaflet)
library(htmlwidgets)

# Create an interactive map of the CASTNET data sites

# Colour palette for the map
sites_pal <- colorFactor("Set1", levels(CASTNET_data_sites$AGENCY))

# Create an interactive map of CASTNET data sites
CASTNET_sites_map <- leaflet(data = CASTNET_data_sites) %>% 
  addTiles() %>%
  addCircleMarkers(~LONGITUDE, ~LATITUDE, color= ~sites_pal(AGENCY), opacity = 0.5,
                   radius = 10, popup = paste("Site Name:", CASTNET_data_sites$SITE_NAME, "<br>",
                                              "Site ID:", CASTNET_data_sites$SITE_ID, "<br>",
                                              "State:", CASTNET_data_sites$STATE, "<br>",
                                              "County:", CASTNET_data_sites$COUNTY, "<br>",
                                              "Total Observations:", CASTNET_data_sites$n)) %>%
  addLegend(position = "topright", pal = sites_pal, values = CASTNET_data_sites$AGENCY,
            title = "Agency operating data collection site")

saveWidget(CASTNET_sites_map, file="./Data/CASTNET Data/CASTNET-Sites-Map.html")
