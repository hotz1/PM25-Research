#############
# Code to extract data from EPA PM2.5 sites (FRM/FEM)
# Data downloaded from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily - Parameter code 88101
# Metadata available at https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_daily_summary_files
#############

library(tidyverse)

setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research/Data/AQS Data")
pm25.files <- list.files(path = "./daily_88101/", pattern = "daily_88101_20*", full.names = TRUE)

# Extract DAILY data Parameter Code 88101 (not speciation PM2.5)
pm25.list <- vector('list', length(pm25.files))

# Merge downloaded data files (one for each year 2000 - 2021) into one large table
for(i in 1:length(pm25.files)) { 
  dat.pm25 <- read_csv(pm25.files[i])
  dat.pm25 <- dat.pm25 %>%
    select(PM25 = `Arithmetic Mean`, `POC`, Date = `Date Local`, `Latitude`, `Longitude`,
           State = `State Name`, County = `County Name`, State.Code = `State Code`,
           County.Code = `County Code`, Site.Num = `Site Num`)

  pm25.list[[i]] <- dat.pm25
}  

pm25.all <- do.call("rbind", pm25.list)

# Create a new variable for the unique site code, based on state, county, site codes.
pm25.all <- pm25.all %>%
  mutate(Site.Code = paste(State.Code, County.Code, Site.Num, sep = "-"))

# Select a subset of the table in California only
pm25.cali <- pm25.all %>% filter(State == "California")

# Save the new tables as CSV files
write_csv(pm25.all, "./AQS_PM25_2000_2021_USA.csv")
write_csv(pm25.cali, "./AQS_PM25_2000_2021_Cali.csv")

counts.all <- pm25.all %>% 
  group_by(Latitude, Longitude, State, County, Site.Code) %>%
  tally()

library(sf)
library(leaflet)
library(htmlwidgets)

m <- leaflet(data = counts.all) %>% 
  addTiles() %>%
  addCircleMarkers(data = counts.all, lng = ~Longitude, lat = ~Latitude, color = "red", opacity = 1) %>%
  addCircleMarkers(~Longitude, ~Latitude, color="blue", opacity = 0.5,
                   radius = 2, popup = paste("State:", counts.all$State, "<br>", 
                                             "County:", counts.all$County, "<br>",
                                             "Site Code:", counts.all$Site.Code, "<br>",
                                             "Total Observations:", counts.all$n))

saveWidget(m, file="./AQS-Sites-Map.html")
