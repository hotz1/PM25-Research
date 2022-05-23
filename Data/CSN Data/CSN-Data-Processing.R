#############
# Code to extract data from EPA PM2.5 sites (FRM)
# Data downloaded from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily Parameter code 88101
# Metadata available at https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_daily_summary_files
#############

library(tidyverse)


setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research/Data/CSN Data")
pm25.files <- list.files(path = "./daily_88101/", pattern = "daily_88101_20*", full.names = TRUE)

# Extract DAILY data Parameter Code 88101 (not speciation PM2.5)
pm25.list <- vector('list', length(pm25.files))

for(i in 1:length(pm25.files)) { 
  dat.pm25 <- read_csv(pm25.files[i])
  dat.pm25 <- dat.pm25 %>%
    select(PM25 = `Arithmetic Mean`, `POC`, Date = `Date Local`, `Latitude`, `Longitude`,
           State = `State Name`, County = `County Name`, State.Code = `State Code`,
           County.Code = `County Code`, Site.Num = `Site Num`)

  pm25.list[[i]] <- dat.pm25
}  

pm25.all <- do.call("rbind", pm25.list)

pm25.cali <- pm25.all %>% filter(State == "California")

write_csv(pm25.all, "./CSN_PM25_2000_2021_USA.csv")
write_csv(pm25.cali, "./CSN_PM25_2000_2021_Cali.csv")

counts.all <- pm25.all %>% group_by(Latitude, Longitude, State, County, Site.Num) %>% tally()

library(sf)
library(leaflet)
library(htmlwidgets)

m <- leaflet(data = counts.all) %>% 
  addTiles() %>%
  addCircleMarkers(data=counts.all, lng = ~Longitude, lat = ~Latitude, color = "red", opacity=1) %>%
  addCircleMarkers(~Longitude, ~Latitude, color="blue", opacity=0.5,
                   radius=2, popup = paste("State:", counts.all$State, "<br>", 
                                           "County:", counts.all$County, "<br>",
                                           "Site Number:", counts.all$Site.Num))

saveWidget(m, file="./PM25_CV_map.html")
