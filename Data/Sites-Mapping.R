#############
# Code to create an interactive map of EPA data collection sites
# Sites included are IMPROVE, CSN, and AQS data measurement locations
#############

library(tidyverse)

setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Load in CSVs containing information about sites for each dataset.
# The names of these sites' attributes will be edited slightly to allow these four datasets to match one another.
AQS_Sites <- read_csv("./Data/AQS Data/AQS_Data_Sites_2000_2021.csv") %>%
  rename(EPA.Code = Site.Code) %>%
  mutate(Dataset = "AQS", Site.Code = NA, Site.Name = NA)

CSN_Sites <- read_csv("./Data/CSN Data/CSN_Data_Sites_2000_2021.csv") %>%
  rename(EPA.Code = Site.Code) %>%
  mutate(Dataset = "CSN", Site.Code = NA, Site.Name = NA)

IMPROVE_Sites <- read_csv("./Data/IMPROVE Data/IMPROVE_Data_Sites_2000_2021.csv") %>%
  select(-c("EPACode")) %>%
  mutate(EPA.Code = paste(State.Code, County.Code, Site.Num, sep = "-"),
         Dataset = "IMPROVE") %>%
  rename(Site.Code = SiteCode, Site.Name = SiteName)

CASTNET_Sites <- read_csv("./Data/CASTNET Data/CASTNET_Data_Sites_2000_2022.csv") %>%
  mutate(EPA.Code = NA, Dataset = "CASTNET") %>%
  rename(Latitude = LATITUDE, Longitude = LONGITUDE, 
         Site.Code = SITE_ID, Site.Name = SITE_NAME)


# Collect only the attributes which we want on the map
AQS_small <- AQS_Sites %>%
  select(Latitude, Longitude, EPA.Code, Site.Code, Site.Name, Dataset)

CSN_small <- CSN_Sites %>%
  select(Latitude, Longitude, EPA.Code, Site.Code, Site.Name, Dataset)

IMPROVE_small <- IMPROVE_Sites %>%
  select(Latitude, Longitude, EPA.Code, Site.Code, Site.Name, Dataset)

CASTNET_small <- CASTNET_Sites %>%
  select(Latitude, Longitude, EPA.Code, Site.Code, Site.Name, Dataset)

# Merge the four different site info datasets together
Sites_Info <- rbind(AQS_small, CSN_small, IMPROVE_small, CASTNET_small)

# Remove sites with missing geographical location
Sites_Info <- Sites_Info %>%
  drop_na(Latitude, Longitude) %>%
  unique()

# Subset to sites in California and nearby states (based on latitude, longitude, and state borders)
Sites_Near_Cali <- Sites_Info %>%
  filter(Latitude <= 42) %>%
  filter(Latitude >= 31.33) %>%
  filter(Longitude <= -109.05) %>%
  filter(Longitude >= -124.43)

library(sf)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)

# Create an interactive map of all data sites

# Colour palette for the map
all_sites_pal <- colorFactor("Set1", levels(Sites_Info$Dataset))

All.Sites.Map <- leaflet(data = Sites_Info) %>% 
  addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, color= ~all_sites_pal(Dataset), opacity = 0.5,
                   radius = 7, popup = paste("Site Name:", Sites_Info$Site.Name, "<br>",
                                             "Site Code:", Sites_Info$Site.Code, "<br>",
                                             "EPA Site Code:", Sites_Info$EPA.Code, "<br>",
                                             "Dataset:", Sites_Info$Dataset)) %>%
  addLegend(position = "topright", pal = all_sites_pal, values = Sites_Info$Dataset,
            title = "Data Collected at Site")

saveWidget(All.Sites.Map, file="./Data/Sites-Map.html")

# Create an interactive map of data sites near California

# Colour palette for the map
cali_sites_pal <- colorFactor("Set1", levels(Sites_Near_Cali$Dataset))

Cali.Sites.Map <- leaflet(data = Sites_Near_Cali) %>% 
  addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, color= ~cali_sites_pal(Dataset), opacity = 0.5,
                   radius = 7, popup = paste("Site Name:", Sites_Near_Cali$Site.Name, "<br>",
                                             "Site Code:", Sites_Near_Cali$Site.Code, "<br>",
                                             "EPA Site Code:", Sites_Near_Cali$EPA.Code, "<br>",
                                             "Dataset:", Sites_Near_Cali$Dataset)) %>%
  addLegend(position = "topright", pal = cali_sites_pal, values = Sites_Near_Cali$Dataset,
            title = "Data Collected at Site")

saveWidget(Cali.Sites.Map, file="./Data/Sites-Near-California-Map.html")
