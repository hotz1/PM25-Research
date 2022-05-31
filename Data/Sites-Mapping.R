#############
# Code to create an interactive map of EPA data collection sites
# Sites included are IMPROVE, CSN, and AQS data measurement locations
#############

library(tidyverse)

setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research/Data")

# Load in CSVs containing information about sites for each dataset.
AQS_Sites <- read_csv("./AQS Data/AQS_Data_Sites_2000_2021.csv") %>%
  rename(EPA.Code = Site.Code) %>%
  mutate(Dataset = "AQS")

CSN_Sites <- read_csv("./CSN Data/CSN_Data_Sites_2000_2021.csv") %>%
  rename(EPA.Code = Site.Code) %>%
  mutate(Dataset = "CSN")

IMPROVE_Sites <- read_csv("./IMPROVE Data/IMPROVE_Data_Sites_2000_2021.csv") %>%
  select(-c("EPACode")) %>%
  mutate(EPA.Code = paste(State.Code, County.Code, Site.Num, sep = "-"),
         Dataset = "IMPROVE")


AQS_small <- AQS_Sites %>%
  select(Latitude, Longitude, EPA.Code, POC, Dataset)

CSN_small <- CSN_Sites %>%
  select(Latitude, Longitude, EPA.Code, POC, Dataset)

IMPROVE_small <- IMPROVE_Sites %>%
  select(Latitude, Longitude, EPA.Code, POC, Dataset)

Sites_Info <- rbind(AQS_small, CSN_small, IMPROVE_small)

Sites_Info <- Sites_Info %>%
  mutate(Dataset = as.factor(Dataset))

library(sf)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)

# Create an interactive map of all data sites

# Colour palette for the map
sites_pal <- colorFactor("Set1", levels(Sites_Info$Dataset))

Sites.Map <- leaflet(data = Sites_Info) %>% 
  addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, color= ~sites_pal(Dataset), opacity = 0.5,
                   radius = 10, popup = paste("EPA Site Code:", Sites_Info$EPA.Code, "<br>",
                                              "POC:", Sites_Info$POC, "<br>",
                                              "Dataset:", Sites_Info$Dataset)) %>%
  addLegend(position = "topright", pal = sites_pal, values = Sites_Info$Dataset,
            title = "Data Collected at Site")

saveWidget(Sites.Map, file="./Sites-Map.html")
