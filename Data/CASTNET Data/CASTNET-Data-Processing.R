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

# Save the new table as a CSV files
write_csv(CASTNET_full, "./DATA/CASTNET Data/CASTNET_Air_Concentrations_2000_2022.csv")

# Select all of the individual data sites which have recorded observations in our CASTNET data
CASTNET_sites_observed <- CASTNET_sites %>% 
  filter(SITE_ID %in% CASTNET_full$SITE_ID)
