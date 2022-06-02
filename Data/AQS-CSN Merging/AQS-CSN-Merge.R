#############
# Code to merge the AQS and CSN data together based on dates + EPA site codes of measurements
# The AQS and CSN data have already been cleaned (in other folders in the repository)
#############

library(tidyverse)
library(dtplyr)

# Set working directory
setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Read in the cleaned + formatted AQS/CSN data 
AQS_data <- read_csv("./Data/AQS Data/AQS_PM25_2000_2021_USA.csv")
CSN_data <- read_csv("./Data/CSN Data/CSN_PM25_SPEC_2000_2021_USA.csv")

# Read in info about AQS, CSN measurement sites. 
# We remove the POC and n() values as we only care about geographic locations of these sites.
AQS_sites <- read_csv("./Data/AQS Data/AQS_Data_Sites_2000_2021.csv") %>%
  select(-c("POC", "n")) %>%
  distinct()
CSN_sites <- read_csv("./Data/CSN Data/CSN_Data_Sites_2000_2021.csv") %>%
  select(-c("POC", "n")) %>%
  distinct()

# Combine individual lists of sites to make a larger set
all.sites <- rbind(AQS_sites, CSN_sites) %>% 
  distinct()
# There are 2111 records in this larger list of sites, and 2108 unique EPA Site IDs, with 3 duplicated ones.

# Subset AQS and CSN data before joining them together
AQS_small <- AQS_data %>%
  select(Date, Site.Code, PM25)
CSN_small <- CSN_data %>%
  select(-c(POC, Latitude, Longitude, State, County, City, State.Code, County.Code, Site.Num))

# Merge the AQS and CSN datasets together. We will use an "inner join" to merge these tables
# so that only observations from sites+dates with both AQS and CSN data are kept in the merged table.
AQS_CSN_merge <- merge(AQS_small, CSN_small)
AQS_CSN_full <- merge(AQS_CSN_merge, all.sites)

# Save the merged table as a new csv file
write_csv(AQS_CSN_full, "./Data/AQS-CSN Merging/AQS_CSN_Data_2000_2021.csv")

# List all of the individual data sites for the merged AQS+CSN data
AQS_CSN_sites <- AQS_CSN_full %>% 
  group_by(Latitude, Longitude, State, County, City, Site.Code) %>%
  tally()

write_csv(AQS_CSN_sites, "./Data/AQS-CSN Merging/AQS_CSN_Data_Sites.csv")
