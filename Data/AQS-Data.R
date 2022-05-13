library(tidyverse)
library(dplyr)
library(data.table)
library(dtplyr)
library(httr)
library(rjson)
library(jsonlite)
library(lubridate)

# R script to collect data from the EPA's Air Quality Services (AQS) website using their API
# Information about the AQS API: https://aqs.epa.gov/aqsweb/documents/data_api.html

# My personal login info for the AQS API
api_user <- "joey.hotz@mail.utoronto.ca"
api_key <- "russetfrog31"

# Retrieve data on codes for each US state
states_json <- jsonlite::fromJSON(paste0("https://aqs.epa.gov/data/api/list/states?email=", api_user, "&key=", api_key))
states_table <- states_json$Data



# Codes for the IMPROVE data available through the AQS API
IMPROVE_param_codes_json <- jsonlite::fromJSON(paste0("https://aqs.epa.gov/data/api/list/parametersByClass?email=",
                                                      api_user, "&key=", api_key, "&pc=IMPROVE_SPECIATION"))
IMPROVE_param_codes <- IMPROVE_param_codes_json$Data


IMPROVE_data_json <- jsonlite::fromJSON(paste0("https://aqs.epa.gov/data/api/dailyData/byState?email=", api_user,
                                               "&key=", api_key, "&param=88104,&bdate=20010229&edate=20010229&state=06"))
IMPROVE_data1 <- IMPROVE_data_json$Data

