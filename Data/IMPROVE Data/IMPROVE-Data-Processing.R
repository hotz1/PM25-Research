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

