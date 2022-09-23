#############
# An R script to create models which predict certain values (Nitrate, Sulfate, PM2.5, Dust Mass, etc.)
# based on the MISR data which was matched with CSN/AQS data from 2000-2022.
# Last updated: September 22, 2022
#############

library(tidyverse)
library(data.table)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Read in MISR data which has been spatially (and date) matched with AQS/CSN datasets
MISR_AQS <- read_csv(paste0(getwd(), "/Data/MISR/MISR_merged_data/MISR_AQS_Matched.csv"), show_col_types = FALSE)
MISR_CSN <- read_csv(paste0(getwd(), "/Data/MISR/MISR_merged_data/MISR_CSN_Matched.csv"), show_col_types = FALSE)

# Select PM2.5 + 8 AOD types from the matched MISR+AQS dataset
PM25_AOD <- MISR_AQS %>%
  select(PM25, Date, c(12:19))

# Select PM2.5 + 74 mixtures from the matched MISR+AQS dataset
PM25_Mixtures <- MISR_AQS %>%
  select(PM25, Date, c(37:110))



# Create correlation heatmap for 74 mixtures (from the merged MISR-AQS dataset)
mixtures <- MISR_AQS %>% 
  select(c(37:110)) %>%
  drop_na()

mixtures_corr <- round(cor(mixtures), 3)
