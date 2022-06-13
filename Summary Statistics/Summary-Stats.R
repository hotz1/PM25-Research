#############
# Code to create summary statistics for the AQS, CSN, CASTNET, and IMPROVE datasets 
# The main variables which we will be summarizing are PM2.5 masses, NO3, SO4, and Dust components
# The formula to compute dust mass is given by Dust = 2.2*Al + 2.49*Si + 1.63*Ca + 1.94*Ti + 2.42*Fe
# These four datasets were downloaded from online sources and cleaned using R code (in the "Data" folder of the repository)
#############

library(tidyverse)
library(dtplyr)
library(lubridate)

# Set working directory
setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Read in cleaned datasets
AQS_data <- read_csv("./Data/AQS Data/AQS_PM25_2000_2021_USA.csv")
AQS_CSN_merged <- read_csv("./Data/AQS-CSN Merging/AQS_CSN_Data_2000_2021.csv")

# Subset datasets to only include the "lower 48" states (and Washington D.C.)
AQS_lower48 <- AQS_data %>%
  filter(!(State %in% c("Alaska", "Canada", "Country Of Mexico", "Hawaii", "Puerto Rico", "Virgin Islands")))
CSN_lower48 <- AQS_CSN_merged %>% 
  filter(!(State %in% c("Alaska", "Canada", "Country Of Mexico", "Hawaii", "Puerto Rico", "Virgin Islands")))

# Mutate Date variables into Day, Week, Month, and Year variables
AQS_lower48 <- AQS_lower48 %>%
  mutate(Day = lubridate::day(Date),
         Week = lubridate::week(Date),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date))
CSN_lower48 <- CSN_lower48 %>%
  mutate(Day = lubridate::day(Date),
         Week = lubridate::week(Date),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date))

# Compute daily average of PM2.5 across the continental US
daily_mean_pm25_USA <- AQS_lower48 %>%
  select(Date, PM25) %>%
  group_by(Date) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute daily average of PM2.5 per state
daily_mean_pm25_statewide <- AQS_lower48 %>%
  select(Date, State, PM25) %>%
  group_by(Date, State) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute 'daily' average of nitrate across the continental US
daily_mean_nitrate_USA <- CSN_lower48 %>%
  select(Date, nitrate) %>%
  group_by(Date) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute 'daily' average of nitrate per state
daily_mean_nitrate_statewide <- CSN_lower48 %>%
  select(Date, State, nitrate) %>%
  group_by(Date, State) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute 'daily' average of sulfate across the continental US
daily_mean_sulfate_USA <- CSN_lower48 %>%
  select(Date, sulfate) %>%
  group_by(Date) %>%
  mutate(sulfate = mean(sulfate, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute 'daily' average of sulfate per state
daily_mean_sulfate_statewide <- CSN_lower48 %>%
  select(Date, State, sulfate) %>%
  group_by(Date, State) %>%
  mutate(sulfate = mean(sulfate, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute dust mass using the given dust mass formula (based on presence of dust components)
CSN_lower48 <- CSN_lower48 %>%
  mutate(Dust = 2.2*Al + 2.49*Si + 1.63*Ca + 1.94*Ti + 2.42*Fe)

# Compute 'daily' averages of dust components and dust mass across the continental US
daily_mean_dust_USA <- CSN_lower48 %>%
  select(Date, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Date) %>%
  mutate(Al = mean(Al),
         Si = mean(Si),
         Ca = mean(Ca),
         Ti = mean(Ti),
         Fe = mean(Fe),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute 'daily' averages of dust components and dust mass per state
daily_mean_dust_statewide <- CSN_lower48 %>%
  select(Date, State, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Date, State) %>%
  mutate(Al = mean(Al),
         Si = mean(Si),
         Ca = mean(Ca),
         Ti = mean(Ti),
         Fe = mean(Fe),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

library(plotly)
library(htmlwidgets)

# Create a time series plot of daily mean PM2.5 measurements across the continental U.S.
daily_pm25_USA_timeseries <- ggplot(data = daily_mean_pm25_USA, aes(x = Date, y = PM25)) +
  geom_line() +
  labs(x = "Date",
       y = "Mean Daily PM2.5",
       title = "Daily Mean PM2.5 measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_PM25_USA.png", plot = daily_pm25_USA_timeseries,
       device = "png", path = "./Summary Statistics")
