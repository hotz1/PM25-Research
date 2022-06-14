#############
# Code to create summary statistics for the AQS, CSN, CASTNET, and IMPROVE datasets 
# The main variables which we will be summarizing are PM2.5 masses, NO3, SO4, and Dust components
# The formula to compute dust mass is given by Dust = 2.2*Al + 2.49*Si + 1.63*Ca + 1.94*Ti + 2.42*Fe
# These four datasets were downloaded from online sources and cleaned using R code (in the "Data" folder of the repository)
#############

library(tidyverse)
library(lubridate)
library(plotly)
library(htmlwidgets)

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

# Mutate Date variable into a datetime variable using the lubridate package
AQS_lower48 <- AQS_lower48 %>%
  mutate(Date = lubridate::as_date(Date))
CSN_lower48 <- CSN_lower48 %>%
  mutate(Date = lubridate::as_date(Date))

# Create variables representing the start of the week, month, and year for each respective day
# By convention, we will start weeks on Mondays, months on the 1st of the month, and years on January 1st
AQS_lower48 <- AQS_lower48 %>%
  mutate(Week_Start = lubridate::floor_date(Date, "week", week_start = getOption("lubridate.week.start", 1)),
         Month_Start = lubridate::floor_date(Date, "month"),
         Year_Start = lubridate::floor_date(Date, "year"))
CSN_lower48 <- CSN_lower48 %>%
  mutate(Week_Start = lubridate::floor_date(Date, "week", week_start = getOption("lubridate.week.start", 1)),
         Month_Start = lubridate::floor_date(Date, "month"),
         Year_Start = lubridate::floor_date(Date, "year"))




# Compute daily average of PM2.5 across the continental U.S.
daily_mean_pm25_USA <- AQS_lower48 %>%
  select(Date, PM25) %>%
  group_by(Date) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute weekly average of PM2.5 across the continental U.S.
weekly_mean_pm25_USA <- AQS_lower48 %>%
  select(Week_Start, PM25) %>%
  group_by(Week_Start) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute monthly average of PM2.5 across the continental U.S.
monthly_mean_pm25_USA <- AQS_lower48 %>%
  select(Month_Start, PM25) %>%
  group_by(Month_Start) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute yearly average of PM2.5 across the continental U.S.
yearly_mean_pm25_USA <- AQS_lower48 %>%
  select(Year_Start, PM25) %>%
  group_by(Year_Start) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Create a time series plot of daily mean PM2.5 measurements across the continental U.S.
daily_pm25_USA_plot <- ggplot(data = daily_mean_pm25_USA, aes(x = Date, y = PM25)) +
  geom_line() +
  labs(x = "Year",
       y = "Mean Daily PM2.5",
       title = "Daily mean PM2.5 measurements at AQS datasites in the continental U.S.",
       subtitle = "Measurements taken at AQS datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_PM25_USA.png", plot = daily_pm25_USA_plot,
       device = "png", path = "./Summary Statistics/PM 2.5 Plots")

# Create a time series plot of weekly mean PM2.5 measurements across the continental U.S.
weekly_pm25_USA_plot <- ggplot(data = weekly_mean_pm25_USA, aes(x = Week_Start, y = PM25)) +
  geom_line() +
  labs(x = "Year",
       y = "Mean Weekly PM2.5",
       title = "Weekly mean PM2.5 measurements at AQS datasites in the continental U.S.",
       subtitle = "Measurements taken at AQS datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Weekly_PM25_USA.png", plot = weekly_pm25_USA_plot,
       device = "png", path = "./Summary Statistics/PM 2.5 Plots")

# Create a time series plot of monthly mean PM2.5 measurements across the continental U.S.
monthly_pm25_USA_plot <- ggplot(data = monthly_mean_pm25_USA, aes(x = Month_Start, y = PM25)) +
  geom_line() +
  labs(x = "Year",
       y = "Mean Monthly PM2.5",
       title = "Monthly mean PM2.5 measurements at AQS datasites in the continental U.S.",
       subtitle = "Measurements taken at AQS datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Monthly_PM25_USA.png", plot = monthly_pm25_USA_plot,
       device = "png", path = "./Summary Statistics/PM 2.5 Plots")

# Create a time series plot of yearly mean PM2.5 measurements across the continental U.S.
yearly_pm25_USA_plot <- ggplot(data = yearly_mean_pm25_USA, aes(x = Year_Start, y = PM25)) +
  geom_line() +
  labs(x = "Year",
       y = "Mean Yearly PM2.5",
       title = "Yearly mean PM2.5 measurements at AQS datasites in the continental U.S.",
       subtitle = "Measurements taken at AQS datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Yearly_PM25_USA.png", plot = yearly_pm25_USA_plot,
       device = "png", path = "./Summary Statistics/PM 2.5 Plots")


# Compute daily average of PM2.5 per state
daily_mean_pm25_statewide <- AQS_lower48 %>%
  select(Date, State, PM25) %>%
  group_by(Date, State) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute weekly average of PM2.5 per state
weekly_mean_pm25_statewide <- AQS_lower48 %>%
  select(Week_Start, State, PM25) %>%
  group_by(Week_Start, State) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute monthly average of PM2.5 per state
monthly_mean_pm25_statewide <- AQS_lower48 %>%
  select(Month_Start, State, PM25) %>%
  group_by(Month_Start, State) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute yearly average of PM2.5 per state
yearly_mean_pm25_statewide <- AQS_lower48 %>%
  select(Year_Start, State, PM25) %>%
  group_by(Year_Start, State) %>%
  mutate(PM25 = mean(PM25, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Create a time series plot of daily mean PM2.5 measurements in California
daily_pm25_Cali_plot <- daily_mean_pm25_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Date, y = PM25)) +
  geom_line() +
  labs(x = "Year",
       y = "Mean Daily PM2.5",
       title = "Daily mean PM2.5 measurements at AQS datasites in California",
       subtitle = "Measurements taken at AQS datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_PM25_California.png", plot = daily_pm25_Cali_plot,
       device = "png", path = "./Summary Statistics/PM 2.5 Plots")

# Create a time series plot of weekly mean PM2.5 measurements in California
weekly_pm25_Cali_plot <- weekly_mean_pm25_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Week_Start, y = PM25)) +
  geom_line() +
  labs(x = "Year",
       y = "Mean Weekly PM2.5",
       title = "Weekly mean PM2.5 measurements at AQS datasites in California",
       subtitle = "Measurements taken at AQS datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Weekly_PM25_California.png", plot = weekly_pm25_Cali_plot,
       device = "png", path = "./Summary Statistics/PM 2.5 Plots")

# Create a time series plot of monthly mean PM2.5 measurements in California
monthly_pm25_Cali_plot <- monthly_mean_pm25_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Month_Start, y = PM25)) +
  geom_line() +
  labs(x = "Year",
       y = "Mean Monthly PM2.5",
       title = "Monthly mean PM2.5 measurements at AQS datasites in California",
       subtitle = "Measurements taken at AQS datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Monthly_PM25_California.png", plot = monthly_pm25_Cali_plot,
       device = "png", path = "./Summary Statistics/PM 2.5 Plots")

# Create a time series plot of yearly mean PM2.5 measurements in California
yearly_pm25_Cali_plot <- yearly_mean_pm25_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Year_Start, y = PM25)) +
  geom_line() +
  labs(x = "Year",
       y = "Mean Yearly PM2.5",
       title = "Yearly mean PM2.5 measurements at AQS datasites in California",
       subtitle = "Measurements taken at AQS datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Yearly_PM25_California.png", plot = yearly_pm25_Cali_plot,
       device = "png", path = "./Summary Statistics/PM 2.5 Plots")




# Compute dust mass using the given dust mass formula (based on presence of dust components)
CSN_lower48 <- CSN_lower48 %>%
  mutate(Dust = 2.2*Al + 2.49*Si + 1.63*Ca + 1.94*Ti + 2.42*Fe)

# Compute daily averages of nitrate, sulfate, dust components and dust mass across the continental U.S.
daily_mean_pollutants_USA <- CSN_lower48 %>%
  select(Date, nitrate, sulfate, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Date) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE),
         sulfate = mean(sulfate, na.rm = TRUE),
         Al = mean(Al, na.rm = TRUE),
         Si = mean(Si, na.rm = TRUE),
         Ca = mean(Ca, na.rm = TRUE),
         Ti = mean(Ti, na.rm = TRUE),
         Fe = mean(Fe, na.rm = TRUE),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute weekly averages of nitrate, sulfate, dust components and dust mass across the continental U.S.
weekly_mean_pollutants_USA <- CSN_lower48 %>%
  select(Week_Start, nitrate, sulfate, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Week_Start) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE),
         sulfate = mean(sulfate, na.rm = TRUE),
         Al = mean(Al, na.rm = TRUE),
         Si = mean(Si, na.rm = TRUE),
         Ca = mean(Ca, na.rm = TRUE),
         Ti = mean(Ti, na.rm = TRUE),
         Fe = mean(Fe, na.rm = TRUE),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute monthly averages of nitrate, sulfate, dust components and dust mass across the continental U.S.
monthly_mean_pollutants_USA <- CSN_lower48 %>%
  select(Month_Start, nitrate, sulfate, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Month_Start) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE),
         sulfate = mean(sulfate, na.rm = TRUE),
         Al = mean(Al, na.rm = TRUE),
         Si = mean(Si, na.rm = TRUE),
         Ca = mean(Ca, na.rm = TRUE),
         Ti = mean(Ti, na.rm = TRUE),
         Fe = mean(Fe, na.rm = TRUE),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute yearly averages of nitrate, sulfate, dust components and dust mass across the continental U.S.
yearly_mean_pollutants_USA <- CSN_lower48 %>%
  select(Year_Start, nitrate, sulfate, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Year_Start) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE),
         sulfate = mean(sulfate, na.rm = TRUE),
         Al = mean(Al, na.rm = TRUE),
         Si = mean(Si, na.rm = TRUE),
         Ca = mean(Ca, na.rm = TRUE),
         Ti = mean(Ti, na.rm = TRUE),
         Fe = mean(Fe, na.rm = TRUE),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Create a time series plot of daily mean nitrate measurements across the continental U.S.
daily_nitrate_USA_plot <- daily_mean_pollutants_USA %>%
  ggplot(aes(x = Date, y = nitrate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Daily Quantity of Nitrate",
       title = "Daily mean nitrate (NO3) measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_Nitrate_USA.png", plot = daily_nitrate_USA_plot,
       device = "png", path = "./Summary Statistics/Nitrate Plots")

# Create a time series plot of daily mean sulfate measurements across the continental U.S.
daily_sulfate_USA_plot <- daily_mean_pollutants_USA %>%
  ggplot(aes(x = Date, y = sulfate)) +
  geom_line(color = "darkblue") +
  labs(x = "Year",
       y = "Mean Daily Quantity of Sulfate",
       title = "Daily mean sulfate (SO4) measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_Sulfate_USA.png", plot = daily_sulfate_USA_plot,
       device = "png", path = "./Summary Statistics/Sulfate Plots")

# Create a time series plot of daily mean dust mass measurements across the continental U.S.
daily_dust_USA_plot <- daily_mean_pollutants_USA %>%
  ggplot(aes(x = Date, y = Dust)) +
  geom_line(color = "darkgrey") +
  labs(x = "Year",
       y = "Mean Daily Dust Mass",
       title = "Daily dust mass measured at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_DustMass_USA.png", plot = daily_dust_USA_plot,
       device = "png", path = "./Summary Statistics/Dust Mass Plots")

# Create a time series plot of weekly mean nitrate measurements across the continental U.S.
weekly_nitrate_USA_plot <- weekly_mean_pollutants_USA %>%
  ggplot(aes(x = Week_Start, y = nitrate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Weekly Quantity of Nitrate",
       title = "Weekly mean nitrate (NO3) measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Weekly_Nitrate_USA.png", plot = weekly_nitrate_USA_plot,
       device = "png", path = "./Summary Statistics/Nitrate Plots")

# Create a time series plot of weekly mean sulfate measurements across the continental U.S.
weekly_sulfate_USA_plot <- weekly_mean_pollutants_USA %>%
  ggplot(aes(x = Week_Start, y = sulfate)) +
  geom_line(color = "darkblue") +
  labs(x = "Year",
       y = "Mean Weekly Quantity of Sulfate",
       title = "Weekly mean sulfate (SO4) measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Weekly_Sulfate_USA.png", plot = weekly_sulfate_USA_plot,
       device = "png", path = "./Summary Statistics/Sulfate Plots")

# Create a time series plot of weekly mean dust mass measurements across the continental U.S.
weekly_dust_USA_plot <- weekly_mean_pollutants_USA %>%
  ggplot(aes(x = Week_Start, y = Dust)) +
  geom_line(color = "darkgrey") +
  labs(x = "Year",
       y = "Mean Weekly Dust Mass",
       title = "Weekly dust mass measured at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Weekly_DustMass_USA.png", plot = weekly_dust_USA_plot,
       device = "png", path = "./Summary Statistics/Dust Mass Plots")

# Create a time series plot of monthly mean nitrate measurements across the continental U.S.
monthly_nitrate_USA_plot <- monthly_mean_pollutants_USA %>%
  ggplot(aes(x = Month_Start, y = nitrate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Monthly Quantity of Nitrate",
       title = "Monthly mean nitrate (NO3) measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Monthly_Nitrate_USA.png", plot = monthly_nitrate_USA_plot,
       device = "png", path = "./Summary Statistics/Nitrate Plots")

# Create a time series plot of monthly mean sulfate measurements across the continental U.S.
monthly_sulfate_USA_plot <- monthly_mean_pollutants_USA %>%
  ggplot(aes(x = Month_Start, y = sulfate)) +
  geom_line(color = "darkblue") +
  labs(x = "Year",
       y = "Mean Monthly Quantity of Sulfate",
       title = "Monthly mean sulfate (SO4) measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Monthly_Sulfate_USA.png", plot = monthly_sulfate_USA_plot,
       device = "png", path = "./Summary Statistics/Sulfate Plots")

# Create a time series plot of monthly mean dust mass measurements across the continental U.S.
monthly_dust_USA_plot <- monthly_mean_pollutants_USA %>%
  ggplot(aes(x = Month_Start, y = Dust)) +
  geom_line(color = "darkgrey") +
  labs(x = "Year",
       y = "Mean Monthly Dust Mass",
       title = "Monthly dust mass measured at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Monthly_DustMass_USA.png", plot = monthly_dust_USA_plot,
       device = "png", path = "./Summary Statistics/Dust Mass Plots")

# Create a time series plot of yearly mean nitrate measurements across the continental U.S.
yearly_nitrate_USA_plot <- yearly_mean_pollutants_USA %>%
  ggplot(aes(x = Year_Start, y = nitrate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Yearly Quantity of Nitrate",
       title = "Yearly mean nitrate (NO3) measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Yearly_Nitrate_USA.png", plot = yearly_nitrate_USA_plot,
       device = "png", path = "./Summary Statistics/Nitrate Plots")

# Create a time series plot of yearly mean sulfate measurements across the continental U.S.
yearly_sulfate_USA_plot <- yearly_mean_pollutants_USA %>%
  ggplot(aes(x = Year_Start, y = sulfate)) +
  geom_line(color = "darkblue") +
  labs(x = "Year",
       y = "Mean Yearly Quantity of Sulfate",
       title = "Yearly mean sulfate (SO4) measurements at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Yearly_Sulfate_USA.png", plot = yearly_sulfate_USA_plot,
       device = "png", path = "./Summary Statistics/Sulfate Plots")

# Create a time series plot of yearly mean dust mass measurements across the continental U.S.
yearly_dust_USA_plot <- yearly_mean_pollutants_USA %>%
  ggplot(aes(x = Year_Start, y = Dust)) +
  geom_line(color = "darkgrey") +
  labs(x = "Year",
       y = "Mean Yearly Dust Mass",
       title = "Yearly dust mass measured at CSN datasites in the continental U.S.",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Yearly_DustMass_USA.png", plot = yearly_dust_USA_plot,
       device = "png", path = "./Summary Statistics/Dust Mass Plots")


# Compute daily averages of nitrate, sulfate, dust components and dust mass per state
daily_mean_pollutants_statewide <- CSN_lower48 %>%
  select(Date, State, nitrate, sulfate, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Date, State) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE),
         sulfate = mean(sulfate, na.rm = TRUE),
         Al = mean(Al, na.rm = TRUE),
         Si = mean(Si, na.rm = TRUE),
         Ca = mean(Ca, na.rm = TRUE),
         Ti = mean(Ti, na.rm = TRUE),
         Fe = mean(Fe, na.rm = TRUE),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute weekly averages of nitrate, sulfate, dust components and dust mass per state
weekly_mean_pollutants_statewide <- CSN_lower48 %>%
  select(Week_Start, State, nitrate, sulfate, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Week_Start, State) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE),
         sulfate = mean(sulfate, na.rm = TRUE),
         Al = mean(Al, na.rm = TRUE),
         Si = mean(Si, na.rm = TRUE),
         Ca = mean(Ca, na.rm = TRUE),
         Ti = mean(Ti, na.rm = TRUE),
         Fe = mean(Fe, na.rm = TRUE),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute monthly averages of nitrate, sulfate, dust components and dust mass per state
monthly_mean_pollutants_statewide <- CSN_lower48 %>%
  select(Month_Start, State, nitrate, sulfate, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Month_Start, State) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE),
         sulfate = mean(sulfate, na.rm = TRUE),
         Al = mean(Al, na.rm = TRUE),
         Si = mean(Si, na.rm = TRUE),
         Ca = mean(Ca, na.rm = TRUE),
         Ti = mean(Ti, na.rm = TRUE),
         Fe = mean(Fe, na.rm = TRUE),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()

# Compute yearly averages of nitrate, sulfate, dust components and dust mass per state
yearly_mean_pollutants_statewide <- CSN_lower48 %>%
  select(Year_Start, State, nitrate, sulfate, Al, Si, Ca, Ti, Fe, Dust) %>%
  group_by(Year_Start, State) %>%
  mutate(nitrate = mean(nitrate, na.rm = TRUE),
         sulfate = mean(sulfate, na.rm = TRUE),
         Al = mean(Al, na.rm = TRUE),
         Si = mean(Si, na.rm = TRUE),
         Ca = mean(Ca, na.rm = TRUE),
         Ti = mean(Ti, na.rm = TRUE),
         Fe = mean(Fe, na.rm = TRUE),
         Dust = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  unique()


# Create a time series plot of daily mean nitrate measurements in California
daily_nitrate_California_plot <- daily_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Date, y = nitrate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Daily Quantity of Nitrate",
       title = "Daily mean nitrate (NO3) measurements at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_Nitrate_California.png", plot = daily_nitrate_California_plot,
       device = "png", path = "./Summary Statistics/Nitrate Plots")

# Create a time series plot of daily mean sulfate measurements in California
daily_sulfate_California_plot <- daily_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Date, y = sulfate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Daily Quantity of Sulfate",
       title = "Daily mean sulfate (SO4) measurements at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_Sulfate_California.png", plot = daily_sulfate_California_plot,
       device = "png", path = "./Summary Statistics/Sulfate Plots")

# Create a time series plot of daily mean dust mass measurements in California
daily_dust_California_plot <- daily_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Date, y = Dust)) +
  geom_line(color = "darkgrey") +
  labs(x = "Year",
       y = "Mean Daily Dust Mass",
       title = "Daily dust mass measured at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Daily_DustMass_California.png", plot = daily_dust_California_plot,
       device = "png", path = "./Summary Statistics/Dust Mass Plots")

# Create a time series plot of weekly mean nitrate measurements in California
weekly_nitrate_California_plot <- weekly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Week_Start, y = nitrate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Weekly Quantity of Nitrate",
       title = "Weekly mean nitrate (NO3) measurements at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Weekly_Nitrate_California.png", plot = weekly_nitrate_California_plot,
       device = "png", path = "./Summary Statistics/Nitrate Plots")

# Create a time series plot of weekly mean sulfate measurements in California
weekly_sulfate_California_plot <- weekly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Week_Start, y = sulfate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Weekly Quantity of Sulfate",
       title = "Weekly mean sulfate (SO4) measurements at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Weekly_Sulfate_California.png", plot = weekly_sulfate_California_plot,
       device = "png", path = "./Summary Statistics/Sulfate Plots")

# Create a time series plot of weekly mean dust mass measurements in California
weekly_dust_California_plot <- weekly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Week_Start, y = Dust)) +
  geom_line(color = "darkgrey") +
  labs(x = "Year",
       y = "Mean Weekly Dust Mass",
       title = "Weekly dust mass measured at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Weekly_DustMass_California.png", plot = weekly_dust_California_plot,
       device = "png", path = "./Summary Statistics/Dust Mass Plots")

# Create a time series plot of monthly mean nitrate measurements in California
monthly_nitrate_California_plot <- monthly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Month_Start, y = nitrate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Monthly Quantity of Nitrate",
       title = "Monthly mean nitrate (NO3) measurements at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Monthly_Nitrate_California.png", plot = monthly_nitrate_California_plot,
       device = "png", path = "./Summary Statistics/Nitrate Plots")

# Create a time series plot of monthly mean sulfate measurements in California
monthly_sulfate_California_plot <- monthly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Month_Start, y = sulfate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Monthly Quantity of Sulfate",
       title = "Monthly mean sulfate (SO4) measurements at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Monthly_Sulfate_California.png", plot = monthly_sulfate_California_plot,
       device = "png", path = "./Summary Statistics/Sulfate Plots")

# Create a time series plot of monthly mean dust mass measurements in California
monthly_dust_California_plot <- monthly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Month_Start, y = Dust)) +
  geom_line(color = "darkgrey") +
  labs(x = "Year",
       y = "Mean Monthly Dust Mass",
       title = "Monthly dust mass measured at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Monthly_DustMass_California.png", plot = monthly_dust_California_plot,
       device = "png", path = "./Summary Statistics/Dust Mass Plots")

# Create a time series plot of yearly mean nitrate measurements in California
yearly_nitrate_California_plot <- yearly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Year_Start, y = nitrate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Yearly Quantity of Nitrate",
       title = "Yearly mean nitrate (NO3) measurements at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Yearly_Nitrate_California.png", plot = yearly_nitrate_California_plot,
       device = "png", path = "./Summary Statistics/Nitrate Plots")

# Create a time series plot of yearly mean sulfate measurements in California
yearly_sulfate_California_plot <- yearly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Year_Start, y = sulfate)) +
  geom_line(color = "darkred") +
  labs(x = "Year",
       y = "Mean Yearly Quantity of Sulfate",
       title = "Yearly mean sulfate (SO4) measurements at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Yearly_Sulfate_California.png", plot = yearly_sulfate_California_plot,
       device = "png", path = "./Summary Statistics/Sulfate Plots")

# Create a time series plot of yearly mean dust mass measurements in California
yearly_dust_California_plot <- yearly_mean_pollutants_statewide %>%
  filter(State == "California") %>%
  ggplot(aes(x = Year_Start, y = Dust)) +
  geom_line(color = "darkgrey") +
  labs(x = "Year",
       y = "Mean Yearly Dust Mass",
       title = "Yearly dust mass measured at CSN datasites in California",
       subtitle = "Measurements taken at CSN datasites from 2000-2021") +
  theme_bw()

# Save the plot created above as a PNG file
ggsave(filename = "Yearly_DustMass_California.png", plot = yearly_dust_California_plot,
       device = "png", path = "./Summary Statistics/Dust Mass Plots")




# Create a table of metadata for the data queried to compute monthly PM2.5 averages in California
# Metadata includes the mean PM2.5 during that month, and the total number of unique sites and observations
cali_monthly_queries_PM25 <- AQS_lower48 %>%
  filter(State == "California") %>%
  select(PM25, Month_Start, Site.Code) %>%
  group_by(Month_Start) %>%
  summarise(Unique_Sites = length(unique(Site.Code)),
            Total_Observations = length(Site.Code),
            Mean_PM25 = mean(PM25)) %>%
  ungroup() %>%
  mutate(Month = paste(lubridate::month(Month_Start, label = TRUE, abbr = FALSE),
                       lubridate::year(Month_Start))) %>%
  select(Month, Unique_Sites, Total_Observations, Mean_PM25)
  
# Save the table above as a csv file
write.csv(cali_monthly_queries_PM25, file = "./Summary Statistics/PM 2.5 Plots/California_Monthly_PM25_Table.csv")

# Create a table of metadata for the data queried to compute monthly nitrate averages in California
# Metadata includes the mean nitrate during that month, and the total number of unique sites and observations
cali_monthly_queries_nitrate <- CSN_lower48 %>%
  filter(State == "California") %>%
  select(nitrate, Month_Start, Site.Code) %>%
  group_by(Month_Start) %>%
  summarise(Unique_Sites = length(unique(Site.Code)),
            Total_Observations = length(Site.Code),
            Mean_Nitrate = mean(nitrate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Month = paste(lubridate::month(Month_Start, label = TRUE, abbr = FALSE),
                       lubridate::year(Month_Start))) %>%
  select(Month, Unique_Sites, Total_Observations, Mean_Nitrate)

# Save the table above as a csv file
write.csv(cali_monthly_queries_nitrate, file = "./Summary Statistics/Nitrate Plots/California_Monthly_Nitrate_Table.csv")

# Create a table of metadata for the data queried to compute monthly sulfate averages in California
# Metadata includes the mean sulfate during that month, and the total number of unique sites and observations
cali_monthly_queries_sulfate <- CSN_lower48 %>%
  filter(State == "California") %>%
  select(sulfate, Month_Start, Site.Code) %>%
  group_by(Month_Start) %>%
  summarise(Unique_Sites = length(unique(Site.Code)),
            Total_Observations = length(Site.Code),
            Mean_Sulfate = mean(sulfate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Month = paste(lubridate::month(Month_Start, label = TRUE, abbr = FALSE),
                       lubridate::year(Month_Start))) %>%
  select(Month, Unique_Sites, Total_Observations, Mean_Sulfate)

# Save the table above as a csv file
write.csv(cali_monthly_queries_sulfate, file = "./Summary Statistics/Sulfate Plots/California_Monthly_Sulfate_Table.csv")


# Create a table of metadata for the data queried to compute monthly dust mass averages in California
# Metadata includes the mean dust mass during that month, and the total number of unique sites and observations
cali_monthly_queries_dust_mass <- CSN_lower48 %>%
  filter(State == "California") %>%
  select(Dust, Month_Start, Site.Code) %>%
  group_by(Month_Start) %>%
  summarise(Unique_Sites = length(unique(Site.Code)),
            Total_Observations = length(Site.Code),
            Mean_Dust_Mass = mean(Dust, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Month = paste(lubridate::month(Month_Start, label = TRUE, abbr = FALSE),
                       lubridate::year(Month_Start))) %>%
  select(Month, Unique_Sites, Total_Observations, Mean_Dust_Mass)

# Save the table above as a csv file
write.csv(cali_monthly_queries_dust_mass, file = "./Summary Statistics/Dust Mass Plots/California_Monthly_Dust_Mass_Table.csv")
