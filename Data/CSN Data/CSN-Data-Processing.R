#############
# Code to extract data from EPA PM2.5 sites (Speciation)
# Data downloaded from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily - PM2.5 Speciation
# Metadata available at https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_daily_summary_files
#############

library(tidyverse)

# Get names and file locations of all the required CSV files
setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research/Data/CSN Data")
pm25.spec.files <- list.files(path = "./daily_SPEC/", pattern = "daily_SPEC_20*", full.names = TRUE)

# Create an empty list to store data from each file
pm25.spec.list <- vector('list', length(pm25.spec.files))

# Clean downloaded data files (one for each year 2000 - 2021)
for(i in 1:length(pm25.spec.files)) { 
  pm25.spec.data <- read_csv(pm25.spec.files[i])
  
  # Filter speciation data based on POC (POC = 5 is the marker for CSN data)
  pm25.spec.data <- pm25.spec.data %>%
    filter(`POC` == 5)
    
  # Create a site code variable from individual state, county, site numbers
  pm25.spec.data <- pm25.spec.data %>%
    mutate(Site.Code = paste(`State Code`, `County Code`, `Site Num`, sep = "-"))

  # Creates a table of daily mean temperatures and the site info 
  mean_temp <- pm25.spec.data %>%
    filter(`Parameter Code` == 68105) %>%
    select(`Mean.Temp` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()

  # Creates a table of daily min temperatures and the site info   
  min_temp <- pm25.spec.data %>%
    filter(`Parameter Code` == 68103) %>%
    select(`Min.Temp` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily max temperatures and the site info 
  max_temp <- pm25.spec.data %>%
    filter(`Parameter Code` == 68104) %>%
    select(`Max.Temp` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily atmospheric pressures and the site info 
  atm_pres <- pm25.spec.data %>%
    filter(`Parameter Code` == 68108) %>%
    select(`Atm.Press` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Ag (silver) concentrations and the site info 
  Ag <- pm25.spec.data %>%
    filter(`Parameter Code` == 88166) %>%
    select(`Ag` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily As (arsenic) concentrations and the site info 
  As <- pm25.spec.data %>%
    filter(`Parameter Code` == 88103) %>%
    select(`As` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Al (aluminum) concentrations and the site info 
  Al <- pm25.spec.data %>%
    filter(`Parameter Code` == 88104) %>%
    select(`Al` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Ba (barium) concentrations and the site info 
  Ba <- pm25.spec.data %>%
    filter(`Parameter Code` == 88107) %>%
    select(`Ba` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Br (bromine) concentrations and the site info 
  Br <- pm25.spec.data %>%
    filter(`Parameter Code` == 88109) %>%
    select(`Br` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Ca (calcium) concentrations and the site info 
  Ca <- pm25.spec.data %>%
    filter(`Parameter Code` == 88111) %>%
    select(`Ca` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Cr (chromium) concentrations and the site info 
  Cr <- pm25.spec.data %>%
    filter(`Parameter Code` == 88112) %>%
    select(`Cr` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Co (cobalt) concentrations and the site info 
  Co <- pm25.spec.data %>%
    filter(`Parameter Code` == 88113) %>%
    select(`Co` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Cu (copper) concentrations and the site info 
  Cu <- pm25.spec.data %>%
    filter(`Parameter Code` == 88114) %>%
    select(`Cu` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Cl (chlorine) concentrations and the site info 
  Cl <- pm25.spec.data %>%
    filter(`Parameter Code` == 88115) %>%
    select(`Cl` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Cd (cadmium) concentrations and the site info 
  Cd <- pm25.spec.data %>%
    filter(`Parameter Code` == 88110) %>%
    select(`Cd` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()  
  
  # Creates a table of daily Ce (cerium) concentrations and the site info 
  Ce <- pm25.spec.data %>%
    filter(`Parameter Code` == 88117) %>%
    select(`Ce` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()  
  
  # Creates a table of daily Cs (cesium) concentrations and the site info 
  Cs <- pm25.spec.data %>%
    filter(`Parameter Code` == 88118) %>%
    select(`Cs` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Fe (iron) concentrations and the site info 
  Fe <- pm25.spec.data %>%
    filter(`Parameter Code` == 88126) %>%
    select(`Fe` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Hg (mercury) concentrations and the site info 
  Hg <- pm25.spec.data %>%
    filter(`Parameter Code` == 88142) %>%
    select(`Hg` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily K (potassium) concentrations and the site info 
  K <- pm25.spec.data %>%
    filter(`Parameter Code` == 88180) %>%
    select(`K` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Mn (manganese) concentrations and the site info 
  Mn <- pm25.spec.data %>%
    filter(`Parameter Code` == 88132) %>%
    select(`Mn` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Mg (magnesium) concentrations and the site info 
  Mg <- pm25.spec.data %>%
    filter(`Parameter Code` == 88140) %>%
    select(`Mg` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Na (sodium) concentrations and the site info 
  Na <- pm25.spec.data %>%
    filter(`Parameter Code` == 88184) %>%
    select(`Na` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Ni (nickel) concentrations and the site info 
  Ni <- pm25.spec.data %>%
    filter(`Parameter Code` == 88136) %>%
    select(`Ni` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily P (phosphorus) concentrations and the site info 
  P <- pm25.spec.data %>%
    filter(`Parameter Code` == 88152) %>%
    select(`P` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Pb (lead) concentrations and the site info 
  Pb <- pm25.spec.data %>%
    filter(`Parameter Code` == 88128) %>%
    select(`Pb` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Rb (rubidium) concentrations and the site info 
  Rb <- pm25.spec.data %>%
    filter(`Parameter Code` == 88176) %>%
    select(`Rb` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily S (sulfur) concentrations and the site info 
  S <- pm25.spec.data %>%
    filter(`Parameter Code` == 88169) %>%
    select(`S` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Sb (antimony) concentrations and the site info 
  Sb <- pm25.spec.data %>%
    filter(`Parameter Code` == 88102) %>%
    select(`Sb` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Se (selenium) concentrations and the site info 
  Se <- pm25.spec.data %>%
    filter(`Parameter Code` == 88154) %>%
    select(`Se` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Si (silicon) concentrations and the site info 
  Si <- pm25.spec.data %>%
    filter(`Parameter Code` == 88165) %>%
    select(`Si` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Sn (tin) concentrations and the site info 
  Sn <- pm25.spec.data %>%
    filter(`Parameter Code` == 88160) %>%
    select(`Sn` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Sr (strontium) concentrations and the site info 
  Sr <- pm25.spec.data %>%
    filter(`Parameter Code` == 88168) %>%
    select(`Sr` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Ti (titanium) concentrations and the site info 
  Ti <- pm25.spec.data %>%
    filter(`Parameter Code` == 88161) %>%
    select(`Ti` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily V (vanadium) concentrations and the site info 
  V <- pm25.spec.data %>%
    filter(`Parameter Code` == 88164) %>%
    select(`V` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Zn (zinc) concentrations and the site info 
  Zn <- pm25.spec.data %>%
    filter(`Parameter Code` == 88167) %>%
    select(`Zn` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Zr (zirconium) concentrations and the site info 
  Zr <- pm25.spec.data %>%
    filter(`Parameter Code` == 88185) %>%
    select(`Zr` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily NH4+ (ammonium ion) concentrations and the site info 
  NH4 <- pm25.spec.data %>%
    filter(`Parameter Code` == 88301) %>%
    select(`NH4` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Cl- (chloride ion) concentrations and the site info 
  Cl_ion <- pm25.spec.data %>%
    filter(`Parameter Code` == 88203) %>%
    select(`Cl_ion` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily Na+ (sodium ion) concentrations and the site info 
  Na_ion <- pm25.spec.data %>%
    filter(`Parameter Code` == 88302) %>%
    select(`Na_ion` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily K+ (potassium ion) concentrations and the site info 
  K_ion <- pm25.spec.data %>%
    filter(`Parameter Code` == 88303) %>%
    select(`K_ion` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily NH3- (nitrate) concentrations and the site info 
  nitrate <- pm25.spec.data %>%
    filter(`Parameter Code` == 88306) %>%
    select(`nitrate` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily organic carbon concentrations and the site info 
  OC <- pm25.spec.data %>%
    filter(`Parameter Code` == 88320) %>%
    select(`OC` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily elemental carbon concentrations and the site info 
  EC <- pm25.spec.data %>%
    filter(`Parameter Code` == 88321) %>%
    select(`EC` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of **unadjusted** daily elemental carbon concentrations and the site info 
  EC2 <- pm25.spec.data %>%
    filter(`Parameter Code` == 88380) %>%
    select(`EC_unadjusted` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()
  
  # Creates a table of daily SO4 2- (sulfate) concentrations and the site info 
  sulfate <- pm25.spec.data %>%
    filter(`Parameter Code` == 88403) %>%
    select(`sulfate` = `Arithmetic Mean`, `POC`, `Date` = `Date Local`, `Latitude`, `Longitude`,
           `State` = `State Name`, `County` = `County Name`, `City` = `City Name`, `Site.Code`,
           `State.Code` = `State Code`, `County.Code` = `County Code`, Site.Num = `Site Num`) %>%
    distinct()

  # Join all the newly-created tables together
  joined.table <- Reduce(function(...) merge(..., all = TRUE),
                         list(mean_temp, min_temp, max_temp, atm_pres, Ag, As, Al, Ba, Br, Ca, Cr, Co, Cu, Cl, 
                              Cd, Ce, Cs, Fe, Hg, K, Mn, Mg, Na, Ni, P, Pb, Rb, S, Sb, Se, Si, Sn, Sr, Ti, V, 
                              Zn, Zr, NH4, Cl_ion, Na_ion, K_ion, nitrate, OC, EC, EC2, sulfate))
  
  pm25.spec.list[[i]] <- joined.table
}

# Merge tables together for all 22 years
pm25.spec.all <- do.call("rbind", pm25.spec.list)

# Select only California data
pm25.spec.cali <- pm25.spec.all %>% filter(State == 'California')

# Save the new tables as CSV files
write_csv(pm25.spec.all, "./CSN_PM25_SPEC_2000_2021_USA.csv")
write_csv(pm25.spec.cali, "./CSN_PM25_SPEC_2000_2021_Cali.csv")

# List all of the individual data sites for the CSN data
CSN.data.sites <- pm25.spec.all %>% 
  group_by(Latitude, Longitude, POC, State, County, City, Site.Code) %>%
  tally()

write_csv(CSN.data.sites, "./CSN_Data_Sites_2000_2021.csv")

library(sf)
library(leaflet)
library(htmlwidgets)

# Create an interactive map of CSN data sites
CSN.sites.map <- leaflet(data = CSN.data.sites) %>% 
  addTiles() %>%
  addCircleMarkers(data = CSN.data.sites, lng = ~Longitude, lat = ~Latitude, color = "red", opacity = 1) %>%
  addCircleMarkers(~Longitude, ~Latitude, color="blue", opacity = 0.5,
                   radius = 2, popup = paste("State:", CSN.data.sites$State, "<br>", 
                                             "County:", CSN.data.sites$County, "<br>",
                                             "City:", CSN.data.sites$City, "<br>",
                                             "Site Code:", CSN.data.sites$Site.Code, "<br>",
                                             "POC:", CSN.data.sites$POC, "<br>",
                                             "Total Observations:", CSN.data.sites$n))

saveWidget(CSN.sites.map, file="./CSN-Sites-Map.html")
