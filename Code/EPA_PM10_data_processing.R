#############
# Code to extract data from EPA PM2.5 sites (FRM)
# Data downloaded from https://www.epa.gov/outdoor-air-quality-data/download-daily-data Parameter code 88101
#############

library(tidyverse)



pm25.files <- list.files("/Users/mf/Documents/AQS/PM10/CA/",pattern="CA_PM10_*",full.names=TRUE)

# Extract DAILY data Parameter Code 88101 (not speciation PM2.5)
pm25.list<-vector('list',length(pm25.files))
for(i in 1:length(pm25.files)) { 
  dat.pm25<-read.csv(pm25.files[i],stringsAsFactors=FALSE)
  names(dat.pm25)[names(dat.pm25) == 'Daily.Mean.PM2.5.Concentration'] <- 'PM25'
  names(dat.pm25)[names(dat.pm25) == 'SITE_LATITUDE'] <- 'latitude'
  names(dat.pm25)[names(dat.pm25) == 'SITE_LONGITUDE'] <- 'longitude'
  names(dat.pm25)[names(dat.pm25) == 'CBSA_NAME'] <- 'CBSA.Name'
  names(dat.pm25)[names(dat.pm25) == 'Date'] <- 'date'
  names(dat.pm25)[names(dat.pm25) == 'AQS_SITE_ID'] <- 'site.id' # two names for site id depending on year of data
  names(dat.pm25)[names(dat.pm25) == 'Site.ID'] <- 'site.id'
  # Subset to FRM sites
  #dat.pm25<-dat.pm25[dat.pm25$AQS_PARAMETER_CODE == 88101,]
  
  dat.pm25<-dat.pm25[dat.pm25$AQS_PARAMETER_CODE == 88101 & dat.pm25$POC == 1, ] 
  dat.pm25<-dat.pm25[ ,which(names(dat.pm25) %in% c("CBSA.Name","site.id","POC","PM25","date","latitude","longitude"))]
  
  pm25.list[[i]]<-dat.pm25
}  

pm25.ca<-do.call("rbind", pm25.list)

# subset to central valley sites 
pm25.ca.cv<-pm25.ca[pm25.ca$CBSA.Name %in% c("Fresno, CA","Modesto, CA","Visalia-Porterville, CA","Bakersfield, CA"),]
write.csv(pm25.ca.cv,"/Users/mf/Documents/AQS/STN/processed/CentralValley_PM25_2000_2018.csv",row.names = FALSE)

counts<-pm25.ca.cv %>% group_by(CBSA.Name,site.id,POC, latitude, longitude) %>% tally()

library(sf)
library(leaflet)
library(htmlwidgets)

#add STN sites
stn.ca.cv<- read.csv("/Users/mf/Documents/AQS/STN/processed/CentralValley_CSN_2000_2018_v2.csv")
counts.stn<-stn.ca.cv %>% group_by(CBSA.Name, Latitude, Longitude) %>% tally()


m <- leaflet(data = counts) %>% addTiles() %>%
  addCircleMarkers(data=counts.stn, lng=~Longitude, lat=~Latitude,color = "red",opacity=1)  %>%
  addCircleMarkers(~longitude, ~latitude, color="blue", opacity=0.5,radius=2,popup = paste("City",counts$CBSA.Name, "<br>", "Site #", counts$site.id))


saveWidget(m, file="/Users/mf/Documents/AQS/STN/processed/PM25_CV_map.html")
