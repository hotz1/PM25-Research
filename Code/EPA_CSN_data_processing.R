#############
# Code to extract data from EPA speciation sites
# Data downloaded from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily
# Merge with PM2.5 FRM from https://www.epa.gov/outdoor-air-quality-data/download-daily-data 
# Notes: Bakersfield, CA 2014 speciation monitor was not functioning see https://www3.epa.gov/ttnamti1/files/ambient/pm25/spec/2014ADSReport.pdf
# Notes: After 2014, Fresno and Bakersfield PM2.5 moved to POC 3 (daily), had to merge
# Notes: Otherwise, POC = 5 for Parameter Code 88502 for PM2.5 was merged in with CSN data (mass not in CSN files)
# Notes: There are many missing OC, EC,Cl_ion,Cd,Ce,Cs,Mg,Na,Sm,Zr consider removing before analysis
#############

library(tidyverse)

stn.files <- list.files("/Users/mf/Documents/AQS/STN/raw",pattern="*.csv",full.names=TRUE)

# Extract data
stn.list<-vector('list',length(stn.files))
for(i in 1:length(stn.files)) {
  dat.stn<-read.csv(stn.files[i],stringsAsFactors=FALSE)
  dat.stn$State.Code<-as.numeric(dat.stn$State.Code)
  dat.stn$POC<-as.numeric(dat.stn$POC)
  dat.stn$Concentration<-as.numeric(dat.stn$Arithmetic.Mean)
  
  #Subset to California and STN sites
  dat.stn<-dat.stn[dat.stn$State.Code==6  & dat.stn$POC==5 & dat.stn$Parameter.Code>61000,] 
  
  temp_mean<-dat.stn[dat.stn$Parameter.Code==68105,c(6:7,12,28,30)]
  temp_mean<-rename(temp_mean, temp_mean=Concentration)
  temp_mean<-temp_mean[!duplicated(temp_mean),]
  
  temp_min<-dat.stn[dat.stn$Parameter.Code==68103,c(6:7,12,28,30)]
  temp_min<-rename(temp_min, temp_min=Concentration)
  temp_min<-temp_min[!duplicated(temp_min),]
  
  temp_max<-dat.stn[dat.stn$Parameter.Code==68104,c(6:7,12,28,30)]
  temp_max<-rename(temp_max, temp_max=Concentration)
  temp_max<-temp_max[!duplicated(temp_max),]
  
  atmpres<-dat.stn[dat.stn$Parameter.Code==68108,c(6:7,12,28,30)]
  atmpres<-rename(atmpres, atmpres=Concentration)
  atmpres<-atmpres[!duplicated(atmpres),]
  
  
  Ag<-dat.stn[dat.stn$Parameter.Code==88166,c(6:7,12,28,30)]
  Ag<-rename(Ag, Ag=Concentration)
  Ag<-Ag[!duplicated(Ag),]
  
  As<-dat.stn[dat.stn$Parameter.Code==88103,c(6:7,12,28,30)]
  As<-rename(As, As=Concentration)
  As<-As[!duplicated(As),]
 
  Al<-dat.stn[dat.stn$Parameter.Code==88104,c(6:7,12,28,30)]
  Al<-rename(Al, Al=Concentration)
  Al<-Al[!duplicated(Al),]
  
  Ba<-dat.stn[dat.stn$Parameter.Code==88107,c(6:7,12,28,30)]
  Ba<-rename(Ba, Ba=Concentration)
  Ba<-Ba[!duplicated(Ba),]
  
  Br<-dat.stn[dat.stn$Parameter.Code==88109,c(6:7,12,28,30)]
  Br<-rename(Br, Br=Concentration)
  Br<-Br[!duplicated(Br),]
  
  Ca<-dat.stn[dat.stn$Parameter.Code==88111,c(6:7,12,28,30)]
  Ca<-rename(Ca, Ca=Concentration)
  Ca<-Ca[!duplicated(Ca),]
  
  Cr<-dat.stn[dat.stn$Parameter.Code==88112,c(6:7,12,28,30)]
  Cr<-rename(Cr, Cr=Concentration)
  Cr<-Cr[!duplicated(Cr),]
  
  Co<-dat.stn[dat.stn$Parameter.Code==88113,c(6:7,12,28,30)]
  Co<-rename(Co, Co=Concentration)
  Co<-Co[!duplicated(Co),]
  
  Cu<-dat.stn[dat.stn$Parameter.Code==88114,c(6:7,12,28,30)]
  Cu<-rename(Cu, Cu=Concentration)
  Cu<-Cu[!duplicated(Cu),]
  
  Cl<-dat.stn[dat.stn$Parameter.Code==88115,c(6:7,12,28,30)]
  Cl<-rename(Cl, Cl=Concentration)
  Cl<-Cl[!duplicated(Cl),]
  
  Cd<-dat.stn[dat.stn$Parameter.Code==88110,c(6:7,12,28,30)]
  Cd<-rename(Cd, Cd=Concentration)
  Cd<-Cd[!duplicated(Cd),]
  
  Ce<-dat.stn[dat.stn$Parameter.Code==88117,c(6:7,12,28,30)]
  Ce<-rename(Ce, Ce=Concentration)
  Ce<-Ce[!duplicated(Ce),]
  
  Cs<-dat.stn[dat.stn$Parameter.Code==88118,c(6:7,12,28,30)]
  Cs<-rename(Cs, Cs=Concentration)
  Cs<-Cs[!duplicated(Cs),]
  
  
  Fe<-dat.stn[dat.stn$Parameter.Code==88126,c(6:7,12,28,30)]
  Fe<-rename(Fe, Fe=Concentration)
  Fe<-Fe[!duplicated(Fe),]

  
  K<-dat.stn[dat.stn$Parameter.Code==88180,c(6:7,12,28,30)]
  K<-rename(K, K=Concentration)
  K<-K[!duplicated(K),]
  
  Mn<-dat.stn[dat.stn$Parameter.Code==88132,c(6:7,12,28,30)]
  Mn<-rename(Mn, Mn=Concentration)
  Mn<-Mn[!duplicated(Mn),]

  
  Mg<-dat.stn[dat.stn$Parameter.Code==88140,c(6:7,12,28,30)]
  Mg<-rename(Mg, Mg=Concentration)
  Mg<-Mg[!duplicated(Mg),]
  
  
  Na<-dat.stn[dat.stn$Parameter.Code==88184,c(6:7,12,28,30)]
  Na<-rename(Na, Na=Concentration)
  Na<-Na[!duplicated(Na),]  

  Ni<-dat.stn[dat.stn$Parameter.Code==88136,c(6:7,12,28,30)]
  Ni<-rename(Ni, Ni=Concentration)
  Ni<-Ni[!duplicated(Ni),]
  
  P<-dat.stn[dat.stn$Parameter.Code==88152,c(6:7,12,28,30)]
  P<-rename(P, P=Concentration)
  P<-P[!duplicated(P),]
  
  Pb<-dat.stn[dat.stn$Parameter.Code==88128,c(6:7,12,28,30)]
  Pb<-rename(Pb, Pb=Concentration)
  Pb<-Pb[!duplicated(Pb),]
  
  
  Rb<-dat.stn[dat.stn$Parameter.Code==88176,c(6:7,12,28,30)]
  Rb<-rename(Rb, Rb=Concentration)
  Rb<-Rb[!duplicated(Rb),]
  
  S<-dat.stn[dat.stn$Parameter.Code==88169,c(6:7,12,28,30)]
  S<-rename(S, S=Concentration)
  S<-S[!duplicated(S),]
  
  Sb<-dat.stn[dat.stn$Parameter.Code==88102,c(6:7,12,28,30)]
  Sb<-rename(Sb, Sb=Concentration)
  Sb<-Sb[!duplicated(Sb),]
  
  Se<-dat.stn[dat.stn$Parameter.Code==88154,c(6:7,12,28,30)]
  Se<-rename(Se, Se=Concentration)
  Se<-Se[!duplicated(Se),]
  
  Si<-dat.stn[dat.stn$Parameter.Code==88165,c(6:7,12,28,30)]
  Si<-rename(Si, Si=Concentration)
  Si<-Si[!duplicated(Si),]
  
  Sn<-dat.stn[dat.stn$Parameter.Code==88160,c(6:7,12,28,30)]
  Sn<-rename(Sn, Sn=Concentration)
  Sn<-Sn[!duplicated(Sn),]
  
  Sr<-dat.stn[dat.stn$Parameter.Code==88168,c(6:7,12,28,30)]
  Sr<-rename(Sr, Sr=Concentration)
  Sr<-Sr[!duplicated(Sr),]
  
  Ti<-dat.stn[dat.stn$Parameter.Code==88161,c(6:7,12,28,30)]
  Ti<-rename(Ti, Ti=Concentration)
  Ti<-Ti[!duplicated(Ti),]
  
  V<-dat.stn[dat.stn$Parameter.Code==88164,c(6:7,12,28,30)]
  V<-rename(V, V=Concentration)
  V<-V[!duplicated(V),]
 
  #Hg<-dat.stn[dat.stn$Parameter.Code==88142,c(6:7,12,28,30)]
  #Hg<-rename(Hg, Hg=Concentration)
  #Hg<-Hg[!duplicated(Hg),]
  
  Zn<-dat.stn[dat.stn$Parameter.Code==88167,c(6:7,12,28,30)]
  Zn<-rename(Zn, Zn=Concentration)
  Zn<-Zn[!duplicated(Zn),]
  
  
  Zr<-dat.stn[dat.stn$Parameter.Code==88185,c(6:7,12,28,30)]
  Zr<-rename(Zr, Zr=Concentration)
  Zr<-Zr[!duplicated(Zr),]
  
  
  NH4<-dat.stn[dat.stn$Parameter.Code==88301,c(6:7,12,28,30)]
  NH4<-rename(NH4, NH4=Concentration)
  NH4<-NH4[!duplicated(NH4),]
  
  Cl_ion<-dat.stn[dat.stn$Parameter.Code==88203,c(6:7,12,28,30)]
  Cl_ion<-rename(Cl_ion, Cl_ion=Concentration)
  Cl_ion<-Cl_ion[!duplicated(Cl_ion),]
  
  Na_ion<-dat.stn[dat.stn$Parameter.Code==88302,c(6:7,12,28,30)]
  Na_ion<-rename(Na_ion, Na_ion=Concentration)
  Na_ion<-Na_ion[!duplicated(Na_ion),]
  
  K_ion<-dat.stn[dat.stn$Parameter.Code==88303,c(6:7,12,28,30)]
  K_ion<-rename(K_ion, K_ion=Concentration)
  K_ion<-K_ion[!duplicated(K_ion),]
  
  nitrate<-dat.stn[dat.stn$Parameter.Code==88306,c(6:7,12,28,30)]
  nitrate<-rename(nitrate, nitrate=Concentration)
  nitrate<-nitrate[!duplicated(nitrate),]
  
  OC<-dat.stn[dat.stn$Parameter.Code==88320,c(6:7,12,28,30)]
  OC<-rename(OC, OC=Concentration)
  OC<-OC[!duplicated(OC),]
  
  OC2<-dat.stn[dat.stn$Parameter.Code==88320,c(6:7,12,28,30)]
  OC2<-rename(OC2, OC2=Concentration)
  OC2<-OC2[!duplicated(OC2),]
  
  EC<-dat.stn[dat.stn$Parameter.Code==88321,]
  EC<-dat.stn[dat.stn$Parameter.Code==88321,c(6:7,12,28,30)]
  EC<-rename(EC, EC=Concentration)
  EC<-EC[!duplicated(EC),]
  
  EC2<-dat.stn[dat.stn$Parameter.Code==88380,]
  EC2<-dat.stn[dat.stn$Parameter.Code==88380,c(6:7,12,28,30)]
  EC2<-rename(EC2, EC2=Concentration)
  EC2<-EC2[!duplicated(EC2),]
  
  #EC3<-dat.stn[dat.stn$Parameter.Code==88307,]
  #EC3<-dat.stn[dat.stn$Parameter.Code==88307,c(6:7,12,28,30)]
  #EC3<-rename(EC3, EC3=Concentration)
  #EC3<-EC3[!duplicated(EC3),]

  sulfate<-dat.stn[dat.stn$Parameter.Code==88403,c(6:7,12,28,30)]
  sulfate<-rename(sulfate, sulfate=Concentration)
  sulfate<-sulfate[!duplicated(sulfate),]
  
  dat.stn.join<- Reduce(function(...) merge(..., all=TRUE), 
                        list(temp_mean,temp_min,temp_max,atmpres,Ag,As,Al,Ba,Br,Ca,Cr,Co,Cd,Ce,Cs,Cu,Cl,
                             Fe,K,Mg,Mn,Na,Ni,Pb,Rb,Sb,Se,Si,S,Sn,Sm,Sr,Ti,V,Zn,Zr,
                             NH4,Na_ion,K_ion,Cl_ion,sulfate,nitrate,EC,OC,EC2,OC2))
  
  # separate m/d/y from Date.Local
  date.stn<-strsplit(dat.stn.join$Date.Local,"-")
  dat.stn.join$year<-as.numeric(sapply(date.stn, "[[", 1) )
  dat.stn.join$month<-as.numeric(sapply(date.stn,"[[",2))
  dat.stn.join$day<-as.numeric(sapply(date.stn,"[[",3))
  
  stn.list[[i]]<-dat.stn.join[,-3]
}

stn.ca<-do.call("rbind", stn.list)
# write
write.csv(stn.ca,"/Users/mf/Documents/AQS/STN/processed/CA_CSN_2000_2018_v2.csv",row.names = FALSE)

# subset to central valley sites (Fresno 2000-present, Bakersfield 2001-present, Modesto 2002-present, Visalia 2002-present)

stn.ca.cv<-stn.ca[stn.ca$CBSA.Name %in% c("Fresno, CA","Modesto, CA","Visalia-Porterville, CA","Bakersfield, CA"),]
write.csv(stn.ca.cv,"/Users/mf/Documents/AQS/STN/processed/CentralValley_CSN_2000_2018_v2.csv",row.names = FALSE)

#  stn.ca.cv2<-stn.ca.cv[stn.ca.cv$year==2016 & stn.ca.cv$CBSA.Name=="Bakersfield, CA",]
# read and merge PM2.5 

pm25.files <- list.files("/Users/mf/Documents/AQS/PM25/CA/",pattern="CA_PM25_*",full.names=TRUE)
# Extract data then subset

# Extract data
pm25.list<-vector('list',length(pm25.files))
for(i in 1:length(pm25.files)) { 
  dat.pm25<-read.csv(pm25.files[i],stringsAsFactors=FALSE)
  names(dat.pm25)[names(dat.pm25) == 'Daily.Mean.PM2.5.Concentration'] <- 'PM25'
  names(dat.pm25)[names(dat.pm25) == 'SITE_LATITUDE'] <- 'Latitude'
  names(dat.pm25)[names(dat.pm25) == 'SITE_LONGITUDE'] <- 'Longitude'
  names(dat.pm25)[names(dat.pm25) == 'CBSA_NAME'] <- 'CBSA.Name'
  names(dat.pm25)[names(dat.pm25) == 'Date'] <- 'Date.Local'

  date.pm25<-strsplit(dat.pm25$Date.Local,"/")
  dat.pm25$month<-as.numeric(sapply(date.pm25, "[[", 1) )
  dat.pm25$day<-as.numeric(sapply(date.pm25,"[[",2))
  dat.pm25$year<-as.numeric(sapply(date.pm25,"[[",3))
  
  # Subset to STN sites
  dat.pm25<-dat.pm25[dat.pm25$AQS_PARAMETER_CODE == 88502,]
  
  dat.pm25<-dat.pm25[dat.pm25$AQS_PARAMETER_CODE == 88502 & dat.pm25$POC %in% c(3,5), ] 
  dat.pm25<-dat.pm25[ , which(names(dat.pm25) %in% c("Date.Local","CBSA.Name","POC","PM25","month","day","year","Latitude","Longitude"))]

  pm25.list[[i]]<-dat.pm25
}  

pm25.ca<-do.call("rbind", pm25.list)

# subset to central valley sites 
pm25.ca.cv<-pm25.ca[pm25.ca$CBSA.Name %in% c("Fresno, CA","Modesto, CA","Visalia-Porterville, CA","Bakersfield, CA"),]
pm25.ca.cv$Date.Local<-as.Date(pm25.ca.cv$Date.Local,"%m/%d/%Y")

# merge speciation with pm25 mass
# Note, 2014-present Modesto and Visalia have POC 5 PM2.5, 2000-2014 Fresno has POC 5 PM2.5, 2000-2013 Bakesrfield has POC PM2.5.

td <- pm25.ca.cv %>%
  group_by(CBSA.Name, POC, Date.Local, Latitude) %>%
  mutate(filter_col = ifelse((CBSA.Name == "Bakersfield, CA" & year < 2014 & POC != 5), 'delete', 'keep'),
         filter_col = ifelse((CBSA.Name == "Bakersfield, CA" & year >= 2014 & Latitude < 35), 'delete', filter_col),
      filter_col = ifelse((CBSA.Name == "Fresno, CA"  &  Date.Local < "2014-10-01" & POC !=5), 'delete', filter_col),
      filter_col =  ifelse((CBSA.Name %in% c("Modesto, CA","Visalia-Porterville, CA") & POC==3),'delete',filter_col)) %>%
  ungroup() %>%
  filter(filter_col == 'keep')


stn.pm.ca.cv <- stn.ca.cv %>% full_join(td, by = c("CBSA.Name", "month","day","year"))
stn.pm.ca.cv2 <- stn.pm.ca.cv[-which(is.na(stn.pm.ca.cv$Latitude.x)),]
#counts<-stn.pm.ca.cv2 %>% group_by(CBSA.Name,month,day,year) %>% tally()

stn.pm.ca.cv2<-stn.pm.ca.cv2 %>% 
        select (-c(Latitude.y, Longitude.y, filter_col, year, month, day)) %>% 
          rename (latitude=Latitude.x, longitude=Longitude.x, date=Date.Local) %>%
            select(CBSA.Name, date,latitude,longitude, POC,PM25, everything())

summary(stn.pm.ca.cv2)

write.csv(stn.pm.ca.cv2,"/Users/mf/Documents/AQS/STN/processed/CentralValley_PM25_CSN_2000_2018.csv",row.names = FALSE)

# Checks
# problem with bakersfield 2014, redownload 2016 seem to be partial for Bakers and Fresno
# problem with fresno 2014 POC 5 ended in october
#stn.pm.ca.cv3<-stn.pm.ca.cv2[stn.pm.ca.cv2$Date.Local<"2001-01-01" & stn.pm.ca.cv2$CBSA.Name=="Fresno, CA",]