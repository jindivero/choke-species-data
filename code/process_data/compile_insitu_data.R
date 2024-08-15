library(readxl)
install.packages("lubridate")
library(lubridate)
install.packages("ggpubr")
library(ggpubr)
install.packages("seacarb")
library(seacarb)
library(dplyr)
library(sf)
install.packages("tidync")
library(tidync)

#Set ggplot themes 
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"

setwd(basewd)
source("code/util_funs.R")

###BC
dfo<- readRDS("data/fish_raw/BC/pbs-haul.rds")
dfo <- dfo[,c("event_id", "date", "lat_start", "lon_start", "depth_m", "year", "temperature_C", "do_mlpL", "salinity_PSU")]

dfo$date <- as.POSIXct(as.Date(dfo$date,format = "%Y-%m-%d"))

colnames(dfo) <- c("event_id", "date", "latitude", "longitude", "depth", "year", "temperature_C", "do_mlpL", "salinity_psu")

dfo<- dfo %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

dfo$month <- month(dfo$date)

#convert oxygen mg/L to umol_kg
SA = gsw_SA_from_SP(dfo$salinity_psu,dfo$depth,dfo$longitude,dfo$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,dfo$temperature_C,dfo$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,dfo$temperature_C,dfo$depth) #conservative temp
dfo$sigma0_kgm3 = gsw_sigma0(SA,CT)
dfo$O2_umolkg = dfo$do_mlpL*44660/(dfo$sigma0_kgm3+1000) 

dfo$year <- ifelse(is.na(dfo$year), year(dfo$date), dfo$year)

#survey
dfo$survey <- "dfo"

#NWFSC trawl survey data
nwfsc <- readRDS("data/oxygen options/joined_nwfsc_data.RDS")
#Unique hauls
nwfsc2 <- distinct(nwfsc, trawl_id, .keep_all = TRUE)

#Columns of interest
nwfsc2 <- nwfsc2[,c("trawl_id", "year", "date", "longitude_dd", "latitude_dd", "o2", "temp", "sal", "depth")]
colnames(nwfsc2) <- c("event_id", "year", "date", "longitude", "latitude", "do_mlpL", "temperature_C", "salinity_psu", "depth")

#Date and month in right format
nwfsc2$date <- as.POSIXct(nwfsc2$date,format = "%Y-%b-%d")
nwfsc2$month <- month(nwfsc2$date)

#convert oxygen mg/L to umol_kg
SA = gsw_SA_from_SP(nwfsc2$salinity_psu,nwfsc2$depth,nwfsc2$longitude,nwfsc2$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,nwfsc2$temperature_C,nwfsc2$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,nwfsc2$temperature_C,nwfsc2$depth) #conservative temp
nwfsc2$sigma0_kgm3 = gsw_sigma0(SA,CT)
nwfsc2$O2_umolkg = nwfsc2$do_mlpL*44660/(nwfsc2$sigma0_kgm3+1000) 

#Convert coordinates
nwfsc2 <- nwfsc2 %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Add survey column
nwfsc2$survey <- "nwfsc"

#WCBTS 2022-2023
nwfsc3 <- read.csv("data/oxygen options/wcbts_2022_2023.csv")
#Catch data to get metadata
dat <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
names(dat) = tolower(names(dat))
#Columns of interest
dat <- dat[,c("year", "depth_m", "longitude_dd", "latitude_dd", "date", "trawl_id")]
dat <- unique(dat)
dat$trawl_id <- as.numeric(dat$trawl_id)
#Combine
nwfsc3 <- left_join(nwfsc3, dat, by="trawl_id")
nwfsc3$year.x <- NULL

#Column names
colnames(nwfsc3) <- c("do_mlpL","salinity_psu", "temperature_C", "event_id", "year", "depth", "longitude", "latitude", "date")

#Convert nwfsc3e to correct format
nwfsc3$date <- as.POSIXct(as.Date(as.POSIXct("1970-01-01")+as.difftime(nwfsc3$date,units="days")))

#convert oxygen mg/L to umol_kg
SA = gsw_SA_from_SP(nwfsc3$salinity_psu,nwfsc3$depth,nwfsc3$longitude,nwfsc3$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,nwfsc3$temperature_C,nwfsc3$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,nwfsc3$temperature_C,nwfsc3$depth) #conservative temp
nwfsc3$sigma0_kgm3 = gsw_sigma0(SA,CT)
nwfsc3$O2_umolkg = nwfsc3$do_mlpL*44660/(nwfsc3$sigma0_kgm3+1000) 

#Convert coordinates
nwfsc3 <- filter(nwfsc3, !is.na(latitude))
nwfsc3 <- nwfsc3 %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +nwfsc3um=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Add survey column
nwfsc3$survey <- "nwfsc"

#Month
nwfsc3$month <- month(nwfsc3$date)


#IPHC data
#iphc <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/IPHC_FISS_set_halibut.xlsx")
iphc <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/Set and Pacific halibut data.xlsx")
colnames(iphc) <- tolower(colnames(iphc))

#Columns of interest
iphc <- iphc[,c("year", "date", "profiler lat", "profiler lon", "profiler bottom depth (m)", "temp c", "salinity psu", "oxygen_ml")]
colnames(iphc) <- c("year", "date", "latitude", "longitude", "depth", "temperature_C", "salinity_psu", "do_mlpL")

#Date and month in correct format
iphc$month <- case_when(grepl("May",iphc$date) ~5,
                       grepl("Jun",iphc$date)  ~6,
                       grepl("Jul",iphc$date)  ~7,
                       grepl("Aug",iphc$date)  ~8,
                       grepl("Sep",iphc$date)  ~9,
                       grepl("Oct",iphc$date)  ~10)
iphc$day <- as.numeric(substr(iphc$date, 1,2))
iphc$date <-  as.POSIXct(as.Date(with(iphc,paste(year,month,day,sep="-")),"%Y-%m-%d"))
iphc$day <- NULL
iphc$year <- as.numeric(iphc$year)

#convert oxygen mg/L to umol_kg
SA = gsw_SA_from_SP(iphc$salinity_psu,iphc$depth,iphc$longitude,iphc$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,iphc$temperature_C,iphc$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,iphc$temperature_C,iphc$depth) #conservative temp
iphc$sigma0_kgm3 = gsw_sigma0(SA,CT)
iphc$O2_umolkg = iphc$do_mlpL*44660/(iphc$sigma0_kgm3+1000) 

#Convert coordinates
iphc <- subset(iphc, !is.na(latitude))
iphc <- iphc %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Add survey column
iphc$survey <- "iphc"

####Alaska data#####
afsc <- readRDS("data/oxygen options/2012-2016_o2.RDS")
afsc <- afsc[,c("latitude", "longitude", "temp", "salinity", "o2", "depth", "year", "hauljoin", "date_time")]
colnames(afsc) <- c("latitude", "longitude", "temperature_C", "salinity_psu", "O2_umolkg", "depth", "year", "event_id", "date")

#Date and month in right format
afsc$date <- as.POSIXct(as.Date(afsc$date),format = "%Y-%b-%d")
afsc$month <- month(afsc$date)

#convert oxygen ppm to umol_kg
SA = gsw_SA_from_SP(afsc$salinity_psu,afsc$depth,afsc$longitude,afsc$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,afsc$temperature_C,afsc$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,afsc$temperature_C,afsc$depth) #conservative temp
afsc$sigma0_kgm3 = gsw_sigma0(SA,CT)

#Convert coordinates
afsc <- afsc %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Add survey column
afsc$survey <- "EBS"

###GOA
goa <- read.csv("data/oxygen options/OceanGOA2013.csv")
goa2 <- read.csv("data/oxygen options/OceanGOA2015.csv")

#Combine
goa <- bind_rows(goa, goa2)
colnames(goa) <- tolower(colnames(goa))

#Join with haul info to get other info (event id and haul)
catch2 <- readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_all_levels.RDS")
#Isolate necessary parts of full data to get specimen weights/lengths per haul
haul <- catch2$haul
colnames(haul) <- tolower(colnames(haul))
haul <- haul[,c("hauljoin", "cruise", "haul", "start_time")]

goa <- left_join(goa, haul)

goa <- goa[,c("hauljoin","latitude", "longitude","temp", "salinity", "o2", "depth", "start_time")]
colnames(goa) <- c("event_id", "latitude", "longitude", "temperature_C", "salinity_psu", "O2_umolkg","depth", "date")

#Date and month in right format
goa$date <- as.POSIXct(as.Date(goa$date),format = "%Y-%b-%d")
goa$month <- month(goa$date)
goa$year <- year(goa$date)

#convert oxygen ppm to umol_kg
SA = gsw_SA_from_SP(goa$salinity_psu,goa$depth,goa$longitude,goa$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,goa$temperature_C,goa$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,goa$temperature_C,goa$depth) #conservative temp
goa$sigma0_kgm3 = gsw_sigma0(SA,CT)

#Convert coordinates
#Remove with missing coordinates
goa <- goa %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Add survey column
goa$survey <- "goa"

##New years of Bering Sea data--
library(ncdf4)
filename <- "data/oxygen options/GAPCTD_2023_EBS.nc"
con <- ncdf4::nc_open(filename)
names(con$var)

# Bottom
 do_mlpL <- as.numeric(ncvar_get(nc = con, varid = "sea_floor_dissolved_oxygen")[ncvar_get(nc = con, varid = "vessel") == 162])
 latitude <- as.numeric(ncvar_get(nc=con, varid="latitude")[ncvar_get(nc = con, varid = "vessel") == 162])
 longitude <- as.numeric(ncvar_get(nc=con, varid="longitude")[ncvar_get(nc = con, varid = "vessel") == 162])
 temperature_C <- as.numeric(ncvar_get(nc=con, varid="sea_floor_temperature")[ncvar_get(nc = con, varid = "vessel") == 162])
 salinity_psu <- as.numeric(ncvar_get(nc=con, varid="sea_floor_salinity")[ncvar_get(nc = con, varid = "vessel") == 162])
 time <- as.Date(ncvar_get(nc=con, varid="time")[ncvar_get(nc = con, varid = "vessel") == 162])
 depth <- as.numeric(ncvar_get(nc=con, varid="haul_depth")[ncvar_get(nc = con, varid = "vessel") == 162])

afsc2 <- as.data.frame(cbind(depth, latitude, longitude, do_mlpL, salinity_psu, temperature_C))
afsc2$date <- as.Date(time)

#Date and month in right format
afsc2$date <- as.POSIXct(afsc2$date,format = "%Y-%b-%d")
afsc2$month <- month(afsc2$date)
afsc2$year <- year(afsc2$date)

#convert oxygen ppm to umol_kg
SA = gsw_SA_from_SP(afsc2$salinity_psu,afsc2$depth,afsc2$longitude,afsc2$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,afsc2$temperature_C,afsc2$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,afsc2$temperature_C,afsc2$depth) #conservative temp
afsc2$sigma0_kgm3 = gsw_sigma0(SA,CT)

#Convert coordinates
#Remove with missing coordinates
afsc2 <- afsc2 %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Add survey column
afsc2$survey <- "EBS"

#Aleutian Islands
ai <- read.csv("data/oxygen options/OceanAI2014.csv")
ai2 <- read.csv("data/oxygen options/OceanAI2016.csv")

#Combine
ai <- bind_rows(ai, ai2)
colnames(ai) <- tolower(colnames(ai))

#Join with haul info to get other info (event id and haul)
ai <- left_join(ai, haul)

ai <- ai[,c("hauljoin","latitude", "longitude","temp", "salinity", "o2", "depth", "start_time")]
colnames(ai) <- c("event_id", "latitude", "longitude", "temperature_C", "salinity_psu", "O2_umolkg","depth", "date")

#Date and month in right format
ai$date <- as.POSIXct(as.Date(ai$date),format = "%Y-%b-%d")
ai$month <- month(ai$date)
ai$year <- year(ai$date)

#convert oxygen ppm to umol_kg
SA = gsw_SA_from_SP(ai$salinity_psu,ai$depth,ai$longitude,ai$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,ai$temperature_C,ai$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,ai$temperature_C,ai$depth) #conservative temp
ai$sigma0_kgm3 = gsw_sigma0(SA,CT)

#Convert coordinates
#Remove with missing coordinates
ai <- ai %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Add survey column
ai$survey <- "ai"

#Bind all together
afsc <- as.data.frame(afsc)
iphc <- as.data.frame(iphc)
nwfsc2 <- as.data.frame(nwfsc2)
nwfsc3 <- as.data.frame(nwfsc3)
dfo <- as.data.frame(dfo)
goa <- as.data.frame(goa)
afsc2 <- as.data.frame(afsc2)
ai <- as.data.frame(ai)
insitu_combined <- bind_rows(dfo, nwfsc2, nwfsc3, iphc, goa, afsc, afsc2, ai)

insitu_combined$date <-as.POSIXct(as.Date(insitu_combined$date),format = "%Y-%b-%d")
insitu_combined$doy <-  as.POSIXlt(insitu_combined$date, format = "%Y-%b-%d")$yday

#Save
saveRDS(insitu_combined, file="data/processed_data/insitu_combined.rds")

