library(readxl)
library(lubridate)
install.packages("ggpubr")
library(ggpubr)
library(seacarb)
library(dplyr)
library(sf)
library(tidync)

#Set ggplot themes 
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"
setwd(basewd)
source("code/util_funs.R")

### Thompson-gathered in situ data
thompson <- readRDS("data/oxygen options/PotentialDensityData.RDS")

#Convert coordinates

#Convert O2
thompson$O2_umolkg <- convert_o2(thompson$do_mlpL, thompson$sigma0_kgm3)

#Isolate just the DFO data
dfo <- subset(thompson, Survey=="DFO_Pacific")

#Convert pressure to depth
dfo$depth <- p2d(lat = dfo$latitude,
                          p = dfo$p_dbar)

#Try to match to haul data
haul_qc <- read.csv("data/fish_raw/BC/QCS_effort.csv")
haul_vi <- read.csv("data/fish_raw/BC/WCVI_effort.csv")
haul_hs <- read.csv("data/fish_raw/BC/HS_effort.csv")
haul_hg <- read.csv("data/fish_raw/BC/WCHG_effort.csv")

#Combine BC bio and haul data to combine together
haul2 <- rbind(haul_hg, haul_hs, haul_qc, haul_vi)
haul_bc <- haul2[,c("Survey.Year", "Set.date", "Start.latitude", "Start.longitude", "Bottom.depth..m.")]
colnames(haul_bc) <- c("year", "date", "latitude", "longitude", "depth_haul")

dfo <- left_join(dfo, haul_bc, by=c("year", "latitude", "longitude"))
dfo <- dfo[,c("Survey", "year", "month", "date", "latitude", "longitude", "salinity", "temperature_C", "p_dbar", "do_mlpL", "sigma0_kgm3","O2_umolkg","depth_haul")]
colnames(dfo) <- c("survey", "year", "month", "date", "latitude", "longitude", "salinity_psu", "temperature_C", "p_dbar", "do_mlpL", "sigma0_kgm3","O2_umolkg","depth")

dfo$date <- as.POSIXct(dfo$date,format = "%Y-%m-%d")

dfo<- dfo %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

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

#Convert coordiantes
nwfsc2 <- nwfsc2 %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Add survey column
nwfsc2$survey <- "nwfsc"

#IPHC data
#iphc <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/IPHC_FISS_set_halibut.xlsx")
iphc <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/Set and Pacific halibut data.xlsx")
colnames(iphc) <- tolower(colnames(iphc))

#Columns of interest
iphc <- iphc[,c("year", "date", "beginlat", "beginlon", "begindepth (fm)", "temp c", "salinity psu", "oxygen_ml")]
colnames(iphc) <- c("year", "date", "latitude", "longitude", "depth", "temperature_C", "salinity_psu", "do_mlpL")

#Date and month in right format
iphc$month <- month(ymd(iphc$date))
iphc$day <- day(ymd(iphc$date))
iphc$date <-  as.POSIXct(as.Date(with(iphc,paste(year,month,day,sep="-")),"%Y-%m-%d"))
iphc$year <- as.numeric(iphc$year)
iphc$day <- NULL

#convert oxygen mg/L to umol_kg
SA = gsw_SA_from_SP(iphc$salinity_psu,iphc$depth,iphc$longitude,iphc$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,iphc$temperature_C,iphc$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,iphc$temperature_C,iphc$depth) #conservative temp
iphc$sigma0_kgm3 = gsw_sigma0(SA,CT)
iphc$O2_umolkg = iphc$do_mlpL*44660/(iphc$sigma0_kgm3+1000) 

#Convert coordinates
#Remove with missing coordinates
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
afsc$survey <- "afsc"

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
goa$survey <- "afsc"

##New years of Bering Sea data--this doesn't actually seem to have oxygen


#Bind all together
afsc <- as.data.frame(afsc)
iphc <- as.data.frame(iphc)
nwfsc2 <- as.data.frame(nwfsc2)
dfo <- as.data.frame(dfo)
goa <- as.data.frame(goa)
insitu_combined <- bind_rows(dfo, nwfsc2, iphc, afsc)

insitu_combined$date <-as.POSIXct(as.Date(insitu_combined$date),format = "%Y-%b-%d")

#Save
saveRDS(insitu_combined, file="data/processed_data/insitu_combined.rds")

