library(dplyr)
library(tidyr)
library(ncdf4)
library(lubridate)
library(sf)
library(seacarb)

basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"
setwd(basewd)

source("code/util_funs.R")


###Process CalCOFI data: https://calcofi.org/data/oceanographic-data/bottle-database/
bottle <- read.csv("data/oxygen options/CalCOFI/194903-202105_Bottle.csv", check.names=F)
ctd <- read.csv("data/oxygen options/CalCOFI/194903-202105_Cast.csv", check.names=F)
ctd <- ctd[,c("Cst_Cnt", "Date", "Year", "Month", "Julian_Day", "Lat_Dec", "Lon_Dec")]
bottle <- bottle[,c("Cst_Cnt", "Depthm", "T_degC", "Salnty", "O2ml_L", "O_qual")]

#Join together
dat <- left_join(bottle, ctd)

#Rename columns
dat$Cst_Cnt <- NULL
colnames(dat) <- c("depth","temperature_C", "salinity_psu", "do_mlpL", "o_qual", "date", "year", "month", "doy", "latitude", "longitude")

#convert oxygen mg/L to umol_kg
SA = gsw_SA_from_SP(dat$salinity_psu,dat$depth,dat$longitude,dat$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,dat$temperature_C,dat$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,dat$temperature_C,dat$depth) #conservative temp
dat$sigma0_kgm3 = gsw_sigma0(SA,CT)
dat$O2_umolkg = dat$do_mlpL*44660/(dat$sigma0_kgm3+1000) 

#Convert coordinates
#Remove with missing coordinates
dat <- dat %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Remove bad quality
#Remove 8 and 9 O_qual
dat <- subset(dat, o_qual!=9|o_qual!=8)

#Add name of survey
dat$survey <- "calCOFI"
dat$type <- "bottle"

#Date in correct format
dat$day <- str_sub(dat$date, 4,5)
dat$date <-  as.POSIXct(as.Date(with(dat,paste(year,month,day,sep="-")),"%Y-%m-%d"))
dat$day <- NULL

#Remove missing oxygen
dat <- subset(dat, !is.na(O2_umolkg))

#Filter by deepest depth
dat <- dat %>% group_by(longitude, latitude, date) %>%
  filter(depth == max(depth)) %>%
  ungroup() 

dat$o_qual <- NULL

saveRDS(dat, "data/processed_data/calCOFI_processed.rds")

###Process P Line data: https://www.ncei.noaa.gov/data/oceans/ncei/ocads/metadata/0234342.html
#Flag info: WOCE QC flags: 1 = Sample drawn from bottle but analysis not recieved; 2 = QC Performed: Acceptable Measurement; 3 = QC Performed: Questionable Measurement; 4 = QC Performed: Bad Measurement; 5 = QC Performed: Not Reported; 6 = Mean of replicate measurements; 9 = Not sampled
dat1 <- read.csv("data/oxygen options/LineP.csv")
colnames(dat1) <- tolower(colnames(dat1))

#Subset columns
dat1 <- dat1[,c("year_utc", "month_utc", "day_utc", "yearday_utc", "longitude_dec", "latitude_dec", "ctdprs_dbar", "ctdsal_pss78", "oxygen_umol_kg", "oxygen_flag_w", "ctdtmp_its90_deg_c")]

#Remove bad oxygen dat1a
dat1 <-subset(dat1, oxygen_flag_w==2)
dat1$oxygen_flag_w <- NULL

#Rename columns
colnames(dat1) <- c("year", "month", "day", "doy","longitude", "latitude", "pressure_dbar", "salinity_psu", "O2_umolkg", "temperature_C")

#Get date in correct format
year <- dat1$year
month <- dat1$month
day <- dat1$day
dat1$date <- as.POSIXct(as.Date(with(dat1,paste(year,month,day,sep="-")),"%Y-%m-%d"))
dat1$day <- NULL

# remove missing data flags
dat1 <- dat1 %>%
  filter(!pressure_dbar==-999, !temperature_C==-999, !salinity_psu == -999)

# convert pressure to depth 
dat1$depth <- p2d(lat = dat1$latitude,
                          p = dat1$pressure_dbar)
#Convert coordinates
#Remove with missing coordinates
dat1 <- dat1 %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +dat1um=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#get sigma
SA = gsw_SA_from_SP(dat1$salinity_psu,dat1$depth,dat1$longitude,dat1$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,dat1$temperature_C,dat1$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,dat1$temperature_C,dat1$depth) #conservative temp
dat1$sigma0_kgm3 = gsw_sigma0(SA,CT)

#Add name of survey
dat1$survey <- "LineP"
dat1$type <- "bottle"

#Remove missing oxygen
dat1 <- subset(dat1, !is.na(O2_umolkg))


#Filter by deepest depth
dat1 <- dat1 %>% group_by(longitude, latitude, date) %>%
  filter(depth == max(depth)) %>%
  ungroup() 


#Save
saveRDS(dat1, "data/processed_data/LineP_processed.rds")

###Newport line: originally from here https://zenodo.org/records/5814071; 
# load Newport data
#This .rds files is the output of the script extract_newport_line_data.R
newport_data <- readRDS("data/oxygen options/newport/data/newport_bottom.RDS")

# Add latitude, year, depth, doy, month
newport_lat <- 44.65
newport_data$latitude <- newport_lat
newport_data$year <- year(newport_data$sample_date)
newport_data$doy <- as.POSIXlt(newport_data$sample_date, format = "%Y-%b-%d")$yday
newport_data$depth <- p2d(lat = newport_lat,
                          p = newport_data$pressure..dbar.)
newport_data$month <- month(newport_data$sample_date)

#select columns
newport_data <- newport_data[,c("sample_date", "longitude..degW.", "temperature..degC.", "practical.salinity", "dissolved.oxygen..ml.L.", "latitude", "year", "doy", "depth")]
colnames(newport_data) <- c("date", "longitude", "temperature_C","salinity_psu", "do_mlpL", "latitude", "year", "doy", "depth")

#  get lat and long in UTC coordinates
newport_data <- newport_data %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#sigma and convert O2
SA = gsw_SA_from_SP(newport_data$salinity_psu,newport_data$depth,newport_data$longitude,newport_data$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,newport_data$temperature_C,newport_data$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,newport_data$temperature_C,newport_data$depth) #conservative temp
newport_data$sigma0_kgm3 = gsw_sigma0(SA,CT)
newport_data$O2_umolkg = newport_data$do_mlpL*44660/(newport_data$sigma0_kgm3+1000) 

#Add survey
newport_data$survey <- "NewportLine"
newport_data$type <- "ctd"
newport_data <- as.data.frame(newport_data)

#Save
saveRDS(newport_data, "data/processed_data/newportline_processed.rds")

##WCOA: https://www.ncei.noaa.gov/access/ocean-carbon-acidification-data-system/oceans/Coastal/WCOA.html
#Quality flags: OXYGEN_FLAG, WOCE quality control flags are used: 2 = good value, 3 = questionable value, 4 = bad value, 5 = value not reported, 6 = mean of replicate measurements, 9 = sample not drawn.
#Read all files in, then combine into one
setwd("data/oxygen options/WCOA")
all_files <- dir("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/oxygen options/WCOA")
df.list <- lapply(all_files, read_excel)
wcoa <- data.table::rbindlist(df.list, fill=T)
#set WD back to normal
setwd(basewd)

d1 <- df.list[[1]]
colnames(d1) <- tolower(colnames(d1))

#Keep only good columns for either CTD or bottle
ctd <- subset(d1, ctdsal_flag==2 & ctdoxygen_flag==2)
bottle <- subset(d1, salinty_flag==2 &oxygen_flag==2)

ctd <- ctd[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar","depth_bottom_meter", "ctdtemp_its90_deg_c","ctdsal_pss78", "ctdoxygen_umol_kg")]
bottle<- bottle[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar", "depth_bottom_meter", "ctdtemp_its90_deg_c","salinity_pss78", "oxygen_umol_kg")]

#Rename columns
colnames(ctd) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")
colnames(bottle) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")

#Add type column
ctd$type <- "ctd"
bottle$type <-"bottle"

#Combine
wcoa_2007 <- bind_rows(ctd, bottle)

#Depth
wcoa_2007$depth <- p2d(lat = wcoa_2007$latitude,
                                             p = wcoa_2007$pressure)
wcoa_2007$pressure <- NULL

##2

d1 <- df.list[[2]]
colnames(d1) <- tolower(colnames(d1))

#Keep only good columns for either CTD or bottle
ctd <- subset(d1, ctdsal_flag==2 & ctdoxygen_flag==2)
bottle <- subset(d1, salinty_flag==2 &oxygen_flag==2)

ctd <- ctd[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar","depth_bottom_meter", "ctdtemp_its90_deg_c","ctdsal_pss78", "ctdoxygen_umol_kg")]
bottle<- bottle[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar", "depth_bottom_meter", "ctdtemp_its90_deg_c","salinity_pss78", "oxygen_umol_kg")]

#Rename columns
colnames(ctd) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")
colnames(bottle) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")

#Add type column
ctd$type <- "ctd"
bottle$type <-"bottle"

wcoa_2011 <- bind_rows(ctd, bottle)

wcoa_2011$depth <- p2d(lat = wcoa_2011$latitude,
                       p = wcoa_2011$pressure)
wcoa_2011$pressure <- NULL



d1 <- df.list[[3]]
colnames(d1) <- tolower(colnames(d1))

#Keep only good columns for either CTD or bottle
ctd <- subset(d1, ctdsal_flag==2 & ctdoxygen_flag==2)
bottle <- subset(d1, salinty_flag==2 &oxygen_flag==2)

ctd <- ctd[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar", "depth_bottom_meter", "ctdtemp_its90_deg_c","ctdsal_pss78", "ctdoxygen_umol_kg")]
bottle<- bottle[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar", "depth_bottom_meter", "ctdtemp_its90_deg_c","salinity_pss78", "oxygen_umol_kg")]

#Rename columns
colnames(ctd) <- c("date", "year", "month", "latitude", "longitude", "pressure","depth2", "temperature_C", "salinity_psu", "O2_umolkg")
colnames(bottle) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")

#Add type column
ctd$type <- "ctd"
bottle$type <-"bottle"

wcoa_2012 <- bind_rows(ctd, bottle)

wcoa_2012$depth <- p2d(lat = wcoa_2012$latitude,
                       p = wcoa_2012$pressure)
wcoa_2012$pressure <- NULL

d1 <- df.list[[4]]
colnames(d1) <- tolower(colnames(d1))

#Keep only good columns for either CTD or bottle
ctd <- subset(d1, ctdsal_flag==2 & ctdoxygen_flag==2)
bottle <- subset(d1, salinty_flag==2 &oxygen_flag==2)

ctd <- ctd[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar", "depth_bottom_meter", "ctdtemp_its90_deg_c","ctdsal_pss78", "ctdoxygen_umol_kg")]
bottle<- bottle[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal","ctdpressure_dbar", "depth_bottom_meter", "ctdtemp_its90_deg_c","salinity_pss78", "oxygen_umol_kg")]

#Rename columns
colnames(ctd) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")
colnames(bottle) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")

#Add type column
ctd$type <- "ctd"
bottle$type <-"bottle"

wcoa_2013 <- bind_rows(ctd, bottle)

wcoa_2013$depth <- p2d(lat = wcoa_2013$latitude,
                       p = wcoa_2013$pressure)
wcoa_2013$pressure <- NULL

d1 <- df.list[[5]]
colnames(d1) <- tolower(colnames(d1))

#Keep only good columns for either CTD or bottle
ctd <- subset(d1, ctdsal_flag==2 & ctdoxygen_flag==2)
bottle <- subset(d1, salinty_flag==2 &oxygen_flag==2)

ctd <- ctd[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar", "depth_bottom_meter", "ctdtemp_its90_deg_c","ctdsal_pss78", "ctdoxygen_umol_kg")]
bottle<- bottle[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar", "depth_bottom_meter", "ctdtemp_its90_deg_c","salinity_pss78", "oxygen_umol_kg")]

#Rename columns
colnames(ctd) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")
colnames(bottle) <- c("date", "year", "month", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")

#Add type column
ctd$type <- "ctd"
bottle$type <-"bottle"

wcoa_2013b <- bind_rows(ctd, bottle)

wcoa_2013b$depth <- p2d(lat = wcoa_2013b$latitude,
                       p = wcoa_2013b$pressure)
wcoa_2013b$pressure <- NULL


d1 <- df.list[[6]]
colnames(d1) <- tolower(colnames(d1))

#Keep only good columns for either CTD or bottle
#No ctd flags in this one
#ctd <- subset(d1, ctdsal_flag==2 & ctdoxygen_flag==2)
bottle <- subset(d1, salinty_flag==2 &oxygen_flag==2)

#ctd <- ctd[,c("date_utc", "year_utc", "month_utc", "latitude_decimal", "longitude_decimal", "depth_bottom_meter", "ctdtemp_its90_deg_c","ctdsal_pss78", "ctdoxygen_umol_kg")]
bottle<- bottle[,c("year_utc", "month_utc", "day_utc", "latitude_decimal", "longitude_decimal", "ctdpressure_dbar", "depth_bottom_meter", "ctdtmp_its90_deg_c","salinity_pss78", "oxygen_umol_kg")]

#Rename columns
#colnames(ctd) <- c("date", "year", "month", "latitude", "longitude", "depth", "temperature_C", "salinity_psu", "O2_umolkg")
colnames(bottle) <- c( "year", "month", "day", "latitude", "longitude", "pressure", "depth2", "temperature_C", "salinity_psu", "O2_umolkg")

#Add type column
#ctd$type <- "ctd"
bottle$type <-"bottle"

#Add date
bottle$date <- as.POSIXct(as.Date(with(bottle,paste(year,month,day,sep="-")),"%Y-%m-%d"))
bottle$day <- NULL

bottle$depth <- p2d(lat = bottle$latitude,
                        p = bottle$pressure)
bottle$pressure <- NULL

wcoa_2016 <- bottle

d1 <- df.list[[7]]
colnames(d1) <- tolower(colnames(d1))

#Keep only good columns for either CTD or bottle
ctd <- subset(d1, ctdsal_flag==2 & ctdoxygen_flag==2)
#No salinity
bottle <- subset(d1, oxygen_flag==2)

ctd <- ctd[,c("date_utc", "year_utc", "month_utc", "latitude_dec", "longitude_dec", "ctdpressure_dbar", "ctdtmp_its90_deg_c","ctdsal_pss78", "ctdoxygen_umol_kg")]
bottle<- bottle[,c("date_utc", "year_utc", "month_utc", "latitude_dec", "longitude_dec", "ctdpressure_dbar", "ctdtmp_its90_deg_c","ctdsal_pss78", "oxygen_umol_kg")]

#Rename columns
colnames(ctd) <- c("date", "year", "month", "latitude", "longitude", "pressure", "temperature_C", "salinity_psu", "O2_umolkg")
colnames(bottle) <- c("date", "year", "month", "latitude", "longitude", "pressure", "temperature_C", "salinity_psu", "O2_umolkg")

#Add type column
ctd$type <- "ctd"
bottle$type <-"bottle"

wcoa_2017 <- bind_rows(ctd, bottle)

#Convert depth
wcoa_2017$depth <- p2d(lat = wcoa_2017$latitude,
                          p = wcoa_2017$pressure)
wcoa_2017$pressure <- NULL

d1 <- df.list[[8]]
colnames(d1) <- tolower(colnames(d1))

#Keep only good columns for either CTD or bottle
ctd <- subset(d1, ctdsal_flag==2 & ctdoxy_flag==2)
bottle <- subset(d1, salinity_flag==2 & oxygen_flag==2)

ctd <- ctd[,c("year_utc", "month_utc", "day_utc", "latitude", "longitude", "ctdpres", "depth_bottom", "depth", "ctdtemp_its90","ctdsal_pss78", "ctdoxy")]
bottle<- bottle[,c("year_utc", "month_utc", "day_utc", "latitude", "longitude", "ctdpres", "depth_bottom", "depth", "ctdtemp_its90","salinity_pss78", "oxygen_djg")]

#Rename columns
colnames(ctd) <- c("year", "month", "day", "latitude", "longitude", "pressure", "depth2", "depth3", "temperature_C", "salinity_psu", "O2_umolkg")
colnames(bottle) <- c("year", "month", "day", "latitude", "longitude", "pressure", "depth2", "depth3", "temperature_C", "salinity_psu", "O2_umolkg")

ctd <- mutate_all(ctd, as.numeric)
bottle <- mutate_all(bottle, as.numeric)

bottle$depth <- p2d(lat = as.numeric(bottle$latitude),
                    p = as.numeric(bottle$pressure))
ctd$depth <- p2d(lat = as.numeric(ctd$latitude),
                    p = as.numeric(ctd$pressure))

#Add type column
ctd$type <- "ctd"
bottle$type <-"bottle"

wcoa_2021 <- bind_rows(ctd, bottle)
wcoa_2021$pressure <- NULL

#Get date
wcoa_2021$date <- as.POSIXct(as.Date(with(wcoa_2021,paste(year,month,day,sep="-")),"%Y-%m-%d"))
wcoa_2021$day <- NULL

#####Combine all
wcoa <- bind_rows(wcoa_2007, wcoa_2011, wcoa_2012, wcoa_2013, wcoa_2013b,wcoa_2016, wcoa_2017, wcoa_2021)

#Remove duplicates
wcoa <- distinct(wcoa)

#Convert coordinates
#  get lat and long in UTC coordinates
wcoa <- wcoa %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#sigma and convert O2
SA = gsw_SA_from_SP(wcoa$salinity_psu,wcoa$depth,wcoa$longitude,wcoa$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,wcoa$temperature_C,wcoa$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,wcoa$temperature_C,wcoa$depth) #conservative temp
wcoa$sigma0_kgm3 = gsw_sigma0(SA,CT)

#Survey
wcoa$survey <- "wcoa"

#DOY
wcoa$doy <- as.POSIXlt(wcoa$date, format = "%Y-%b-%d")$yday

#Remove missing O2 data

wcoa <- subset(wcoa, !is.na(O2_umolkg))

#Deepest depth
wcoa<- wcoa %>% group_by(longitude, latitude, date, type) %>%
  filter(depth == max(depth)) %>%
  ungroup() 

#save
saveRDS(wcoa, "data/processed_data/wcoa_processed.rds")

#CODAP https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0219960
codap <- read.csv("data/oxygen options/CODAP_NA_v2021.csv")
colnames(codap) <- tolower(colnames(codap))

bottle <- subset(codap, (oxygen_flag==2 & salinity_flag==2))
ctd <- subset(codap, (ctdoxy_flag==2 & ctdsal_flag==2))

bottle <- bottle[,c("year_utc", "month_utc", "day_utc", "latitude", "longitude", "depth", "oxygen", "salinity_pss78", "ctdtemp_its90")]
ctd <- ctd[,c("year_utc", "month_utc", "day_utc", "latitude", "longitude", "depth", "ctdtemp_its90", "ctdsal_pss78", "ctdoxy")]

bottle[] <- sapply(bottle, as.numeric)
ctd[] <- sapply(ctd, as.numeric)

colnames(bottle) <- c('year', 'month', "day", "latitude", "longitude", "depth", "O2_umolkg", "salinity_psu", "temperature_C")
colnames(ctd) <- c('year', 'month', "day", "latitude", "longitude", "depth", "temperature_C", "salinity_psu", "O2_umolkg")

bottle$type <- "bottle"
ctd$type <- "ctd"
codap <- bind_rows(bottle, ctd)
  
#Coordinates
codap <- codap %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#sigma and convert O2
SA = gsw_SA_from_SP(codap$salinity_psu,codap$depth,codap$longitude,codap$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,codap$temperature_C,codap$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,codap$temperature_C,codap$depth) #conservative temp
codap$sigma0_kgm3 = gsw_sigma0(SA,CT)

#date
codap$date <- as.POSIXct(as.Date(with(codap,paste(year,month,day,sep="-")),"%Y-%m-%d"))
codap$day <-NULL

#day of year
codap$doy <- as.POSIXlt(codap$date, format = "%Y-%b-%d")$yday

#West Coast only
codap <- subset(codap, latitude>30 & latitude<69)
codap <- subset(codap, longitude> -179 & longitude < -110)

#Remove missing oxygen
codap <- subset(codap, !is.na(O2_umolkg))

#Deepest depth
codap <- codap %>% group_by(longitude, latitude, date, type) %>%
  filter(depth == max(depth)) %>%
  ungroup() 

codap$survey <- "codap"

saveRDS(codap, "data/processed_data/codap_processed.rds")

#####OCNMS#####: https://zenodo.org/records/10466124 and https://zenodo.org/records/11167853
#oxygen in ml per L, temperature C, salinity psu, depth m
setwd("data/oxygen options/ocnms/OCNMS1/netcdf_files/binned_profiles")
all_files <- list.files(pattern=".nc", recursive=T)

ocnms <- list()
for (i in 1:length(all_files)){
file <- all_files[i]
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

dim_lat<- ncvar_get(nc_ds, "latitude", collapse_degen=FALSE)
dim_lon<- ncvar_get(nc_ds, "longitude", collapse_degen=FALSE)
dim_time <- ncvar_get(nc_ds, "time", collapse_degen=FALSE)
dim_depth <- ncvar_get(nc_ds, "pressure", collapse_degen=FALSE)

#time is seconds 
coords <- as.data.frame(as.matrix(expand.grid(dim_lon, dim_lat, dim_depth, dim_time)))
colnames(coords) <- c("longitude", "latitude", "pressure", "date")
coords$date <- as.POSIXct("1970-01-01")+dseconds(dim_time)
coords$date <- as.POSIXct(as.Date(coords$date, format = "%Y-%b-%d"))

#variables dissolved_oxygen, salinity, temperature, and depth
coords$do_mlpL <- ncvar_get(nc_ds, "dissolved_oxygen", collapse_degen=FALSE)
coords$temperature_C <- ncvar_get(nc_ds, "dissolved_oxygen", collapse_degen=FALSE)
coords$depth <- ncvar_get(nc_ds, "depth", collapse_degen=FALSE)
coords$salinity_psu <- ncvar_get(nc_ds, "salinity", collapse_degen=FALSE)

#Deepest depth
coords <- coords %>% group_by(longitude, latitude, date) %>%
  filter(depth == max(depth)) %>%
  ungroup() 

ocnms[[i]] <- list(coords)
}

ocnms <- as.data.frame(bind_rows(ocnms))

#  get lat and long in UTC coordinates
ocnms <- ocnms %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#sigma and convert O2
SA = gsw_SA_from_SP(ocnms$salinity_psu,ocnms$depth,ocnms$longitude,ocnms$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,ocnms$temperature_C,ocnms$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,ocnms$temperature_C,ocnms$depth) #conservative temp
ocnms$sigma0_kgm3 = gsw_sigma0(SA,CT)
ocnms$O2_umolkg = ocnms$do_mlpL*44660/(ocnms$sigma0_kgm3+1000) 

#Survey
ocnms$survey <- "ocnms"

#Type
ocnms$type <- "ctd"

#DOY
ocnms$doy <- as.POSIXlt(ocnms$date, format = "%Y-%b-%d")$yday

#month
ocnms$month <- month(ocnms$date)
ocnms$year <- year(ocnms$date)

setwd(basewd)
saveRDS(ocnms, file="data/processed_data/ocnms_processed.rds")

##The other OCNMS data
setwd(basewd)
setwd("data/oxygen options/ocnms/OCNMS2/netcdf_files/binned_profiles")
all_files <- list.files(pattern=".nc", recursive=T)

ocnms <- list()
for (i in 1:length(all_files)){
  file <- all_files[i]
  nc_ds <-  ncdf4::nc_open(file)
  nc_open(file)
  names(nc_ds$dim) #display dimensions
  names(nc_ds$var) #display variables
  
  dim_lat<- ncvar_get(nc_ds, "latitude", collapse_degen=FALSE)
  dim_lon<- ncvar_get(nc_ds, "longitude", collapse_degen=FALSE)
  dim_time <- ncvar_get(nc_ds, "time", collapse_degen=FALSE)
  dim_depth <- ncvar_get(nc_ds, "pressure", collapse_degen=FALSE)
  
  #time is seconds 
  coords <- as.data.frame(as.matrix(expand.grid(dim_lon, dim_lat, dim_depth, dim_time)))
  colnames(coords) <- c("longitude", "latitude", "pressure", "date")
  coords$date <- as.POSIXct("1970-01-01")+dseconds(dim_time)
  coords$date <- as.POSIXct(as.Date(coords$date, format = "%Y-%b-%d"))
  
  #variables dissolved_oxygen, salinity, temperature, and depth
  coords$do_mlpL <- ncvar_get(nc_ds, "dissolved_oxygen", collapse_degen=FALSE)
  coords$temperature_C <- ncvar_get(nc_ds, "dissolved_oxygen", collapse_degen=FALSE)
  coords$depth <- ncvar_get(nc_ds, "depth", collapse_degen=FALSE)
  coords$salinity_psu <- ncvar_get(nc_ds, "salinity", collapse_degen=FALSE)
  
  #Deepest depth
  coords <- coords %>% group_by(longitude, latitude, date) %>%
    filter(depth == max(depth)) %>%
    ungroup() 
  ocnms[[i]] <- list(coords)
}

ocnms <- as.data.frame(bind_rows(ocnms))

#  get lat and long in UTC coordinates
ocnms <- ocnms %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#sigma and convert O2
SA = gsw_SA_from_SP(ocnms$salinity_psu,ocnms$depth,ocnms$longitude,ocnms$latitude) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,ocnms$temperature_C,ocnms$depth) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,ocnms$temperature_C,ocnms$depth) #conservative temp
ocnms$sigma0_kgm3 = gsw_sigma0(SA,CT)
ocnms$O2_umolkg = ocnms$do_mlpL*44660/(ocnms$sigma0_kgm3+1000) 

#Survey
ocnms$survey <- "ocnms"

#Type
ocnms$type <- "ctd"

#DOY
ocnms$doy <- as.POSIXlt(ocnms$date, format = "%Y-%b-%d")$yday

#month
ocnms$month <- month(ocnms$date)
ocnms$year <- year(ocnms$date)

setwd(basewd)
saveRDS(ocnms, file="data/processed_data/ocnms2_processed.rds")
