# GLORYS climatology using CDO
# organize files
install.packages("tidyverse")
library(tidyverse)
install.packages("tidync")
library(tidync)
install.packages("ncdf4")
library(ncdf4)
library(raster)
install.packages("lubridate")
library(lubridate)
library(dplyr)
library(parallel)
install.packages("ncdf4.helpers")
library(ncdf4.helpers)
library(RANN)
library(ggplot2)
library(here)
library(readxl)

####Info about CDO commands
#Format of cdo commands: "cdo function *.nc new_file_name.nc"
#Remember that every element of the command needs a space in between! So when using paste0, make sure add in spaces

####Set wd
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/zone1")

####Zone 1 (West Coast)
###Load haul info for NOAA West Coast and IPHC
load("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/fish_raw/NOAA/nwfsc_haul.rda")
IPHC <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/iphc-2023-fiss-hadj-20231031.xlsx")
##Housekeeping
colnames(IPHC) <- tolower(colnames(IPHC))
IPHC <- IPHC[,c("stlkey", "year", "date", "midlat", "midlon")]
IPHC$lat_start <- IPHC$midlat
IPHC$midlat <- NULL
IPHC$lon_start <- IPHC$midlon
IPHC$midlon <- NULL

##Combine
haul_combined <- bind_rows(nwfsc_haul, IPHC)

##More housekeeping
#Restrict to southern latitudes of interest
haul_combined <- subset(haul_combined, lat_start <49)
#Isolate just date
haul_combined$date2 <- as.POSIXct(substr(haul_combined$date, 1,11))
#Restrict to only years that we have GLORYS data for
haul_combined <- subset(haul_combined, date2>as.POSIXct("1993-01-01"))

###GLORYS Temperature and salinity 
combined <- "cdo mergetime *.nc glorys_tempsal_1993_2020_raw.nc"
system(combined)

#Checking timesteps
#time_ref <- tidync("glorys_tempsal_1993_2020_raw.nc")%>%
  #activate("D0")%>% hyper_tibble()%>% mutate(date=as_date("1950-01-01")+hours(time))
#ncdf4::nc_open("glorys_tempsal_1993_2020_raw.nc")
#Looks right!

##Look at dimensions to get time and variables
file <- ("glorys_tempsal_wc_1993_2020_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

#thetao (temp, degrees C)
#so (salinity, PSU)
#time (gregorian, hours since 1950-01-01)
#longitude and latitude
#depth (m)

##Convert .nc to a readable file in R to match with survey data
#Make a dataframe-formatted file
nc <- tidync(file)

#Get depths and times
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

#Function to extract bottom variable
extract_bottom_vars <- function(tstep){
  nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
    hyper_tibble() %>% 
    # filter for bottom-most depth layer
    group_by(longitude,latitude) %>% 
    filter(depth==max(depth))
  nc_df
}

#Another way to extract bottom layer, but it removes the depth variable, which we need to have
#cmd <- paste0('cdo -bottomvalue ',"glorys_tempsal_1993_2020_raw.nc",  " glorys_tempsal_1993_2020_raw_bottom.nc")
#system(cmd)

# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
nc_bottom_all2 <- nc_bottom_all[,c("thetao", "so", "longitude", "latitude", "depth", "time")]
saveRDS(nc_bottom_all2, file="glorys_tempsal_wc_full_region_bottom.rds")
nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_tempsal_wc_full_region_bottom.rds")
nc_bottom_all2 <- unique(nc_bottom_all2)
##Match to survey data

##Find closest matching date
#List of all the days
date_glorys <- unique(nc_bottom_all2$time)
#Get index of the dates
date_index <- matrix(nrow=length(haul_combined$date2))
for(i in 1:length(haul_combined$date2)){
  date_index[i] <- which(abs(date_glorys-haul_combined$date2[i]) == min(abs(date_glorys-haul_combined$date2[i])))
}
#Get the date of that index
date <- matrix(nrow=length(haul_combined$date2))
date_glorys <- as.character(unique(nc_bottom_all2$time))
for(i in 1:length(haul_combined$date2)){
  date[i] <- date_glorys[date_index[i]]
}

#Subset data for those dates
nc_bottom_all2$time <- as.character(nc_bottom_all2$time)
nc_bottom_all3 <- filter(nc_bottom_all2, time %in% date)

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all3[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all3[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.vector(date)
#Extract data
nc_bottom_all4 <- left_join(points, nc_bottom_all3)
colnames(nc_bottom_all4) <- c("lat_glorysphys", "lon_glorysphys", "date_glorysphys", "temp_glorys", "sal_glorys", "depth_glorysphys")
glorys_wc <- bind_cols(nc_bottom_all4, haul_combined)
saveRDS(glorys_wc, file="glorys_tempsal_WC.rds")

###oxygen
setwd("~/Dropbox/choke species/code/Copernicus/o2/nwfsc")

combined <- "cdo mergetime *.nc glorys_o2_wc_1993_2020_raw.nc"
system(combined)

#Checking timesteps
#time_ref <- tidync("glorys_tempsal_1993_2020_raw.nc")%>%
#activate("D0")%>% hyper_tibble()%>% mutate(date=as_date("1950-01-01")+hours(time))
#ncdf4::nc_open("glorys_tempsal_1993_2020_raw.nc")
#Looks right!

##Look at dimensions to get time and variables
file <- ("glorys_o2_wc_1993_2020_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

#thetao (temp, degrees C)
#so (salinity, PSU)
#time (gregorian, hours since 1950-01-01)
#longitude and latitude
#depth (m)

##Convert .nc to a readable file in R to match with survey data
#Make a dataframe-formatted file
nc <- tidync(file)

#Get depths and times
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
saveRDS(nc_bottom_all, file="wc_glorys_o2.rds")
nc_bottom_all2 <- as.data.frame(readRDS("wc_glorys_o2.rds"))

##Match to survey data

##Find closest matching date
#List of all the days
#date_glorys <- as.POSIXct(unique(nc_bottom_all2$time), format="%Y-%m-%d")
#Get index of the dates
#date_index <- matrix(nrow=length(haul_combined$date2))
#for(i in 1:length(haul_combined$date2)){
 # date_index[i] <- which(abs(date_glorys-haul_combined$date2[i]) == min(abs(date_glorys-haul_combined$date2[i])))
#}

#Get the date of that index
#date <- matrix(nrow=length(haul_combined$date2))
#date_glorys <- as.character(unique(nc_bottom_all2$time))
#for(i in 1:length(haul_combined$date2)){
#  date[i] <- date_glorys[date_index[i]]
#}

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
#Extract data
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))
nc_bottom_all2 <- unique(nc_bottom_all2)
nc_bottom_all4 <- left_join(points, nc_bottom_all2)

glorys_wc <- bind_cols(nc_bottom_all4, haul_combined)
colnames(nc_bottom_all4) <- c("lat_gloryso2", "lon_gloryso2", "date_gloryso2", "no3_glorys", "o2_glorys", "po4_glorys", "chl_glorys","si_glorys", "nppv_glorys", "depth_glorys")

saveRDS(glorys_wc, file="glorys_o2_WC.rds")

####Alaska
####Set wd
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/zone2")

###Haul data
IPHC <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/iphc-2023-fiss-hadj-20231031.xlsx")
bio2 <-readRDS("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_all_levels.RDS")
haul <- bio2$haul

##Housekeeping
colnames(IPHC) <- tolower(colnames(IPHC))
colnames(haul) <- tolower(colnames(haul))
IPHC <- IPHC[,c("stlkey", "year", "date", "midlat", "midlon")]
IPHC$lat_start <- IPHC$midlat
IPHC$midlat <- NULL
IPHC$lon_start <- IPHC$midlon
IPHC$midlon <- NULL

haul <- haul[,c("hauljoin", "start_time", "start_latitude", "start_longitude")]
colnames(haul) <- c("hauljoin", "date", "lat_start", "lon_start")
##Combine
haul_combined <- bind_rows(haul, IPHC)

##More housekeeping
#Restrict to northern latitudes of interest
min(haul$lat_start)
haul_combined <- subset(haul_combined, lat_start >51)
#Isolate just date
haul_combined$date2 <- as.POSIXct(substr(haul_combined$date, 1,11))
#Restrict to only years that we have ROMS data for
haul_combined <- subset(haul_combined, date2>as.POSIXct("1993-01-01"))


###GLORYS Temperature and salinity 
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/zone2")
combined <- "cdo mergetime *.nc glorys_tempsal_ak_1993_2020_raw.nc"
system(combined)

#Checking timesteps
#time_ref <- tidync("glorys_tempsal_1993_2020_raw.nc")%>%
#activate("D0")%>% hyper_tibble()%>% mutate(date=as_date("1950-01-01")+hours(time))
#ncdf4::nc_open("glorys_tempsal_1993_2020_raw.nc")
#Looks right!

##Look at dimensions to get time and variables
file <- ("glorys_tempsal_ak_1993_2020_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

#thetao (temp, degrees C)
#so (salinity, PSU)
#time (gregorian, hours since 1950-01-01)
#longitude and latitude
#depth (m)

##Convert .nc to a readable file in R to match with survey data
#Make a dataframe-formatted file
nc <- tidync(file)

#Get depths and times
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

#Another way to extract bottom layer, but it removes the depth variable, which we need to have
#cmd <- paste0('cdo -bottomvalue ',"glorys_tempsal_1993_2020_raw.nc",  " glorys_tempsal_1993_2020_raw_bottom.nc")
#system(cmd)

# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
nc_bottom_all2 <- as.data.frame(nc_bottom_all)
saveRDS(nc_bottom_all2, file="glorys_tempsal_ak_full_region_bottom.rds")
nc_bottom_all2 <- unique(nc_bottom_all2)
##Match to survey data

##Find closest matching date
#List of all the days
date_glorys <- unique(nc_bottom_all2$time)
#Get index of the dates
date_index <- matrix(nrow=length(haul_combined$date2))
for(i in 1:length(haul_combined$date2)){
  date_index[i] <- which(abs(date_glorys-haul_combined$date2[i]) == min(abs(date_glorys-haul_combined$date2[i])))
}
#Get the date of that index
date <- matrix(nrow=length(haul_combined$date2))
date_glorys <- as.character(unique(nc_bottom_all2$time))
for(i in 1:length(haul_combined$date2)){
  date[i] <- date_glorys[date_index[i]]
}

#Subset data for those dates
nc_bottom_all2$time <- as.character(nc_bottom_all2$time)
nc_bottom_all3 <- filter(nc_bottom_all2, time %in% date)

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all3[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all3[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.vector(date)
#Extract data
nc_bottom_all4 <- left_join(points, nc_bottom_all3)
colnames(nc_bottom_all4) <- c("lat_glorysphys", "lon_glorysphys", "date_glorysphys", "temp_glorys", "sal_glorys", "depth_glorysphys")
glorys_ak <- bind_cols(nc_bottom_all4, haul_combined)
saveRDS(glorys_ak, file="glorys_tempsal_AK.rds")

###oxygen
setwd("~/Dropbox/choke species/code/Copernicus/o2/alaska")

combined <- "cdo mergetime *.nc glorys_o2_ak_1993_2020_raw.nc"
system(combined)

setwd("~/Dropbox/choke species/code/Copernicus/o2/alaska/alaska2")
combined2 <- "cdo mergetime *.nc glorys_o2_ak2_1993_2020_raw.nc"
system(combined2)

##Look at dimensions to get time and variables
setwd("~/Dropbox/choke species/code/Copernicus/o2/alaska")
file <- ("glorys_o2_ak_1993_2020_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

setwd("~/Dropbox/choke species/code/Copernicus/o2/alaska/alaska2")
file2 <- ("glorys_o2_ak2_1993_2020_raw.nc")
nc_ds2 <-  ncdf4::nc_open(file2)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

##Convert .nc to a readable file in R to match with survey data
#Make a dataframe-formatted file
setwd("~/Dropbox/choke species/code/Copernicus/o2/alaska")
nc <- tidync(file)
setwd("~/Dropbox/choke species/code/Copernicus/o2/alaska/alaska2")
nc2 <- tidync(file2)

#Get depths and times
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

depths2 <- nc2 %>% activate("D1") %>% hyper_tibble()
times2 <- nc2 %>% activate("D0") %>% hyper_tibble()

# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)
nc_bottom_all_b <- purrr::map_df(times2$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))
nc_bottom_all_b <- nc_bottom_all_b %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
saveRDS(nc_bottom_all, file="glorys_o2_ak_full_region_bottom.rds")
saveRDS(nc_bottom_all_b, file="glorys_o2_ak2_full_region_bottom.rds")

#Combine
nc_bottom_all2 <- bind_rows(nc_bottom_all, nc_bottom_all_b)

##Match to survey data

##Find closest matching date
#List of all the days
#date_glorys <- as.POSIXct(unique(nc_bottom_all2$time), format="%Y-%m-%d")
#Get index of the dates
#date_index <- matrix(nrow=length(haul_combined$date2))
#for(i in 1:length(haul_combined$date2)){
# date_index[i] <- which(abs(date_glorys-haul_combined$date2[i]) == min(abs(date_glorys-haul_combined$date2[i])))
#}

#Get the date of that index
#date <- matrix(nrow=length(haul_combined$date2))
#date_glorys <- as.character(unique(nc_bottom_all2$time))
#for(i in 1:length(haul_combined$date2)){
#  date[i] <- date_glorys[date_index[i]]
#}

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
#Extract data
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))
nc_bottom_all2 <- unique(nc_bottom_all2)
nc_bottom_all4 <- left_join(points, nc_bottom_all2)

glorys_wc <- bind_cols(nc_bottom_all4, haul_combined)
colnames(nc_bottom_all4) <- c("lat_gloryso2", "lon_gloryso2", "date_gloryso2", "no3_glorys", "o2_glorys", "po4_glorys", "chl_glorys","si_glorys", "nppv_glorys", "depth_glorys")

saveRDS(glorys_wc, file="glorys_o2_AK.rds")

####BC
###Load haul info for BC and IPHC
pbs_haul <- readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/fish_raw/BC/pbs-haul.rds")
IPHC <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/iphc-2023-fiss-hadj-20231031.xlsx")
##Housekeeping
pbs_haul$performance <- as.character(pbs_haul$performance)
colnames(IPHC) <- tolower(colnames(IPHC))
IPHC <- IPHC[,c("stlkey", "year", "date", "midlat", "midlon")]
IPHC$lat_start <- IPHC$midlat
IPHC$midlat <- NULL
IPHC$lon_start <- IPHC$midlon
IPHC$midlon <- NULL
pbs_haul <- pbs_haul[c("event_id", "date", "lat_start", "lon_start")]
##Combine
haul_combined <- bind_rows(pbs_haul, IPHC)

##More housekeeping
#Restrict to southern latitudes of interest

haul_combined <- subset(haul_combined, lat_start >47 & lat_start< 55)
#Isolate just date
haul_combined$date2 <- as.POSIXct(substr(haul_combined$date, 1,11))
#Restrict to only years that we have GLORYS data for
haul_combined <- subset(haul_combined, date2>as.POSIXct("1993-01-01"))

###GLORYS Temperature and salinity 
####Set wd
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/zone3")
combined <- "cdo mergetime *.nc glorys_tempsal_bc_1993_2020_raw.nc"
system(combined)

##Look at dimensions to get time and variables
file <- ("glorys_tempsal_bc_1993_2020_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

#thetao (temp, degrees C)
#so (salinity, PSU)
#time (gregorian, hours since 1950-01-01)
#longitude and latitude
#depth (m)

##Convert .nc to a readable file in R to match with survey data
#Make a dataframe-formatted file
nc <- tidync(file)

#Get depths and times
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

#Function to extract bottom variable
extract_bottom_vars <- function(tstep){
  nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
    hyper_tibble() %>% 
    # filter for bottom-most depth layer
    group_by(longitude,latitude) %>% 
    filter(depth==max(depth))
  nc_df
}

#Another way to extract bottom layer, but it removes the depth variable, which we need to have
#cmd <- paste0('cdo -bottomvalue ',"glorys_tempsal_1993_2020_raw.nc",  " glorys_tempsal_1993_2020_raw_bottom.nc")
#system(cmd)

# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
nc_bottom_all2 <- nc_bottom_all[,c("thetao", "so", "longitude", "latitude", "depth", "time")]
saveRDS(nc_bottom_all2, file="glorys_tempsal_bc_full_region_bottom.rds")
nc_bottom_all2 <- unique(nc_bottom_all2)
##Match to survey data

##Find closest matching date
#List of all the days
date_glorys <- unique(nc_bottom_all2$time)
#Get index of the dates
date_index <- matrix(nrow=length(haul_combined$date2))
for(i in 1:length(haul_combined$date2)){
  date_index[i] <- which(abs(date_glorys-haul_combined$date2[i]) == min(abs(date_glorys-haul_combined$date2[i])))
}
#Get the date of that index
date <- matrix(nrow=length(haul_combined$date2))
date_glorys <- as.character(unique(nc_bottom_all2$time))
for(i in 1:length(haul_combined$date2)){
  date[i] <- date_glorys[date_index[i]]
}

#Subset data for those dates
nc_bottom_all2$time <- as.character(nc_bottom_all2$time)
nc_bottom_all3 <- filter(nc_bottom_all2, time %in% date)

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all3[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all3[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.vector(date)
#Extract data
nc_bottom_all4 <- left_join(points, nc_bottom_all3)
colnames(nc_bottom_all4) <- c("lat_glorysphys", "lon_glorysphys", "date_glorysphys", "temp_glorys", "sal_glorys", "depth_glorysphys")
glorys_wc <- bind_cols(nc_bottom_all4, haul_combined)
saveRDS(glorys_wc, file="glorys_tempsal_BC.rds")

###oxygen
setwd("~/Dropbox/choke species/code/Copernicus/o2/bc")

combined <- "cdo mergetime *.nc glorys_o2_bc_1993_2020_raw.nc"
system(combined)

#Checking timesteps
#time_ref <- tidync("glorys_tempsal_1993_2020_raw.nc")%>%
#activate("D0")%>% hyper_tibble()%>% mutate(date=as_date("1950-01-01")+hours(time))
#ncdf4::nc_open("glorys_tempsal_1993_2020_raw.nc")
#Looks right!

##Look at dimensions to get time and variables
file <- ("glorys_o2_bc_1993_2020_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

#thetao (temp, degrees C)
#so (salinity, PSU)
#time (gregorian, hours since 1950-01-01)
#longitude and latitude
#depth (m)

##Convert .nc to a readable file in R to match with survey data
#Make a dataframe-formatted file
nc <- tidync(file)

#Get depths and times
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
saveRDS(nc_bottom_all, file="glorys_o2_bc_full_region_bottom.rds")

##Match to survey data

##Find closest matching date
#List of all the days
#date_glorys <- as.POSIXct(unique(nc_bottom_all2$time), format="%Y-%m-%d")
#Get index of the dates
#date_index <- matrix(nrow=length(haul_combined$date2))
#for(i in 1:length(haul_combined$date2)){
# date_index[i] <- which(abs(date_glorys-haul_combined$date2[i]) == min(abs(date_glorys-haul_combined$date2[i])))
#}

#Get the date of that index
#date <- matrix(nrow=length(haul_combined$date2))
#date_glorys <- as.character(unique(nc_bottom_all2$time))
#for(i in 1:length(haul_combined$date2)){
#  date[i] <- date_glorys[date_index[i]]
#}

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
#Extract data
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))
nc_bottom_all2 <- unique(nc_bottom_all2)
nc_bottom_all4 <- left_join(points, nc_bottom_all2)

glorys_wc <- bind_cols(nc_bottom_all4, haul_combined)
colnames(nc_bottom_all4) <- c("lat_gloryso2", "lon_gloryso2", "date_gloryso2", "no3_glorys", "o2_glorys", "po4_glorys", "chl_glorys","si_glorys", "nppv_glorys", "depth_glorys")

saveRDS(glorys_wc, file="glorys_o2_BC.rds")


###Combine oxygen, temp, and salinity


###Combine