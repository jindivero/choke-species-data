# Process raw GLORYS data into proper format for joining with haul data
# organize files
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
library(readxl)
library(sf)
library(tidync)

#extract specific value
extract_haul <- function(tstep, depth_haul, lon_haul, lat_haul){
  nc_df <- nc %>% hyper_filter(time=(time==tstep), depth=(depth==depth_haul), longitude=(longitude==lon_haul)) %>% 
    hyper_tibble() %>% 
    filter(abs(latitude - lat_haul) == min(abs(latitude - lat_haul)))
  nc_df
}

###Load haul info for NOAA West Coast and IPHC
load("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/fish_raw/NOAA/nwfsc_haul.rda")
IPHC <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/IPHC_FISS_set_halibut.xlsx")

##Housekeeping
colnames(IPHC) <- tolower(colnames(IPHC))
IPHC <- IPHC[,c("stlkey", "date", "beginlat", "beginlon", "begindepth (fm)", "temp c", "salinity psu", "oxygen_ml", "oxygen_umol", "oxygen_sat")]
IPHC$lat_start <- IPHC$beginlat
IPHC$beginlat <- NULL
IPHC$lon_start <- IPHC$beginlon
IPHC$beginlon <- NULL
IPHC$depth_IPHC <- IPHC$`begindepth (fm)`
IPHC$`begindepth (fm)` <- NULL

##Combine
haul_combined <- bind_rows(nwfsc_haul, IPHC)

##More housekeeping
#Restrict to coordinates of WC
haul_combined <- subset(haul_combined, lat_start<=49)
#Isolate just date
haul_combined$date2 <- as.POSIXct(substr(haul_combined$date, 1,11))
#Restrict to only years that we have GLORYS data for
haul_combined <- subset(haul_combined, date2>as.POSIXct("1993-01-01"))
#Remove 2022 (only have GLORYS data for 2021)
haul_combined <- subset(haul_combined, date2<as.POSIXct("2021-12-31"))
#Combine IPHC and trawl depths into same column
haul_combined$depth_m <- ifelse(is.na(haul_combined$depth_m), haul_combined$depth_IPHC, haul_combined$depth_m)

###oxygen
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/wc")
###GLORYS Temperature and salinity 
combined <- "cdo mergetime *.nc glorys_tempsal_1993_2021_raw.nc"
system(combined)

##Look at dimensions to get time and variables
file <- ("glorys_tempsal_1993_2021_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
##Look at dimensions to get time and variables
file <- ("glorys_tempsal_1993_2021_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

#Get list of the GLORYS data coordinates, dates, and depths
dim_lon <- ncvar_get(nc_ds, "longitude")
dim_lat <- ncvar_get(nc_ds, "latitude")
dim_time <- ncvar_get(nc_ds, "time")
depth <- ncvar_get(nc_ds, "depth")

#Convert time format to day format
dim_time2 <- (as_datetime("1950-01-01 00:00:00")+hours(dim_time))

#List dates of hauls
time_haul <- as.POSIXct(haul_combined$date2)

#Get index of date that matches to each haul
date_index <- matrix(ncol=1, nrow=length(time_haul))
for(i in 1:length(time_haul)){
  date_index[i] <-which(abs(dim_time2-time_haul[i]) == min(abs(dim_time2-time_haul[i])))
}

#Pull the GLORYS time unit
tstep <- dim_time[c(date_index)]

#Depth
haul_depths <- haul_combined$depth_m
depth_index <- matrix(ncol=1, nrow=length(haul_depths))
haul_depths <- haul_combined$depth_m
for(i in 1:length(haul_depths)){
  depth_index[i] <-which(abs(depth-haul_depths[i]) == min(abs(depth-haul_depths[i])))
}

glorys_depth <- depth[c(depth_index)]

#Make sure coordinates are in the right format
#Combine lat and lon into 
coords <- expand.grid(dim_lon, dim_lat)
colnames(coords) <- c("longitude", "latitude")

#Transform to UTM
coords2<- coords %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(gridID_phy=row_number()) %>% 
  mutate(lonUTM=st_coordinates(.)[,1],latUTM=st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

#Transform haul coordinates to UTM
trawl_locs <- haul_combined[,c("lon_start", "lat_start")] %>% 
  # convert to UTM10
  st_as_sf(coords=c('lon_start','lat_start'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(lontrawlUTM=st_coordinates(.)[,1],lattrawlUTM=st_coordinates(.)[,2])%>% 
  st_set_geometry(NULL)

#Match closest points
#Both types of coordinate systems get the same answers! Amazing
glorys_coords <- RANN::nn2(coords2[, c('lonUTM', 'latUTM')], trawl_locs[, c('lontrawlUTM', "lattrawlUTM")],k = 1)
glorys_coords2 <- RANN::nn2(coords2[, c('longitude', 'latitude')], trawl_locs[, c('lon_start', "lat_start")],k = 1)

#Extract out the lats and lons in the GLORYS reference system
points <- coords2[c(glorys_coords2$nn.idx),c("longitude", "latitude")]
haul_lon <- points$longitude
haul_lat <- points$latitude

#Extract out the points that match date, depth, lon, and lat from the full GLORYS file
#First tidy the file
nc <- tidync(file)

#Looping through each set of points
fingers_crossed <- list()
for(i in 1:length(time_haul)){
  message(i, Sys.time() )
  x <- extract_haul(tstep[i], glorys_depth[i], haul_lon[i], haul_lat[i])
  fingers_crossed[[i]] <- x
}

saveRDS(fingers_crossed, file="glorys_wc_tempsal_raw.rds")


#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
saveRDS(nc_bottom_all, file="glorys_o2_wc_full_region_bottom.rds.rds")
nc_bottom_all2 <- nc_bottom_all
nc_bottom_all2 <- as.data.frame(readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_o2_wc_full_region_bottom.rds.rds"))








##Extra slice
setwd("~/Dropbox/choke species/code/Copernicus/o2/nwfsc/")

#Combine all the separate files into one
combined <- "cdo mergetime *.nc glorys_o2_wc2_1993_2021_raw.nc"
system(combined)

##Look at dimensions to get time and variables
file <- ("glorys_o2_wc2_1993_2021_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

#Get depths and times
nc <- tidync(file)
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
saveRDS(nc_bottom_all, file="glorys_o2_wc_full_region_bottom.rds.rds")
nc_bottom_all2 <- nc_bottom_all

##Match to survey data
#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
#Extract data
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))
nc_bottom_all2 <- unique(nc_bottom_all2)
colnames(nc_bottom_all2) <- c("no3_glorys", "o2_glorys", "po4_glorys", "chl_glorys", "si_glorys", "nppv_glorys", "lon_gloryso2", "lat_gloryso2", "depth_gloryso2", "date_gloryso2")
colnames(points) <- c("lat_gloryso2", "lon_gloryso2", "date_gloryso2")
nc_bottom_all4 <- left_join(points, nc_bottom_all2)
glorys_wc <- bind_cols(nc_bottom_all4, haul_combined)
saveRDS(glorys_wc, file="glorys_o2_WC.rds")


####Temperature########
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/wc")
###GLORYS Temperature and salinity 
combined <- "cdo mergetime *.nc glorys_tempsal_1993_2021_raw.nc"
system(combined)

#Checking timesteps
#time_ref <- tidync("glorys_tempsal_1993_2020_raw.nc")%>%
#activate("D0")%>% hyper_tibble()%>% mutate(date=as_date("1950-01-01")+hours(time))
#ncdf4::nc_open("glorys_tempsal_1993_2020_raw.nc")
#Looks right!

##Look at dimensions to get time and variables
file <- ("glorys_tempsal_1993_2021_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

dim_lon <- ncvar_get(nc_ds, "longitude")
dim_lat <- ncvar_get(nc_ds, "latitude")
dim_time <- ncvar_get(nc_ds, "time")
depth <- ncvar_get(nc_ds, "depth")

#Convert time format to day format
dim_time2 <- (as_datetime("1950-01-01 00:00:00")+hours(dim_time))

#Get index of date that matches to each haul
date_index <- matrix(ncol=1, nrow=length(time_haul))
for(i in 1:length(time_haul)){
  date_index[i] <-which(abs(dim_time2-time_haul[i]) == min(abs(dim_time2-time_haul[i])))
}
#Pull the GLORYS time unit
tstep <- dim_time[c(date_index)]

#List dates of hauls
time_haul <- as.POSIXct(haul_combined$date2)

#Depth
depth_index <- matrix(ncol=1, nrow=length(haul_depths))
haul_depths <- haul_combined$depth_m
for(i in 1:length(haul_depths)){
  depth_index[i] <-which(abs(depth-haul_depths[i]) == min(abs(depth-haul_depths[i])))
}

depth_haul <- depth[c(depth_index)]

#Make sure coordinates are in the right format

#Combine the lat, lon and pull the coordinates

#Get list of closest matching lat and lon, day, and depth
##Variables
#thetao (temp, degrees C)
#so (salinity, PSU)
#time (gregorian, hours since 1950-01-01)
#longitude and latitude
#depth (m)

depth_index <- matrix(ncol=1, nrow=length(haul_depths))
haul_depths <- haul_combined$depth_m
for(i in 1:length(haul_depths)){
  depth_index[i] <-which(abs(depth-haul_depths[i]) == min(abs(depth-haul_depths[i])))
}

#Convert to tidy object, convert to dataframe, and filter by max depth
nc <- tidync(file)
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()
# apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
nc_bottom_all2 <- nc_bottom_all
nc_bottom_all2 <- unique(nc_bottom_all2)

saveRDS(nc_bottom_all2, file="glorys_tempsal_wc_full_region_bottom.rds")
nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_tempsal_wc_full_region_bottom.rds")

##Match to survey data

##Find closest matching date
#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))
#Extract data
nc_bottom_all4 <- left_join(points, nc_bottom_all2)
colnames(nc_bottom_all4) <- c("lat_glorysphys", "lon_glorysphys", "date_glorysphys", "temp_glorys", "sal_glorys", "depth_glorysphys")
glorys_wc <- bind_cols(nc_bottom_all4, haul_combined)
saveRDS(glorys_wc, file="glorys_tempsal_WC.rds")
