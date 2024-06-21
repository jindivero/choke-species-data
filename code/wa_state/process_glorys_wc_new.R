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

#Extract bottom values
extract_bottom_vars <- function(tstep){
  nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
    hyper_tibble() %>% 
    # filter for bottom-most depth layer
    filter(longitude=long_haul, latitude=lat_haul) %>% 
    filter(depth==depth_haul)
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
setwd("~/Dropbox/choke species/code/Copernicus/o2/nwfsc")

#Combine all the separate files into one
combined <- "cdo mergetime *.nc glorys_o2_wc_1993_2021_raw.nc"
system(combined)

##Look at dimensions to get time and variables
file <- ("glorys_o2_wc_1993_2021_raw.nc")
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
glorys_dat <- list()
for(i in 1:length(time_haul)){
  print(i)
  x <- extract_haul(tstep[i], glorys_depth[i], haul_lon[i], haul_lat[i])
  glorys_dat[[i]] <- x
}

saveRDS(fingers_crossed, file="glorys_wc_o2.rds")

#Average if multiple rows (with multiple latitudes)
glorys_dat2 <- list()
for(i in 1:length(glorys_dat)){
  if(nrow(glorys_dat[[i]])>1) {
    x <- glorys_dat[[i]]%>%summarise_all(funs(mean))
    glorys_dat2[[i]] <- x
  }
  if(nrow(glorys_dat[[i]])==1){
  x <- glorys_dat[[i]]
  glorys_dat2[[i]] <- x
  }
}

#Bind into one dataframe
glorys <- bind_rows(glorys_dat2)

#Convert time into correct date format
glorys$time <- (as_datetime("1950-01-01")+hours(glorys$time))
glorys$time <- as.character(as.Date(glorys$time, format='%m/%d/%Y'))

#Rename columns
colnames(glorys) <- c("no3_glorys", "o2_glorys", "po4_glorys", "chl_glorys", "si_glorys", "nppv_glorys", "lon_gloryso2", "lat_gloryso2", "depth_gloryso2", "date_gloryso2")

saveRDS(glorys, file="glorys_wc_o2.rds")

#Bind with haul data
glorys_haul <- bind_cols(glorys, haul_combined)
saveRDS(glorys, file="glorys_haul_wc_o2.rds")

