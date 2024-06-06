# Process raw GLORYS data into proper format for joining with haul data
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

extract_bottom_vars <- function(tstep){
  nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
    hyper_tibble() %>% 
    # filter for bottom-most depth layer
    group_by(longitude,latitude) %>% 
    filter(depth==max(depth))
  nc_df
}

####Alaska
####Set wd
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/ak_phys")

###Haul data
IPHC <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/IPHC_FISS_set_halibut.xlsx")
bio2 <-readRDS("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_all_levels.RDS")
haul <- bio2$haul

##Housekeeping
colnames(haul) <- tolower(colnames(haul))
colnames(IPHC) <- tolower(colnames(IPHC))
IPHC <- IPHC[,c("stlkey", "date", "beginlat", "beginlon", "begindepth (fm)", "temp c", "salinity psu", "oxygen_ml", "oxygen_umol", "oxygen_sat")]
IPHC$lat_start <- IPHC$beginlat
IPHC$beginlat <- NULL
IPHC$lon_start <- IPHC$beginlon
IPHC$beginlon <- NULL
IPHC$depth_IPHC <- IPHC$`begindepth (fm)`
IPHC$`begindepth (fm)` <- NULL

haul <- haul[,c("hauljoin", "start_time", "start_latitude", "start_longitude", "bottom_depth", "gear_temperature")]
colnames(haul) <- c("hauljoin", "date", "lat_start", "lon_start", "bottom_depth", "gear_temperature")
##Combine
haul_combined <- bind_rows(haul, IPHC)

##More housekeeping
#Restrict to lat/lon with GLORYS data
haul_combined <- subset(haul_combined, lat_start >=51 & lon_start <= -133)
#Isolate just date
haul_combined$date2 <- as.POSIXct(substr(haul_combined$date, 1,11))
#Restrict to only years that we have ROMS data for
haul_combined <- subset(haul_combined, date2>as.POSIXct("1993-01-01"))
#Remove 2022 (only have GLORYS data for 2021)
haul_combined <- subset(haul_combined, date2<as.POSIXct("2020-12-31"))

###GLORYS Temperature and salinity 
combined <- "cdo mergetime *.nc glorys_tempsal_ak_1993_2021_raw.nc"
system(combined)

##Look at dimensions to get time and variables
file <- ("glorys_tempsal_ak_1993_2021_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

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
nc_bottom_all2 <- as.data.frame(nc_bottom_all)
saveRDS(nc_bottom_all2, file="glorys_tempsal_ak_full_region_bottom.rds")
nc_bottom_all2 <- unique(nc_bottom_all2)
nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_tempsal_ak_full_region_bottom.rds")


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
