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

#Subset IPHC to separate from BC data
IPHC <- subset(IPHC, lat_start >=54 | (lat_start >51 & lon_start < -134) | (lat_start >50 & lon_start >0))

haul <- haul[,c("hauljoin", "start_time", "start_latitude", "start_longitude", "bottom_depth", "gear_temperature")]
colnames(haul) <- c("hauljoin", "date", "lat_start", "lon_start", "bottom_depth", "gear_temperature")
##Combine
haul_combined <- bind_rows(haul, IPHC)

##More housekeeping
#Isolate just date
haul_combined$date2 <- as.POSIXct(substr(haul_combined$date, 1,11))
#Restrict to only years that we have glorys data for
haul_combined <- subset(haul_combined, date2>as.POSIXct("1993-01-01"))
#Remove 2022 (only have GLORYS data for 2021)
haul_combined <- subset(haul_combined, date2<as.POSIXct("2021-12-31"))

###Data
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/ak_phys")
combined <- "cdo mergetime *.nc glorys_tempsal_ak_1993_2021_raw.nc"
system(combined)

setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/ak2")
combined2 <- "cdo mergetime *.nc glorys_tempsal_ak2_1993_2021_raw.nc"
system(combined2)

#thetao (temp, degrees C)
#so (salinity, PSU)
#time (gregorian, hours since 1950-01-01)
#longitude and latitude
#depth (m)

##Convert .nc to a readable file in R to match with survey data
#Make a dataframe-formatted file
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/ak_phys")
file <- "glorys_tempsal_ak_1993_2021_raw.nc"
nc <- tidync(file)

#Get depths and times
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

##Subset list of dates of hauls
#Get unique list of haul dates
haul_dates <- as.character(unique(haul_combined$date2))
#Convert 
times2 <- as.data.frame(as_datetime(times$time, "1950-01-01")+hours(times$time))
colnames(times2) <- "date"
times2$time <- times$time
times2$date <- as.vector(as.character(as.Date(times2$date, format='%m/%d/%Y')))
#Subset 
times3 <- filter(times2, date %in% haul_dates)

# Add a line to the function used for others to identify apply to all time steps and bind into a combined df
nc_bottom_all <- purrr::map_df(times$time,extract_bottom_vars)

setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/ak2")
file2 <- "glorys_tempsal_ak2_1993_2021_raw.nc"
nc <- tidync(file2)

##Get depths and times
depths <- nc %>% activate("D1") %>% hyper_tibble()
times <- nc %>% activate("D0") %>% hyper_tibble()

##Subset list of dates of hauls
#Get unique list of haul dates
haul_dates <- as.character(unique(haul_combined$date2))
#Convert 
times2 <- as.data.frame(as_datetime(times$time, "1950-01-01")+hours(times$time))
colnames(times2) <- "date"
times2$time <- times$time
times2$date <- as.vector(as.character(as.Date(times2$date, format='%m/%d/%Y')))
#Subset 
times3 <- filter(times2, date %in% haul_dates)

# apply to all time steps and bind into a combined df
nc_bottom_all_b <- purrr::map_df(times3$time,extract_bottom_vars)

#Convert time to calendar day
nc_bottom_all <- nc_bottom_all %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))
nc_bottom_all_b <- nc_bottom_all_b %>% mutate(time=as_datetime("1950-01-01 00:00:00")+hours(time))

#Save
saveRDS(nc_bottom_all, file="glorys_tempsal_ak_full_region_bottom.rds")
saveRDS(nc_bottom_all_b, file="glorys_tempsal_ak2_full_region_bottom.rds")

#Combine
nc_bottom_all <-readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_tempsal_ak_full_region_bottom.rds")
nc_bottom_all_b <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_tempsal_ak2_full_region_bottom.rds")
nc_bottom_all2 <- bind_rows(nc_bottom_all, nc_bottom_all_b)
nc_bottom_all2 <- distinct(nc_bottom_all2, latitude, longitude, time, .keep_all = TRUE)
#Get into the right 
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))

#saveRDS(nc_bottom_all2, file="glorys_tempsal_ak_combinededited_full_region_bottom.rds")
#nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_tempsal_akcombined_full_region_bottom.rds")

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
#Extract data
nc_bottom_all4 <- left_join(points, nc_bottom_all2)
colnames(nc_bottom_all4) <- c("lat_glorysphys", "lon_glorysphys", "date_glorysphys", "sal_glorys", "temp_glorys", "depth_glorysphys")
glorys_ak <- bind_cols(nc_bottom_all4, haul_combined)
saveRDS(glorys_ak, file="glorys_tempsal_AKnew.rds")

#######OXYGEN#############################################
nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_o2_ak_combinededited_full_region_bottom.rds")
nc_bottom_all2$time <- as.character(nc_bottom_all2$time)
nc_bottom_all2 <- distinct(nc_bottom_all2, latitude, longitude, time, .keep_all = TRUE)

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
#Extract data
nc_bottom_all4 <- left_join(points, nc_bottom_all2)
colnames(nc_bottom_all4) <- c("lat_gloryso2", "lon_gloryso2", "date_gloryso2", "no3_glorys", "o2_glorys", "po4_glorys", "chl_glorys","si_glorys", "nppv_glorys","depth_gloryso2")
glorys_ak <- bind_cols(nc_bottom_all4, haul_combined)
saveRDS(glorys_ak, file="glorys_o2_AKnew.rds")







###Add in August 31--September 30 that missed the first time around
setwd("~/Dropbox/choke species/code/Copernicus/o2/alaska/ak3")
combined2 <- "cdo mergetime *.nc glorys_o2_ak3_1993_2021_raw.nc"
system(combined2)

##Look at dimensions to get time and variables
file <- ("glorys_o2_bc3_1993_2021_raw.nc")
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
nc_bottom_all2 <- nc_bottom_all
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))
nc_bottom_all2 <- distinct(nc_bottom_all2, latitude, longitude, time, .keep_all = TRUE)
