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
####BC
###Load haul info for BC and IPHC
pbs_haul <- readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/fish_raw/BC/pbs-haul.rds")
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
haul_combined <- bind_rows(pbs_haul, IPHC)

##More housekeeping
#Restrict to southern latitudes of interest

haul_combined <- subset(haul_combined, lat_start >=48 & lat_start<= 54 & lon_start >= -134 & lon_start <=-124)
#Isolate just date
haul_combined$date2 <- as.POSIXct(substr(haul_combined$date, 1,11))
#Restrict to only years that we have GLORYS data for
haul_combined <- subset(haul_combined, date2>as.POSIXct("1993-01-01"))
#Remove 2022 (only have GLORYS data for 2021)
haul_combined <- subset(haul_combined, date2<as.POSIXct("2020-12-31"))

###GLORYS Temperature and salinity 
####Set wd
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/bc")
combined <- "cdo mergetime *.nc glorys_tempsal_bc_1993_2021_raw.nc"
system(combined)

##Look at dimensions to get time and variables
file <- ("glorys_tempsal_bc_1993_2021_raw.nc")
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
nc_bottom_all2 <- nc_bottom_all
nc_bottom_all2 <- unique(nc_bottom_all2)
saveRDS(nc_bottom_all2, file="glorys_tempsal_bc_full_region_bottom.rds")
#nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_tempsal_bc_full_region_bottom.rds")

##Match to survey data
#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
#Extract data
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))
nc_bottom_all4 <- left_join(points, nc_bottom_all2)
colnames(nc_bottom_all4) <- c("lat_glorysphys", "lon_glorysphys", "date_glorysphys", "temp_glorys", "sal_glorys", "depth_glorysphys")
saveRDS(nc_bottom_all4, file="glorys_tempsal_BC.rds")

###oxygen
setwd("~/Dropbox/choke species/code/Copernicus/o2/bc")

combined <- "cdo mergetime *.nc glorys_o2_bc_1993_2021_raw.nc"
system(combined)

##Look at dimensions to get time and variables
file <- ("glorys_o2_bc_1993_2021_raw.nc")
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
#nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_o2_bc_full_region_bottom.rds")
nc_bottom_all2 <- nc_bottom_all

#Extract out for each haul the closest matching lat and lon
# use nn2() to calculate min distance to nearest ROMS lat/long for each date
test <- RANN::nn2(nc_bottom_all2[, c('latitude', 'longitude')], haul_combined[, c('lat_start', "lon_start")],k = 1)
points <- nc_bottom_all2[c(test$nn.idx),c("latitude", "longitude")]
#Combine date and coordinates
points$time <- as.character(haul_combined$date2)
#Extract data
nc_bottom_all2$time <- as.character(as.Date(nc_bottom_all2$time, format='%m/%d/%Y'))
nc_bottom_all4 <- left_join(points, nc_bottom_all2)
colnames(nc_bottom_all4) <- c("lat_gloryso2", "lon_gloryso2", "date_gloryso2", "no3_glorys", "o2_glorys", "po4_glorys", "chl_glorys","si_glorys", "nppv_glorys", "depth_gloryso2")

saveRDS(nc_bottom_all4, file="glorys_o2_BC.rds")
