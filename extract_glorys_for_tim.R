library(tidync)
library(lubridate)
library(sf)
library(dplyr)

### 1) Set WD to folder with raw GLORYS data
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/GLORYS/wc_o2/for_tim")

### 2) Pick file for year
##One year
#Note: the GLORYS files are for -138 through -114 deg W, and 26--51 deg N, from 0.51 to 1600m deep, from May 1st through October 29th
file <- "cmems_mod_glo_bgc_my_0.25deg_P1D-m_o2_138.00W-114.00W_26.00N-51.00N_0.51-1516.36m_2019-05-01-2019-10-29.nc"

### 3) Convert GLORYS raw file to a dataframe
nc <- tidync(file)
nc_df <- nc %>%
  hyper_tibble
#Note: o2 is in umol per m^3 (i.e. umol per L)

### 4) Convert hours since January 1st, 1950 to normal human calendar date
nc_df$time <- (as_datetime("1950-01-01")+hours(nc_df$time))

### 5) Conver lat/lon to UTM
coords <- nc_df %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(lonUTM=st_coordinates(.)[,1],latUTM=st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

#Coords will be a dataframe with o2, latitude, longitude, depth, time, and lat & long in UTM

### 6) Make this a function and run for each year
##List of files for looping
files <- list.files("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/GLORYS/wc_o2/for_tim")

convert_glorys <- function(file) {
  file <- file_name
nc <- tidync(file) 
nc_df <- nc %>%
  hyper_tibble
nc_df$time <- (as_datetime("1950-01-01")+hours(nc_df$time))
coords <- nc_df %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(lonUTM=st_coordinates(.)[,1],latUTM=st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)
return(coords)
}

dats <- lapply(files, convert_glorys)