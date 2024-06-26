library(tidync)
library(lubridate)
library(sf)
library(dplyr)
library(sdmTMB)
library(seacarb)
library(respR)

basewd <- getwd()

### 1) Set WD to folder with raw GLORYS data
setwd("/Users/essing/Library/CloudStorage/Dropbox/for_tim")

### 2) Pick file for year
##One year
#Note: the GLORYS files are for -138 through -114 deg W, and 26--51 deg N, from 0.51 to 1600m deep, from May 1st through October 29th
file <- "cmems_mod_glo_bgc_my_0.25deg_P1D-m_o2_138.00W-114.00W_26.00N-51.00N_0.51-1516.36m_2019-05-01-2019-10-29.nc"

### 3) Convert GLORYS raw file to a dataframe, use only depths > 30 meters
nc <- tidync(file)
nc_df <- nc %>%
  hyper_tibble %>%
  filter(depth >=30)
# remove large list from memory
rm(nc)

#Note: o2 is in umol per m^3 (i.e. umol per L)

### 4) Convert hours since January 1st, 1950 to normal human calendar date
nc_df$time <- (as_datetime("1950-01-01")+hours(nc_df$time))

### 5) Convert lat/lon to UTM - slower version
nc_df <- nc_df %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 


# add column for day of year (doy).  Will be the time variable
red_day <- nc_df %>% distinct(time)
red_day$doy <- as.POSIXlt(red_day$time, format = "%Y-%b-%d")$yday

nc_df$doy <- NA
for (i in 1:nrow(red_day)) {
  index <- which(nc_df$time == red_day$time[i])
  nc_df$doy[index] <- red_day$doy[i]
}

#nc_df$doy <- as.POSIXlt(nc_df$time, format = "%Y-%b-%d")$yday
#nc_df will be a dataframe with o2, latitude, longitude, depth, time, and lat & long in UTM

### 6) Make this a function and run for each year
##List of files for looping

run_all_years <- F
if (run_all_years) {
files <- list.files("/Users/essing/Library/CloudStorage/Dropbox/for_tim")

convert_glorys <- function(file_name) {
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
}

# load Newport data
setwd(basewd)
newport_data <- readRDS("test_data/data/newport_bottom.RDS")

# neeed to add latitude, depth, and convert DO to mmol
newport_lat <- 44.65
newport_data$latitude <- newport_lat
newport_data$year <- year(newport_data$sample_date)
newport_data$doy <- as.POSIXlt(newport_data$sample_date, format = "%Y-%b-%d")$yday
newport_data$depth <- p2d(lat = newport_lat,
                          p = newport_data$pressure..dbar.)

newport_data$o2 <-convert_DO(x = newport_data$dissolved.oxygen..ml.L.,
                                                                from = "ml / l",
                                                                to = "mmol / l",
                                                                S = newport_data$practical.salinity,
                                                                t = newport_data$temperature..degC.)
# now get lat and long in UTC coordinates
newport_data <- newport_data %>%
  st_as_sf(coords=c('longitude..degW.','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 


# setup sdmTMB
spde <- make_mesh(data = nc_df, xy_cols = c("X","Y"), n_knots = 200)
m1 <- sdmTMB(formula = o2  ~ 0 + s(depth) ,
             mesh = spde,
             data = nc_df, 
             family = gaussian(), 
             time = "doy",
             spatial = "on",
             spatiotemporal  = "ar1")
