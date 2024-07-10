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

####File info
###3-day average values from the bottom layer of the model at each model grid
###236 cells in x-axis (longitude) and 410 cells in y (latitude)
###Variables
  ##All
    #ocean_time = time in days since 1-Jan-2070. There are 3516 records with 3-day average values from Jan 1993 to Dec 2021
    #lon_rho = is the longitude at the center of each grid cell
    #lat_rho = is the latitude at the center of each grid cell
    #mask_rho = is the land/ocean mask (1=ocean, 0=land)
    #mask_var = is the mask I use to remove cells that are not well represented in the model (inlet and narrow passages)
    #h = is the bathymetry (in m) at the center of each grid cell
  ##Temp bcc42_r43d_1993to2021_temp_bot.nc
    #temp = is the temperature (in °C) a the center of each grid at each time step.
  ##Salinity bcc42_r43d_1993to2021_salt_bot.nc
    #salt = is the salinity at the center of each grid at each time step. (in PSU--confirmed with Angelica over email)
  ##Oxygen bcc42_r43d_1993to2021_Oxygen_bot.nc
    #Oxygen = is the dissolved oxygen (in mmol/m3) at the center of each grid at each time step.

setwd("~/Dropbox/choke species/code/choke-species-data/data")
file <- ("~/Dropbox/choke species/code/choke-species-data/data/bcc42_r43d_1993to2021_temp_bot.nc")
file2 <- ("~/Dropbox/choke species/code/choke-species-data/data/bcc42_r43d_1993to2021_salt_bot.nc")
file3 <- ("~/Dropbox/choke species/code/choke-species-data/data/bcc42_r43d_1993to2021_Oxygen_bot.nc")

#Extract spatial and temporal information
nc_ds <-  nc_open(file)
nc_ds2 <-  nc_open(file2)
nc_ds3 <-  nc_open(file3)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

dim_lon <- ncvar_get(nc_ds, "lon_rho")
dim_lat <- ncvar_get(nc_ds, "lat_rho")
dim_time <- ncvar_get(nc_ds, "ocean_time")
depth <- ncvar_get(nc_ds, "h")
mask_rho <- ncvar_get(nc_ds, "mask_rho")
mask_var <- ncvar_get(nc_ds, "mask_var")

#Convert time to YYY-MM-DD format (number of days since 1-Jan-1970)
date <- as.POSIXct(ymd("1970-01-01") + dseconds(dim_time))

#Convert 2D coordinates to 1D coordinates
meta <- data.frame(id=1:length(dim_lon),lon=as.vector(dim_lon), lat=as.vector(dim_lat), depth=as.vector(depth), mask_rho=as.vector(mask_rho), mask_var=as.vector(mask_var))

#Convert to correct format
#Remove land and points poorly represented
meta <- subset(meta, mask_rho==1)
meta <- subset(meta, mask_var==1)

#Extract variables--3D array
temp <- ncvar_get(nc_ds, "temp")
sal <- ncvar_get(nc_ds2, "salt")
o2 <- ncvar_get(nc_ds3, "Oxygen")

###Vector of lat, long, date of fish catch data
##Load haul info for NOAA West Coast and BC and IPHC
load("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/fish_raw/NOAA/nwfsc_haul.rda")
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

##Combine
haul_combined <- bind_rows(nwfsc_haul, pbs_haul, IPHC)

##More housekeeping
#Restrict to southern latitudes of interest
haul_combined <- subset(haul_combined, lat_start >46)
#Isolate just date
haul_combined$date2 <- as.POSIXct(substr(haul_combined$date, 1,11))
#Restrict to only years that we have ROMS data for
haul_combined <- subset(haul_combined, date2>as.POSIXct("1993-01-01"))

#Plot ROMS and haul data to visualize
ggplot(meta, aes(x=lon, y=lat))+geom_point(size=0.01)+geom_point(haul_combined, mapping=aes(x=lon_start, y=lat_start), colour="white")

#Find closest matching lat and lon and identify index of lat and lon for getting depth
# use nn2() to calculate min distance to nearest ROMS lat/long
test <- RANN::nn2(meta[, c('lat', 'lon')], haul_combined[, c('lat_start', "lon_start")],k = 1)

# Pull lat and lon from these IDs
points <- meta[c(test$nn.idx),]

#Combine with haul info
haul_combined <- cbind(haul_combined, points)

#Find closest matching date
date_index <- matrix(nrow=length(haul_combined$date2))
for(i in 1:length(haul_combined$date2)){
  date_index[i] <- which(abs(date-haul_combined$date2[i]) == min(abs(date-haul_combined$date2[i])))
}

#Add ROMS date to haul data
haul_combined$ROMS_date <- date[c(date_index)]

#Identify index of matrix for lat and lon that correspond to nearest point for extracting 
index_lon <- matrix(nrow=length(haul_combined$lon), ncol=2)
for(i in 1:length(haul_combined$lon)){
  test <- which(dim_lon == haul_combined$lon[i], arr.ind = TRUE)
  index_lon[i,1] <- test[1,1]
  index_lon[i,2] <- test[1,2]
}

index_lat <- matrix(nrow=length(haul_combined$lon), ncol=2)
for(i in 1:length(haul_combined$lat)){
  test <- which(dim_lat == haul_combined$lat[i], arr.ind = TRUE)
  index_lat[i,1] <- test[1,1]
  index_lat[i,2] <- test[1,2]
}

#Confirm that match--yay!

#Extract temp, o2, and salinity from lat, lon, date index
temps <- matrix(nrow=length(haul_combined$lat))
for(i in 1:length(haul_combined$lat)){
  temps[i] <- temp[index_lat[i,1], index_lat[i,2], date_index[i]]
}
sals <- matrix(nrow=length(haul_combined$lat))
for(i in 1:length(haul_combined$lat)){
  sals[i] <- sal[index_lat[i,1], index_lat[i,2], date_index[i]]
}
o2s <- matrix(nrow=length(haul_combined$lat))
for(i in 1:length(haul_combined$lat)){
  o2s[i] <- o2[index_lat[i,1], index_lat[i,2], date_index[i]]
}

#Add to haul_combined
haul_combined$o2_ROMS <- o2s
haul_combined$temp_ROMS <- temps
haul_combined$sal_ROMS <- sals

saveRDS(haul_combined, file="haul_combined_ROMS.rds")

#### Monthly, seasonal, annual summaries

#### Identify heatwaves

#### Calculate anomalies









##### Using CDO tools
install.packages("tidyverse")
library(tidyverse)
install.packages("tidync")
library(tidync)
library(here)

setwd("~/Dropbox/choke species/code/choke-species-data/data")
test <- "cdo mergetime *.bcc42_r43d_1993to2021_temp_bot.nc"
system(test)
