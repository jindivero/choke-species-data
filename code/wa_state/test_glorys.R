library(dplyr)
library(ggplot2)

setwd("~/Dropbox/choke species/code/choke-species-data/code/wa_state")

#Load all glorys datasets
#o2_ak <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_o2_AK.rds")
o2_bc <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_o2_BC.rds")
o2_wc <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_o2_WC.rds")

#ts_ak <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_tempsal_AK.rds")
ts_bc <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_tempsal_BC.rds")
ts_wc <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_tempsal_WC.rds")

##Temp and sal
#Clean up 
#ts_ak$event_id <- ts_ak$hauljoin
#ts_ak$hauljoin <- NULL
ts_wc <- ts_wc[,c(1:8,12:13,16,20:28)]
ts_bc <- ts_bc[,c(1:8, 12:13, 16, 20:27)]

#Combine
#dat_phys <- bind_rows(ts_ak, ts_bc, ts_wc)
dat_phys <- bind_rows(ts_bc, ts_wc)
dat_phys$id <- rownames(dat_phys)

#Clean up more
dat_phys$year <- NULL
dat_phys$date_haul <- dat_phys$date2
dat_phys$date2 <- NULL
dat_phys$date <- NULL
dat_phys <- as.data.frame(dat_phys)

##Oxygen
#Clean up 
#o2_ak$date_gloryso2 <- as.character(o2_ak$date_gloryso2)
#o2_ak$event_id <- o2_ak$hauljoin
#o2_ak$hauljoin <- NULL
o2_wc <- o2_wc[,c(1:12,16:17,20,24:32)]
o2_bc <- o2_bc[,c(1:12, 16:17, 20, 24:31)]

#Combine 
dat_o2 <- bind_rows(o2_bc, o2_wc)
dat_o2$id <- rownames(dat_o2)

#Clean more
dat_o2$year <- NULL
dat_o2$date_haul <- dat_o2$date2
dat_o2$date2 <- NULL
dat_o2$date <- NULL
dat_o2 <- as.data.frame(dat_o2)

##Combine phys and oxygen data
#dat <- left_join(dat_o2, dat_phys, by=c("id", "lat_start", "lon_start", "date_haul", "depth_m", "bottom_temp_c", "bottom_depth", "gear_temperature", "stlkey", "event_id", "survey_name", "temp c", "salinity psu", "oxygen_ml", "oxygen_sat", "depth_IPHC", ""))
dat <- left_join(dat_o2, dat_phys, by=c("id", "lat_start", "lon_start", "date_haul", "depth_m", "bottom_temp_c", "stlkey", "event_id", "survey_name", "temp c", "salinity psu", "oxygen_ml","oxygen_umol", "oxygen_sat", "depth_IPHC"))

###Distance of haul from GLORYS point
##Oxygen
test <- matrix(nrow=nrow(dat))
for(i in 1:nrow(dat)){
  dist <- spDistsN1(as.matrix(dat[i,c("lon_start", "lat_start")]),as.matrix(dat[i,c("lon_gloryso2", "lat_gloryso2")]),longlat=FALSE) 
  test[i] <- dist
}
dat$dist_o2 <- test

##Temp-sal
dat2 <- subset(dat, !is.na(dat$lat_glorysphys))
test <- matrix(nrow=nrow(dat))
for(i in 1:nrow(dat)){
  dist <- spDistsN1(as.matrix(dat[i,c("lon_start", "lat_start")]),as.matrix(dat[i,c("lon_glorysphys", "lat_glorysphys")]),longlat=FALSE) 
  test[i] <- dist
}

dat$dist_phys <- test

## Plot
ggplot(dat, aes(x=dist_o2))+geom_histogram()

ggplot(dat, aes(x=dist_phys))+geom_histogram()

##First pass at looking through 
#Look into what is happening with those huge distance ones...
#dat2 <- subset(dat, dist_o2>1 | dist_phys>1)

#Remove ones where Alaska physical data is totally wrong 
#dat2 <- subset(dat2, lon_glorysphys<0)

#Remove ones where IPHC longitude is incorrect (greater than 179, which is the highest latitude)
#dat2 <- subset(dat2, lon_start> -179 & lon_start<179)
#Remove ones where IPHC longitude is positive
#dat2 <- subset(dat2, lon_start<0)

##Second pass
dat2 <- subset(dat, dist_o2>1 | dist_phys>1)
#All are IPHC data
#Some are the ones in the positive latitudes category
#Remove ones in year with no GLORYS data downloaded yet
#Increase distance to 3 km
#All remaining are latitude 51-54, -157 through -179
#I think it's because portions of Alaska IPHC were merged with GLORYS data from non-AK regions
#Remove these for now

#Remove bad Alaska IPHC ones
dat <- subset(dat, lon_start<0 & lon_start>-170)

## Re-Plot
ggplot(dat, aes(x=dist_o2))+geom_histogram()

ggplot(dat, aes(x=dist_phys))+geom_histogram()

###Difference in depth between coordinates (root squared error)
dat$depth_diffo2 <- sqrt((dat$depth_m-dat$depth_gloryso2)^2)
dat$depth_diffts <- sqrt((dat$depth_m-dat$depth_glorysphys)^2)

dat$depth_diffo2 <- dat$depth_m-dat$depth_gloryso2
dat$depth_diffts <- dat$depth_m-dat$depth_glorysphys

#Plot
ggplot(dat, aes(x=depth_diffo2))+geom_histogram()
ggplot(dat, aes(x=depth_diffts))+geom_histogram()

###Compare to in situ data

#Convert oxygen
#GLORYS is in: o2 [mmol/m3]

#Combine with ROMS data to get comparison

#Convert oxygen
#GLORYS is in: o2 [mmol/m3]