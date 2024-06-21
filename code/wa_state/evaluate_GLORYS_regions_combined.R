library(dplyr)
library(ggplot2)
library(sp)
library(tidyr)
#Set working directory
setwd("~/Dropbox/choke species/code/choke-species-data/code/wa_state")

### Set ggplot themes ###
theme_set(theme_bw(base_size = 35))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Combined GlORYS data
dat <- readRDS("glorys_combined.rds")

#Label regions
dat$survey <- case_when(!is.na(dat$stlkey)~"IPHC",
                        grepl("SYN", dat$survey_name)~"BC",
                        grepl("NWFSC", dat$survey_name)|grepl("Triennial", dat$survey_name) ~"WC",
                        is.na(dat$stlkey)&is.na(dat$survey_name)~"AK")

###Distance of haul from GLORYS point
##Oxygen
test <- matrix(nrow=nrow(dat))
for(i in 1:nrow(dat)){
  dist <- spDistsN1(as.matrix(dat[i,c("lon_start", "lat_start")]),as.matrix(dat[i,c("lon_gloryso2", "lat_gloryso2")]),longlat=FALSE) 
  test[i] <- dist
}
dat$dist_o2 <- test

##Temp-sal
test <- matrix(nrow=nrow(dat))
for(i in 1:nrow(dat)){
  dist <- spDistsN1(as.matrix(dat[i,c("lon_start", "lat_start")]),as.matrix(dat[i,c("lon_glorysphys", "lat_glorysphys")]),longlat=FALSE) 
  test[i] <- dist
}

dat$dist_phys <- test

## Plot
ggplot(dat, aes(x=dist_o2))+geom_histogram()+xlab("Distance between haul and o2 GLORYS (km)")

ggplot(dat, aes(x=dist_phys))+geom_histogram()+xlab("Distance between haul and temp GLORYS (km)")

###Difference in depth between coordinates (root squared error)
#Get haul_depth column
#bottom_depth, depth_m, depth_IPHC
dat$depth_haul <- case_when(is.na(dat$depth_m) & is.na(dat$bottom_depth)~dat$depth_IPHC,
                            is.na(dat$depth_m) & is.na(dat$depth_IPHC)~dat$bottom_depth,
                            is.na(dat$bottom_depth) & is.na(dat$depth_IPHC)~dat$depth_m)
dat$depth_diffo2 <- sqrt((dat$depth_haul-dat$depth_gloryso2)^2)
dat$depth_diffts <- sqrt((dat$depth_haul-dat$depth_glorysphys)^2)

dat$depth_diffo2 <- dat$depth_haul-dat$depth_gloryso2
dat$depth_diffts <- dat$depth_haul-dat$depth_glorysphys

#Plot
ggplot(dat, aes(x=depth_diffo2))+geom_histogram()+xlab("Difference in reported haul depth and GLORYS depth o2 (m)")
ggplot(dat, aes(x=depth_diffts))+geom_histogram()+xlab("Difference in reported haul depth and GLORYS depth temp (m)")
ggplot(dat, aes(x=depth_diffts))+geom_histogram(aes(group=survey, fill=survey))+xlab("Difference in reported haul depth and GLORYS depth temp (m)")
ggplot(dat, aes(x=depth_diffo2))+geom_histogram(aes(group=survey, fill=survey))+xlab("Difference in reported haul depth and GLORYS depth o2 (m)")
ggplot(dat, aes(abs(x=depth_diffo2)))+geom_histogram(aes(group=survey, fill=survey), binwidth=10)+xlab("Difference in reported haul depth and GLORYS depth o2 (m)")
ggplot(dat, aes(x=depth_diffo2))+geom_histogram(aes(group=survey, fill=survey), binwidth=10)+xlab("Difference in reported haul depth and GLORYS depth o2 (m)")

###Compare to in situ data
##Temperature
#Combine temperature
#In situ column names: gear_temperature, temp C, bottom_temp_c
dat$temp_haul<- case_when(is.na(dat$gear_temperature) & is.na(dat$'temp c')~dat$bottom_temp_c,
                          is.na(dat$gear_temperature) & is.na(dat$bottom_temp_c)~dat$'temp c',
                          is.na(dat$'temp c') & is.na(dat$bottom_temp_c)~dat$gear_temperature)

dat$temp_diff <- dat$temp_haul-dat$temp_glorys
ggplot(dat, aes(x=temp_diff))+geom_histogram()+xlab("Difference in reported haul temp and GLORYS temp (C)")
ggplot(dat, aes(abs(x=temp_diff)))+geom_histogram(aes(group=survey, fill=survey), binwidth=1)+xlab("Absolute difference in in situ temp and GLORYS temp (C)")

ggplot(dat, aes(x=temp_haul, y=temp_glorys))+geom_point(aes(group=survey, colour=survey))+xlab("Temp Haul")+ylab("Temp GLORYS")

#Salinity
dat$sal_diff <- dat$'salinity psu'-dat$sal_glorys
ggplot(dat, aes(x=sal_diff))+geom_histogram()+xlab("Difference in reported haul salinity and GLORYS sal (psu)")
ggplot(dat, aes(x=dat$'salinity psu', y=sal_glorys))+geom_point(aes(group=survey, colour=survey))+xlab("Sal Haul")+ylab("Sal GLORYS")

#Convert all oxygen to correct units
#GLORYS is in: o2 [mmol/m3], IPHC oxygen_ml is ml per L
SA = gsw_SA_from_SP(dat$'salinity psu', dat$depth_IPHC,dat$lon_start,dat$lat_start) #absolute salinity for pot T calc
pt = gsw_pt_from_t(SA,dat$'temp c',dat$depth_IPHC) #potential temp at a particular depth
CT = gsw_CT_from_t(SA,dat$'temp c',dat$depth_IPHC) #conservative temp
sigma0 = gsw_sigma0(SA,CT)
o2_umolkg = dat$oxygen_ml/(sigma0+1000) 
dat$o2_IPHC <- o2_umolkg

dat$o2_diff <- dat$o2_IPHC-dat$o2_glorys
ggplot(dat, aes(x=o2_diff))+geom_histogram()+xlab("Difference in reported haul oxygen and GLORYS o2 (mmol kg-1)")

#Depth differences and sal differences
ggplot(dat, aes(x=depth_diffts,y=temp_diff))+geom_point(aes(group=survey, colour=survey))+xlab("Difference in haul depth and GLORYS temp depth (m)")+ylab("Difference in haul temp and GLORYS temp")

#Map points
ggplot(subset(dat, lon_start<0), aes(x=lon_start, y=lat_start))+geom_point(aes(group=survey, colour=survey), size=0.)+geom_point(mapping=aes(x=lon_glorysphys, y=lat_glorysphys), size=0.05)

#Larger distance
ggplot(subset(dat, lon_start< 122.5 & lon_start>-133 & lat_start>48&lat_start<54.75), aes(x=lon_start, y=lat_start))+geom_point(aes(group=survey, colour=survey), size=0.8)+geom_point(mapping=aes(x=lon_glorysphys, y=lat_glorysphys), size=1)

ggplot(subset(dat, dist_phys>1 | dist_o2 >1), aes(x=lon_start, y=lat_start))+geom_point(aes(group=survey, colour=survey), size=0.8)+geom_point(mapping=aes(x=lon_glorysphys, y=lat_glorysphys), size=1)

#Missing data
ggplot(subset(dat, is.na(dat$o2_glorys)|is.na(dat$temp_glorys)), aes(x=lon_start, y=lat_start))+geom_point(aes(group=survey, colour=survey), size=0.8)+geom_point(mapping=aes(x=lon_glorysphys, y=lat_glorysphys), size=1)

#Combine with ROMS data to get comparison
ROMS <- as.data.frame(readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/haul_combined_ROMS.rds"))
ROMS2 <- as.data.frame(ROMS[,c("event_id","lon", "lat", "depth", "o2_ROMS", "temp_ROMS", "sal_ROMS")])
ROMS2$event_id <- as.character(ROMS2$event_id)
ROMS2$o2_ROMS <- as.numeric(ROMS2$o2_ROMS)
ROMS2$temp_ROMS <- as.numeric(ROMS2$temp_ROMS)
ROMS2$sal_ROMS <- as.numeric(ROMS2$sal_ROMS)

dat$event_id <- as.character(dat$event_id)
dat2 <- filter(dat, event_id %in% ROMS2$event_id)
dat2 <- filter(dat2, !is.na(dat2$event_id))
dat2 <- left_join(ROMS2, dat2, by="event_id")

#Compare ROMS and GLORYS
#Difference in depth
dat2$depths <- dat2$depth-dat2$depth_gloryso2
#Difference in O2
dat2$diff_oxs <- dat2$o2_glorys-dat2$o2_ROMS
#Plot difference in depth
ggplot(dat2, aes(x=depths))+geom_histogram()+xlab("Difference in depth between ROMS and GLORYS")

#Plot distribution of oxygen over depth
ggplot(dat2, aes(y=-depth_haul, x=o2_ROMS))+geom_point(aes(group=survey, colour=survey))
ggplot(dat2, aes(y=-depth_haul, x=o2_glorys))+geom_point(aes(group=survey, colour=survey))

#Scatterplot of temp, sal, and O2 in GLORYS vs ROMS
ggplot(dat2, aes(x=temp_ROMS, y=temp_glorys))+geom_point(aes(group=survey, colour=survey))
ggplot(dat2, aes(x=sal_ROMS, y=sal_glorys))+geom_point(aes(group=survey, colour=survey))
ggplot(dat2, aes(x=o2_ROMS, y=o2_glorys))+geom_point(aes(group=survey, colour=survey))

#Plot difference in oxygen over depth
ggplot(dat2,aes(x=depth_haul, y=diff_oxs))+geom_point(aes(group=survey, colour=survey))+xlab("Depth of haul")+ylab("Difference in GLORYS and ROMS oxygen")
#Plot difference in depth over difference in o2
ggplot(dat2,aes(x=depth_diffo2, y=diff_oxs))+geom_point(aes(group=survey, colour=survey))+xlab("Difference in haul depth and GLORYS depth")+ylab("Difference in GLORYS and ROMS oxygen")

#ROMS differences w/ haul data
dat2$depthdiff_ROMS <- dat2$depth_haul-dat2$depth
ggplot(dat2, aes(x=depthdiff_ROMS))+geom_histogram()+xlab("Difference in reported haul depth and ROMS depth (m)")
ggplot(dat2, aes(x=abs(depth_diffo2), y=abs(depthdiff_ROMS)))+geom_point()
ggplot(dat2, aes(x=depth_diffo2, y=depthdiff_ROMS))+geom_point()

dat2$tempdiff_ROMS <- dat2$temp_haul-dat2$temp_ROMS
dat2$saldiff_ROMS <-dat2$'salinity psu'-dat2$sal_ROMS

ggplot(dat2, aes(x=tempdiff_ROMS))+geom_histogram()+xlab("Difference in reported temp and ROMS")
ggplot(dat2, aes(x=saldiff_ROMS))+geom_histogram()+xlab("Difference in reported salinity and ROMS")

###Evaluate for the data far from point
far <- subset(dat, dist_o2>1.5 | dist_phys >1.5)

##Evaluate for missing data
missing <- subset(dat, is.na(dat$o2_glorys))
saveRDS(missing, file="missing_o2.rds")
#1300 missing points--are 51--64, -132-- -179, and 51-52, 173--179

missing <- subset(dat, is.na(dat$temp_glorys))
saveRDS(missing_ts, file="missing_ts_glorys_data.rds")

##Evaluate time series of a day to see if we need to smooth it
#Temperature
#Upload the BC full bottom oxgyen data, just because it's the smallest file
nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_tempsal_bc_full_region_bottom.rds")
#Pick a random latitude/longitude
#Pick a random coordinate by drawing a random number from the dataset, and then subsetting the data to only that coordinate
test <-sample(1:nrow(nc_bottom_all2), 1)
test <- nc_bottom_all2[test, 3:4]
test <- subset(nc_bottom_all2, latitude==test$latitude & longitude==test$longitude)
#Plot time series of one year
ggplot(subset(test, time>as.POSIXct("2020-08-01") & time<as.POSIXct("2020-09-01")), aes(x=time, y=thetao))+geom_line()+ylab("Temperature (C)")+xlab("Date")

#Oxygen
nc_bottom_all2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/full_regions_bottom/glorys_o2_bc_full_region_bottom copy.rds")
#Pick a random latitude/longitude
#Pick a random coordinate by drawing a random number from the dataset, and then subsetting the data to only that coordinate
test <-sample(1:nrow(nc_bottom_all2), 1)
test <- nc_bottom_all2[test, 7:8]
test <- subset(nc_bottom_all2, latitude==test$latitude & longitude==test$longitude)
#Plot time series of one year
ggplot(subset(test, time>as.POSIXct("2009-07-01") & time<as.POSIXct("2009-07-30")), aes(x=time, y=o2))+geom_line()+ylab("Oxygen mmol m^3")+xlab("Date")
