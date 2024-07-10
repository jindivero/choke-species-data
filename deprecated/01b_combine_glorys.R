library(dplyr)
library(ggplot2)
library(sp)
library(tidyr)

setwd("~/Dropbox/choke species/code/choke-species-data/code/wa_state")
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")

#Load all glorys datasets
o2_ak <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_o2_AKnew.rds")
o2_bc <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_o2_BC.rds")
o2_wc <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_o2_WC.rds")

ts_ak <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_tempsal_AKnew.rds")
ts_bc <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_tempsal_BC.rds")
ts_wc <- readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_tempsal_WC.rds")

#Add BC October data where missing
bc_oct_o2 <-  readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_o2_bc3_full_region_bottom_octdates.rds")
bc_oct_ts <-  readRDS("~/Dropbox/choke species/code/choke-species-data/data/glorys/glorys_hauls/glorys_tempsal_bc3_full_region_bottom_octdates.rds")

##Temp and sal
#Clean up 
ts_ak$event_id <- ts_ak$hauljoin
ts_ak$date_glorysphys <- as.character(ts_ak$date_glorysphys)
ts_ak$hauljoin <- NULL
ts_wc <- ts_wc[,c(1:8,12:13,16,20:28)]
ts_bc <- ts_bc[,c(1:8, 12:13, 16, 20:27)]
#Fix the few points where sal and temp got switched
ts_bc$temp_glorys2 <- case_when(ts_bc$temp_glorys>16 ~ ts_bc$sal_glorys,
                               ts_bc$temp_glorys<16~ts_bc$temp_glorys)
ts_bc$sal_glorys2 <- case_when(ts_bc$sal_glorys<28 ~ ts_bc$temp_glorys,
                              ts_bc$sal_glorys>28 ~ ts_bc$sal_glorys)
ts_bc$temp_glorys <- ts_bc$temp_glorys2
ts_bc$temp_glorys2 <- NULL
ts_bc$sal_glorys <- ts_bc$sal_glorys2
ts_bc$sal_glorys2 <- NULL

#Add October dates
ts_bc_octs <- subset(ts_bc, is.na(temp_glorys) & date_glorysphys > as.POSIXct("2007-09-30") & date_glorysphys <as.POSIXct("2007-11-01"))
bc_oct_ts2 <- unique(bc_oct_ts)

#Combine
dat_phys <- bind_rows(ts_ak, ts_bc, ts_wc)
#dat_phys$id <- rownames(dat_phys)

#Clean up more
dat_phys$year <- NULL
dat_phys$date_haul <- dat_phys$date2
dat_phys$date2 <- NULL
dat_phys$date <- NULL
dat_phys <- as.data.frame(dat_phys)



missing <- subset(dat_phys, is.na(dat_phys$temp_glorys))
missing <- subset(missing, date_glorysphys<as.POSIXct("2021-06-30"))

missing_oct <- subset(dat_phys, date_glorysphys=="2007-10-01"|date_glorysphys=="2007-10-01"|date_glorysphys=="2007-10-03"|date_glorysphys=="2007-10-08"|date_glorysphys=="2007-10-09"|date_glorysphys=="2007-10-10"|date_glorysphys=="2007-10-11"|date_glorysphys=="2007-10-12")
missing_oct <- missing_oct[,c("lat_glorysphys", "lon_glorysphys", "date_glorysphys")]
missing_oct <- unique(missing_oct)
saveRDS(missing_oct, file="missing_oct_ts.rds")

##Oxygen
#Clean up 
o2_ak$date_gloryso2 <- as.character(o2_ak$date_gloryso2)
o2_ak$event_id <- o2_ak$hauljoin
o2_ak$hauljoin <- NULL
o2_wc <- o2_wc[,c(1:12,16:17,20,24:32)]
o2_bc <- o2_bc[,c(1:12, 16:17, 20, 24:31)]

#Combine 
dat_o2 <- bind_rows(o2_ak, o2_bc, o2_wc)
#dat_o2$id <- rownames(dat_o2)

#Clean more
dat_o2$year <- NULL
dat_o2$date_haul <- dat_o2$date2
dat_o2$date2 <- NULL
dat_o2$date <- NULL
dat_o2 <- as.data.frame(dat_o2)

missing <- subset(dat, is.na(dat$o2_glorys))
missing <- missing[,c("lat_gloryso2", "lon_gloryso2", "date_gloryso2")]
missing <- unique(missing)

##Combine phys and oxygen data
dat <- mutate(dat_o2,dat_phys)

#Isolate missing data

#Add Alaska Aug 31--Sept data where missing

saveRDS(dat, "glorys_combined.rds")
#dat <- full_join(dat_o2, dat_phys, by=c("lat_start", "lon_start", "date_haul", "depth_m", "bottom_temp_c", "bottom_depth", "gear_temperature", "stlkey", "event_id", "survey_name", "temp c", "salinity psu", "oxygen_ml", "oxygen_sat", "depth_IPHC"))


##Code from when I messed up the GLORYS download before
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
#dat2 <- subset(dat, dist_o2>1 | dist_phys>1)
#All are IPHC data
#Some are the ones in the positive latitudes category
#Remove ones in year with no GLORYS data downloaded yet
#Increase distance to 3 km
#All remaining are latitude 51-54, -157 through -179
#I think it's because portions of Alaska IPHC were merged with GLORYS data from non-AK regions
#Remove these for now

#Remove bad Alaska IPHC ones
#dat <- subset(dat, lon_start<0 & lon_start>-170)

## Re-Plot
#ggplot(dat, aes(x=dist_o2))+geom_histogram()

#ggplot(dat, aes(x=dist_phys))+geom_histogram()