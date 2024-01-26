### load helper functions ####
source("code/util_funs.R")
library(ggplot2)
library(readxl)

library(ggplot2)

#Specify species
sci_name <- "anoplopoma fimbria" 
spc <- "sablefish" 

#NWFSC Bio weight/length data availability
bio <- readRDS("data/fish_raw/NOAA/nwfsc_bio.rds")
load("data/fish_raw/NOAA/nwfsc_haul.rda")
haul <- nwfsc_haul
catch <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
names(catch) = tolower(names(catch))
names(bio) = tolower(names(bio))
names(haul) = tolower(names(haul))
bio$scientific_name <- tolower(bio$scientific_name)

bio$trawl_id = as.character(bio$trawl_id)
haul$trawl_id = as.character(haul$event_id)
catch$trawl_id=as.character(catch$trawl_id)
haul$year <- as.character(substr(haul$date, start=1, stop=4))
bio$year <- as.character(bio$year)
catch$date <- NULL
bio$date <- NULL
bio$year <- NULL

#haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
#haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)

#Combine data
dat = dplyr::left_join(catch[,c("trawl_id","common_name", "subsample_count","area_swept_ha","longitude_dd", "latitude_dd",
                                "subsample_wt_kg","total_catch_numbers","total_catch_wt_kg","cpue_kg_km2")], haul, relationship = "many-to-many") %>%
  dplyr::left_join(filter(bio[,c("trawl_id", "scientific_name", "common_name", "weight", "ageing_lab", "oto_id", "length_cm", "width_cm", "sex", "age")], !is.na(length_cm)), relationship = "many-to-many") %>%
  filter(performance == "Satisfactory")


# filter out species of interest from joined (catch/haul/bio) dataset
dat_sub = dplyr::filter(dat, common_name == spc)

# fit length-weight regression by year to predict fish weights that have lengths only.
test <- subset(dat_sub, !is.na(dat_sub$length_cm)|!is.na(dat_sub$weight))
ggplot(test, aes(longitude_dd, latitude_dd))+geom_hex()+facet_wrap("year")

#Frequency per haul
num <- aggregate(common_name~trawl_id+year, test, FUN=length)

ggplot(num, aes(x=common_name))+geom_histogram(bins=150)+xlab("Number of specimens per trawl")
ggplot(num, aes(x=common_name))+geom_histogram(bins=150)+xlab("Number of specimens per trawl")+facet_wrap("year")

world <- map_data("world")
years <- unique(dat_sub$year)
nyrs <- length(years)
nw_map <- cbind(world[rep(1:nrow(world), times=nyrs),],
                year=rep(years, each=nrow(world)))
ggplot() +
  geom_polygon(data = nw_map, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-130,-120), ylim=c(31,50))+
  geom_point(data=test, aes(x=lon_start, y=lat_start))+
  facet_wrap("year")


#BC Bio weight/length data availability

#Alaska weight/length data availability


###Species catch data range availability###
dat.by.size <- length_expand_nwfsc(spc)
dat_nw <- load_data_nwfsc(spc = spc, dat.by.size = dat.by.size)

dat.by.size <- length_expand_bc(sci_name)
dat_bc <- load_data_bc(sci_name = sci_name, dat.by.size = dat.by.size)

#dat.by.size <- length_expand_afsc(sci_name)
#dat_afsc <- load_data_afsc(spc = sci_name, dat.by.size = dat.by.size)

load("data/fish_raw/NOAA/afsc_haul.rda")
haul <- afsc_haul
load("data/fish_raw/NOAA/afsc_catch.rda")
catch <- afsc_catch
names(catch) = tolower(names(catch))
names(haul) = tolower(names(haul))
catch$scientific_name <- tolower(catch$scientific_name)
dat_ak <- left_join(catch, haul, relationship="many-to-many")
dat_ak <- filter(dat_ak, scientific_name== sci_name)
  
iphc_halibut <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/IPHC_FISS_set_halibut.xlsx")


#Make base map
world <- map_data("world")
#Map all together
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(35,70))+
  geom_point(data=haul, aes(x=lon_start, y=lat_start))
