### load helper functions ####
source("code/util_funs.R")
library(ggplot2)
library(readxl)

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
#Key
sci_names <- c("gadus chalcogrammus", "anoplopoma fimbria", "microstomus pacificus", "eopsetta jordani", "gadus macrocephalus", "sebastolobus altivelis", "sebastolobus alascanus", "hippoglossus stenolepis", "sebastes pinniger", "ophiodon elongatus", "sebastes crameri", "oncorhynchus keta", "oncorhynchus tshawytscha", "oncorhynchus gorbuscha", "atheresthes stomias", "merluccius productus")
spcs <- c("walleye pollock", "sablefish", "dover sole", "petrale sole", "pacific cod", "longspine thornyhead", "shortspine thornyhead", "pacific halibut", "canary rockfish", "lingcod", "darkblotched rockfish", "chum salmon", "chinook salmon", "pink salmon", "arrowtooth flounder", "pacific hake")
#1 walleye pollock, 2 sablefish, 3 dover sole, 4 petrale sole, 5, pacific cod, 6 longspine thornyhead, 7 shortspine thornyhead, 
#8 pacific halibut, 9 canary rockfish, 10 lingcod", 11 darkblotched rockfish, 12 chum salmon, 13 chinook salmon, 14 pink salmon,
#15 arrowtooth flounder, 16 pacific hake, 17, pacific ocean perch
#Flathead sole, rex sole, southern rock sole
#starry flounded
#Rockfish: yellowtail, greenstriped, widow, bocaccio, rosethorn, sharpchin, canary, splitnose, redstripe, darkblotched, stripetail
#Spotted ratfish

#Specify species
sci_name <-sci_names[1]
spc <- spcs[1]

dat.by.size <- length_expand_nwfsc(spc, sci_name)
dat_nw <- load_data_nwfsc(spc = spc, dat.by.size = dat.by.size, length=F)

dat.by.size <- length_expand_bc(sci_name)
dat_bc <- load_data_bc(sci_name = sci_name, dat.by.size = dat.by.size)

dat.by.size <- length_expand_afsc(sci_name, years=T, region=T)
dat_afsc <- load_data_afsc(sci_name = sci_name, dat.by.size = dat.by.size, length=F)
  
iphc_halibut <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/IPHC_FISS_set_halibut.xlsx")

#Bind together
dat <- bind_rows(dat_nw, dat_bc,dat_afsc)

#Calculate proportion of biomass of intermediate size
dat$cpue_kg_km2a <- dat$cpue_kg_km2 * (dat$p2+dat$p3)
#Make base map
world <- map_data("world")
#Remove background
#Map all together by year colored by bulk biomass
ggplot() + ggtitle(spc)+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=filter(dat,cpue_kg_km2a>0), aes(x=long, y=lat, colour=log(cpue_kg_km2a)), size=0.2)+facet_wrap("year")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot() + ggtitle(spc)+
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=filter(dat,cpue_kg_km2>0), aes(x=long, y=lat, colour=log(cpue_kg_km2)), size=0.2)+facet_wrap("year")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Save
ggsave(
  "figures/data_availability/pink_salmon.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 8,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)

#How many observations have CPUE but no length data?
test <- subset(dat, year>1998 &is.na(p1)&cpue_kg_km2>0)

#Length observations
ggplot(dat, aes(x=nlength, group=project, fill=project))+geom_density()+facet_wrap("year")

###Other ways to map
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=dat, aes(x=long, y=lat))

#Colour for bulk biomass
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=dat, aes(x=long, y=lat, colour=log(cpue_kg_km2)+1), size=0.5)

#Colour for size-expanded biomass
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=dat, aes(x=long, y=lat, colour=log(cpue_kg_km2a)+1), size=0.5)

#Only positive catches, size-expanded biomass
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=filter(dat,cpue_kg_km2a>0), aes(x=long, y=lat, colour=log(cpue_kg_km2a)), size=0.5)

#Only positive catches, bulk biomass
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=filter(dat,cpue_kg_km2>0), aes(x=long, y=lat, colour=log(cpue_kg_km2)), size=0.5)

#Positive catches by year
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=filter(dat,cpue_kg_km2>0), aes(x=long, y=lat, colour=log(cpue_kg_km2)), size=0.5)+facet_wrap("year")

#All catches by year
ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="darkgrey",fill="grey", alpha=1) +
  coord_sf(xlim = c(-180,-120), ylim=c(30,70))+
  geom_point(data=dat, aes(x=long, y=lat), size=0.5)+facet_wrap("year")


