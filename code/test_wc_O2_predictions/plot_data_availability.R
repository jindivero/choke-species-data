library(sdmTMB)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)

#Load functions
source("code/test_wc_O2_predictions/helper_funs.R")

theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Load oxygen data
dat <- as.data.frame(readRDS("data/processed_data/all_o2_dat_filtered.rds"))

count <- dat %>% count(year, survey, region)
count <- dat %>% count(survey)
count <- dat %>% count(region)

#number in each survey

#Region labels
labs <- c("Aleutian Islands", "British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska")
names(labs) <- c("ai", "bc", "cc", "ebs", "goa")
dat$region <- factor(dat$region, levels=c("cc", "bc", "goa", "ebs", "ai")) 

##Plot barplot of all data available 
ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=3, labeller = labeller(region=labs), scales="free_y")+
  scale_x_continuous(breaks=c(2000,2010,2020), limits=c(2000,2024))+
  theme_minimal(base_size=20)+
  xlab("Year")+
  ylab("Number of Observations")+
  scale_fill_discrete(labels=
  c("*AFSC Aleutian Islands", "calCOFI", "CODAP", "*DFO", "*AFSC Bering Sea", 
    "*AFSC Gulf of Alaska", "NWFSC Hake", "*IPHC Longline", "Newport Line", "*NWFSC West Coast", 
    "OCNMS", "WCOA"))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/data_available.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 7,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###Map of all data available
# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)


###Map of data available
ggplot(us_coast_proj) + geom_sf() +
  xlim(min(dat$X)*1000, max(dat$X)*1000)+
  ylim(min(dat$Y)*1000, max(dat$Y)*1000)+
  geom_point(filter(dat, year>1999), mapping=aes(x=X*1000, y=Y*1000,colour=survey))+
  facet_wrap("year", ncol=7)+
  theme_minimal(base_size=20)+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_colour_discrete(labels=
                        c("*AFSC Aleutian Islands", "calCOFI", "CODAP", "*DFO", "*AFSC Bering Sea", 
                          "*AFSC Gulf of Alaska", "NWFSC Hake", "*IPHC Longline", "Newport Line", "*NWFSC West Coast", 
                          "OCNMS", "WCOA"))+
  theme(legend.position="none")
  

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/data_available_map.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 11,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###Plot of trawl data available versus synoptic data available
##Read in example fish data
fish_dat <- as.data.frame(readRDS("data/processed_data/dat_sablefish.rds"))
fish_dat$region <- case_when(fish_dat$survey=="dfo"~"bc",
                             fish_dat$survey=="EBS"~"ebs",
                             fish_dat$survey=="GOA"~"goa",
                             fish_dat$survey=="nwfsc"~"cc",
                             fish_dat$survey=="NBS"~"ebs")
labs <- c("British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska")
names(labs) <- c("bc", "cc", "ebs", "goa")
dat2 <- filter(dat, region!="ai")
dat2 <- dat2 %>%
  filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))
dat2$type <- "Synoptic Oxygen Data"
fish_dat$type <- "Bottom Trawl Survey"
labs <- c("British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska")
names(labs) <- c("bc", "cc", "ebs", "goa")
dats <- bind_rows(fish_dat, dat2)
dats$region <- factor(dats$region, levels=c("cc", "bc", "goa", "ebs")) 

ggplot(dats, aes(x=year, fill=type, linetype=type))+
  stat_count()+
  facet_wrap("region", ncol=2, labeller = labeller(region=labs))+
  #xlim(2000,2024)+
  theme_minimal(base_size=20)+
  xlab("Year")+
  ylab("Number of Observations")+
  theme(legend.position="top")+
  scale_fill_manual(values=c("black", "grey70"))+
  theme(legend.title=element_blank())

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/fish_trawl_synoptic.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 6.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Example plot of GOBH data for region and year
##GOBH data
load("code/test_wc_O2_predictions/outputs/o2_models_glorys/cc.Rdata")
cc <- output
cc <- cc[[4]][["glorys_data"]]
dat <- filter(dat, region=="cc"&year==2012)
dat$type <- "Bottom Trawl Survey"
cc$type <- "Global Ocean Biogeochemisty Hindcast"

test <- bind_rows(dat,cc)

ggplot(us_coast_proj) + geom_sf() +
  ylim(4400*1000, 4600*1000)+
  geom_point(test, mapping=aes(x=X*1000, y=Y*1000,shape=type, colour=type))+
  theme_minimal(base_size=20)+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(breaks=c(-125,-126), limits=c(300*1000, 450*1000))+
  theme(legend.position="top")+
  theme(legend.direction="vertical")+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12))+
  scale_colour_manual(values=c("navyblue", "orange"))+
  scale_shape_manual(values=c(19,8))

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/gobh_trawl_example.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 5,
  height = 5.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)


##Plot regional extents
ggplot(us_coast_proj) + geom_sf() +
  xlim(min(dat$X)*1000, max(dat$X)*1000)+
  ylim(min(dat$Y)*1000, max(dat$Y)*1000)+
  geom_point(filter(dat, year>1999), mapping=aes(x=X*1000, y=Y*1000,colour=region))+
  theme_minimal(base_size=20)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(legend.position="none")

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/map_all_regions.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 8.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

####Other options
#Filter by region
dat.2.use <- as.data.frame(filter(dat, region=="cc"))

count <- dat.2.use %>% count(year, survey)

ggplot(dat.2.use, aes(x=year))+stat_count(aes(fill=survey))+xlim(2012,2019)


dat.2.use <- as.data.frame(filter(dat, region=="bc"))

count <- dat.2.use %>% count(year, survey)

dat$region <- case_when(dat$region=="ai"~"Aleutian Islands", 
                        dat$region=="bc"~"British Columbia", 
                        dat$region=="cc"~"California Current", 
                        dat$region=="ebs"~"Eastern Bering Sea", 
                        dat$region=="goa"~"Gulf of Alaska")

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
 theme_minimal()+ylab("Number of oxygen observations")+xlab("Year")

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
  theme_minimal()+
  ylab("Number of oxygen observations")+
  xlab("Year")+
  xlim(1995,2023)

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
  theme_minimal()+
  ylab("Number of oxygen observations")+
  xlab("Year")+
  xlim(2015,2023)