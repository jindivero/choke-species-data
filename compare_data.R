library(ggplot2)
library(viridis)

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##Pull real data (example sablefish data from Chapter 2) ## 
example <- readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/example_data.rds")
#Clean up and add environmental data
kelvin = 273.15
boltz = 0.000086173324
tref <- 12
#Calculate inverse temp
example$invtemp <- (1 / boltz)  * ( 1 / (example$temp + 273.15) - 1 / (tref + 273.15))
#Calculate MI for usual case
example$mi = example$po2*exp(0.291* example$invtemp)
#Scale and such
example$temp_s <- (scale(example$temp))
example$po2_s <- (scale(example$po2))
example$mi_s <- (scale(example$mi))
example$log_depth_scaled <- scale(log(example$depth))
example$log_depth_scaled2 <- with(example, log_depth_scaled ^ 2)
example$jday_scaled <- scale(example$julian_day)
example$jday_scaled2 <- with(example, jday_scaled ^ 2)
example$X <- example$longitude
example$Y <- example$latitude
example$cpue_kg_km2 <- example$cpue_kg_km2 * (example$p2+example$p3)
example$year <- as.factor(example$year)

setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")

setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

###Pull species of interest
#One species
sci_name <- "anoplopoma fimbria" 
spc <- "sablefish" 

dat <- prepare_data(spc=spc, sci_name=sci_name, ROMS=F)
#Key: mi1= median, mi2=low, mi3=high
#cpue_kg_km2_sub = size-expanded (i.e. = dat$cpue_kg_km2 * (dat$p2+dat$p3))
#

#Remove outliers catch > 10 sd above the mean
dat$cpue_s <- scale(dat$cpue_kg_km2)
dat <- dplyr::filter(dat, cpue_s <=20)

#Separate regions
dat_nw <- subset(dat, region=="NWFSC.Combo")
dat_bc <- subset(dat, region=="BC")
dat_goa <- subset(dat, region=="GOA")
dat_bs <- subset(dat, region=="NBS"|region=="EBS")

#Calculate MI with old parameter
Eo <- 0.4480175
Ao <- 4.4733481
n <- -0.2201635
avgbn <- 0.112  # this works for the adult class (0.5 - 6 kg).  for the large adult, adjust
dat_nw$mi_old = avgbn*Ao*dat_nw$po2 *exp(Eo * dat_nw$invtemp)

#Compare NW
#Looks right
ggplot(dat_nw, aes(x=longitude, y=latitude))+geom_point()+facet_wrap("year")
ggplot(example, aes(x=X, y=Y))+geom_point()+facet_wrap("year")


ggplot(dat_nw, aes(x=mi1, y=-depth_m))+geom_point()+geom_point(example, mapping=aes(mi, -depth), colour="red", alpha=0.5)
ggplot(dat_nw, aes(x=mi_old, y=cpue_kg_km2_sub))+geom_point()+geom_point(example, mapping=aes(mi, cpue_kg_km2), colour="red", alpha=0.1)
ggplot(dat_nw, aes(x=mi_old, y=cpue_kg_km2_sub))+geom_point()

#Temperature looks similar
ggplot(dat_nw, aes(x=invtemp, y=-depth_m))+geom_point()+geom_point(example, mapping=aes(invtemp, -depth), colour="red", alpha=0.8)

#Oxygen  
ggplot(dat_nw, aes(x=po2, y=cpue_kg_km2_sub))+geom_point()+facet_wrap("year")+geom_point(example, mapping=aes(po2, cpue_kg_km2), colour="red")
ggplot(dat_nw, aes(x=po2))+geom_histogram()+facet_wrap("year")+geom_histogram(example, mapping=aes(po2), colour="red")

#Oxygen across depth
ggplot(dat_nw, aes(y=-depth_m, x=po2))+geom_point(aes(colour=log(cpue_kg_km2_sub)))+geom_point(example, mapping=aes(y=-depth, x=po2), colour="red", alpha=0.5)

#Combine and plot
example$event_id <- as.character(example$trawl_id)
combined <- left_join(example, dat_nw, by="event_id")

ggplot(combined, aes(x=po2.x, y=po2.y))+geom_point()+xlab("pO2 in situ")+ylab("pO2 GLORYS")
ggplot(combined, aes(x=po2.x, y=po2.y))+geom_point(aes(colour=depth_m))+xlab("pO2 in situ")+ylab("pO2 GLORYS")

ggplot(combined, aes(x=po2.x, y=po2.y))+geom_point(aes(colour=depth_m))+xlab("pO2 in situ")+ylab("pO2 GLORYS")

ggplot(combined, aes(x=po2.x, y=po2.y))+geom_point(aes(colour=depth_gloryso2))+xlab("pO2 in situ")+ylab("pO2 GLORYS")
combined$diff <- combined$depth_gloryso2-combined$depth_m

ggplot(combined, aes(x=po2.x, y=po2.y))+geom_point(aes(colour=diff), size=5)+xlab("pO2 in situ")+ylab("pO2 GLORYS")+scale_colour_viridis()+theme(legend.position=c(0.8, 0.5))

#Read in barth 2021 data
barth <- read.table("DOsummer2021.txt", sep =" ", header = FALSE, skip=15)

ggplot(barth, aes(x=V1, y=V2))+geom_point(aes(colour=V3), size=0.5)+scale_color_viridis()
ggplot(barth, aes(x=V3))+geom_histogram(bins=500)+xlab("Dissolved oxygen umol kg-1")+scale_x_continuous(breaks=seq(0, 500, 25), limits=c(0,500))

#Match barth and closest point
#Isolate just 2021
dat <- subset(dat_nw, year=="2019")

ggplot(barth, aes(x=V3))+geom_histogram(bins=500)+xlab("Dissolved oxygen umol kg-1")+geom_histogram(dat_nw, mapping=aes(x=o2_glorys), bins=500,colour="blue", alpha=0.5)+scale_x_continuous(breaks=seq(0, 500, 25), limits=c(0,500))

test <- RANN::nn2(dat_nw[, c('lat', 'long')], barth[, c('V2', "V3")],k = 1)
points <- dat_nw[c(test$nn.idx),]

colnames(barth) <- c("long", "lat", "o2_barth")
barth <- bind_cols(barth, points)

ggplot(barth, aes(x=o2_glorys, y=o2_barth))+geom_point()

#Alaska in situ data
ak_o2 <- readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/2012-2016_o2.rds")
#Combine with dat by hauljoin

ak_o2$event_id <- as.integer(ak_o2$hauljoin)
test$event_id <- as.integer(test$event_id)

ak_comp <- left_join(ak_o2, test, by="event_id")


