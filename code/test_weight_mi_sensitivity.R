setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

library(ggplot2)
library(viridis)

###Compare for sablefish
#One species
sci_name <- "anoplopoma fimbria" 
spc <- "sablefish" 

dat <- prepare_data(spc=spc, sci_name=sci_name)

#Calculate bias
dat$bias <- dat$mi1-dat$mi5

#Plot of MI as a function of depth when using median weight vs weight for each haul
ggplot(dat, aes(x=mi1, y=-depth_m))+geom_point(size=2)+geom_point(dat, mapping=aes(x=mi5, y=-depth_m), colour="blue", size=2)
  
#Plot as x and y, with colors for depth:
ggplot(dat, aes(x=mi1, y=mi5))+geom_point(aes(color=depth_m), size=2)+geom_abline(a=0, b=1)+xlab("MI with median weight")+ylab("MI with haul-specific mean weight")+theme(legend.position=c(0.8,0.2))+scale_colour_viridis()
#And biomass:
ggplot(dat, aes(x=mi1, y=mi5))+geom_point(aes(color=log(cpue_kg_km2)), size=2)+geom_abline(a=0, b=1)+xlab("MI with median weight")+ylab("MI with haul-specific mean weight")+theme(legend.position=c(0.8,0.2))+scale_colour_viridis()

#Plot of bias across depth, colored by catch
ggplot(dat, aes(x=depth_m, y=bias))+geom_point(aes(color=log(cpue_kg_km2)), size=2)+theme(legend.position=c(0.8,0.2))+scale_colour_viridis()

#Plot size vs depth
ggplot(dat, aes(x=depth_m, y=haul_weight))+geom_point()
ggplot(dat, aes(x=depth_m, y=haul_weight))+geom_point(aes(colour=as.factor(project)))
ggplot(dat, aes(x=depth_m, y=haul_weight))+geom_point(aes(colour=log(cpue_kg_km2)))+scale_colour_viridis()
ggplot(dat, aes(x=depth_m, y=haul_weight))+geom_point(aes(colour=mi1))+scale_colour_viridis()



###Compare for pollock
