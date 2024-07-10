###Run a daisy-chained model for a single species for all regions###
devtools::install_github("pbs-assess/sdmTMB", ref="newlogistic2", dependencies = TRUE, force=TRUE)
install.packages("TMB", type="source")
library(sdmTMB)
library(TMB)
library(ggplot2)
library(purrr)
library(mvtnorm)
library(stringr)
install.packages("visreg")
library(visreg)

setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")

setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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

dat_nw$fake <- sqrt(log(dat_nw$mi1))
dat_nw$fake_s <- scale(dat_nw$fake)
ggplot(dat_nw, aes(x=fake))+geom_histogram()
ggplot(dat_nw, aes(x=fake_s))+geom_histogram()
ggplot(dat_nw, aes(x=mi1_s2))+geom_histogram()

#Scale separately?


#Look at data
ggplot(dat, aes(x=mi1_s, y=cpue_kg_km2))+geom_point(aes(group=region, color=region))+facet_wrap("year")
ggplot(dat, aes(x=mi1, y=cpue_kg_km2_sub))+geom_point(aes(group=region, color=region))+facet_wrap("year")

#Make list of region data
dats <- list(dat_nw, dat_bc, dat_goa, dat_bs)

#Starting values
start <- matrix(data=c(-1.1, 0.1, 212), nrow=3,ncol=1) #From model fit to original sablefish data
start <- matrix(data=c(-0.01, 1, 50), nrow=3,ncol=1) #From model fit to original sablefish data

start <- matrix(data=c(0.5, 2), nrow=2,ncol=1) #From model fit to original sablefish data

#For initial prior on s50, we need to get the scaled version of 2 (based on )
dat_nw$mi1_s2 <- scale(dat_nw$mi1)
s50_prior <- (2-attr(dat_nw$mi1_s2, "scaled:center"))/attr(dat_nw$mi1_s2, "scaled:scale")

#Create matrix to store estimated effects
effects <- matrix(NA, nrow=(length(dats)-1), ncol=6)

#Initialize list to save model outputs
models <- vector(mode='list', length=length(dats))

for (i in 1:length(dats)){
  #Isolate dataset and create mesh
  dat <- dats[i]
  mesh <- make_mesh(dat_nw,xy_cols = c('X','Y'), n_knots=250) #cutoff=20
  #For initial model, fit a model with a generalized lognormal prior:
  for (i in 1:1){
m2 <- try(sdmTMB(cpue_kg_km2_sub ~ -1+year+breakpt(fake_s)+log_depth_scaled+log_depth_scaled2,
                 data = dat_nw, 
                 spatial = "on",
                 spatiotemporal="off",
                 mesh=mesh,
                 anisotropy=T,
                 reml=F,
                  time=NULL,
                 family =tweedie(link="log"),
                 #extra_time?, 
               # priors=sdmTMBpriors(threshold = normal(c(NA, s50_prior), c(NA, 0.5))),
                control = sdmTMBcontrol(
                start = list(b_threshold = start))))
                #other control options to try
                # eval.max = 1000L,
                # iter.max = 100L,
                  #multiphase=T,
             #    map=list(b_threshold=c(NA, NA, 212)))))
                # lower = list(b_threshold = c(NA,0,NA)),
                # upper=list(b_threshold = c(NA,NA,NA)),
                #newton_loops=100,
               # nlminb_loops=100)))
  }
  #For subsequent models, fit a model with a normal prior using the estimated parameters from the previous region
for (i in 2:length(dats)){
  m2 <- try(sdmTMB(cpue_kg_km2_sub ~ -1+as.factor(year)+breakpt(mi1_s)+log_depth_scaled+log_depth_scaled2, 
                   data = dat, 
                   spatial = "on",
                   spatiotemporal="off",
                   mesh=mesh,
                   family =tweedie(link="log"),
                   priors=sdmTMBpriors(threshold = mvnormal(c(effects[i-1,1], effects[i-1,3]), c(effects[i-1,2], effects[i-1,4]))),
                   control = sdmTMBcontrol(
                  start = list(b_threshold = start))))
}
  #For each of those models, pull out the model output and parameters and add to the effects matrix
for (i in 1:length(dats)){
  #Extract model output and add to list
  try(tidy(m2))
  models[i] <- m2
  par_estimates <- as.data.frame(tidy(m2, conf.int = TRUE, effects="fixed"))
  row_names(par_estimates) <- par_estimates$term
  
  #Extract threshold parameter estimates to feed into next prior
  effects[i,1]  <- 
  effects[i,2]  <- parfit$sd[]
  effects[i,3] <- parfit$value[grep("s95", parnames)]
  effects[i,4] <- parfit$value[grep("s95", parnames)]
  effects[i,5] <- parfit$value[grep("smax", parnames)]
  effects[i,6] <- parfit$value[grep("smax", parnames)]
}
}

#Save model output
try(tidy(m2))
#Extract model median
#Extract model SD
parfit <- model$sd_report
}
