###Run a daisy-chained model for a single species for all regions###
devtools::install_github("pbs-assess/sdmTMB", ref="newlogistic2", dependencies = TRUE, force=TRUE)
library(sdmTMB)
install.packages("TMB")
library(TMB)
install.packages("glmmTMB", type="source")
library(glmmTMB)
library(ggplot2)
library(purrr)
library(mvtnorm)

setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")
library(stringr)

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
dat$cpue_s <- scale(dat$cpue_kg_km2_sub)
dat <- dplyr::filter(dat, cpue_s <=20)

#Separate regions
dat_nw <- subset(dat, region=="NWFSC.Combo")
dat_bc <- subset(dat, region=="BC")
dat_goa <- subset(dat, region=="GOA")
dat_bs <- subset(dat, region=="NBS"|region=="EBS")

#Make list of region data
dats <- list(dat_nw, dat_bc, dat_goa, dat_bs)

#Starting values
start <- matrix(data=c(-5, 5, 100), nrow=3,ncol=1)

#Create matrix to store estimated effects
effects <- matrix(NA, nrow=(length(dats)-1), ncol=6)


#Initialize list to save model outputs
models <- vector(mode='list', length=length(dats))

for (i in 1:length(dats)){
  #Isolate dataset and create mesh
  dat <- dats[i]
  mesh <- make_mesh(dat,xy_cols = c('X','Y'), cutoff = 20) #was 20
  #For initial model, fit a model with a generalized lognormal prior:
  for (i in 1:1){
m2 <- try(sdmTMB(cpue_kg_km2_sub ~ -1+as.factor(year)+logistic(mi1_s)+log_depth_scaled+log_depth_scaled2, 
                 data = dat, 
                 spatial = "on",
                 spatiotemporal="off",
                 mesh=mesh,
                 anisotropy=T,
                 reml=F,
                 time=NULL,
                 family =tweedie(link="log"),
               #  priors=sdmTMBpriors(threshold = normal(c(5, NA, NA), c(1, NA, NA))),
                 control = sdmTMBcontrol(
                  start = list(b_threshold = start),
                   newton_loops=2,
                   nlminb_loops=5)))
  }
  #For subsequent models, fit a model with a normal prior using the estimated parameters from the previous region
for (i in 2:length(dats)){
  init_vals$petralesole$m2a$prior <- normal(c(NA, NA, NA, 0.3306), c(NA, NA, NA, 0.173))
  m2 <- try(sdmTMB(cpue_kg_km2_sub ~ -1+as.factor(year)+logistic(mi_s)+log_depth_scaled+log_depth_scaled2, 
                   data = dat, 
                   spatial = "on",
                   spatiotemporal="off",
                   mesh=mesh,
                   family =tweedie(link="log"),
                   priors=sdmTMBpriors(threshold = normal(c(effects[i-1,1], effects[i-1,3], effects[i-1,5]), c(effects[i-1,2], effects[i-1,4], effects[i-1,6]))),
                   control = sdmTMBcontrol(
                  start = list(b_threshold = start))))
}
  #For each of those models, pull out the model output and parameters and add to the effects matrix
for (i in 1:length(dats)){
  #Extract model output and add to list
  try(tidy(m2))
  models[i] <- m2
  parfit <- m2$sd_report
  
  #Extract threshold parameter estimates to feed into next prior
  effects[i,1]  <- parfit$value[grep("s50", parnames)]
  effects[i,2]  <- parfit$value[grep("s50", parnames)]
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