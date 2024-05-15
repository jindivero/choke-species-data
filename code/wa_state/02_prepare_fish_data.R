#Prepare data
library(rgdal)
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
install.packages("TMB", type="source")
library(TMB)

devtools::install("~/Downloads/sdmTMB-newlogistic2")
library(sdmTMB)

##Pull species of interest
#One species
sci_name <- "anoplopoma fimbria" 
spc <- "sablefish" 

#Prepare data
dat <- prepare_data(spc=spc, sci_name=sci_name)

###Data diagnostics
#How many positive catches vs zero catches, and how many hauls with missing length data?
positive_catches <- positive_catches(dat)

### Make mesh ###
mesh <- make_mesh(dat, xy_cols = c("X", "Y"), n_knots=250)
start <- c(1,1,1)
test <- sdmTMB(cpue_kg_km2 ~ 1+year+logistic(po2_s), 
                    data = dat,
                    time = NULL,
                    reml = F,
                    anisotropy = TRUE,
                    spatiotemporal = FALSE,
                    mesh=mesh,
                    family =tweedie(link="log"))
