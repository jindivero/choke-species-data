### load helper functions ####
setwd("~/Dropbox/GitHub/estimating_mi_from_distribution2")
source("Code/util_funs.R")

devtools::install_github("pbs-assess/sdmTMB", ref="newlogistic2", dependencies = TRUE, force=TRUE)
library(sdmTMB)

### Set ggplot themes ###
theme_set(theme_bw(base_size = 35))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### Load Data ####
sci_name <-"Anoplopoma fimbria" #"Sebastolobus altivelis"#"Anoplopoma fimbria"#"Anoplopoma fimbria"  #"Eopsetta jordani" 
spc <-"sablefish"  #"longspine thornyhead" # "petrale sole" # "sablefish" 
dat.by.size <- length_expand(sci_name)
dat <- load_data(spc = spc, dat.by.size = dat.by.size)

#Constrain depth?
constrain_depth <- T
if(constrain_depth) dat <- subset(dat, depth<600)

#Constrain depth for petrale
constrain_depth <- F
if(constrain_depth) dat <- subset(dat, depth<500)

## Scale, rename, and calculate variables ##
dat$temp_s <- (scale(dat$temp))
dat$po2_s <- (scale(dat$po2))
dat$mi_s <- (scale(dat$mi))
dat$log_depth_scaled <- scale(log(dat$depth))
dat$log_depth_scaled2 <- with(dat, log_depth_scaled ^ 2)
dat$jday_scaled <- scale(dat$julian_day)
dat$jday_scaled2 <- with(dat, jday_scaled ^ 2)
dat$X <- dat$longitude
dat$Y <- dat$latitude
dat$cpue_kg_km2 <- dat$cpue_kg_km2 * (dat$p2+dat$p3)
dat$year <- as.factor(dat$year)

# Remove outliers = catch > 10 sd above the mean
dat$cpue_s <- scale(dat$cpue_kg_km2)
dat <- dplyr::filter(dat, cpue_s <=20)
### Calculate inverse temp ####
kelvin = 273.15 #To convert to Kelvin
boltz = 0.000086173324 #Boltzman's constant
tref <- 7 #Reference temperature in celsius
dat$invtemp <- (1 / boltz)  * ( 1 / (dat$temp + 273.15) - 1 / (tref + 273.15)) #invtemp 

### Make mesh ####
mesh <- make_mesh(dat, xy_cols = c("X", "Y"), n_knots = 250)

##Test
init_vals <- get_inits()
start <- matrix(data=c(-1, 0.25, 100), nrow=3,ncol=1)

m3 <- sdmTMB(cpue_kg_km2 ~ -1+year+logistic(po2_s)+log_depth_scaled+log_depth_scaled2,
             data = dat, 
             spatial = "on",
             mesh=mesh,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               start = list(b_threshold = start),
               #lower = list(b_threshold = lower),
               # upper = list(b_threshold = upper),
               newton_loops = 2,
               nlminb_loops=2))

summary(m3)
m3 <- sdmTMB(cpue_kg_km2 ~ -1+year+logistic(mi_s)+log_depth_scaled+log_depth_scaled2,
             data = dat, 
             spatial = "on",
             mesh=mesh,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               start = list(b_threshold = start),
               #lower = list(b_threshold = lower),
               # upper = list(b_threshold = upper),
               newton_loops = 2,
               nlminb_loops=2))
summary(m3)
