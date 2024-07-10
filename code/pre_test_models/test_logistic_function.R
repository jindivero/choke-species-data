devtools::install_github("pbs-assess/sdmTMB", ref="newlogistic2", dependencies = TRUE, force=TRUE)
library(sdmTMB)
install.packages("TMB")
library(TMB)
install.packages("glmmTMB", type="source")
library(glmmTMB)
library(ggplot2)
library(purrr)
library(mvtnorm)

source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

### Set ggplot themes ###
theme_set(theme_bw(base_size = 35))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Simulating data from sablefish trawl data ####

##Set parameter values for data generation and model fitting ###
s50 <-2 # same as sablefish= 0.88; had been 2 in previous data simulation
delta <- 2 #same as sablefish = 0.57; had been 2 in previous data simulation
smax <- 30 # maximum effect of MI; had been 4 in previous data simulation

b_years <- rnorm(n = 6, mean = 4, sd = 1)
beta1 <- 1.5
beta2 <- -1
phi <- 10 # 16 corresponds to sablefish 
p <- 1.51
range <- 85
sigma_O <- 1.77

#Lower spatial variation and observation error: make sigma_O, phi less
phi <- 0.1
range <- 1
sigma_O <- 0.01

### load helper functions ####
### Load Data ####

##Pull real data (example sablefish data from Chapter 2) ## 
dat <- readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/example_data.rds")
#Clean up and add environmental data
kelvin = 273.15
boltz = 0.000086173324
tref <- 12
#Calculate inverse temp
dat$invtemp <- (1 / boltz)  * ( 1 / (dat$temp + 273.15) - 1 / (tref + 273.15))
#Calculate MI for usual case
dat$mi = dat$po2*exp(0.291* dat$invtemp)
#Scale and such
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

#Plot real effect
dat$logmu_real <- logfun_basic(mi=dat$mi_s, smax, s50, s95=delta)
ggplot(dat, aes(x=mi_s, y=logmu_real))+geom_point()

### Fish density distribution simulation ####

## Set how many data sets to produce ##
n <- 25

## Make list of parameter values ##
model.pars <- list(b_years = b_years,
                   beta1 = beta1,
                   beta2 = beta2,
                   phi = phi,
                   p = p,
                   range = range,
                   sigma_O=sigma_O)
model.pars2 <- list(b_years = b_years,
                   beta1 = beta1,
                   beta2 = beta2,
                   phi = phi,
                   p = p)

## Make mesh ##
mesh <- make_mesh(dat, xy_cols = c("X", "Y"), n_knots=250)

### Simulate data ####
simdat <- map(seq_len(n), ~simulate_fish(dat = dat,
                                           mesh = mesh,
                                           s50 = s50,
                                           delta = delta,
                                           smax = smax,
                                           modelpars = model.pars))
### Fit model ###
start <- matrix(data=c(-10, 2, 5), nrow=3,ncol=1)
start <- matrix(data=c(delta, s50, smax), nrow=3,ncol=1)
fits <- lapply(simdat, run_sdmTMB_noprior, 
               start=start, mesh=mesh)


pars1 <- lapply(fits, extract_pars)
pars1 <- clean_pars(pars1, fits=fits)

ggplot(subset(pars1, term=="mi_s-smax"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
 geom_hline(yintercept=smax, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("smax")

ggplot(subset(pars1, term=="mi_s-s50"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=s50, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("s50")

ggplot(subset(pars1, term=="mi_s-s95"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=delta, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("s95 (really delta)")

#Plot effect
pars_wide <- pivot_wider(pars1, id_cols=c(id), names_from=term, values_from=estimate)
logmus <- matrix(data=NA, nrow=nrow(dat), ncol=nrow(pars_wide))
for (i in 1:nrow(pars_wide)){
  logmus[,i] <- logfun_basic(mi=dat$mi_s, smax=pars_wide$`mi_s-smax`[i], s50=pars_wide$`mi_s-s50`[i], s95=pars_wide$`mi_s-s95`[i])
}

#Add mi
logmus <- as.data.frame(logmus)
logmus$mi_s <- dat$mi_s

#Flip long
logmus2 <- pivot_longer(logmus, 1:20)
ggplot(logmus2, aes(x=mi_s, y=value))+geom_point(aes(group=name, colour=name))

#Just one
test <- logfun_basic(mi=dat$mi_s, smax=pars_wide$`mi_s-smax`[5], s50=pars_wide$`mi_s-s50`[5], s95=pars_wide$`mi_s-s95`[5])
test <- as.data.frame(test)
colnames(test) <- "effect"
test$mi_s <- dat$mi_s
ggplot(test, aes(x=mi_s, y=effect))+geom_point()

#### Random environmental data simulation
simulate_environmental <- function(ndata, dat, po2_lim, temp_lim, depth_lim){
  ##Set seed
  seed <- sample(1:1000, 1)
  set.seed(seed)
  ##Simulate oxygen and temperature data
  #Get variance covariances of depth, temperature and po2
  env.dat <- dplyr::select(dat, "po2", "depth", "temp")
  var.covar <- var(log(env.dat))
  env.bar <- as.matrix(colMeans(log(env.dat)))
  #Simulate from MVN
  log.env <- matrix(nrow=ndata, ncol=3, NA)
  colnames(log.env) <- c("po2", "depth", "temp")
  #Random generation of environmental data, with constraints
  for (i in 1:ndata){
    log.env[i,] <- rmvnorm(n = 1, mean=(env.bar - diag(var.covar) / 2), sigma=(var.covar), method="eigen")
    while(log.env[i,1]>log(po2_lim) | log.env[i,2]>log(depth_lim)| log.env[i,3]>log(temp_lim)){
      log.env[i,] <- rmvnorm(n = 1, mean=(env.bar - diag(var.covar) / 2), sigma=(var.covar), method="eigen")
    }
  }
  #Exponentiate
  env <- as.data.frame(exp(log.env))
  ##Metabolic Index
  #Constants
  #Ao <- 1.81
  #n <- -0.12
  #B = 1000 # size in grams, roughly average
  #avgbn <-B^n  # this works for the adult class (0.5 - 6 kg).  for the large adult, adjust
  kelvin = 273.15
  boltz = 0.000086173324
  tref <- 12
  #Calculate inverse temp
  env$invtemp <- (1 / boltz)  * ( 1 / (env$temp + 273.15) - 1 / (tref + 273.15))
  #Calculate MI
  env$mi = env$po2*exp(0.3* env$invtemp)
  
  ##Add variables to dataset
  #La/lon
  env$X <- dat$latitude
  env$Y <- dat$longitude
  #Year
  env$year <- dat$year
  ##Log and scale depth
  env$log_depth <- log(env$depth)
  env$log_depth_scaled <- scale(env$log_depth)
  env$log_depth_scaled2 <- env$log_depth_sc ^ 2
  ##Scale mi and oxygen, just in case
  env$mi_s <- scale(env$mi)
  env$po2_s <- scale(env$po2)
  #Seed
  env$seed <- seed
  return(env)
}

##Run environmental data simulation
n <- 25 #Number of simulations
ndata <- nrow(dat) #Number of data points
#Environmental data limits
po2_lim <- 35
temp_lim <-15
depth_lim <- 1500
#Simulate data
env_sims <- map(seq_len(n), ~simulate_environmental(ndata, dat, po2_lim, temp_lim,depth_lim)) 

#Simulate fish data
simdat_ran <- lapply(env_sims, simulate_fish,
                     mesh = mesh,
                     s50 = s50,
                     delta = delta,
                     smax = smax,
                     modelpars = model.pars)
simdat_ran <- lapply(env_sims, simulate_fish2,
                     mesh = mesh,
                     s50 = s50,
                     delta = delta,
                     smax = smax,
                     modelpars = model.pars2)

fits <- lapply(simdat_ran, run_sdmTMB_noprior, 
               start=start, mesh=mesh)


pars1 <- lapply(fits, extract_pars)
pars1 <- clean_pars(pars1, fits=fits)

ggplot(subset(pars1, term=="mi_s-smax"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=smax, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("smax")

ggplot(subset(pars1, term=="mi_s-s50"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=s50, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("s50")

ggplot(subset(pars1, term=="mi_s-s95"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=delta, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("s95 (really delta)")

#Calculate effect and plot
smax_test$logmu1 <- logfun_basic(smax_test$po2_prime, smax=5, s50=s50, delta)

#Test with updated data
sci_name <- "anoplopoma fimbria" 
spc <- "sablefish" 

dat <- prepare_data(spc=spc, sci_name=sci_name, ROMS=F)
#Remove outliers catch > 10 sd above the mean
dat$cpue_s <- scale(dat$cpue_kg_km2_sub)
dat <- dplyr::filter(dat, cpue_s <=20)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test with real data
#Constrain depth?
constrain_depth <- T
if(constrain_depth) dat <- subset(dat, depth<600)

#Constrain depth for petrale
constrain_depth <- F
if(constrain_depth) dat <- subset(dat, depth<500)


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



