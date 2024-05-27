devtools::install_github("pbs-assess/sdmTMB", ref="newlogistic2", dependencies = TRUE, force=TRUE)
library(sdmTMB)
install.packages("TMB")
library(TMB)
install.packages("glmmTMB")
library(glmmTMB)

### Set ggplot themes ###
theme_set(theme_bw(base_size = 35))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### Simulating data from sablefish trawl data ####

##Set parameter values for data generation and model fitting ###
s50 <-2 # same as sablefish= 0.88; had been 2 in previous data simulation
delta <- 2 #same as sablefish = 0.57; had been 2 in previous data simulation
smax <- 30 # maximum effect of MI; had been 4 in previous data simulation
Eo <- 0.3
Eo2 <- 0.7

b_years <- rnorm(n = 6, mean = 4, sd = 1)
beta1 <- 1.5
beta2 <- -1
phi <- 10 # 16 corresponds to sablefish 
p <- 1.51
range <- 85
sigma_O <- 1.77

### load helper functions ####
### Load Data ####
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

##Pull real data ##
sci_name <- "anoplopoma fimbria" 
spc <- "sablefish" 
dat <- prepare_data(spc=spc, sci_name=sci_name)

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
## Make mesh ##
mesh <- make_mesh(dat, xy_cols = c("X", "Y"), n_knots=250)

#Edit for new model
simulate_fish<- function(dat,mesh, s50, delta, smax, modelpars) {
  # extract model pars
  parnames <- names(modelpars)
  for (i in 1:length(parnames)) eval(parse(text = paste0(parnames[i],"<-", modelpars[i])))
  
  seed <- sample(1:1000, 1)
  sim <- sdmTMB_simulate(formula=~-1+as.factor(year)+logistic(mi_usual_s)+log_depth_scaled+log_depth_scaled2,
                         data=dat,
                         family=tweedie(link="log"),
                         tweedie_p=p,
                         phi=phi,
                         range=range,
                         sigma_O=sigma_O,
                         sigma_E=NULL,
                         mesh=mesh,
                         threshold_coefs=c(s50, delta, smax),
                         B=c(b_years, beta1, beta2),
                         seed=seed)
  dat$sim <- sim$observed
  return(dat)
}

### Simulate data ####
simdat <- map(seq_len(n), ~simulate_fish(dat = dat,
                                           mesh = mesh,
                                           s50 = s50,
                                           delta = delta,
                                           smax = smax,
                                           modelpars = model.pars))
### Fit model ###
start <- matrix(data=c(-10, 2, 5), nrow=3,ncol=1)
start <- matrix(data=c(s95, s50, smax), nrow=3,ncol=1)
fits <- lapply(simdat, run_sdmTMB_noprior, 
               start=start, mesh=mesh)


pars1 <- lapply(fits, extract_pars)
pars1 <- clean_pars(pars1, fits=fits)

ggplot(subset(pars1, term=="mi_usual_s-smax"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
 geom_hline(yintercept=smax, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("smax")

ggplot(subset(pars1, term=="mi_usual_s-smax"&id!=6), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=smax, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("smax")

ggplot(subset(pars1, term=="mi_usual_s-s50"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=s50, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("s50")

ggplot(subset(pars1, term=="mi_usual_s-s50"&id!=10), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=s50, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("s50")

ggplot(subset(pars1, term=="mi_usual_s-s95"), aes(y=estimate, x=id)) +
  geom_point()+geom_errorbar(aes(ymin = estimate - std.error,max = estimate + std.error))+ 
  geom_hline(yintercept=delta, colour="red")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("s95 (really delta)")







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
