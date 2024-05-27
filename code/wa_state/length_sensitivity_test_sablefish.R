### Install packages ####
install_local <- F
library(devtools)
install.packages("tidyr")
library(tidyr)
library(dplyr)

if (install_local) devtools::install_local("/Users/juliaindivero/Library/CloudStorage/Dropbox/sdmTMB-mi.zip")
if (!install_local) remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE, ref="newlogistic")
library(sdmTMB)

### load helper functions ####
source("code/util_funs.R")

### Load Data ####
sci_name <-"Anoplopoma fimbria"#"Anoplopoma fimbria" # "Sebastolobus altivelis"   #"Eopsetta jordani" 
spc <- "sablefish"  #"longspine thornyhead" # "petrale sole" # "sablefish" 
dat.by.size <- length_expand(sci_name)
dat <- load_data(spc = spc, dat.by.size = dat.by.size)

#Constrain depth?
constrain_depth <- F
if(constrain_depth) dat <- subset(dat, depth<600)

#Constrain depth for petrale
constrain_depth <- F
if(constrain_depth) dat <- subset(dat, depth<500)

## Scale, rename, and calculate variables ##
dat$temp_s <- (scale(dat$temp))
dat$po2_s <- (scale(dat$po2))
dat$log_depth_scaled <- scale(log(dat$depth))
dat$log_depth_scaled2 <- with(dat, log_depth_scaled ^ 2)
dat$jday_scaled <- scale(dat$julian_day)
dat$jday_scaled2 <- with(dat, jday_scaled ^ 2)
dat$X <- dat$longitude
dat$Y <- dat$latitude
dat$year <- as.factor(dat$year)

### Calculate inverse temp ####
kelvin = 273.15 #To convert to Kelvin
boltz = 0.000086173324 #Boltzman's constant
tref <- 7 #Reference temperature in celsius
dat$invtemp <- (1 / boltz)  * ( 1 / (dat$temp + 273.15) - 1 / (tref + 273.15)) #invtemp 

#Create separate column for juvenile-excluded
dat2 <- dat
dat2$cpue_kg_km2 <- dat$cpue_kg_km2 * (dat$p2+dat$p3)

# Remove outliers = catch > 10 sd above the mean
dat$cpue_s <- scale(dat$cpue_kg_km2)
dat <- dplyr::filter(dat, cpue_s <=20)

dat2$cpue_s <- scale(dat2$cpue_kg_km2)
dat2 <- dplyr::filter(dat2, cpue_s <=20)

### Make mesh ####
mesh <- make_mesh(dat, xy_cols = c("X", "Y"), n_knots = 250)
mesh2 <- make_mesh(dat2, xy_cols = c("X", "Y"), n_knots = 250)

## Get initial values ##                                                                                                
init_vals <- get_inits()

### Plot data vs. depth and po2 ####
ggplot(data = dat, aes(x = po2, y = -depth, col = log(cpue_kg_km2+1))) +
  geom_point() +
  scale_colour_viridis_c(limits = c(0, 6), oob = scales::squish) +
  xlim(c(0, 16))+
  theme(legend.position=c(0.6,0.3))

### Fit Breakpoint model to po2 ####
#start <- init_vals$"Sebastolobus altivelis"$m1$start
start <- init_vals$sablefish$m1$start
#lower<- init_vals$"Sebastolobus altivelis"$m1$lower
#upper <- init_vals$"Sebastolobus altivelis"$m1$upper
m1 <- sdmTMB(cpue_kg_km2 ~ 1+year+breakpt(po2_s)+log_depth_scaled+log_depth_scaled2, 
             data = dat,
             time = NULL,
             reml = F,
             anisotropy = TRUE,
             spatiotemporal = FALSE,
             mesh=mesh,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               start = list(b_threshold = start),
               #lower = list(b_threshold = lower), 
               #upper = list(b_threshold = upper),
               newton_loops = 2))

summary(m1)
AIC(m1)

m1_original <- sdmTMB(cpue_kg_km2 ~ 1+year+breakpt(po2_s)+log_depth_scaled+log_depth_scaled2, 
             data = dat2,
             time = NULL,
             reml = F,
             anisotropy = TRUE,
             spatiotemporal = FALSE,
             mesh=mesh2,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               start = list(b_threshold = start),
               #lower = list(b_threshold = lower), 
               #upper = list(b_threshold = upper),
               newton_loops = 2))

summary(m1_original)
AIC(m1_original)

#### Plot fitted relationship ####

##### extract estimates ####
m1_pars <- m1$sd_report$par.fixed
m1_parnames <- names(m1_pars)
b_threshold <- m1_pars[grep("b_threshold", m1_parnames)]
##### plot ####
dat$thresh_pred <- exp(brkptfun(x = dat$po2_s, b_slope = b_threshold[1], b_thresh = b_threshold[2]))
ggplot(dat, aes(x = po2, y = thresh_pred, col = log(cpue_kg_km2 + 1))) +
  geom_point()
  #geom_jitter(width = 0.1, height = 0.01) +
  scale_colour_viridis_c(limits = c(0, 5), oob = scales::squish)+
  xlim(c(0,5))+
  ylab("Marginal Effect")
  

### Fit Eo estimation - po2 prime model ####
#Set starting parameters: 
start <- init_vals$"sablefish"$m2$start
start <- init_vals$"sablefish"$m2$start
lower <- init_vals$"sablefish"$m2$lower
upper <- init_vals$"sablefish"$m2$upper

m2 <- sdmTMB(cpue_kg_km2 ~ -1+year+logistic(mi)+log_depth_scaled+log_depth_scaled2, 
             data = dat, 
             time = NULL,
             reml = F,
             anisotropy = TRUE,
             spatiotemporal = FALSE,
             mesh=mesh,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               start = list(b_threshold = start),
            #lower = list(b_threshold = lower),
             # upper = list(b_threshold = upper),
               newton_loops = 2,
               nlminb_loops=2))

summary(m2)
AIC(m2)

m2_original <- sdmTMB(cpue_kg_km2 ~ -1+year+logistic(mi)+log_depth_scaled+log_depth_scaled2, 
             data = dat2, 
             time = NULL,
             reml = F,
             anisotropy = TRUE,
             spatiotemporal = FALSE,
             mesh=mesh2,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               start = list(b_threshold = start),
               #lower = list(b_threshold = lower),
               # upper = list(b_threshold = upper),
               newton_loops = 2,
               nlminb_loops=2))

summary(m2_original)
AIC(m2_original)


#### Plot fitted relationshop ####
##### extract estimates ####

Eo <- getEo(model = m2)

##### plot ####
dat$po2_prime <- dat$po2 * exp(Eo * dat$invtemp)
dat$Eo_unconstrained <- exp(logfun(dat$po2_prime, model = m2, mi = T))
ggplot(dat, aes(x = po2_prime, y = Eo_unconstrained, col = log(cpue_kg_km2 + 1))) +
 # geom_point()+
  geom_rect(aes(xmin=-Inf, xmax=0.88, ymin=-Inf, ymax=Inf), fill="grey88", colour=NA, alpha=0.1)+
  geom_jitter(width = 0.1, height = 0.01) +
  scale_colour_viridis_c(limits = c(0, 6), oob = scales::squish) + 
  xlim(c(0,3))+
  xlab("g(pO2,T)")+
  ylab("f(pO2,T")
  


### Fit Eo estimation - po2 prime model (with prior) ####
## Set starting parameters:

start <- init_vals$"Sebastolobus altivelis"$m2a$start
start <- init_vals$sablefish$m2a$start

prior <- normal(c(NA, NA, NA, 0.331), c(NA, NA, NA, 0.176))

m2a <- sdmTMB(cpue_kg_km2 ~ -1+year+logistic(mi)+log_depth_scaled+log_depth_scaled2,
             data = dat, 
             time = NULL,
             reml = F,
             anisotropy = TRUE,
             spatiotemporal = FALSE,
             mesh=mesh,
             family =tweedie(link="log"),
             priors=sdmTMBpriors(threshold = prior),
             control = sdmTMBcontrol(
               start = list(b_threshold = start),
            #   upper = list(b_threshold = upper),
             #  lower = list(b_threshold = lower),
               newton_loops = 2))

summary(m2a)
AIC(m2a)

#### Plot fitted relationship ####
##### extract estimates ####
Eo <- getEo(model = m2a)

##### plot ####
## Calculate po2 prime from parameter ##
dat$po2_prime <- dat$po2 * exp(Eo * dat$invtemp)

##### plot ####

dat$Eo_constrained <- exp(logfun(dat$po2_prime, model = m2a, mi = T))
ggplot(dat, aes(x = po2_prime, y = Eo_constrained, col = log(cpue_kg_km2 + 1))) +
  geom_jitter(width = 0.1, height = 0.01) +
  scale_colour_viridis_c(limits = c(0, 6), oob = scales::squish) +
  xlim(c(0,5))+
  ylab("Marginal Effect")
  


### Fit logistic po2 model ####
#Starting values (work for both all depths and depth-constraine)
start <- init_vals$sablefish$m3$start
upper <-  matrix(init_vals$sablefish$m3$upper)
lower <- matrix(init_vals$sablefish$m3$lower)

m3 <- sdmTMB(cpue_kg_km2 ~ -1+year+logistic(po2_s)+log_depth_scaled+log_depth_scaled2,
             data = dat, 
             spatial = "on",
             mesh=mesh,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               start = list(b_threshold=start),
             #  lower = list(b_threshold = lower),
             #  upper = list(b_threshold = upper)
               )
        )

m3_original <- sdmTMB(cpue_kg_km2 ~ -1+year+logistic(po2_s)+log_depth_scaled+log_depth_scaled2,
             data = dat2, 
             spatial = "on",
             mesh=mesh2,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               start = list(b_threshold=start),
               #  lower = list(b_threshold = lower),
               #  upper = list(b_threshold = upper)
             )
)
summary(m3)


#### Plot fitted relationship ####
##### extract estimates ####
dat$logistic_pos <- exp(logfun(dat$po2_s, model = m3, mi = F))
##### plot ####
ggplot(dat, aes(x = po2, y = logistic_pos, col = log(cpue_kg_km2 + 1))) +
  geom_jitter(width = 0.1, height = 0.01) +
  scale_colour_viridis_c(limits = c(0, 6), oob = scales::squish)+
  xlim(c(0,5))+
  ylab("Marginal Effect")

### Fit null model ####
m4 <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2, 
             data = dat, 
             spatial = "on",
             mesh=mesh,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               newton_loops = 2,
               )
             )
summary(m4)
AIC(m4)

m4_original <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2, 
             data = dat2, 
             spatial = "on",
             mesh=mesh2,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               newton_loops = 2,
             )
)

### Fit other alternative models ###
## Temperature ##

m5 <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s,
             data = dat, 
             spatial = "on",
             mesh=mesh,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               newton_loops = 1,
             )
)

m5_original <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s,
             data = dat2, 
             spatial = "on",
             mesh=mesh2,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(
               newton_loops = 1,
             )
)

summary(m5)
AIC(m5)

##Oxygen only##
m6 <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2+po2_s,
             data = dat, 
             spatial = "on",
             mesh=mesh,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(newton_loops = 1,
             )
)

m6_original <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2+po2_s,
             data = dat2, 
             spatial = "on",
             mesh=mesh2,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(newton_loops = 1,
             )
)

summary(m6)
AIC(m6)

## Temp and o2 ##
m7 <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s + po2_s,
             data = dat, 
             spatial = "on",
             mesh=mesh,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(newton_loops = 1,
             )
)

m7_original <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s + po2_s,
             data = dat2, 
             spatial = "on",
             mesh=mesh2,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(newton_loops = 1,
             )
)
summary(m7)
AIC(m7)

##Temp and o2 interaction ##
m8 <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s * po2_s,
             data = dat, 
             spatial = "on",
             mesh=mesh,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(newton_loops = 1,
             )
)

m8_original <- sdmTMB(cpue_kg_km2 ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s * po2_s,
             data = dat2, 
             spatial = "on",
             mesh=mesh2,
             anisotropy=T,
             reml=F,
             time=NULL,
             family =tweedie(link="log"),
             control = sdmTMBcontrol(newton_loops = 1,
             )
)

summary(m8)
AIC(m8)


### Create dAIC table ###
## Make list of model names##
models <- c("breakpt-pO2", "Eo estimation and logistic po2'","logistic-pO2", "Null", "temp", "po2", "temp+po2", "temp * po2")
## Create table and add AIC for each ##
AIC <- as.data.frame(matrix(NA, ncol = 2, nrow =length(models), dimnames = list(models)))
AIC[1,1] <- AIC(m1)
AIC[2,1] <- AIC(m2)
AIC[3,1] <- AIC(m3)
AIC[4,1] <- AIC(m4)
AIC[5,1] <- AIC(m5)
AIC[6,1] <- AIC(m6)
AIC[7,1] <- AIC(m7)
AIC[8,1] <- AIC(m8)

AIC[1,2] <- AIC(m1_original)
AIC[2,2] <- AIC(m2_original)
AIC[3,2] <- AIC(m3_original)
AIC[4,2] <- AIC(m4_original)
AIC[5,2] <- AIC(m5_original)
AIC[6,2] <- AIC(m6_original)
AIC[7,2] <- AIC(m7_original)
AIC[8,2] <- AIC(m8_original)

## Calculate delta-AIC ##
min1 <- min(AIC$V1)
min2 <- min(AIC$V2)
AIC$dAIC1 <- abs(min1-(AIC$V1))
AIC$dAIC2 <- abs(min2-(AIC$V2))



#### Plot marginal effects w/ +- SE using Tim's sdm_o2 code ####

### Method 1: Plot effect size ###
##Breakpoint##
m1_pars <- m1$sd_report$par.fixed
m1_sd <- m1$sd_report$sd
m1_parnames <- names(m1_pars)
b_threshold1 <- m1_pars[grep("b_threshold", m1_parnames)]
b_thresh_sd1<- m1_sd[3:4]
z <- 1.96

dat$thresh_pred <- exp(brkptfun(x = dat$po2_s, b_slope = b_threshold1[1], b_thresh = b_threshold1[2]))
dat$thresh_pred_high <- exp(brkptfun(x = dat$po2_s, b_slope = (b_threshold1[1]+z*b_thresh_sd1[1]), b_thresh = (b_threshold1[2]+z*b_thresh_sd1[2])))
dat$thresh_pred_low <- exp(brkptfun(x = dat$po2_s, b_slope = (b_threshold1[1]-z*b_thresh_sd1[1]), b_thresh = (b_threshold1[2]-z*b_thresh_sd1[2])))

# Plot
ggplot(dat, aes(x = po2, y = thresh_pred)) +
  geom_ribbon(aes(x=po2,y=thresh_pred, ymin = thresh_pred_low, ymax = thresh_pred_high), alpha=0.4)+
  geom_point(aes(colour = log(cpue_kg_km2 + 1)))+
  scale_colour_viridis_c(limits = c(0, 5), oob = scales::squish)+
  geom_line(aes(x=po2, y=thresh_pred))+
  xlim(c(0,5))+
  ylab("Marginal Effect")

##pO2' ##
Eo <- getEo(model = m2)
m2_pars <- m2$sd_report$par.fixed
m2_sd <- m2$sd_report$sd
m2_parnames <- names(m2_pars)
b_threshold2 <- m2_pars[grep("b_threshold", m2_parnames)]
b_thresh_sd2 <- m2_sd[3:6]
z <- 1.96
dat$po2_prime <- dat$po2 * exp(Eo * dat$invtemp)
dat$po2_prime_min <- dat$po2 * exp((Eo-b_thresh_sd2[4]) * dat$invtemp)
dat$po2_prime_max <- dat$po2 * exp((Eo+b_thresh_sd2[4]) * dat$invtemp)
dat$thresh_pred2 <- exp(logfun(dat$po2_prime, model = m2, mi = T))

#Get lower and upper bound#
dat$thresh_pred_high2 <- logfun_basic(dat$po2_prime_max, s50=(b_threshold2[1]+z*b_thresh_sd2[1]), s95=(b_threshold2[2]+z*b_thresh_sd2[2]), smax=(b_threshold2[2]+z*b_thresh_sd2[2]))
dat$thresh_pred_low2 <- logfun_basic(dat$po2_prime_min, s50=(b_threshold2[1]-z*b_thresh_sd2[1]), s95=(b_threshold2[2]-z*b_thresh_sd2[2]), smax=(b_threshold2[2]-z*b_thresh_sd2[2]))

ggplot(dat, aes(x = po2_prime, y = thresh_pred2)) +
  geom_ribbon(aes(x=po2,y=thresh_pred, ymax = thresh_pred_low2, ymin = thresh_pred_high2), alpha=0.4)+
  geom_point(aes(colour = log(cpue_kg_km2 + 1)))+
  scale_colour_viridis_c(limits = c(0, 5), oob = scales::squish)+
  #geom_line(aes(x=po2, y=thresh_pred2))+
  xlim(c(0,5))+
  ylab("Marginal Effect")

ggplot(dat, aes(x = po2_prime, y = thresh_pred2)) +
  geom_ribbon(aes(y=thresh_pred, xmin= po2_prime_min, ymin = po2_prime_max), alpha=0.4)+
  geom_point(aes(colour = log(cpue_kg_km2 + 1)))+
  scale_colour_viridis_c(limits = c(0, 5), oob = scales::squish)+
  #geom_line(aes(x=po2, y=thresh_pred2))+
  xlim(c(0,5))+
  ylab("Marginal Effect")








### Method 2: Plot marginal effect (in cpue kg km2) ###

## Breakpoint ##
# Make model output tidy #
tidy(m1,"ran_pars",conf.int = TRUE)
tidy(m1,"fixed", conf.int = TRUE)

# Create new data with everything set to 0 except for pO2 #
nd_po2 <- data.frame(po2_s = seq(min(dat$po2_s), max(dat$po2_s), length.out = 300), 
                     temp = 0,
                     log_depth_scaled = 0,
                     log_depth_scaled2 = 0,
                     year = as.factor(2010L)
)
nd_po2 <- convert_class(nd_po2)

# predict to new data #
p_o2 <- predict(m1, newdata = nd_po2, se_fit = TRUE, re_form = NA)

# plot predictions with uncertainty
z <- 1.96 # for 90% CI
p_o2$po2 <-back.convert(p_o2$po2_s, attr(dat$po2_s, "scaled:center"), attr(dat$po2_s, "scaled:scale"))
ggplot(p_o2, aes(x=po2, y=exp(est), 
                 ymin = exp(est - z *est_se), ymax = exp(est + z * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4) + 
  scale_x_continuous(limits = c(0, 3), expand = expansion(mult = c(0, 0.0))) +
  labs(x = "Partial Pressure of Oxygen (kPa)", y = bquote('Population Density'~(kg~km^-2)))


## Constrained Eo ##
tidy(m2,"ran_pars",conf.int = TRUE)
tidy(m2,"fixed", conf.int = TRUE)

# predict on full dataframe # 
nd_po2 <- data.frame(po2 = seq(min(dat$po2), max(dat$po2), length.out = 300), 
                     invtemp = mean(dat$invtemp),
                     log_depth_scaled = 0,
                     log_depth_scaled2 = 0,
                     year = as.factor(2010L)
)
nd_po2 <- convert_class(nd_po2)
p_o2 <- predict(m2, newdata = nd_po2, se_fit = TRUE, re_form = NA)

#Calculate po2 prime #
Eo <- getEo(model = m2)
p_o2$prime <- p_o2$po2 * exp(Eo * p_o2$invtemp)

# Plot--ends up super squiggly #
ggplot(p_o2, aes(x=prime, y=exp(est),
                 ymin = exp(est - z *est_se), ymax = exp(est + z * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4) + 
  scale_y_continuous(limits = c(0, 75), expand = expansion(mult = c(0, 0.0))) +
  scale_x_continuous(limits = c(0, 1.5), expand = expansion(mult = c(0, 0.0))) +
  labs(x = "Partial Pressure of Oxygen (kPa)", y = bquote('Population Density'~(kg~km^-2)))

#Try to manually thin it to create a smoother line--this still makes a squiggly plot #
p_o2 <- arrange(p_o2, p_o2$prime)
ggplot(p_o2[seq(1, nrow(p_o2), 10), ], aes(x=prime, y=exp(est),
                                          ymin = exp(est - z *est_se), ymax = exp(est + z * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4) + 
  scale_y_continuous(limits = c(0, 75), expand = expansion(mult = c(0, 0.0))) +
  scale_x_continuous(limits = c(0, 1.5), expand = expansion(mult = c(0, 0.0))) +
  labs(x = "Partial Pressure of Oxygen (kPa)", y = bquote('Population Density'~(kg~km^-2)))

##Option 2: creating new dataframe with smoothed po2 and invtemp #
# Back-calculate pO2 prime, po2, and invtemp values #
Eo <- getEo(model = m2)
dat$po2_prime <- dat$po2 * exp(Eo * dat$invtemp)
prime_preds <- data.frame(prime =seq(min(dat$po2_prime), max(dat$po2_prime), length.out=1000))
prime_preds$po2 <- seq(min(dat$po2), max(dat$po2), length.out=1000)
prime_preds$invtemp <- log(prime_preds$prime/prime_preds$po2)/Eo
prime_preds$test <- prime_preds$po2 * exp(Eo * prime_preds$invtemp)

nd_po2 <- data.frame(po2 = prime_preds$po2, 
                     invtemp = prime_preds$invtemp,
                     log_depth_scaled = 0,
                     log_depth_scaled2 = 0,
                     year = as.factor(2010L)
)
nd_po2 <- convert_class(nd_po2)

# predict to new data--this Rbombs#
p_o2 <- predict(m2, newdata = nd_po2, se_fit = TRUE, re_form = NA)

ggplot(p_o2, aes(x=po2, y=exp(est),ymin = exp(est - z *est_se), ymax = exp(est + z * est_se))) +
 geom_line()+geom_ribbon(alpha = 0.4) + 
  scale_y_continuous(limits = c(0, 75), expand = expansion(mult = c(0, 0.0))) +
  scale_x_continuous(limits = c(0, 1.5), expand = expansion(mult = c(0, 0.0))) +
  labs(x = "Partial Pressure of Oxygen (kPa)", y = bquote('Population Density'~(kg~km^-2)))

## Plot logistic ##
# Make model output tidy #
tidy(m3,"ran_pars",conf.int = TRUE)
tidy(m3,"fixed", conf.int = TRUE)

# Create new data with everything set to 0 except for pO2 #
nd_po2 <- data.frame(po2_s = seq(min(dat$po2_s), max(dat$po2_s), length.out = 300), 
                     temp = 0,
                     log_depth_scaled = 0,
                     log_depth_scaled2 = 0,
                     year = as.factor(2010L)
)
nd_po2 <- convert_class(nd_po2)

# predict to new data #
p_o2 <- predict(m3, newdata = nd_po2, se_fit = TRUE, re_form = NA)

# plot predictions with uncertainty
z <- 1.645 # for 90% CI
p_o2$po2 <-back.convert(p_o2$po2_s, attr(dat$po2_s, "scaled:center"), attr(dat$po2_s, "scaled:scale"))
plot_logistic <- ggplot(p_o2, aes(x=po2, y=exp(est), 
                                    ymin = exp(est - z *est_se), ymax = exp(est + z * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4) + 
  scale_y_continuous(limits = c(0, 250), expand = expansion(mult = c(0, 0.0))) +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.0))) +
  labs(x = "Partial Pressure of Oxygen (kPa)", y = bquote('Population Density'~(kg~km^-2)))

plot_logistic

multipanel <- gridExtra::grid.arrange(breakpoint, logistic, Eo_constrained, nrow = 3, ncol = 1)
ggsave("plots/marginal_effects.png", multipanel, width = 6, height = 3, units = "in", device = "png")

##pO2 and Temp data and po2/temp curve
#Calculate for simulated data, combos of po2 and temp that are MI=s50 (i.e. 2)
dat$pO2_s50 <-  0.88/exp(Eo*dat$invtemp)

ggplot(dat, aes(x=temp, y=log(po2)))+
  #stat_density_2d_filled(alpha=0.8)+
  #scale_fill_distiller(palette= "Spectral", direction=-1)+
  theme(legend.position="none")+
  geom_ribbon(ymin=-Inf, aes(ymax=pO2_s50), fill='grey88')+
  #geom_ribbon(aes(ymin=pO2_s50), ymax=Inf, fill='grey88', alpha=0.4)+
  #geom_hex()+
  # scale_fill_grey()+
  geom_point(aes(colour=log(cpue_kg_km2+1)))+
  scale_colour_viridis_c(limits = c(0, 6), oob = scales::squish) +
  geom_line(dat, mapping=aes(x=temp, y=pO2_s50), size=2, colour="black")+
  xlab("Observed Temperature (C)")+
  ylab("Observed log Dissolved Oxygen (kPa)")




##### Older versions, ignore ####
### Plot depth effects  ####
## Set ggplot theme ##
theme_set(theme_bw(base_size = 30))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
## Internal Eo mode ##
# Extract parameters another way for internal Eo model #
par_estimates <- as.data.frame(tidy(m2, conf.int = TRUE, effects="fixed"))
par_estimates_rand <- as.data.frame(tidy(m2, conf.int = TRUE, effects="ran_pars"))
par_estimates <- bind_rows(par_estimates, par_estimates_rand)
pars <- par_estimates[1:2]
pars <- pivot_wider(pars, names_from=term, values_from=estimate)
# Calculate temp-corrected po2 from Eo value #
dat$mi_pred <- dat$po2*exp(pars$"mi-Eo"* dat$invtemp)
# Calculate effect of MI # Tim note: probably better to use the logfun function above to calculate the log effect size from po2prime or po2.  
dat$mi_effect <- pars$"mi-smax" * (1 / (1 + exp(-log(19) * (dat$mi_pred - pars$`mi-s50`) / pars$"mi-delta")) - 1) # this is exponentiated. 
# Calculate combined depth effect #
dat$depth_effect_combined <- (dat$log_depth_scaled*pars$log_depth_scaled)+(dat$log_depth_scaled2*pars$log_depth_scaled2)

## For breakpt(o2) model ##
par_estimates2 <- as.data.frame(tidy(m1, conf.int = TRUE, effects="fixed"))
par_estimates_rand2 <- as.data.frame(tidy(m1, conf.int = TRUE, effects="ran_pars"))
par_estimates2 <- bind_rows(par_estimates2, par_estimates_rand2)
pars2 <- par_estimates2[1:2]
pars2 <- pivot_wider(pars2, names_from=term, values_from=estimate)
# alculate depth effects #
dat$depth_effect_combined2 <- (dat$log_depth_scaled*pars2$log_depth_scaled)+(dat$log_depth_scaled2*pars2$log_depth_scaled2)
# Calculate po2 effect #
dat$po2_effect <- ifelse(dat$po2_s>pars2$`po2_s-breakpt`,pars2$`po2_s-breakpt`*pars2$`po2_s-slope`,dat$`po2_s`*pars2$`po2_s-slope`)

# Plot depth effects compared between model #
ggplot(dat, aes(y=depth_effect_combined, x=depth))+geom_line(size=1.3)+geom_line(dat, mapping=aes(y=depth_effect_combined2, x=depth), color="red", size=1.3)+ylab("Depth Effect Combined")
ggplot(subset(dat, depth>450), aes(y=depth_effect_combined, x=depth))+geom_point()+geom_point(subset(dat, depth>450), mapping=aes(y=depth_effect_combined2, x=depth), color="red")+ylab("Depth Effect Combined")

# Plot metabolic index vs depth #
ggplot(dat, aes(y=depth, x=mi_pred))+geom_point(aes(color=log(cpue_kg_km2)))+xlab("pO2'")+theme(legend.position=c(0.8, 0.8))

#Plot oxygen vs depth
ggplot(dat, aes(y=-depth, x=po2))+geom_point(aes(color=log(cpue_kg_km2)))+xlab("pO2")+theme(legend.position=c(0.7, 0.3))+scale_colour_viridis_c()

#Save models
save(m1,m2,m2a,m3,m4,m5,m6,m7,m8, file="models_petralesole.RDS")

#Calculate percent below
below <- subset(dat, dat$po2_prime < 1.45 & dat$cpue_kg_km2>0)
below2 <- subset(dat,  dat$cpue_kg_km2>0)
below <- subset(dat, po2_s < -1.09 & dat$cpue_kg_km2>0)

below_s50 <- subset(dat, dat$po2_prime <0.88)
below_s95 <- subset(dat, dat$po2_prime <1.45)
