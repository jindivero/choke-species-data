library(sf)
library(sdmTMB)
library(tidyverse)
library(cowplot)
library(viridis)
library(Metrics)

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"
source("test_data/code/convert_funs.R")

density_data <- readRDS("data/oxygen options/PotentialDensityData.RDS")
head(density_data)
summary(density_data)

density_O2 <- density_data

density_O2<- density_O2 %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

#Coordinate transformation they used
#density_O2_sf <- sf::st_as_sf(density_O2, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 
#density_O2_sf <- sf::st_transform(density_O2_sf, crs =  "EPSG: 32610") 
#density_coords <- as.data.frame(st_coordinates(density_O2_sf)/1000)
#density_O2 <- cbind(density_O2, density_coords)

#Convert O2
density_O2$O2_umolkg <- convert_o2(density_O2$do_mlpL, density_O2$sigma0_kgm3)
density_O2$O2_umolkg_ln <- log(density_O2$O2_umolkg)
density_O2$O2_ln <- log(density_O2$do_mlpL)
density_O2$p_dbar_ln <- log(density_O2$p_dbar)
density_O2$sigma_exp <- exp(density_O2$sigma0_kgm3)

density_no_O2 <- density_O2 %>% 
  filter(is.na(do_mlpL)| do_mlpL<=0)

density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)

density_O2 %>% 
  ggplot(aes(x = X, y = Y, color = Survey))+
  geom_point()+
  facet_wrap(~year)

#If want to train
train <- F
if(train){
set.seed(7)
density_O2$test <- rbinom(n = nrow(density_O2), size = 1, prob = 0.3)
train <- density_O2[density_O2$test == 0,]
test <- density_O2[density_O2$test == 1,]

train <- as.data.frame(train)
}

if(!train){
train <- density_O2
}

train <- as.data.frame(train)
spde <- make_mesh(data = train, xy_cols = c("X","Y"), cutoff = 30)

plot(spde)

# m_1 <- sdmTMB(formula = O2_ln  ~ 0 + s(sigma_exp) + s(temperature_C) + s(p_dbar_ln),
#               mesh = spde,
#               data = train, 
#               family = gaussian(), 
#               #time = "year",
#               spatial = "on",
#               spatiotemporal  = "off")
# 
# if(max(m_1$gradients)>0.01){
#   m_1 <- sdmTMB::run_extra_optimization(m_1, nlminb_loops = 1L, newton_loops = 1L)
# }


m_2 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma_exp) + s(temperature_C) + s(p_dbar_ln) + s(month, bs = "cc", k = 4),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")

m_3 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma_exp) + s(temperature_C) + s(p_dbar_ln) + s(month, bs = "cc", k = 4)+as.factor(year),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")

#if(max(m_2$gradients)>0.01){
 # m_2 <- sdmTMB::run_extra_optimization(m_2, nlminb_loops = 1L, newton_loops = 1L)
#}

# m_3 <- sdmTMB(formula = O2_ln  ~ 0 + s(sigma_exp) + s(temperature_C) + s(p_dbar_ln) + s(month, bs = "cc", k = 4),
#               mesh = spde,
#               data = train, 
#               family = gaussian(), 
#               extra_time = unique(density_no_O2$year)[is.na(match(unique(density_no_O2$year), unique(train$year)))],
#               time = "year",
#               spatial = "on",
#               spatiotemporal = "IID")
# 
# if(max(m_3$gradients)>0.01){
#   m_3 <- sdmTMB::run_extra_optimization(m_3, nlminb_loops = 1L, newton_loops = 1L)
# }
# 
# m_4 <- sdmTMB(formula = O2_ln  ~ 0 + s(sigma_exp) + s(temperature_C) + s(p_dbar_ln) + s(month, bs = "cc", k = 4),
#               mesh = spde,
#               data = train, 
#               family = gaussian(), 
#               extra_time = unique(density_no_O2$year)[is.na(match(unique(density_no_O2$year), unique(train$year)))],
#               time = "year",
#               spatial = "on",
#               spatiotemporal = "AR1")
# 
# if(max(m_4$gradients)>0.01){
#   m_4 <- sdmTMB::run_extra_optimization(m_4, nlminb_loops = 1L, newton_loops = 1L)
# }
AIC(m_2, m_3)

O2_model <- m_2 #m_4

###Newport data to predict to
# load Newport data
setwd(basewd)
newport_data <- readRDS("test_data/data/newport_bottom.RDS")

# need to add latitude, depth, and convert DO to mmol
newport_lat <- 44.65
newport_data$latitude <- newport_lat
newport_data$year <- year(newport_data$sample_date)
newport_data$doy <- as.POSIXlt(newport_data$sample_date, format = "%Y-%b-%d")$yday
newport_data$depth <- p2d(lat = newport_lat,
                          p = newport_data$pressure..dbar.)


#  get lat and long in UTC coordinates
newport_data <- newport_data %>%
  st_as_sf(coords=c('longitude..degW.','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 
source("test_data/code/convert_funs.R")

newport_data$sigma <- calc_sigma( s = newport_data$practical.salinity,
                                  t = newport_data$temperature..degC.,
                                  p = newport_data$pressure..dbar.)
newport_data$o2 <- convert_o2(newport_data$dissolved.oxygen..ml.L., newport_data$sigma)

newport_data <- as.data.frame(newport_data)

##Subset to years
#Add month column
newport_data$month <- month(newport_data$sample_date)
#Make column names the same
newport_data$sigma_exp <- exp(newport_data$sigma)
newport_data$p_dbar_ln <- log(newport_data$pressure..dbar.)
newport_data$temperature_C <- newport_data$temperature..degC.

##Subset to years
newport2 <- subset(newport_data, year>2016 & year<2020)

tmp_predict2 <- predict(m_2, newdata = newport2)
tmp_predict <- predict(m_2, newdata = newport_data)


#plot

ggplot(tmp_predict, aes(x=o2, y=exp(est)))+geom_point(aes(colour=depth), size=3)+xlab("Newport Oxygen")+ylab("Predicted Oxygen from model fit to in situ data")+geom_abline(slope=1, intercept=0)

#estimate uncertainty####
O2_sims <- predict(O2_model, newport2, nsim = 100)
O2_lower <- apply(O2_sims, MARGIN = 1, FUN = function(x) quantile(x, probs = 0.025))
O2_upper <- apply(O2_sims, MARGIN = 1, FUN = function(x) quantile(x, probs = 0.975))
O2_SE <- apply(O2_sims, MARGIN = 1, FUN = sd)

O2_CI <- as.data.frame(O2_lower)
O2_CI$upper <-O2_upper
O2_CI$true <- newport2$o2
O2_CI$pred <- tmp_predict$est
O2_CI$date <- newport2$sample_date
O2_CI$true_ln <- log(O2_CI$true)
ggplot(O2_CI, aes(x=date, y=true))+geom_point(colour="blue")+geom_pointrange(aes(x=date, y=exp(pred), ymax=exp(upper), ymin=exp(O2_lower)))

#How many within 95%?
O2_CI$within <- ifelse(log(O2_CI$true) > O2_CI$O2_lower & log(O2_CI$true) < O2_CI$upper, 1, 0)

#RMSE
rmse(tmp_predict$o2, exp(tmp_predict$est))
sd(tmp_predict$o2)

####Add based on GLORYS model for comparison
basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"

### 1) Set WD to folder with raw GLORYS data
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/GLORYS/wc_o2/for_tim")

### 2) Loop through all years, extract and format glorys data
run_all_years <- T
do_threshold <- 0
if (run_all_years) {
  setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/GLORYS/wc_o2/for_tim")
  files <- list.files("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/GLORYS/wc_o2/for_tim")
  convert_glorys <- function(file_name, do_threshold) {
    nc <- tidync(file_name)
    nc_df <- nc %>%
      hyper_tibble %>%
      group_by(longitude, latitude) %>%
      filter(depth == max(depth)) %>%
      ungroup() 
    # replace DO below threshold with the threshold level
    nc_df <- nc_df %>%
      mutate(o2 = case_when(
        o2 < do_threshold ~ do_threshold,
        TRUE ~ o2  # Keep other values unchanged
      ))
    
    # remove large list from memory
    rm(nc)
    days <- unique(nc_df$time)
    n_days <- length(days)
    first_day <- days[1]
    last_day <- days[n_days]
    days.2.use <- seq(first_day, last_day, by = 10)
    nc_df <- nc_df %>%
      filter(time %in% days.2.use)
    nc_df$time <- (as_datetime("1950-01-01")+hours(nc_df$time))
    nc_df <- nc_df %>%
      st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
      st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
      mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>% 
      st_set_geometry(NULL)
    nc_df$doy <- as.POSIXlt(nc_df$time, format = "%Y-%b-%d")$yday
    nc_df$year <- year(nc_df$time)
    nc_df$month <- month(nc_df$time)
    return(nc_df)
  }
  # get first year of glorys
  yearly_glorys <- convert_glorys(files[1], do_threshold)
  # get remaining years of glorys and combine into single data frame
  for (i in 2:length(files)) {
    tmp_glorys <- convert_glorys(files[i], do_threshold)
    yearly_glorys <- rbind(yearly_glorys, tmp_glorys)
  }
}
# remove unused file
rm(tmp_glorys)

### Loop through years, extract glorys, fit model, and get newport predictions ####
glory_years <- unique(yearly_glorys$year)

red_glorys <- yearly_glorys %>%
  filter(year == glory_years[1])
##### Fit spatial model ####
red_glorys <- as.data.frame(red_glorys)
spde <- make_mesh(data = red_glorys, xy_cols = c("X","Y"), n_knots = 500)

m1 <- sdmTMB(formula = log(o2) ~ 0 +s(depth) + s(doy),
             mesh = spde,
             data = red_glorys, 
             family = gaussian(), 
             spatial = "on",
             spatiotemporal  = "off")
summary(m1)
sanity(m1)

# extract year and dates of newport data  within time frame of model
red_newport <- newport_data %>%
  filter(year %in% glory_years[1],
         doy >= min(red_glorys$doy),
         doy <= max(red_glorys$doy)
  )

newport_predict <- predict(m1, newdata = red_newport)

# repeat for remaining years
for (i in 2:length(glory_years)) {
  red_glorys <- yearly_glorys %>%
    filter(year == glory_years[i])
  ##### Fit spatial model ####
  red_glorys <- as.data.frame(red_glorys)
  #spde <- make_mesh(data = red_glorys, xy_cols = c("X","Y"), n_knots = 250)
  m1 <- sdmTMB(formula = log(o2)  ~ 0 +  s(depth) + s(doy),
               mesh = spde,
               data = red_glorys, 
               family = gaussian(), 
               spatial = "on",
               spatiotemporal  = "off")
  summary(m1)
  sanity(m1)
  
  # extract year and dates of newport data  within time frame of model
  red_newport <- newport_data %>%
    filter(year %in% glory_years[i],
           doy >= min(red_glorys$doy),
           doy <= max(red_glorys$doy)
    )
  
  tmp_predict <- predict(m1, newdata = red_newport)
  # add new predictions to data frame
  newport_predict <- rbind(newport_predict, tmp_predict)
  
}

#Plot and compare:
ggplot(tmp_predict2, aes(x=o2, y=exp(est)))+geom_point(colour="blue", size=3)+xlab("Newport Oxygen")+ylab("Predicted Oxygen")+geom_abline(slope=1, intercept=0)+
      geom_point(newport_predict, mapping=aes(x=o2, y=exp(est), size=3), colour="red")+

  theme(legend.position="none")



#
m1 <- sdmTMB(formula = log(o2) ~ 0 +s(depth) + s(month, bs = "cc", k = 4), #s(doy),
             mesh = spde,
             data = red_glorys, 
             family = gaussian(), 
             spatial = "on",
             spatiotemporal  = "off")
summary(m1)
sanity(m1)

# extract year and dates of newport data  within time frame of model
red_newport <- newport_data %>%
  filter(year %in% glory_years[1],
         doy >= min(red_glorys$doy),
         doy <= max(red_glorys$doy)
  )

newport_predict <- predict(m1, newdata = red_newport)

# repeat for remaining years
for (i in 2:length(glory_years)) {
  red_glorys <- yearly_glorys %>%
    filter(year == glory_years[i])
  ##### Fit spatial model ####
  red_glorys <- as.data.frame(red_glorys)
  #spde <- make_mesh(data = red_glorys, xy_cols = c("X","Y"), n_knots = 250)
  m1 <- sdmTMB(formula = log(o2)  ~ 0 +  s(depth) + s(month, bs = "cc", k = 4),#s(doy)  ,
               mesh = spde,
               data = red_glorys, 
               family = gaussian(), 
               spatial = "on",
               spatiotemporal  = "off")
  summary(m1)
  sanity(m1)
  
  # extract year and dates of newport data  within time frame of model
  red_newport <- newport_data %>%
    filter(year %in% glory_years[i],
           doy >= min(red_glorys$doy),
           doy <= max(red_glorys$doy)
    )
  
  tmp_predict <- predict(m1, newdata = red_newport)
  # add new predictions to data frame
  newport_predict <- rbind(newport_predict, tmp_predict)
  
}

#Quick comparison 
#Isolate IPHC data in the NWFSC zone
IPHC <- subset(density_O2, latitude<49 & Survey=="IPHC")
NWFSC <- subset(density_O2, Survey=="NOAA_West_Coast")
NWFSC$o2_true <- NWFSC$O2_umolkg_ln
NWFSC$O2_umolkg_ln <- NULL
combined <- bind_rows(IPHC, NWFSC)
IPHC <- as.data.frame(IPHC)
combined <- as.data.frame(combined)
spde <- make_mesh(data = combined, xy_cols = c("X","Y"), cutoff = 30)

m_2 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma_exp) + s(temperature_C) + s(p_dbar_ln) + s(month, bs = "cc", k = 4),
              mesh = spde,
              data = combined, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")

NWFSC <- as.data.frame(NWFSC)
tmp_predict <- predict(m_2, newdata = NWFSC)

ggplot(tmp_predict, aes(x=O2_umolkg, y=exp(est)))+geom_point(aes(colour=p_dbar))+xlab("NWFSC Observed Oxygen")+ylab("Predicted Oxygen from IPHC data")+geom_abline(slope=1, intercept=0)+facet_wrap("year")

tmp_predict2 <- subset(tmp_predict, exp(est)< 1)
ggplot(tmp_predict2, aes(x=X, y=Y))+geom_point(aes(colour=p_dbar))+facet_wrap("year")

tmp_predict2 <- subset(tmp_predict, latitude>38.9)
tmp_predict2 <- subset(tmp_predict, exp(est)>4)
ggplot(tmp_predict2, aes(x=O2_umolkg, y=exp(est)))+geom_point(aes(colour=p_dbar))+xlab("NWFSC Observed Oxygen")+ylab("Predicted Oxygen from IPHC data")+geom_abline(slope=1, intercept=0)+facet_wrap("year")


#############################
###REST OF THOMPSON'S CODE####
test_predict_O2 <- predict(O2_model, newdata = test)
train_predict_O2 <- predict(O2_model, newdata = train)
rsq <- function (x, y) cor(x, y) ^ 2
rsq(test_predict_O2$O2_ln, test_predict_O2$est)
rsq(train_predict_O2$O2_ln, train_predict_O2$est)
install.packages("Metrics")
library(Metrics)
rmse(test_predict_O2$O2_ln, test_predict_O2$est)/(max(test_predict_O2$O2_ln)- min(test_predict_O2$O2_ln))

save(m_1, m_2, O2_model, test, train, test_predict_O2, train_predict_O2,file = "./data/O2_mod.RData")
save(test_predict_O2, train_predict_O2,file = "./data/O2_predict.RData")

test_predict_O2 %>% 
  mutate(resid = O2_ln-est) %>% 
  ggplot(aes(color = sigma_exp, x = do_mlpL, y = exp(est)))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = 1)+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_viridis_c()+
  ylab("predicted O2")

test_predict_O2 %>% 
  mutate(resid = O2_ln-est) %>% 
  ggplot(aes(x = year, y = resid, group = year, color = O2_ln))+
  geom_jitter()+
  ylab("residuals")+
  scale_color_viridis_c(option = "B")

plot_grid(
  ggplot(test_predict_O2, aes(x = p_dbar, y = do_mlpL, color = temperature_C))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10(limits = c(0.03,10))+
    scale_color_viridis(option = "B")+
    ylab("observed O2"),
  
  ggplot(test_predict_O2, aes(x = p_dbar, y = exp(est), color = temperature_C))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10(limits = c(0.03,10))+
    scale_color_viridis(option = "B")+
    ylab("predicted O2"), nrow = 2
)

plot_grid(
  ggplot(test_predict_O2, aes(x = p_dbar, y = exp(est_non_rf), color = temperature_C))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()+
    scale_color_viridis(option = "B")+
    ylab("non_random component O2"),
  
  ggplot(test_predict_O2, aes(x = p_dbar, y = est_rf, color = temperature_C))+
    geom_point()+
    scale_x_log10()+
    scale_color_viridis(option = "B")+
    ylab("random component O2"), nrow = 2)

ggplot(test_predict_O2, aes(color = est,x = X, y = Y))+
  geom_point()+
  scale_color_viridis(option = "B")+
  coord_equal()+

ggplot(test_predict_O2, aes(color = est_non_rf,x = X, y = Y))+
  geom_point()+
  scale_color_viridis(option = "B")+
  coord_equal()+

ggplot(test_predict_O2, aes(color = est_rf,x = X, y = Y))+
  geom_point()+
  scale_color_viridis(option = "B")+
  coord_equal()

ggplot(test_predict_O2, aes(color = exp(est_rf), x = X, y = Y))+
  geom_point()+
  scale_color_viridis(option = "B")+
  coord_equal()+
  facet_wrap(~year)

#predict O2 in data without obs####
O2_predictions <- predict(O2_model, density_no_O2)

ggplot(O2_predictions, aes(x = p_dbar, y = exp(est), color = temperature_C))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_viridis(option = "B")+
  ylab("observed O2")

ggplot(O2_predictions, aes(color = p_dbar, y = exp(est), x = temperature_C))+
  geom_point()+
  #scale_x_log10()+
  scale_y_log10()+
  scale_color_viridis(option = "F", trans = "log10", direction = -1)+
  ylab("observed O2")

env.df <- bind_rows(test_predict_O2, train_predict_O2, O2_predictions)
summary(env.df)

env.df <- env.df %>% 
  select(X, Y, Survey, year, month, latitude, longitude, salinity, temperature_C, p_dbar, p_dbar_ln, 
         sigma0_kgm3, sigma_exp, O2_obs = do_mlpL, O2_ln_obs = O2_ln, O2_predicted_ln = est) %>% 
  mutate(O2_predicted = exp(O2_predicted_ln)) %>% 
  mutate(O2 = ifelse((!is.na(O2_obs) & O2_obs>0), O2_obs, O2_predicted)) %>% 
  mutate(O2_ln = log(O2)) %>% 
  filter(Survey != "IPHC")

ggplot(env.df, aes(x = X, y = Y, color = O2))+
  geom_point()+
  facet_wrap(~year)+
  scale_color_viridis_c()+
  coord_equal()

#estimate uncertainty####
O2_sims <- predict(O2_model, density_no_O2, nsim = 100)
O2_lower <- apply(O2_sims, MARGIN = 1, FUN = function(x) quantile(x, probs = 0.025))
O2_upper <- apply(O2_sims, MARGIN = 1, FUN = function(x) quantile(x, probs = 0.975))
O2_SE <- apply(O2_sims, MARGIN = 1, FUN = sd)

O2_predictions$O2_lower <- exp(O2_lower)
O2_predictions$O2_upper <- exp(O2_upper)
O2_predictions$SE <- O2_SE

save(O2_predictions, file = "./data/O2_predictions.RData")

ggplot(O2_predictions, aes(x = exp(est), y =  SE))+
  geom_point()+
  scale_x_log10()

ggplot(O2_predictions, aes(x = exp(est), y =  (O2_upper - O2_lower)))+
  geom_point()+
  scale_y_log10()+
  scale_x_log10()


ggplot(O2_predictions, aes(x = X, y = Y, color = (O2_upper - O2_lower)/exp(est)))+
  geom_point()+
  facet_wrap(~year)+
  scale_color_viridis_c()+
  coord_equal()

ggplot(O2_predictions, aes(x = exp(est), y =  (O2_upper - O2_lower)))+
  geom_point()+
  facet_wrap(~year)+
  scale_y_log10()+
  scale_x_log10()

#merge with survey data####
combined_data <- read_rds(file = "./data/combined_surveys.rds")
combined_data <- combined_data %>% 
  select(fishing_event_id, trip_id, survey_id, year, month, latitude, longitude, Survey, Survey2, valid_name, common_name, cpue_kg_per_ha_der, depth_m,)

combined_data2 <- left_join(combined_data, env.df)
summary(combined_data2)
write_rds(combined_data2, file = "./data/combined_surveys_mod_O2.rds")
