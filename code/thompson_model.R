library(sf)
library(sdmTMB)
library(tidyverse)
library(cowplot)
library(viridis)
library(patchwork)

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


density_data <- readRDS("./data/PotentialDensityData.RDS")
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
  
set.seed(7)
density_O2$test <- rbinom(n = nrow(density_O2), size = 1, prob = 0.3)
train <- density_O2[density_O2$test == 0,]
test <- density_O2[density_O2$test == 1,]

train <- as.data.frame(train)
spde <- make_mesh(data = train, xy_cols = c("X","Y"), cutoff = 30)

plot(spde)

m_1 <- sdmTMB(formula = O2_ln  ~ 0 + s(sigma_exp) + s(temperature_C) + s(p_dbar_ln),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")

if(max(m_1$gradients)>0.01){
  m_1 <- sdmTMB::run_extra_optimization(m_1, nlminb_loops = 1L, newton_loops = 1L)
}


m_2 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma_exp) + s(temperature_C) + s(p_dbar_ln) + s(month, bs = "cc", k = 4),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")

if(max(m_2$gradients)>0.01){
  m_2 <- sdmTMB::run_extra_optimization(m_2, nlminb_loops = 1L, newton_loops = 1L)
}

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
AIC(m_1, m_2)

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
newport2 <- subset(newport_data, year>2016 & year<2020)
#Add month column
newport2$month <- month(newport2$sample_date)
#Make column names the same
newport2$sigma_exp <- exp(newport2$sigma)
newport2$p_dbar_ln <- log(newport2$pressure..dbar.)
newport2$temperature_C <- newport2$temperature..degC.

tmp_predict <- predict(m_2, newdata = newport2)

#plot
#Convert oxygen units

ggplot(tmp_predict, aes(x=o2, y=exp(est)))+geom_point(aes(colour=depth), size=3)+xlab("Newport Oxygen")+ylab("Predicted Oxygen from model fit to in situ data")




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
