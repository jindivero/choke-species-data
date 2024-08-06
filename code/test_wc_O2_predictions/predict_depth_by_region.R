library(tidyverse)
library(sdmTMB)
library(Metrics)
library(sf)
library(viridis)

# load bathymetry data
bathy_all <- readRDS("data/processed_data/bathymetry_regions_from_grids.rds")
# load regional polygons
regions.hull <- readRDS("data/processed_data/regions_hull.rds")


# fit models for each region

make_depth_model <- function(bathydat) {

spde <- make_mesh(data = as.data.frame(bathydat), xy_cols = c("X", "Y"), n_knots = 300)

depth_model <- sdmTMB(log(noaadepth) ~ 1,
                         data = as.data.frame(bathydat),
                         spatial = "on", 
                         mesh = spde,
                         family = gaussian()
)
}
depth_models <- lapply(X = bathy_all,
                       FUN = make_depth_model)


# load all data
dat <- readRDS("data/processed_data/all_o2_dat.rds")
dat_df <-  st_as_sf(dat, coords = c("longitude", "latitude"), crs = st_crs(4326))

# cycle through all regions
region_list <- c("ai", "bc", "cc", "ebs", "goa")

for (i in 1:length(region_list)) {
  region <- region_list[i]
  poly <- regions.hull[i,2]
  model.2.use <- depth_models[[i]]
  # pull out observations within each region
  region_dat  <- st_filter(dat_df, poly)
  
  # get predicted log(depth) for each observation, based on model fit to that region
  region_dat_predict <- predict(model.2.use, as.data.frame(region_dat))
  region_dat_predict$region <- region
  
  # save results in new data frame called dat_predict
  if (i == 1)
    dat_predict <- region_dat_predict
  if (i > 1)
    dat_predict <- bind_rows(dat_predict, region_dat_predict)
}


###Evaluate predictions
##Set ggplot themes
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Create a column for CTD vs synoptic
ctd_dat <- dat_predict %>%
  filter(!survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))
ctd_dat$type <- "ctd"

trawl_dat <- dat_predict %>%
  filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))
trawl_dat$type <- "synoptic"

dat_predict <- bind_rows(trawl_dat, ctd_dat)

##Plot
ggplot(dat_predict, aes(x=depth, y=exp(est)))+geom_point(aes(colour=survey))+geom_abline()+facet_wrap("survey")

##Predict back to the bathymetry data
for (i in 1:length(region_list)) {
  region <- region_list[i]
  # poly <- regions.hull[i,2]
  model.2.use <- depth_models[[i]]
  # pull out observations within each region
  #  region_dat  <- st_filter(dat_df, poly)
  
  # get predicted log(depth) for each observation, based on model fit to that region
  region_dat_predict <- predict(model.2.use, as.data.frame(bathy_all[[i]]))
  region_dat_predict$region <- region
  
  # save results in new data frame called dat_predict
  if (i == 1)
    dat_predict2 <- region_dat_predict
  if (i > 1)
    dat_predict2 <- bind_rows(dat_predict2, region_dat_predict)
}

#Plot
ggplot(dat_predict2, aes(x=noaadepth, y=exp(est)))+
  geom_point(aes(colour=region))+
  geom_abline()+
  facet_wrap("region")+
  ylab("Predicted Depth (m)")+
  xlab("Actual Depth (m)")

dat_predict2$resid <- log(dat_predict2$noaadepth) - dat_predict2$est
ggplot(dat_predict2, aes(x=X, y=Y))+
  geom_point(aes(colour=resid), size=0.3)+
  facet_wrap("region", scales="free")+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_colour_distiller(palette = "RdBu")

#RMSE
rmse <- rmse(dat_predict2$noaadepth, exp(dat_predict2$est))

#Per region
rmse_survey <- dat_predict2 %>%
  group_by(region) %>%
  summarise(rmse = rmse(noaadepth, exp(est)))


####Back to predictions#####
#Remove the outliers
dat_predict <- filter(dat_predict, depth>0 & exp(est)<20000)

#Re-plot
ggplot(dat_predict, aes(x=depth, y=exp(est)))+
  geom_point(aes(colour=survey))+
  geom_abline()+
  facet_wrap("survey", scales="free")+
  theme(legend.position="none")+
  ylab("Predicted Depth (m)")+
  xlab("Actual Depth (m)")

#For each region
ggplot(dat_predict, aes(x=depth, y=exp(est)))+
  geom_point(aes(colour=survey))+
  geom_abline()+
  facet_wrap("region", scales="free")+
  ylab("Predicted Depth (m)")+
  xlab("Actual Depth (m)")

#For each region, synoptic only
ggplot(filter(dat_predict, type=="synoptic"), aes(x=depth, y=exp(est)))+
  geom_point(aes(colour=survey))+
  geom_abline()+
  facet_wrap("region", scales="free")+
  ylab("Predicted Depth (m)")+
  xlab("Actual Depth (m)")

##Get the RMSE for trawl surveys in each region
#Total
rmse <- rmse(dat_predict$depth, exp(dat_predict$est))

##RMSE
dat_rmse_survey <- dat_predict %>%
  group_by(survey) %>%
  summarise(rmse = rmse(depth, exp(est)))

dat_rmse_region<- dat_predict %>%
  group_by(region) %>%
  summarise(rmse = rmse(depth, exp(est)))

#Synoptic only
dat_rmse_region<- filter(dat_predict, type=="synoptic") %>%
  group_by(region) %>%
  summarise(rmse = rmse(depth, exp(est)))

dat_rmse_region<- filter(dat_predict, type=="synoptic") %>%
  group_by(region, survey) %>%
  summarise(rmse = rmse(depth, exp(est)))
dat_rmse_region <- pivot_wider(dat_rmse_region, names_from=survey, values_from=rmse)

###Look more into IPHC
#see what is going on with IPHC depths   

#IPHC in each region
ggplot(filter(dat_predict, survey=="iphc"), aes(x=depth, y=exp(est)))+
  geom_point()+
  geom_abline()+
  facet_wrap("region", scales="free")+
  ylab("Predicted Depth (m)")+
  xlab("Actual Depth (m)")

###Tim's code for plotting residuals--work on looping through each region

# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")
dat.2.use <- filtered_dat
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(282853, 1025581)
ylimits = c(3549000, 5366000)

output <- list()
set.seed(789)

data_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(
    data = test_predict_O2,
    aes(
      x = X * 1000,
      y = Y * 1000,
      col = do
    ),
    size = 1.0,
    alpha = 1.0
  ) +
  scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_viridis_c(
    limits = c(0, 200),
    oob = scales::squish,
    name = bquote(O[2]),
    breaks = c(0, 100, 200)
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom") +
  guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                    0.5))

predict_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(
    data = test_predict_O2,
    aes(
      x = X * 1000,
      y = Y * 1000,
      col = (est)
    ),
    size = 1.0,
    alpha = 1.0
  ) +
  scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_viridis_c(
    limits = c(0, 200),
    oob = scales::squish,
    name = bquote(O[2]),
    breaks = c(0, 100, 200)
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom") +
  guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                    0.5))

residual_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(
    data = test_predict_O2,
    aes(
      x = X * 1000,
      y = Y * 1000,
      col = residual
    ),
    size = 1.0,
    alpha = 1.0
  ) +
  scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_distiller(palette = "RdBu", limits = c(-50, 50)) +
  #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom") +
  guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                    0.5))

## put all plots in one ####
grid.arrange(data_plot, predict_plot, residual_plot, ncol = 3)
# plot residuals vs. prediction
resid_vs_pred <- ggplot(data = test_predict_O2, aes(x = (est), y = residual, col = Y)) +
  geom_point() +
  scale_colour_viridis_c(
    limits = c(31, 50),
    oob = scales::squish,
    name = "latitude",
    breaks = c(35, 40, 45)
  ) +
  ggtitle(test_year) +
  labs(x = "Predicted", y = "Residual") +
  theme(legend.position = "none")
pred_vs_actual <- ggplot(data = test_predict_O2, aes(x = do, y = est, col = Y)) +
  geom_point() +
  scale_colour_viridis_c(
    limits = c(31, 50),
    oob = scales::squish,
    name = "latitude",
    breaks = c(35, 40, 45)
  ) +
  ggtitle(test_year) +
  labs(x = "Observed", y = "Predicted") +
  geom_abline(intercept = 0, slope = 1) +
  theme(legend.position = "none")

