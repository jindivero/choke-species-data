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

# extract latitude and longitude so that dataframe is consistent with others
lon_lats <- sf::st_coordinates(dat_predict$geometry)
dat_predict$longitude <- lon_lats[,1]
dat_predict$latitude <- lon_lats[,2]

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
ggplot(trawl_dat, aes(x=log(depth), y=est))+
  geom_point(aes(colour=survey))+
  geom_abline()+
  facet_wrap("survey", scales="free")+
  xlab("Log(depth) Reported")+
  ylab("Log(depth) Estimated")


# Calculate RMSE by region for synoptic measurements (trawl / longline) 
rmse <- trawl_dat %>%
  group_by(region) %>%
  summarise(rmse = rmse(log(depth), est))

# calculate difference between observed / expected in log space for CTD casts
ctd_dat$depth_error <- with(ctd_dat, log(depth) - est)

# Loop through regions, for each, pull out CTD data for that region, and retain 
# rows where depth residual (in log space) is < 2 * RMSE for that region
for (i in 1:length(region_list)) {
  # extract CTD casts in this region
  ctd_region_dat <- dplyr::filter(ctd_dat, region == region_list[i],
                                  abs(depth_error) <= 2 * rmse$rmse[i])
  if (i == 1) ctd_dat_filtered <- ctd_region_dat
  if (i > 1) ctd_dat_filtered <- bind_rows(ctd_dat_filtered, ctd_region_dat)
}
  
# combine filtered CTD data with synoptic trawl data
dat_filtered <- bind_rows(trawl_dat, ctd_dat_filtered)

# select columns that we want to retain
dat_filtered <- dat_filtered %>%
  select(survey, year, doy, X, Y, latitude, longitude, temp, o2, sigma0, depth, region)
  
# make this a dataframe by removing the geometry
dat_filtered <- st_drop_geometry(dat_filtered)

# save file
write_rds(x = dat_filtered, file = "data/processed_data/all_o2_dat_filtered.rds")
