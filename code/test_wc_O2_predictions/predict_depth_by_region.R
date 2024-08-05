library(tidyverse)
library(sdmTMB)

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
  poly <- regions.hull$geometry[i]
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


# Starter code for looking at different data types 

trawl_dat <- dat %>%
  filter(survey %in% c("nwfsc", "dfo", "goa", "EBS"))

iphc_dat <- dplyr::filter(dat, survey == "iphc")

ctd_dat <- dat %>%
  filter(!survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))


    
         