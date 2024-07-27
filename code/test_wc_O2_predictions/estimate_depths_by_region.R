library(FishStatsUtils)
library(concaveman)
library(marmap)
library(tidyverse)


# get all noaa bathymetry data within data range
# playground code for creating grids, extract data for UW west coast only
dat <- readRDS("data/processed_data/all_o2_dat.rds")

dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range
latrange <- range(dat_noiphc$latitude)
lonrange <- range(dat_noiphc$longitude)

wcbts <- dplyr::filter(dat, survey == "nwfsc")
dfo <- dplyr::filter(dat, survey == "dfo")

noaa_depths <- getNOAA.bathy(lon1 = lonrange[1], 
                             lon2 = -117,
                             lat1 = latrange[1], 
                             lat2 = 32, 
                             resolution = 4, 
                             keep = TRUE)
depths_sp <- as.SpatialGridDataFrame(noaa_depths)
depths_sf <- st_as_sf(depths_sp, crs = st_crs(4326))

# Go by region, first US CC

data("california_current_grid")

grid.2.use <- california_current_grid
# rename Lat and Lon
grid.2.use <- grid.2.use %>%
  rename(lon = Lon,
         lat = Lat)
# make grid a sf object
grid.2.use_sf <- st_as_sf(grid.2.use, coords = c("lon", "lat"), crs = st_crs(4326))

# create polygon covering extent of grid
poly <- st_convex_hull(st_union(grid.2.use_sf))
depths_in_region <- st_filter(depths_sf,poly)

# not working, showing only 11 depths within the polygon.   Need help from group!

# check polygon by plotting
plot(poly)
points(grid.2.use$lon, grid.2.use$lat)
