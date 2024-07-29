install.packages("marmap")
library(marmap)
library(sf)
library(dplyr)
library(sdmTMB)

# playground code for creating grids, extract data for UW west coast only
dat <- readRDS("data/processed_data/all_o2_dat.rds")

dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range
latrange <- range(dat_noiphc$latitude)
lonrange <- range(dat_noiphc$longitude)

wcbts <- dplyr::filter(dat, survey == "nwfsc")
dfo <- dplyr::filter(dat, survey == "dfo")

noaa_depths <- getNOAA.bathy(lon1 = lonrange[1], 
                             lon2 = lonrange[2],
                             lat1 = latrange[1], 
                             lat2 = latrange[2], 
                             resolution = 4, 
                             keep = TRUE)

depths_sp <- as.SpatialGridDataFrame(noaa_depths)
depths_sf <- st_as_sf(depths_sp, crs = st_crs(4326))


# Tip Vancouver island to southern california
maxlat_region = 50.95
dfo_south <- dplyr::filter(dfo, latitude <=maxlat_region) 
# get most western point of trawls in this region
minlon_region <- min(wcbts$longitude, dfo_south$longitude)

# pull out bathymetry data w/in this region
maxdepth <- max(dfo_south$depth, wcbts$depth)
# remove depths_sf rows that are above 0 
depths_sf <- dplyr::filter(depths_sf, layer <0)



# create a dataframe of noaa bathymetry
tmp <- st_coordinates(depths_sf)
depthlist <- -depths_sf$layer
newdepth <- tibble(longitude = tmp[,1],
                   latitude = tmp[,2],
                   noaadepth = depthlist)

# restrict latitude and longitude
newdepth <- newdepth %>%
  filter(latitude <= maxlat_region, longitude >=minlon_region, noaadepth <= 1.1 * maxdepth )
# fit model
#Convert coordinates

newdepth <- newdepth %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

newdepth <- as.data.frame(newdepth)
spde <- make_mesh(newdepth, xy_cols = c("X", "Y"), n_knots = 250)
depth_model_1 <- sdmTMB(formula = log(noaadepth) ~ 1,
                      mesh = spde,
                      data = newdepth,
                      family = gaussian(),
                      spatial = "on",
                      anisotropy = TRUE)
s_vancouver_island <- list(latrange = range(newdepth$latitude),
                           lonrange = range(newdepth$longitude),
                           maxdepth = maxdepth,
                           spde = spde,
                           depth_model = depth_model_1)

############################################################
# N vancouver island to mid Chicago
minlat_region = 50.95
maxlat_region = 57.45
# get trawl data in this region
minlon_region <- -136.6

# check w goa and dfo
goa_south <- dplyr::filter(dat, survey == "goa", latitude <= maxlat_region, longitude >= -140)
dfo_north <- dplyr::filter(dat, survey == "dfo", latitude <= maxlat_region & latitude > minlat_region )    
min(c(goa_south$longitude, dfo_north$longitude))
# good check!
# recreate newdepth
newdepth <- tibble(longitude = tmp[,1],
                   latitude = tmp[,2],
                   noaadepth = depthlist)

maxdepth <- max(c(goa_south$depth, dfo_north$depth))


# restrict latitude and longitude
newdepth <- newdepth %>%
  filter(latitude <= maxlat_region, latitude > minlat_region,
         longitude >=minlon_region, 
         noaadepth <= 1.1 * maxdepth )

newdepth <- newdepth %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

newdepth <- as.data.frame(newdepth)
spde <- make_mesh(newdepth, xy_cols = c("X", "Y"), n_knots = 250)
depth_model_2 <- sdmTMB(formula = log(noaadepth) ~ 1,
                        mesh = spde,
                        data = newdepth,
                        family = gaussian(),
                        spatial = "on",
                        anisotropy = TRUE)
# save bc and s. alaska

bc_s_ak <- list(latrange = range(newdepth$latitude),
                           lonrange = range(newdepth$longitude),
                           maxdepth = maxdepth,
                           spde = spde,
                           depth_model = depth_model_2)

######## More complex, gulf of alaska
minlat_region <-57.45

n_goa <- dplyr::filter(dat, survey == "goa",
                       longitude < -136.6)

