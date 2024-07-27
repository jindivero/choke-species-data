# map location of non-synoptic o2 measurements

library(tidync)
library(sf)
library(dplyr)
library(tidyr)
library(sdmTMB)
library(seacarb)
library(respR)
library(ggplot2)
library(Metrics)
library(rnaturalearth)
library(gridExtra)
library(RColorBrewer)

map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")
us_coast_proj <- sf::st_transform(map_data, crs = 32610)
# remove trawl surveys and DFO data
survey.2.use <- c("nwfsc", "iphc", "NewportLine", "calCOFI", "wcoa", "copdap", "ocnms", "EBS", "goa")

dat <- readRDS("data/processed_data/all_o2_dat.rds")
unique(dat$survey)

dat %>%
  group_by(survey) %>%
  summarise(maxlong = max(longitude),
            minlong = min(longitude))
            
latrange <- range(dat$latitude)
lonrange <- range(dat$longitude)

mindoy <- 137-14
maxdoy <- 296+14
dat <- dat %>%
  filter(survey %in% survey.2.use,
         (doy >=mindoy & doy<=maxdoy))

xlimits = range(dat$X) * 1000
ylimits = range(dat$Y) * 1000

data_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(
    data = dat,
    aes(
      x = X * 1000,
      y = Y * 1000,
      col = survey
    ),
    size = 1.0,
    alpha = 1.0
  ) +
  scale_x_continuous( limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_viridis_d(option = "turbo",  ) +
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
  theme(legend.position = "bottom") 
data_plot

# make summary of min and max years
sum_dat <- dat %>%
  group_by(survey) %>%
  summarise(minyear = min(year), maxyear = max(year))

print(sum_dat)

# figure out the difference between minimum depth and estimated bottom depth


library(marmap)

# playground code for creating grids, extract data for UW west coast only
minlon = lonrange[1]
maxlon = lonrange[2]
minlat = latrange[1]
maxlat = latrange[2]

noaa_depths <- getNOAA.bathy(lon1 = minlon, lon2 = maxlon,
                             lat1 = minlat, lat2 = maxlat, resolution = 4, 
                             keep = TRUE)
dat$sampled_depth <- dat$depth
depths_sp <- as.SpatialGridDataFrame(noaa_depths)
depths_sf <- st_as_sf(depths_sp, crs = st_crs(4326))
depths_sf$geometry
dat_sf <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = st_crs(4326))

# remove depths_sf rows that are above 0 
depths_sf <- dplyr::filter(depths_sf, layer <0)

# create a dataframe of noaa bathymetry
tmp <- st_coordinates(depths_sf)
depthlist <- -depths_sf$layer
newdepth <- tibble(longitude = tmp[,1],
                   latitude = tmp[,2],
                   noaadepth = depthlist)

newdepth <- newdepth %>%
  filter(noaadepth <= 1000)


#Convert coordinates
newdepth <- newdepth %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

newdepth <- as.data.frame(newdepth)

# fit spatial model to predict depth
spde <- make_mesh(newdepth, xy_cols = c("X", "Y"), n_knots = 250)

depth_model <- sdmTMB(formula = log(noaadepth) ~ 1,
                      mesh = spde,
                      data = newdepth,
                      family = gaussian(),
                      spatial = "on",
                      anisotropy = TRUE)

# now load IPHC data

iphc_dat <- dat %>%
  filter(survey == "iphc", (latitude >= minlat &  latitude <= maxlat),
         (longitude >=minlon & longitude <= maxlon ))


# predict based oon fitted sdM model
iphc_predict <- predict(depth_model, newdata = iphc_dat)
plot(log(iphc_predict$depth), (iphc_predict$est), main = "iphc")
abline(a = 0, b=1, lwd = 2)

# not great, a bunch right at the intercept.


# try again but with wcbts data
wcbts <- dat %>%
  filter(survey == "nwfsc", (latitude >= minlat &  latitude <= maxlat),
         (longitude >=minlon & longitude <= maxlon ))

wcbts_predict <- predict(depth_model, newdata = wcbts)
rmse(log(wcbts_predict$depth), wcbts_predict$est)
plot(log(wcbts_predict$depth), (wcbts_predict$est))
abline(a = 0, b=1, lwd = 2)

rmse(log(wcbts_predict$depth), wcbts_predict$est)

# do some other surveys, newport line!
newportline <- dat %>%
  filter(survey == "NewportLine", (latitude >= minlat &  latitude <= maxlat),
         (longitude >=minlon & longitude <= maxlon ))

newport_predict <- predict(depth_model, newdata = newportline)
plot(log(newport_predict$depth), (newport_predict$est), main = "Newport Line")
abline(a = 0, b=1, lwd = 2)

# wcoa

wcoa <- dat %>%
  filter(survey == "wcoa", (latitude >= minlat &  latitude <= maxlat),
         (longitude >=minlon & longitude <= maxlon ))

wcoa_predict <- predict(depth_model, newdata = wcoa)
plot(log(wcoa_predict$depth), (wcoa_predict$est), main = "WCOA")
abline(a = 0, b=1, lwd = 2)

# ocnms

ocnms <- dat %>%
  filter(survey == "ocnms")

ocnms_predict <- predict(depth_model, newdata = ocnms)
plot(log(ocnms_predict$depth), (ocnms_predict$est), main = "OCNMS")
abline(a = 0, b=1, lwd = 2)

# calCOFI
calcofi <-  dat %>%
  filter(survey == "calCOFI")

calcofi_predict <- predict(depth_model, newdata = calcofi)
plot(log(calcofi_predict$depth), (calcofi_predict$est), main = "calCOFI")
abline(a = 0, b=1, lwd = 2)


