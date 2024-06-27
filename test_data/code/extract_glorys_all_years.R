library(tidync)
library(lubridate)
library(sf)
library(dplyr)
library(sdmTMB)
library(seacarb)
library(respR)
library(ggplot2)
basewd <- getwd()

### 1) Set WD to folder with raw GLORYS data
setwd("/Users/essing/Library/CloudStorage/Dropbox/for_tim")

### 2) Pick file for year
##One year
#Note: the GLORYS files are for -138 through -114 deg W, and 26--51 deg N, from 0.51 to 1600m deep, from May 1st through October 29th
file <- "cmems_mod_glo_bgc_my_0.25deg_P1D-m_o2_138.00W-114.00W_26.00N-51.00N_0.51-1516.36m_2019-05-01-2019-10-29.nc"

### 3) Convert GLORYS raw file to a dataframe, use only depths > 30 meters
nc <- tidync(file)
nc_df <- nc %>%
  hyper_tibble %>%
  group_by(longitude, latitude) %>%
  filter(depth == max(depth)) %>%
  ungroup() %>%
  filter(depth >=30)

# remove large list from memory
rm(nc)


# get unique days

days <- unique(nc_df$time)
n_days <- length(days)

first_day <- days[1]
last_day <- days[n_days]
days.2.use <- seq(first_day, last_day, by = 10)

nc_df <- nc_df %>%
  filter(time %in% days.2.use)

#Note: o2 is in umol per m^3 (i.e. umol per L)

### 4) Convert hours since January 1st, 1950 to normal human calendar date
nc_df$time <- (as_datetime("1950-01-01")+hours(nc_df$time))

### 5) Convert lat/lon to UTM -
nc_df <- nc_df %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

nc_df$doy <- as.POSIXlt(nc_df$time, format = "%Y-%b-%d")$yday



### 6) Make this a function and run for each year
##List of files for looping

run_all_years <- T
if (run_all_years) {
  setwd("/Users/essing/Library/CloudStorage/Dropbox/for_tim")
  
  files <- list.files("/Users/essing/Library/CloudStorage/Dropbox/for_tim")
  convert_glorys <- function(file_name) {

  nc <- tidync(file_name)
  nc_df <- nc %>%
    hyper_tibble %>%
    group_by(longitude, latitude) %>%
    filter(depth == max(depth)) %>%
    ungroup() %>%
    filter(depth >=30)
  
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
  
  return(nc_df)
  }
  yearly_glorys <- convert_glorys(files[1])
  
  for (i in 2:length(files)) {
    tmp_glorys <- convert_glorys(files[i])
    yearly_glorys <- rbind(yearly_glorys, tmp_glorys)
  }
  
  }

rm(tmp_glorys)

# load Newport data
setwd(basewd)
newport_data <- readRDS("test_data/data/newport_bottom.RDS")

# neeed to add latitude, depth, and convert DO to mmol
newport_lat <- 44.65
newport_data$latitude <- newport_lat
newport_data$year <- year(newport_data$sample_date)
newport_data$doy <- as.POSIXlt(newport_data$sample_date, format = "%Y-%b-%d")$yday
newport_data$depth <- p2d(lat = newport_lat,
                          p = newport_data$pressure..dbar.)


# now get lat and long in UTC coordinates
newport_data <- newport_data %>%
  st_as_sf(coords=c('longitude..degW.','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 

newport_data <- as.data.frame(newport_data)


glory_years <- unique(yearly_glorys$year)
# extract year and samples within time frame of model
yearly_newport <- newport_data %>%
  filter(year %in% glory_years,
         doy >= min(yearly_glorys$doy),
         doy <= max(yearly_glorys$doy)
  )
source("test_data/code/convert_funs.R")
yearly_newport$sigma <- calc_sigma( s = yearly_newport$practical.salinity,
                                    t = yearly_newport$temperature..degC.,
                                    p = yearly_newport$pressure..dbar.)
yearly_newport$o2 <- convert_o2(yearly_newport$dissolved.oxygen..ml.L., yearly_newport$sigma)
yearly_newport$o2_fun <- convert_DO(x = yearly_newport$dissolved.oxygen..ml.L.,
                                    from = "ml /l", 
                                    to = "umol / kg",
                                    S = yearly_newport$practical.salinity,
                                    t = yearly_newport$temperature..degC.)
# setup sdmTMB
yearly_glorys <- as.data.frame(yearly_glorys)
spde <- make_mesh(data = yearly_glorys, xy_cols = c("X","Y"), n_knots = 250)
m1 <- sdmTMB(formula = log(o2)  ~ 0 +  s(depth) + +s(doy)  ,
             mesh = spde,
             data = yearly_glorys, 
             family = gaussian(), 
             time = "year",
             spatial = "on",
             spatiotemporal  = "iid")
summary(m1)
sanity(m1)
### Plot marginal effect of depth ####

depth_test <- tibble(X = yearly_newport$X[1],
                     Y =  yearly_newport$Y[1],
                     doy = mean(yearly_newport$doy),
                     year = 2017,
                     depth = seq(30,1300, by = 10))

depth_predict <- predict(m1, newdata = depth_test)

ggplot(depth_predict, aes(x = depth, y = exp(est))) + 
         geom_line()
xrange<- range(yearly_newport$X)
yrange <- c(0.9, 1.1) * yearly_newport$Y[1]

# plot yearly glorys near the newport line
red_glorys <- yearly_glorys %>%
  filter(X >= xrange[1], X <= xrange[2],
         Y >= yrange[1], Y<= yrange[2])

ggplot(red_glorys, aes(x = depth, y = o2, col = doy)) + 
  geom_point()

ggplot(yearly_newport, aes(x = depth, y = o2, col = doy)) + 
  geom_point()
newport_predict <- predict(m1, newdata = yearly_newport)


ggplot(newport_predict, aes(log(o2), est, col = doy)) + 
         geom_point()
       

ggplot(newport_predict, aes(log(o2), est, col = depth)) + 
  geom_point()

