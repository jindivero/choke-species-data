library(tidync)
library(lubridate)
library(sf)
library(dplyr)
install.packages("sdmTMB", type="source")
library(sdmTMB)
library(seacarb)
library(respR)
library(ggplot2)

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
  m1 <- sdmTMB(formula = log(o2)  ~ 0 +  s(depth) + +s(doy)  ,
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
# Plot observations vs predictions####
newport_predict$year <- as.factor(newport_predict$year)
ggplot(newport_predict, aes(log(o2), est, col = year)) + 
  geom_point() +
  scale_color_viridis_d()

ggplot(newport_predict, aes(o2, exp(est), col = year)) + 
  geom_point() +
  scale_color_viridis_d()

# to do: make area maps of fitted o2

# plot glorys in vicinity
xrange <- range(red_newport$X)
yrange <- c(0.995, 1.005) * red_newport$Y

near_newport <- red_glorys %>%
  filter(X >=- xrange[1],
         X <= xrange[2],
         Y >=- yrange[1],
         Y <= yrange[2],
  )
ggplot(near_newport, aes(y = -depth, x = o2)) +
  geom_point() + 
  xlim(c(0, 200)) +
  ylim(c(-2050,0))
ggplot(red_newport, aes(y = -depth, x = o2))  +
  geom_point() +
  xlim(c(0, 200)) +
  ylim(c(-2050,0))

####Compare to compiled in situ data
##Pull in Patrick Thompson code
insitu <- readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/oxygen options/PotentialDensityData.RDS")
load("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/oxygen options/O2_predictions.RData")
#Filter to years and latitudes used for GLORYS model
o2_thom <- subset(O2_predictions, year>2016 & year<2022)
o2_thom <- subset(o2_thom, latitude<=51)
#Add column for doy

#Convert depth


#Run loop to predict
