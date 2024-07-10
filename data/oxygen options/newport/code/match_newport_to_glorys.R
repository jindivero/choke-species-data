install.packages("seacarb")
library(seacarb)
library(tidync)
install.packages("respR")
library(respR)
library(dplyr)
library(lubridate)
library(sf)
library(ncdf4.helpers)
library(ncdf4)
library(ggplot2)

#extract specific value
extract_haul <- function(tstep, depth_haul, lon_haul, lat_haul){
  nc_df <- nc %>% hyper_filter(time=(time==tstep), depth=(depth==depth_haul), longitude=(longitude==lon_haul)) %>% 
    hyper_tibble() %>% 
    filter(abs(latitude - lat_haul) == min(abs(latitude - lat_haul)))
  nc_df
}

basewd <- "/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"
### Single GLORYS year file
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/GLORYS/wc_o2/for_tim")
file <- "cmems_mod_glo_bgc_my_0.25deg_P1D-m_o2_138.00W-114.00W_26.00N-51.00N_0.51-1516.36m_2019-05-01-2019-10-29.nc"

### 3) Convert GLORYS raw file to a dataframe, use only depths > 30 meters
nc <- tidync(file)
nc_df <- nc %>%
  hyper_tibble %>%
  group_by(longitude, latitude) %>%
  filter(depth == max(depth)) %>%
  ungroup()
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

# extract year and samples within time frame of model
yearly_newport <- newport_data %>%
  filter(year == 2019,
         doy >= min(nc_df$doy),
         doy <= max(nc_df$doy)
  )

setwd(basewd)
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

##Look at dimensions to get time and variables
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/GLORYS/wc_o2/for_tim")
nc_ds <-  ncdf4::nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

#Get list of the GLORYS data coordinates, dates, and depths
dim_lon <- ncvar_get(nc_ds, "longitude")
dim_lat <- ncvar_get(nc_ds, "latitude")
dim_time <- ncvar_get(nc_ds, "time")
depth <- ncvar_get(nc_ds, "depth")

#Convert time format to day format
dim_time2 <- (as_datetime("1950-01-01 00:00:00")+hours(dim_time))

#List dates of hauls
time_haul <- as.POSIXct(yearly_newport$sample_date)

#Get index of date that matches to each haul
date_index <- matrix(ncol=1, nrow=length(time_haul))
for(i in 1:length(time_haul)){
  date_index[i] <-which(abs(dim_time2-time_haul[i]) == min(abs(dim_time2-time_haul[i])))
}

#Pull the GLORYS time unit
tstep <- dim_time[c(date_index)]

#Depth
haul_depths <- yearly_newport$depth
depth_index <- matrix(ncol=1, nrow=length(haul_depths))
for(i in 1:length(haul_depths)){
  depth_index[i] <-which(abs(depth-haul_depths[i]) == min(abs(depth-haul_depths[i])))
}

glorys_depth <- depth[c(depth_index)]

#Make sure coordinates are in the right format
#Combine lat and lon into 
coords <- expand.grid(dim_lon, dim_lat)
colnames(coords) <- c("longitude", "latitude")

#Transform to UTM
coords2<- coords %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(lonUTM=st_coordinates(.)[,1],latUTM=st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

#Match closest points
#Both types of coordinate systems get the same answers! Amazing
glorys_coords <- RANN::nn2(coords2[, c('lonUTM', 'latUTM')], yearly_newport[, c('X', "Y")],k = 1)

#Extract out the lats and lons in the GLORYS reference system
points <- coords[c(glorys_coords$nn.idx),c("longitude", "latitude")]
haul_lon <- points$longitude
haul_lat <- points$latitude

#Extract out the points that match date, depth, lon, and lat from the full GLORYS file
#First tidy the file
nc <- tidync(file)

#Looping through each set of points
glorys_dat <- list()
for(i in 1:length(time_haul)){
  print(i)
  x <- extract_haul(tstep[i], glorys_depth[i], haul_lon[i], haul_lat[i])
  glorys_dat[[i]] <- x
}

#Average if multiple rows (with multiple latitudes)
glorys_dat2 <- list()
for(i in 1:length(glorys_dat)){
  if(nrow(glorys_dat[[i]])>1) {
    x <- glorys_dat[[i]]%>%summarise_all(funs(mean))
    glorys_dat2[[i]] <- x
  }
  if(nrow(glorys_dat[[i]])==1){
    x <- glorys_dat[[i]]
    glorys_dat2[[i]] <- x
  }
}

#Bind into one dataframe
glorys <- bind_rows(glorys_dat2)

#Convert time into correct date format
glorys$time <- (as_datetime("1950-01-01")+hours(glorys$time))
glorys$time <- as.character(as.Date(glorys$time, format='%m/%d/%Y'))

#Add Newport data back
glorys$newport <- yearly_newport$o2_fun
glorys$newport_depth <- yearly_newport$depth
glorys$depth_diff <- glorys$depth - glorys$newport_depth

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Plot
ggplot(glorys, aes(x=newport, y=o2))+geom_point(aes(color=depth), size=5)+ylab("Glorys")


