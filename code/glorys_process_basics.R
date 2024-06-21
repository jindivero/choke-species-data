library(tidync)
library(ncdf4)
library(lubridate)
library(dplyr)
library(RANN)
library(sf)

###This will combine multiple GLORYS files of different time chunks into a single netcdf file, but need to have the same lat/lon and depth dimensions:
##This requires installing the CDO set of functions, instructions here: https://code.mpimet.mpg.de/projects/cdo/wiki
#List of other CDO functions here: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#setwd for folder where all the files you want to combine are
setwd("")
#Make combine command
#These functions operate as "cdo name_of_function files_to_do_function_on name_of_output_file"
combined <- "cdo mergetime *.nc glorys_tempsal_1993_2021_raw.nc"
system(combined)

###If GLORYS file is already only one file:
##Name the file name
file <- ("glorys_tempsal_1993_2021_raw.nc")

###Getting glorys file metadata
##Look at dimensions to get time and variables
file <- ("glorys_tempsal_1993_2021_raw.nc")
nc_ds <-  ncdf4::nc_open(file)
nc_open(file)
names(nc_ds$dim) #display dimensions
names(nc_ds$var) #display variables

###Function to convert time to correct format
#pull glorys times
dim_time <- ncvar_get(nc_ds, "time")
#Convert to normal day
dim_time <- (as_datetime("1950-01-01 00:00:00")+hours(dim_time))

##General function to convert netcdf into a tidy dataframe
#Need to run tidync on the glorys file
nc <- tidync(file)
#Filter the netcdf by something (e.g. one specific time, by a range of lat/long, by a certain depth, by a range of oxygen values, etc.)
#hyper_filter() function info: https://www.rdocumentation.org/packages/tidync/versions/0.3.0/topics/hyper_filter
timestep <- "2020-06-01"
nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
  hyper_tibble()
#Other examples of how can hyper_filter
nc_df <- nc %>% hyper_filter(latitude=latitude>40, longitude=longitude<-135) %>% 
hyper_tibble()
hyper_filter(time=time>=tstart&time<=tstop,longitude=longitude>-130)
#Etc.
##Once hyper_filter and hyper_tibble, can use normal dplyr functions to filter, mutate, etc. the dataframe
#For example:
nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
  hyper_tibble() %>% 
  # filter for bottom-most depth layer
  filter(depth==depth_haul)

#Take one time step, group by long/lat, and summarize depths or variables at each coordinate:
nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
  hyper_tibble() %>% 
  group_by(longitude,latitude) %>% 
  summarise(bot=last(depth)) %>% 
  ungroup()

#Create a function to do some kind of action (summary, filter depth, etc.)
#For instance, extract the bottom depth by creating a function that feeds a list of dates, then for each day groups the lon/lat and gets the deepest depth available
extract_bottom_vars <- function(tstep){
  nc_df <- nc %>% hyper_filter(time=time==tstep) %>% 
    hyper_tibble() %>% 
    # filter for bottom-most depth layer
    group_by(longitude,latitude) %>% 
    filter(depth==max(depth))
  nc_df
}


#####################################################################
#Create a function to feed it a list of coordinates and extract out very specific GLORYS points
#######Function to extract a date, lat, lon,and depth from GLORYS (can use in lapply, purrr, or loop)
#Note: hyper_filter filters from the netcdf file itself
#Function
extract_haul <- function(tstep, depth_haul, lon_haul, lat_haul){
  nc_df <- nc %>% hyper_filter(time=(time==tstep), depth=(depth==depth_haul), longitude=(longitude==lon_haul)) %>% 
    hyper_tibble() %>% 
    #This line helps if there is not an exact lat drown
    filter(abs(latitude - lat_haul) == min(abs(latitude - lat_haul)))
  nc_df
}

#Get list of the GLORYS data coordinates, dates, and depths
dim_lon <- ncvar_get(nc_ds, "longitude")
dim_lat <- ncvar_get(nc_ds, "latitude")
dim_time <- ncvar_get(nc_ds, "time")
depth <- ncvar_get(nc_ds, "depth")

#Convert time format to day format
dim_time2 <- (as_datetime("1950-01-01 00:00:00")+hours(dim_time))

#List dates that you want to extract (either from a set of trawl data, or just construct)
time_haul <- as.POSIXct(haul_combined$date) #e.g. using haul data

#Get index of date of GLORYS date that matches to each haul (because GLORYS uses the weird time format)
date_index <- matrix(ncol=1, nrow=length(time_haul))
for(i in 1:length(time_haul)){
  date_index[i] <-which(abs(dim_time2-time_haul[i]) == min(abs(dim_time2-time_haul[i])))
}

#Pull those GLORYS times
tstep <- dim_time[c(date_index)]

#Pull depths 
#Depths wanted
haul_depths <- haul_combined$depth_m
#Create list of depths and match depth of haul to depth of closest GLORYS depth level
depth_index <- matrix(ncol=1, nrow=length(haul_depths))
haul_depths <- haul_combined$depth_m
for(i in 1:length(haul_depths)){
  depth_index[i] <-which(abs(depth-haul_depths[i]) == min(abs(depth-haul_depths[i])))
}
#Extract out list of GLORYS depths
glorys_depth <- depth[c(depth_index)]

#Make sure coordinates are in the right format
#Combine lat and lon into one dataframe by expanding (b/c GLORYS just lists a list of lats and a list of lons, not coordinate pairs)
coords <- expand.grid(dim_lon, dim_lat)
colnames(coords) <- c("longitude", "latitude")

#Transform to UTM
coords2<- coords %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(gridID_phy=row_number()) %>% 
  mutate(lonUTM=st_coordinates(.)[,1],latUTM=st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

#Transform haul coordinates to UTM
trawl_locs <- haul_combined[,c("lon_start", "lat_start")] %>% 
  # convert to UTM10
  st_as_sf(coords=c('lon_start','lat_start'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
  mutate(lontrawlUTM=st_coordinates(.)[,1],lattrawlUTM=st_coordinates(.)[,2])%>% 
  st_set_geometry(NULL)

##Match closest points
#Both types of coordinate systems get the same answers! So pick whichever
glorys_coords <- RANN::nn2(coords2[, c('lonUTM', 'latUTM')], trawl_locs[, c('lontrawlUTM', "lattrawlUTM")],k = 1)
glorys_coords2 <- RANN::nn2(coords2[, c('longitude', 'latitude')], trawl_locs[, c('lon_start', "lat_start")],k = 1)

#Extract out the lats and lons in the GLORYS reference system
points <- coords2[c(glorys_coords2$nn.idx),c("longitude", "latitude")]
haul_lon <- points$longitude
haul_lat <- points$latitude

#Extract out the points that match date, depth, lon, and lat from the full GLORYS file
#First tidy the file
nc <- tidync(file)

#Looping through each set of points
glorys_dat <- list()
for(i in 1:length(time_haul)){
  message(i, Sys.time() )
  x <- extract_haul(tstep[i], glorys_depth[i], haul_lon[i], haul_lat[i])
  glorys_dat[[i]] <- x
}

#Convert time to calendar day in correct format
glorys$time <- (as_datetime("1950-01-01")+hours(glorys$time))
glorys$time <- as.character(as.Date(glorys$time, format='%m/%d/%Y'))
#####################################################################