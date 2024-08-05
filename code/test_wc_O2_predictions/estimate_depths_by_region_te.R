library(marmap)
library(sf)
library(dplyr)
library(ggplot2)
library(mapview)
library(FishStatsUtils)


#### Oxygen data--get regions
dat <- readRDS("data/processed_data/all_o2_dat.rds")

#Filter out IPHC
dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range

latrange <- range(dat_noiphc$latitude)
lonrange <- range(dat_noiphc$longitude)

# get noaa bathymetry data
noaa_depths <- getNOAA.bathy(lon1 = lonrange[1], 
                             lon2 = -117,
                             lat1 = latrange[1], 
                             lat2 = 32, 
                             resolution = 4, 
                             keep = TRUE)
depths_sp <- as.SpatialGridDataFrame(noaa_depths)
depths_sf <- st_as_sf(depths_sp, crs = st_crs(4326))
depths_sf <- dplyr::filter(depths_sf, layer <0)



###Assign a column to label data in each region
##california current
data("california_current_grid")
cc <- california_current_grid
cc$region <- "cc"

data("bc_coast_grid")

bc <- bc_coast_grid
bc$region <- "bc"

data("gulf_of_alaska_grid")

goa <- gulf_of_alaska_grid
goa$region <- "goa"


data("eastern_bering_sea_grid")
ebs <- as.data.frame(eastern_bering_sea_grid)
ebs$region <- "ebs"

data("northern_bering_sea_grid")
nbs <- as.data.frame(northern_bering_sea_grid)
nbs$region <- "nbs"


##Combine back together
surveys <- bind_rows(cc, bc, goa, ebs, nbs)

#Check that all points were included

surveys <- dplyr::rename(surveys, longitude = Lon, latitude = Lat)

####Create a convex hull polygon for region using the oxygen data
#Separate just columns of interest
polygons_grid <- surveys[,c("longitude", "latitude", "region")]
#Coordinate system
surveys.sf <- polygons_grid %>%
  st_as_sf( coords = c( "longitude", "latitude" ), crs = 4326 )
#For each region, create a geometry and then convex hull polygon
surveys.hull <- surveys.sf %>%
  group_by(region) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()

#Check
mapview::mapview(surveys.hull)

#Get survey names, separate into a list so lapply is easier to use
survey_names <- surveys.hull$region
surveys.hull <- split(surveys.hull[,2], seq(nrow(surveys.hull)))
names(surveys.hull) <- survey_names

####Create convex hull polygon for each region using the fish catch survey extents
#dat <- readRDS("data/processed_data/dat_sablefish.rds")
#dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range
#combine EBS and NBS
#dat_noiphc$region <- ifelse(dat_noiphc$survey=="EBS"|dat_noiphc$survey=="NBS", "bs", dat_noiphc$survey)
#Separate columns needed
#polygons <- dat_iphc[,c("longitude", "latitude", "region")]
#Convert coordinates
#surveys.sf <- polygons %>%
#  st_as_sf( coords = c( "longitude", "latitude" ), crs = 4326 )
#Geometry and make convext hull polygon
#surveys.hull2 <- surveys.sf %>%
#  group_by(region) %>%
#  summarise( geometry = st_combine( geometry ) ) %>%
#  st_convex_hull()

#Check
#mapview::mapview(surveys.hull2)

#Get survey names and create list of objects
#survey_names <- surveys.hull2$region
#surveys.hull2 <- split(surveys.hull2[,2], seq(nrow(surveys.hull2)))
#names(surveys.hull2) <- survey_names

#Convert to sp, then convert coordinate reference system and make sf object

##Clip the bathymetry to the survey polygon
#Create a function that does this
cut_survey <- function(x){
  test <- depths_sf[x,]
}

#Run this function for each set of polygons (the o2 and the catch ones)--this creates a list of the clipped sf data
bathymetry_regions_sf <- lapply(surveys.hull, cut_survey)


##Convert these sf lists to dataframes and clean up
#Get max depth in surveys for filtering the bathymetry data
wcbts <- filter(dat_noiphc, survey=="nwfsc")
maxdepth <-  max(wcbts$depth)*1.1
#Create a function to convert the clipped sf lists above to dataframes
sf_to_dataframe <- function(x){
  #Get coordinates and depth
  tmp <- st_coordinates(x)
  depthlist <- -x$layer
  #Create tibble
  newdepth <- tibble(longitude = tmp[,1],
                     latitude = tmp[,2],
                     noaadepth = depthlist)
  #Filter the depth to the depths of the data
  newdepth <- newdepth %>%
    filter(noaadepth <= 1.1 * maxdepth )
  #Convert the coordinate reference system to the X and Y that we use
  newdepth <- newdepth %>%
    st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
    mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 
}

#Run function on each region for each of the two sets of polygons
bath_surveys<- lapply(bathymetry_regions_sf, sf_to_dataframe)


#Save lists of dataframes
saveRDS(bath_surveys, file="data/processed_data/bathymetry_surveys_from_o2.rds")
saveRDS(bath_surveys2, file="data/processed_data/bathymetry_surveys_from_catch.rds")

#Separate out the dataframes for plotting
bath_bc <- bath_surveys[["bc"]]
bath_ca <- bath_surveys[["ca_current"]]
bath_goa <- bath_surveys[["goa"]]
bath_bs <- bath_surveys[["bs"]]

#bath_bc2 <- bath_surveys2[["dfo"]]
#bath_ca2 <- bath_surveys2[["nwfsc"]]
#bath_goa2 <- bath_surveys2[["GOA"]]
#bath_bs2 <- bath_surveys2[["bs"]]

#check
mapview::mapview(bath_bc)
mapview::mapview(bath_ca)
mapview::mapview(bath_goa)
mapview::mapview(bath_bs)

mapview::mapview(bath_bc2)
mapview::mapview(bath_ca2)
mapview::mapview(bath_goa2)
mapview::mapview(bath_bs2)

#Re-combine into one dataframe with columns for region for more plotting
bath_bc2$region <- "bc"
bath_ca2$region <- "ca_current"
bath_goa2$region <- "goa"
bath_bs2$region <- "bs"
bath_all <- bind_rows(bath_bc2, bath_ca2, bath_goa2, bath_bs2)

ggplot(bath_all, aes(x=longitude, y=latitude))+geom_point(aes(colour=region), size=0.2)

mapview(bath_all, zcol="region", cex=1, lwd=0.1)
