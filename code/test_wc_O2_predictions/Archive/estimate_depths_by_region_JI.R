library(marmap)
library(sf)
library(dplyr)
library(ggplot2)
library(mapview)

#### Oxygen data--get regions
dat <- readRDS("data/processed_data/all_o2_dat.rds")

#Filter out IPHC
dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range

###Assign a column to label data in each region
##california current
ca_current <- filter(dat_noiphc, latitude<=50.95)
ca_current$region <- "ca_current"

##british columbia
bc <- filter(dat_noiphc, survey=="LineP"|survey=="dfo")
maxlat <- max(bc$latitude)
minlon <- min(bc$longitude)
bc <- filter(dat_noiphc, (latitude>50.95 & latitude<=maxlat & longitude>=minlon))
bc$region <- "bc"
#check
ggplot(bc, aes(x=longitude, y=latitude))+geom_point(aes(colour=survey))

##gulf of alaska
goa <- filter(dat_noiphc, survey=="goa")
latrange <- range(goa$latitude)
lonrange <- range(goa$longitude)
goa <- filter(dat_noiphc, (latitude>=latrange[1] & latitude<=61 & longitude>=-170 & longitude<=-132 & survey!="EBS" & survey!="dfo"))
goa$region <- "goa"
#Check
ggplot(goa, aes(x=longitude, y=latitude))+geom_point(aes(colour=survey))
#Remove CODAP bering sea data
goa$remove <- ifelse((goa$survey=="codap" & goa$latitude>55 & goa$longitude < -160), 1, 0)
goa <- filter(goa, remove==0)
goa$remove <- NULL
#Check
ggplot(goa, aes(x=longitude, y=latitude))+geom_point(aes(colour=survey))

##eastern bering sea + northern bering sea
bs <- filter(dat_noiphc, survey=="EBS"| survey=="NBS"|(survey=="codap" & latitude>=55 & longitude<=-160))
bs$region <- "bs"
#check
ggplot(bs, aes(x=longitude, y=latitude))+geom_point(aes(colour=survey))

##Combine back together
surveys <- bind_rows(ca_current, bc, goa, bs)
#Check that all points were included
nrow(dat_noiphc)
nrow(surveys) #few extra in here--some might have gotten included in two regions, which should be fine for polygon creating but need to check for modeling
test <- anti_join(dat_noiphc, surveys) #None were missed

####Create a convex hull polygon for region using the oxygen data
#Separate just columns of interest
polygons <- surveys[,c("longitude", "latitude", "region")]
#Coordinate system
surveys.sf <- polygons %>%
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
dat <- readRDS("data/processed_data/dat_sablefish.rds")
dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range
#combine EBS and NBS
dat_noiphc$region <- ifelse(dat_noiphc$survey=="EBS"|dat_noiphc$survey=="NBS", "bs", dat_noiphc$survey)
#Separate columns needed
polygons <- dat_noiphc[,c("longitude", "latitude", "region")]
#Convert coordinates
surveys.sf <- polygons %>%
  st_as_sf( coords = c( "longitude", "latitude" ), crs = 4326 )
#Geometry and make convext hull polygon
surveys.hull2 <- surveys.sf %>%
  group_by(region) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()

#Check
mapview::mapview(surveys.hull2)

#Get survey names and create list of objects
survey_names <- surveys.hull2$region
surveys.hull2 <- split(surveys.hull2[,2], seq(nrow(surveys.hull2)))
names(surveys.hull2) <- survey_names

####Extract out the baythymetry data that falls within each polygomn
## NOAA bathymetry data
#Ranges of lat and long for clipping
latrange <- range(dat_noiphc$latitude)
lonrange <- range(dat_noiphc$longitude)
#Cut out the big block of data for the area
noaa_depths <- getNOAA.bathy(lon1 = lonrange[1], 
                             lon2 = lonrange[2],
                             lat1 = latrange[1], 
                             lat2 = latrange[2], 
                             resolution = 4, 
                             keep = TRUE)
#Convert to sp, then convert coordinate reference system and make sf object
depths_sp <- as.SpatialGridDataFrame(noaa_depths)
depths_sf <- st_as_sf(depths_sp, crs = st_crs(4326))
#Remove negative depths
depths_sf <- dplyr::filter(depths_sf, layer <0)

##Clip the bathymetry to the survey polygon
#Create a function that does this
cut_survey <- function(x){
  test <- depths_sf[x,]
}

#Run this function for each set of polygons (the o2 and the catch ones)--this creates a list of the clipped sf data
bathymetry_regions_sf <- lapply(surveys.hull, cut_survey)
bathymetry_regions2_sf <- lapply(surveys.hull2, cut_survey)

##Convert these sf lists to dataframes and clean up
#Get max depth in surveys for filtering the bathymetry data
wcbts <- filter(dat_noiphc, survey=="nwfsc")
maxdepth <-  max(wcbts$depth) * 1.1
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
  #Get deepest depths at each coordinate (????)
#  newdepth <- newdepth %>% 
#    group_by(latitude, longitude) %>%
#    filter(noaadepth==max(noaadepth)) %>%
#    ungroup()
}

#Run function on each region for each of the two sets of polygons
bath_surveys<- lapply(bathymetry_regions_sf, sf_to_dataframe)
bath_surveys2 <- lapply(bathymetry_regions2_sf, sf_to_dataframe)

#Save lists of dataframes
saveRDS(bath_surveys, file="data/processed_data/bathymetry_surveys_from_o2.rds")
saveRDS(bath_surveys2, file="data/processed_data/bathymetry_surveys_from_catch.rds")

#Separate out the dataframes for plotting
bath_bc <- bath_surveys[["bc"]]
bath_ca <- bath_surveys[["ca_current"]]
bath_goa <- bath_surveys[["goa"]]
bath_bs <- bath_surveys[["bs"]]

bath_bc2 <- bath_surveys2[["dfo"]]
bath_ca2 <- bath_surveys2[["nwfsc"]]
bath_goa2 <- bath_surveys2[["GOA"]]
bath_bs2 <- bath_surveys2[["bs"]]

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
