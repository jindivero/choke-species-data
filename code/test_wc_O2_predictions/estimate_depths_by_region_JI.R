install.packages("marmap")
library(marmap)
library(sf)
library(dplyr)
library(sdmTMB)
library(ggplot2)
install.packages("mapview")
library(mapview)

## Oxygen data--get regions
dat <- readRDS("data/processed_data/all_o2_dat.rds")

dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range

##Assign a column to label data in each region
#california current
ca_current <- filter(dat_noiphc, latitude<=50.95)
ca_current$region <- "ca_current"

#british columbia
bc <- filter(dat_noiphc, survey=="LineP"|survey=="dfo")
maxlat <- max(bc$latitude)
minlon <- min(bc$longitude)

bc <- filter(dat_noiphc, (latitude>50.95 & latitude<=maxlat & longitude>=minlon))
bc$region <- "bc"

#check
ggplot(bc, aes(x=longitude, y=latitude))+geom_point(aes(colour=survey))

#gulf of alaska
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

#eastern bering sea + northern bering sea
bs <- filter(dat_noiphc, survey=="EBS"| survey=="NBS"|(survey=="codap" & latitude>=55 & longitude<=-160))
bs$region <- "bs"

#check
ggplot(bs, aes(x=longitude, y=latitude))+geom_point(aes(colour=survey))

#Combine back together
surveys <- bind_rows(ca_current, bc, goa, bs)
#Check that all points were included
nrow(dat_noiphc)
nrow(surveys)

#Create a convex hull polygon for region
polygons <- surveys[,c("longitude", "latitude", "region")]
surveys.sf <- polygons %>%
  st_as_sf( coords = c( "longitude", "latitude" ), crs = 4326 )

surveys.hull <- surveys.sf %>%
  group_by(region) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()

#Check
mapview::mapview(surveys.hull)

survey_names <- surveys.hull$region
surveys.hull <- split(surveys.hull[,2], seq(nrow(surveys.hull)))
names(surveys.hull) <- survey_names

##Or just using the fish catch survey extents
dat <- readRDS("data/processed_data/dat_sablefish.rds")
dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range
#combine EBS and NBS
dat_noiphc$region <- ifelse(dat_noiphc$survey=="EBS"|dat_noiphc$survey=="NBS", "bs", dat_noiphc$survey)
polygons <- dat_noiphc[,c("longitude", "latitude", "region")]
surveys.sf <- polygons %>%
  st_as_sf( coords = c( "longitude", "latitude" ), crs = 4326 )

surveys.hull2 <- surveys.sf %>%
  group_by(region) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()

#Check
mapview::mapview(surveys.hull2)

survey_names <- surveys.hull2$region
surveys.hull2 <- split(surveys.hull2[,2], seq(nrow(surveys.hull2)))
names(surveys.hull2) <- survey_names

##Extract out the baythymetry data
## NOAA bathymetry data
latrange <- range(dat_noiphc$latitude)
lonrange <- range(dat_noiphc$longitude)

noaa_depths <- getNOAA.bathy(lon1 = lonrange[1], 
                             lon2 = lonrange[2],
                             lat1 = latrange[1], 
                             lat2 = latrange[2], 
                             resolution = 4, 
                             keep = TRUE)

depths_sp <- as.SpatialGridDataFrame(noaa_depths)
depths_sf <- st_as_sf(depths_sp, crs = st_crs(4326))
depths_sf <- dplyr::filter(depths_sf, layer <0)

##Extract for each region
#Make a list of regions
#Do for each region

cut_survey <- function(x){
  test <- depths_sf[x,]
}

bathymetry_regions <- lapply(surveys.hull, cut_survey)
bathymetry_regions2 <- lapply(surveys.hull2, cut_survey)

#Max depth in surveys
wcbts <- filter(dat_noiphc, survey=="nwfsc")
maxdepth <-  max(wcbts$depth)
#Convert sf to dataframes
sf_to_dataframe <- function(x){
  # remove depths_sf rows that are above 0 
  tmp <- st_coordinates(x)
  depthlist <- -x$layer
  newdepth <- tibble(longitude = tmp[,1],
                     latitude = tmp[,2],
                     noaadepth = depthlist)
  newdepth <- newdepth %>%
    filter(noaadepth <= 1.1 * maxdepth )
  newdepth <- newdepth %>%
    st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
    mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 
  newdepth <- newdepth %>% 
    group_by(latitude, longitude) %>%
    filter(noaadepth==max(noaadepth)) %>%
    ungroup()
}

bath_surveys<- lapply(bathymetry_regions, sf_to_dataframe)
bath_surveys2 <- lapply(bathymetry_regions2, sf_to_dataframe)

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

