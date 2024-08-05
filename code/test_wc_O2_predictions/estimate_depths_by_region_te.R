library(marmap)
library(sf)
library(dplyr)
library(ggplot2)
library(mapview)
library(devtools)
library(FishStatsUtils)

#### Oxygen data--get regions
dat <- readRDS("data/processed_data/all_o2_dat.rds")

#Filter out IPHC
dat_noiphc <- dplyr::filter(dat, !survey == "iphc") # because this crosses over into Long E, creates problems with longitude range

latrange <- range(dat_noiphc$latitude)
lonrange <- range(dat_noiphc$longitude)

latrange <- range(dat$latitude)
lonrange <- range(dat$longitude)

# get noaa bathymetry data
noaa_depths <- getNOAA.bathy(lon1 = -180, 
                             lon2 = 180,
                             lat1 = latrange[1], 
                             lat2 = latrange[2], 
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


# combined EBS and NBS
data("eastern_bering_sea_grid")
ebs <- as.data.frame(eastern_bering_sea_grid)

data("northern_bering_sea_grid")
nbs <- as.data.frame(northern_bering_sea_grid)
ebs <- bind_rows(ebs, nbs)

ebs$region <- "ebs"

# Aleutian Islands
data("aleutian_islands_grid")
ai <- as.data.frame(aleutian_islands_grid)
ai$region <- "ai"

##Combine back together
regions <- bind_rows(cc, bc, goa, ebs, ai)

#Check that all points were included

regions <- dplyr::rename(regions, longitude = Lon, latitude = Lat)

####Create a convex hull polygon for region using the oxygen data
#Separate just columns of interest
polygons_grid <- as.data.frame(regions[,c("longitude", "latitude", "region")])
#Coordinate system
regions.sf <- polygons_grid %>%
  st_as_sf( coords = c( "longitude", "latitude" ), crs = 4326 )
#For each region, create a geometry and then convex hull polygon
regions.hull <- regions.sf %>%
  group_by(region) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()

#Check
mapview::mapview(regions.hull)

#Get survey names, separate into a list so lapply is easier to use
region_names <- regions.hull$region
regions.hull <- split(regions.hull[,2], seq(nrow(regions.hull)))
names(regions.hull) <- region_names

##Clip the bathymetry to the survey polygon
#Create a function that does this
cut_region <- function(x){
  test <- depths_sf[x,]
}

#Run this function for each set of polygons (the o2 and the catch ones)--this creates a list of the clipped sf data
bathymetry_regions_sf <- lapply(regions.hull, cut_region)

##Convert these sf lists to dataframes and clean up
#Get max depth in surveys for filtering the bathymetry data
trawl_surveys <- filter(dat_noiphc, survey %in% c("nwfsc", "dfo", "goa", "EBS"))
maxdepth <-  max(trawl_surveys$depth)*1.1

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
bath_regions<- lapply(bathymetry_regions_sf, sf_to_dataframe)

#Save lists of dataframes
saveRDS(bath_regions, file="data/processed_data/bathymetry_regions_from_grids.rds")

#Separate out the dataframes for plotting
bath_bc <- bath_regions[["bc"]]
bath_cc <- bath_regions[["cc"]]
bath_goa <- bath_regions[["goa"]]
bath_ebs <- bath_regions[["ebs"]]
bath_ai <- bath_regions[["ai"]]

#check
mapview::mapview(bath_bc)
mapview::mapview(bath_cc)
mapview::mapview(bath_goa)
mapview::mapview(bath_ebs)
mapview::mapview(bath_ai)

#Re-combine into one dataframe with columns for region for more plotting
bath_bc$region <- "bc"
bath_cc$region <- "cc"
bath_goa$region <- "goa"
bath_ebs$region <- "ebs"
bath_ai$region <- "ai"
bath_all <- as.data.frame(bind_rows(bath_bc, bath_cc, bath_goa, bath_ebs, bath_ai))

saveRDS(bath_all, file="data/processed_data/bathymetry_regions_dataframe.rds")

ggplot(bath_all, aes(x=longitude, y=latitude))+geom_point(aes(colour=region), size=0.2)

mapview(bath_all, zcol="region", cex=1, lwd=0.1)

#Do the Aleutian Islands work with longitude spanning W and E?
library(sdmTMB)

bath_ai<- as.data.frame(bath_ai)
mesh <- make_mesh(bath_ai, xy_cols = c("X", "Y"), n_knots=250)
plot(mesh)
ggplot(test, aes(x=X, y=Y))+geom_point(aes(colour=longitude))

