setwd("~/Dropbox/choke species/code/choke-species-data/data/survey_shapefiles")

library(ggplot2)
library(sf)
library(raster)


#Alaska
ebs = st_read("alaska_shapefiles/EBSshelf")
nbs = st_read("alaska_shapefiles/NBS")

bs = st_geometry(st_union( ebs, nbs))
bs =st_transform(bs, crs=st_crs("+proj=longlat +datum=WGS84") )

bs_coords <- as.data.frame(st_coordinates(bs))
bs_coords <- bs_coords[,1:2]

write.csv(bs_coords, "bs")

goa = st_read("alaska_shapefiles/GOA")

goa =st_transform(goa, crs=st_crs("+proj=longlat +datum=WGS84") )

plot(st_geometry(bs))
plot(st_geometry(goa))
     
bs_ascii <- st_write(bs, "bs.csv")
write.csv(bs_ascii, file="test.csv")

#West Coast


#BC
load("survey_boundaries.rda")
load("synoptic_grid.rda")


