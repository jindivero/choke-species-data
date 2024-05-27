setwd("~/Dropbox/choke species/code/choke-species-data/data/survey_shapefiles")

library(ggplot2)
library(sf)


#Alaska
ebs = st_read("alaska_shapefiles/EBSshelf")
nbs = st_read("alaska_shapefiles/NBS")
goa = st_read("alaska_shapefiles/GOA")
bs = st_geometry(st_union( ebs, nbs))
bs =st_transform(bs, crs=st_crs("+proj=longlat +datum=WGS84") )
goa =st_transform(goa, crs=st_crs("+proj=longlat +datum=WGS84") )

plot(st_geometry(bs))
plot(st_geometry(goa))
     
writeRaster(format="ascii")

#West Coast


#BC
load("survey_boundaries.rda")
load("synoptic_grid.rda")

