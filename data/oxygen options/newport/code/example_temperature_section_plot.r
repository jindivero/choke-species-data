# load packages
library(ncdf4)
library(chron)
library(cmocean)
library(oce)
library(tidyverse)
library(respR)

#load bathymetry data
bathymetry <- read_csv('test_data/data/newport_hydrographic_line_bathymetry.csv',show_col_types = FALSE)

#load climatology data
nc_data <- nc_open('test_data/data/newport_hydrographic_line_gridded_sections.nc')

# get time
time <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data,"time","units")
# get pressure
pressure <- ncvar_get(nc_data, "pressure")
# get latitude
latitude <- ncvar_get(nc_data, "latitude")
# get longitude
longitude <- ncvar_get(nc_data, "longitude")-360
# get temperature
temperature <- ncvar_get(nc_data, "temperature")
salinity <- ncvar_get(nc_data, "salinity")
oxygen <- ncvar_get(nc_data, "dissolved_oxygen")
fillvalue <- ncatt_get(nc_data, "temperature", "_FillValue")
o2fillvalue <- ncatt_get(nc_data, "dissolved_oxygen", "_FillValue")
salfillvalue <- ncatt_get(nc_data, "salinity", "_FillValue")
temperature[temperature==fillvalue$value] <- NA
oxygen[oxygen==o2fillvalue$value] <- NA
salinity[salinity==salfillvalue$value] <- NA

nc_close(nc_data)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
time <- chron(time,origin=c(tmonth, tday, tyear))

as.POSIXlt(time, format = "%Y-%b-%d")$yday

#extract July 8, 2021 transect
d <- 556
temperature_slice <- t(temperature[d,,])
oxygen_slice <- t(oxygen[d,,])
salinity_slice <- t(salinity[d,,])


#plot July 8, 2021 transect
par(mfrow=c(2,2))

imagep(longitude, pressure, temperature_slice, col=cmocean('thermal'), breaks=seq(7, 14, .1), 
       flipy=TRUE,  xlab="Longitude (?W)",  ylab="Pressure (dbar)", zlab=format(time[d], "%Y-%m-%dT%H:%M:%S"),
       xlim=c(-124.65, -124.1))
par(new=T)
matplot(bathymetry[, 1]-360, bathymetry[, 2], type="l", xlab = '', xaxt='n', ylab = '', yaxt='n',
        xlim=c(-124.65, -124.12), ylim=c(-150,0))

imagep(longitude, pressure, oxygen_slice, col=cmocean('thermal'), breaks=seq(0, 9, .1), 
       flipy=TRUE,  xlab="Longitude (?W)",  ylab="Pressure (dbar)", zlab=format(time[d], "%Y-%m-%dT%H:%M:%S"),
       xlim=c(-124.65, -124.1))

par(new=T)
matplot(bathymetry[, 1]-360, bathymetry[, 2], type="l", xlab = '', xaxt='n', ylab = '', yaxt='n',
        xlim=c(-124.65, -124.12), ylim=c(-150,0))

imagep(longitude, pressure, salinity_slice, col=cmocean('thermal'), breaks=seq(30, 35, .5), 
       flipy=TRUE,  xlab="Longitude (?W)",  ylab="Pressure (dbar)", zlab=format(time[d], "%Y-%m-%dT%H:%M:%S"),
       xlim=c(-124.65, -124.1))
par(new=T)
matplot(bathymetry[, 1]-360, bathymetry[, 2], type="l", xlab = '', xaxt='n', ylab = '', yaxt='n',
        xlim=c(-124.65, -124.12), ylim=c(-150,0))


## Repeat for 2020
#extract July 24, 2020 transect
d <- 539
temperature_slice <- t(temperature[d,,])
oxygen_slice <- t(oxygen[d,,])
salinity_slice <- t(salinity[d,,])

#plot  transect
par(mfrow=c(2,2))

imagep(longitude, pressure, temperature_slice, col=cmocean('thermal'), breaks=seq(7, 14, .1), 
       flipy=TRUE,  xlab="Longitude (?W)",  ylab="Pressure (dbar)", zlab=format(time[d], "%Y-%m-%dT%H:%M:%S"),
       xlim=c(-124.65, -124.1))
par(new=T)
matplot(bathymetry[, 1]-360, bathymetry[, 2], type="l", xlab = '', xaxt='n', ylab = '', yaxt='n',
        xlim=c(-124.65, -124.12), ylim=c(-150,0))

imagep(longitude, pressure, oxygen_slice, col=cmocean('thermal'), breaks=seq(0, 9, .1), 
       flipy=TRUE,  xlab="Longitude (?W)",  ylab="Pressure (dbar)", zlab=format(time[d], "%Y-%m-%dT%H:%M:%S"),
       xlim=c(-124.65, -124.1))

par(new=T)
matplot(bathymetry[, 1]-360, bathymetry[, 2], type="l", xlab = '', xaxt='n', ylab = '', yaxt='n',
        xlim=c(-124.65, -124.12), ylim=c(-150,0))

imagep(longitude, pressure, salinity_slice, col=cmocean('thermal'), breaks=seq(30, 35, .5), 
       flipy=TRUE,  xlab="Longitude (?W)",  ylab="Pressure (dbar)", zlab=format(time[d], "%Y-%m-%dT%H:%M:%S"),
       xlim=c(-124.65, -124.1))
par(new=T)
matplot(bathymetry[, 1]-360, bathymetry[, 2], type="l", xlab = '', xaxt='n', ylab = '', yaxt='n',
        xlim=c(-124.65, -124.12), ylim=c(-150,0))
