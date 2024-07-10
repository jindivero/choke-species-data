# load packages
library(ncdf4)
library(chron)
library(cmocean)
library(oce)
library(tidyverse)

#load bathymetry data
bathymetry <- read_csv('newport_hydrographic_line_bathymetry.csv',show_col_types = FALSE)

#load climatology data
nc_data <- nc_open('newport_hydrographic_line_gridded_section_climatologies.nc')

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
fillvalue <- ncatt_get(nc_data, "temperature", "_FillValue")
temperature[temperature==fillvalue$value] <- NA

#close NetCDF file
nc_close(nc_data)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
time <- chron(time,origin=c(tmonth, tday, tyear))

#extract July climatology
m <- 7
temperature_slice <- t(temperature[m,,])

#plot July climatology
imagep(longitude, pressure, temperature_slice, col=cmocean('thermal'), breaks=seq(7, 14, .1), 
       flipy=TRUE,  xlab="Longitude (°W)",  ylab="Pressure (dbar)", zlab=format(time[m], "%Y-%m-%dT%H:%M:%S"),
       xlim=c(-124.65, -124.1))
par(new=T)
matplot(bathymetry[, 1]-360, bathymetry[, 2], type="l", xlab = '', xaxt='n', ylab = '', yaxt='n',
        xlim=c(-124.65, -124.12), ylim=c(-150,0))
