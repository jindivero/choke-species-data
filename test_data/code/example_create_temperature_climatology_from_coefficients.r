# load packages
library(ncdf4)
library(chron)
library(cmocean)
library(oce)
library(tidyverse)

#load bathymetry data
bathymetry <- read_csv('newport_hydrographic_line_bathymetry.csv',show_col_types = FALSE)

#load climatology data
nc_data <- nc_open('newport_hydrographic_line_gridded_section_coefficients.nc')

# get pressure
pressure <- ncvar_get(nc_data, "pressure")
# get latitude
latitude <- ncvar_get(nc_data, "latitude")
# get longitude
longitude <- ncvar_get(nc_data, "longitude")-360
# get temperature beta_coefficients
tc <- ncvar_get(nc_data, "temperature_coefficients")
fillvalue <- ncatt_get(nc_data, "temperature_coefficients", "_FillValue")
tc[tc==fillvalue$value] <- NA

#close NetCDF file
nc_close(nc_data)

#Calculate daily temperature climatology: Jan 1 - Dec 31
f=1/365.2422
new_t=0:364
seasonal_cycle_temperature_daily <- array(NA, dim = c(150,56,365))

for (ii in 1:150) {
  for (jj in 1:56) {
    seasonal_cycle_temperature_daily[ii,jj,] <- (tc[1,ii,jj]+tc[2,ii,jj]*sin(2*pi*f*new_t)+
                                                   tc[3,ii,jj]*cos(2*pi*f*new_t)+
                                                   tc[4,ii,jj]*sin(4*pi*f*new_t)+
                                                   tc[5,ii,jj]*cos(4*pi*f*new_t)+
                                                   tc[6,ii,jj]*sin(6*pi*f*new_t)+
                                                   tc[7,ii,jj]*cos(6*pi*f*new_t))
  }
}

#Calculate monthly mean for January
January <- seasonal_cycle_temperature_daily[,,1:31]
January <- t(apply(January, c(1,2), mean))

#plot January climatology
imagep(longitude, pressure, January, col=cmocean('thermal'), breaks=seq(7, 14, .1), 
       flipy=TRUE,  xlab="Longitude (°W)",  ylab="Pressure (dbar)", zlab="January",
       xlim=c(-124.65, -124.1))
par(new=T)
matplot(bathymetry[, 1]-360, bathymetry[, 2], type="l", xlab = '', xaxt='n', ylab = '', yaxt='n',
        xlim=c(-124.65, -124.12), ylim=c(-150,0))
