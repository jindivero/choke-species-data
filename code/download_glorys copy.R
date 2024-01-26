install.packages("CopernicusMarine")
library(CopernicusMarine)
install.packages("lubridate")
library(lubridate)

# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description
# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_BGC_001_029/description

##Alaska
download <- function(start, end){
copernicus_download_motu(
  username = "jindivero",
  password = "MaRcH131995**",
  destination= "~/Dropbox/Mac (2)/Downloads",
  product= "GLOBAL_MULTIYEAR_BGC_001_029",
  layer= "cmems_mod_glo_bgc_my_0.25_P1D-m",
  variable="po2",
  output="netcdf",
  region=c(-179.6, 51,-133, 65),
  timerange=c(start, end),
  verticalrange=c(9,1300),
  overwrite = FALSE
)
}

#Create list of start and end dates by week
starts <- seq(ymd(19930501), ymd(20200501), by="1 year")
ends <- seq(ymd(19930930), ymd(20200930), by="1 year")

starts <- seq(ymd(20160501), ymd(20200501), by="1 year")
ends <- seq(ymd(20160930), ymd(20200930), by="1 year")

#Apply list of dates 
mapply(download, starts, ends)

##Northwest
download2 <- function(start, end){
  copernicus_download_motu(
    username = "jindivero",
    password = "MaRcH131995**",
    destination= "~/Dropbox/Mac (2)/Downloads",
    product= "GLOBAL_MULTIYEAR_BGC_001_029",
    layer= "cmems_mod_glo_bgc_my_0.25_P1D-m",
    variable="po2",
    output="netcdf",
    region=c(-125, 31, -117, 48),
    timerange=c(start, end),
    verticalrange=c(9,1300),
    overwrite = FALSE
  )
}

#Create list of start and end dates by week
starts2 <- seq(ymd(20010501), ymd(20200501), by="1 year")
ends2 <- seq(ymd(20011031), ymd(20201031), by="1 year")

#Apply list of dates 
mapply(download2, starts2, ends2)

###Alaska extra
download3 <- function(start, end){
  copernicus_download_motu(
    username = "jindivero",
    password = "MaRcH131995**",
    destination= "~/Dropbox/Mac (2)/Downloads",
    product= "GLOBAL_MULTIYEAR_BGC_001_029",
    layer= "cmems_mod_glo_bgc_my_0.25_P1D-m",
    variable="po2",
    output="netcdf",
    region=c(173, 51,179, 65),
    timerange=c(start, end),
    verticalrange=c(9,1300),
    overwrite = FALSE
  )
}

#Create list of start and end dates by week
starts3 <- seq(ymd(19930501), ymd(20200501), by="1 year")
ends3 <- seq(ymd(19930831), ymd(20200831), by="1 year")

#Apply list of dates 
mapply(download3, starts3, ends3)

###BC
download4 <- function(start, end){
  copernicus_download_motu(
    username = "jindivero",
    password = "MaRcH131995**",
    destination= "~/Dropbox/Mac (2)/Downloads",
    product= "GLOBAL_MULTIYEAR_BGC_001_029",
    layer= "cmems_mod_glo_bgc_my_0.25_P1D-m",
    variable="po2",
    output="netcdf",
    region=c(-134, 48,-124, 54),
    timerange=c(start, end),
    verticalrange=c(9,1300),
    overwrite = FALSE
  )
}

#Create list of start and end dates by week
starts4 <- seq(ymd(19930501), ymd(20200501), by="1 year")
ends4 <- seq(ymd(19930930), ymd(20200930), by="1 year")

#Apply list of dates 
mapply(download4, starts4, ends4)


###Temperature data
download5 <- function(start, end){
  copernicus_download_motu(
    username = "jindivero",
    password = "MaRcH131995**",
    destination= "~/Dropbox/Mac (2)/Downloads",
    product= "GLOBAL_MULTIYEAR_PHY_001_030",
    layer= "cmems_mod_glo_phy_my_0.083_P1D-m",
    variable="thetao",
    output="netcdf",
    region=c(-179.6, 51,-133, 65),
    sub_variables="so",
    timerange=c(start, end),
    verticalrange=c(9,1300),
    overwrite = FALSE
  )
}

#Create list of start and end dates by week
starts5 <- seq(ymd(19930501), ymd(20200501), by="1 year")
ends5 <- seq(ymd(19930630), ymd(20200630), by="1 year")

#Apply list of dates 
mapply(download5, starts5, ends5)

starts5a <- seq(ymd(20090701), ymd(20200701), by="1 year")
ends5a <- seq(ymd(20090831), ymd(20200831), by="1 year")

starts5b <- seq(ymd(19930901), ymd(20200901), by="1 year")
ends5b <- seq(ymd(19930930), ymd(20200930), by="1 year")

starts5 <- append(starts5a, starts5a)
ends5 <- append(ends5a, ends5b)

mapply(download5, starts5b, ends5b)

##Northwest
download6 <- function(start, end){
  copernicus_download_motu(
    username = "jindivero",
    password = "MaRcH131995**",
    destination= "~/Dropbox/Mac (2)/Downloads",
    product= "GLOBAL_MULTIYEAR_PHY_001_030",
    layer= "cmems_mod_glo_phy_my_0.083_P1D-m",
    variable=c("so", "thetao"),
    output="netcdf",
    region=c(-125, 31, -117, 48),
    timerange=c(start, end),
    verticalrange=c(9,1300),
    overwrite = FALSE
  )
}

#Create list of start and end dates by year and combine
starts6 <- seq(ymd(19930501), ymd(20200501), by="1 year")
ends6 <- seq(ymd(19930630), ymd(20200630), by="1 year")

starts6a <- seq(ymd(20090701), ymd(20200701), by="1 year")
ends6a <- seq(ymd(20090831), ymd(20200831), by="1 year")

starts6b <- seq(ymd(19930901), ymd(20200901), by="1 year")
ends6b <- seq(ymd(19930930), ymd(20200930), by="1 year")

starts6 <- append(starts6, starts6a)
starts6 <- append(starts6, starts6b)
ends6 <- append(ends6, ends6a)
ends6 <- append(ends6, ends6b)

#Apply list of dates 
mapply(download6, starts6, ends6)


###Alaska extra
download7 <- function(start, end){
  copernicus_download_motu(
    username = "jindivero",
    password = "MaRcH131995**",
    destination= "~/Dropbox/Mac (2)/Downloads",
    product= "GLOBAL_MULTIYEAR_PHY_001_030",
    layer= "cmems_mod_glo_phy_my_0.083_P1D-m",
    variable="po2",
    output="netcdf",
    region=c(173, 51,179, 65),
    timerange=c(start, end),
    verticalrange=c(9,1300),
    overwrite = FALSE
  )
}

mapply(download7, starts6, ends6)

###BC
download8 <- function(start, end){
  copernicus_download_motu(
    username = "jindivero",
    password = "MaRcH131995**",
    destination= "~/Dropbox/Mac (2)/Downloads",
    product= "GLOBAL_MULTIYEAR_BGC_001_029",
    layer= "cmems_mod_glo_bgc_my_0.25_P1D-m",
    variable="po2",
    output="netcdf",
    region=c(-134, 48,-124, 54),
    timerange=c(start, end),
    verticalrange=c(9,1300),
    overwrite = FALSE
  )
}

#Create list of start and end dates by week
#Apply list of dates 
mapply(download8, starts6, ends6)


###Convert netcdf4 format to csv/matrix
install.packages("ncdf4")
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation

nc_data <- nc_open('~/Dropbox/Mac (2)/Downloads/cmems_mod_glo_bgc_my_0.25_P1D-m_1691610748846.nc')
# Save the print(nc) dump to a text file
{
  sink('gimms3g_ndvi_1982-2012_metadata.txt')
  print(nc_data)
  sink()
}
