install.packages("CopernicusMarine")
library(CopernicusMarine)
install.packages("lubridate")
library(lubridate)

###Temperature data
download <- function(start, end){
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

starts <- seq(ymd(20090701), ymd(20200701), by="1 year")
ends <- seq(ymd(20090831), ymd(20200831), by="1 year")

mapply(download, starts, ends)