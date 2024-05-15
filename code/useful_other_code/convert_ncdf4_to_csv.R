install.packages("ncdf4")
library(ncdf4)
install.packages("lubridate")
library(lubridate)
library(dplyr)
library(parallel)

##Oxygen
#List files
files <- list.files("~/Dropbox/choke species/code/Copernicus/o2/alaska", all.files=T, pattern=".nc", full.names=T)
files2 <- list.files("~/Dropbox/choke species/code/Copernicus/o2/nwfsc", all.files=T, pattern=".nc", full.names=T)
files3 <- list.files("~/Dropbox/choke species/code/Copernicus/o2/bc", all.files=T, pattern=".nc", full.names=T)

#files <- append(files, files2)
#files <- append(files, files3)
#Read file

# Function to extract all information from each one
extract_info <- function(file){
#Extract coordinates, time, depth
nc_ds <-  nc_open(file)
dim_lon <- ncvar_get(nc_ds, "longitude")
dim_lat <- ncvar_get(nc_ds, "latitude")
dim_depth <- ncvar_get(nc_ds, "depth")
dim_time <- ncvar_get(nc_ds, "time")

#Convert time to YYY-MM-DD format
t_units <- ncatt_get(nc_ds, "time", "units")
t_ustr <- strsplit(t_units$value, " ")
t_dstr <- strsplit(unlist(t_ustr)[3], "-")
date <- ymd(t_dstr) + dhours(dim_time)

#Create coordinates
coords <- as.matrix(expand.grid(dim_lon, dim_lat, dim_depth, date))

#Extract the variables
var1 <- ncvar_get(nc_ds, "o2", collapse_degen=FALSE)

nc_df <- as.data.frame(cbind(coords, var1))
names(nc_df) <- c("lon", "lat", "depth", "time", "po2")
return(nc_df)
}

test <- files[1]
o2 <- mclapply(files, extract_info)

extract_info(test)

o2 <- unlist(o2)
o2 <- bind_rows(o2)

write.table(nc_df, csv_fname, row.names=FALSE, sep=";")

##Temperature and salinity
#List files
files <- list.files("~/Dropbox/choke species/code/Copernicus/o2/alaska", all.files=T, pattern=".nc", full.names=T)
files2 <- list.files("~/Dropbox/choke species/code/Copernicus/o2/nwfsc", all.files=T, pattern=".nc", full.names=T)
files3 <- list.files("~/Dropbox/choke species/code/Copernicus/o2/bc", all.files=T, pattern=".nc", full.names=T)

files <- append(files, files2)
files <- append(files, files3)
#Read file

# Function to extract all information from each one
extract_info <- function(file){
  #Extract coordinates, time, depth
  nc_ds <-  nc_open(file)
  dim_lon <- ncvar_get(nc_ds, "longitude")
  dim_lat <- ncvar_get(nc_ds, "latitude")
  dim_depth <- ncvar_get(nc_ds, "depth")
  dim_time <- ncvar_get(nc_ds, "time")
  
  #Convert time to YYY-MM-DD format
  t_units <- ncatt_get(nc_ds, "time", "units")
  t_ustr <- strsplit(t_units$value, " ")
  t_dstr <- strsplit(unlist(t_ustr)[3], "-")
  date <- ymd(t_dstr) + dhours(dim_time)
  
  #Create coordinates
  coords <- as.matrix(expand.grid(dim_lon, dim_lat, dim_depth, date))
  
  #Extract the variables
  var1 <- ncvar_get(nc_ds, "o2", collapse_degen=FALSE)
  
  nc_df <- as.data.frame(cbind(coords, var1))
  names(nc_df) <- c("lon", "lat", "depth", "time", "po2")
  return(nc_df)
}

o2 <- mclapply(files, extract_info)

o2 <- unlist(o2)
o2 <- bind_rows(o2)

write.table(nc_df, csv_fname, row.names=FALSE, sep=";")

