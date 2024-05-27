# GLORYS climatology using CDO
# organize files
install.packages("tidyverse")
library(tidyverse)
install.packages("tidync")
library(tidync)
library(here)

#Set wd
setwd("~/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/zone1")

#Format of cdo commands: "cdo function *.nc new_file_name.nc"
#Remember that every element of the command needs a space in between! So when using paste0, make sure add in spaces

####Zone 1 (West Coast)
###Temperature and salinity 
combined <- "cdo mergetime *.nc glorys_o2_1993_2020_raw.nc"
system(combined)

#Checking timesteps
time_ref <- tidync("glorys_o2_1993_2020_raw.nc")%>%
  activate("D0")%>% hyper_tibble()%>% mutate(date=as_date("1950-01-01")+hours(time))
ncdf4::nc_open("glorys_o2_1993_2020_raw.nc")
#Looks right!

#Extract bottom layer
cmd <- paste0('cdo -bottomvalue ',"glorys_o2_1993_2020_raw.nc",  " glorys_o2_1993_2020_raw_bottom.nc")
system(cmd)

#Cut out survey footprint
cmd <- ("cdo selregion,dcw:ES+PT glorys_o2_1993_2020_raw_bottom.nc glorys_o2_1993_2020_raw_bottom.nc")
system(cmd)

#depth_test<- tidync("test.nc")%>%
 # hyper_tibble()

#ggplot(depth_test, aes(x=longitude, y=latitude))+
  #geom_tile(aes(fill=thetao))+
  #coord_equal()+
  #facet_wrap("time")

#

###Oxygen

##Summaries
#Monthly means
cmd <- paste0('cdo -ymonmean ',"glorys_o2_1993_2020_raw.nc", " glorys_o2_monthly_mean.nc")
system(cmd)

#Make and save grid file
cmd <- paste0('cdo -griddes', " glorys_o2_monthly_mean.nc")
#Name file "glorys3d.grd"





#Plot
sst <- tidync("glorys_o2_monthly_mean.nc") %>%
  hyper_filter(depth=index==30)

all_fl <- list.files(here(""),full.names = T)
glorys_fl <- all_fl[grepl("GLORYS2v4",all_fl)]

calc_glorys_clim <- function(hfile){
  vn <- tidync(hfile) %>% hyper_vars() %>% pull('name')
  ofile <- paste0('glorys_climatology/glorys_',vn,"_clim.nc")
  cmd <- paste0('cdo -ymonmean ',"glorys_o2_1993_2020_raw.nc", " glorys_o2_monthly_mean.nc")
  system(cmd)
}

# apply to everything
purrr::walk(glorys_fl,calc_glorys_clim)

