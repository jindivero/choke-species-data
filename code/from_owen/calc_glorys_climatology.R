# GLORYS climatology using CDO
# organize files
install.packages("tidyverse")
library(tidyverse)
install.packages("tidync")
library(tidync)
library(here)


setwd("/Users/juliaindivero/Library/CloudStorage/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/zone1")
test <- "cdo mergetime *.nc glorys_o2_1993_2020_raw.nc"
system(test)

#Checking timesteps
time_ref <- tidync("glorys_o2_1993_2020_raw.nc")%>%
  activate("D0")%>% hyper_tibble()%>% mutate(date=as_date("1950-01-01")+hours(time))
ncdf4::nc_open("glorys_o2_1993_2020_raw.nc")

#Monthly means
cmd <- paste0('cdo -ymonmean ',"glorys_o2_1993_2020_raw.nc", " glorys_o2_monthly_mean.nc")
system(cmd)

#Make and save grid file
cmd <- paste0('cdo -griddes', " glorys_o2_monthly_mean.nc")
#Name file "glorys3d.grd"





#Extract bottom layer
cmd <- paste0('cdo -bottomvalue ',"glorys_o2_monthly_mean.nc",  " test.nc")
system(cmd)

depth_test<- tidync("test.nc")%>%
  hyper_tibble()

ggplot(depth_test, aes(x=longitude, y=latitude))+
  geom_tile(aes(fill=thetao))+
  coord_equal()+
  facet_wrap("time")


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


## Step 2: Add timesteps for future!
# all GLORYS outputs from the previous step are for year 2018
# yrs_to_expand <- 2006:2100-2018
# yrs_to_expand <- yrs_to_expand[which(yrs_to_expand!=0)]
# 
# cat_glorys_clim <- function(gfile){
#   shift1=paste0(yrs_to_expand[1],'year')
#   system(paste0('sudo cdo -shifttime,',shift1,' ',gfile,' glorys_climatology/yrx.nc'))
#   for(i in 2:length(yrs_to_expand)){
#     shift=paste0(yrs_to_expand[i],'year')
#     system(paste0('sudo cdo -shifttime,',shift,' ',gfile,' glorys_climatology/tmp1.nc'))
#     system(paste0('sudo cdo -cat glorys_climatology/yrx.nc glorys_climatology/tmp1.nc glorys_climatology/tmp2.nc'))
#     system(paste0('sudo mv glorys_climatology/tmp2.nc glorys_climatology/yrx.nc'))
#   }
#   system(paste0('sudo mv glorys_climatology/yrx.nc ',paste0(str_replace(gfile,"_clim\\.","_clim_exp."))))
# }
# 
# gfile <- "glorys_climatology/glorys_no3_clim.nc"
# cat_glorys_clim(gfile)
