



### To summarize whole region for monthly climatology ###
#Cut out survey footprint
cmd <- ("cdo selregion,dcw:ES+PT glorys_o2_1993_2020_raw_bottom.nc glorys_o2_1993_2020_raw_bottom.nc")
system(cmd)

#Monthly means
cmd <- paste0('cdo -ymonmean ',"glorys_o2_1993_2020_raw.nc", " glorys_o2_monthly_mean.nc")
system(cmd)

#Make and save grid file
cmd <- paste0('cdo -griddes', " glorys_o2_monthly_mean.nc")
#Name file "glorys3d.grd"

#depth_test<- tidync("test.nc")%>%
# hyper_tibble()

#ggplot(depth_test, aes(x=longitude, y=latitude))+
#geom_tile(aes(fill=thetao))+
#coord_equal()+
#facet_wrap("time")

all_fl <- list.files(here(""),full.names = T)
glorys_fl <- all_fl[grepl("GLORYS2v4",all_fl)]

calc_glorys_clim <- function(hfile){
  vn <- tidync(hfile) %>% hyper_vars() %>% pull('name')
  ofile <- paste0('glorys_climatology/glorys_',vn,"_clim.nc")
  cmd <- paste0('cdo -ymonmean ',"glorys_o2_1993_2020_raw.nc", " glorys_o2_monthly_mean.nc")
  system(cmd)
}

#Plot
sst <- tidync("glorys_o2_monthly_mean.nc") %>%
  hyper_filter(depth=index==30)

###Other code that might be useful
#Another way to extract bottom layer, but it removes the depth variable, which we need to have
#cmd <- paste0('cdo -bottomvalue ',"glorys_tempsal_1993_2020_raw.nc",  " glorys_tempsal_1993_2020_raw_bottom.nc")
#system(cmd)

#Extract lat, long, time, depth
dim_lon <- ncvar_get(nc_ds, "longitude")
dim_lat <- ncvar_get(nc_ds, "latitude")
dim_time <- ncvar_get(nc_ds, "time")
depth <- ncvar_get(nc_ds, "depth")

# plot to check it out
nc1_bottom %>% 
  st_as_sf(coords=c("longitude","latitude"),remove = F,crs=4326) %>% 
  ggplot(aes(color=o2))+
  geom_sf()+
  scale_color_viridis()
# looks about right

#Convert time to YYY-MM-DD format (number of hours since 1-Jan-1950)
date <- as.POSIXct(ymd("1950-01-01") + dhours(dim_time))

#Convert 2D coordinates to 1D coordinates
meta <- data.frame(id=1:length(dim_lon),lon=as.vector(dim_lon), lat=as.vector(dim_lat))

#Extract variables
temp <- ncvar_get(nc_ds, "no3")
sal <- ncvar_get(nc_ds, "o2")
