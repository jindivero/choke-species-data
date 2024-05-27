#Calc_glorys_climatology
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