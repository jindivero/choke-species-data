install.packages("reticulate")
library(reticulate)
install.packages("lubridate")
library(lubridate)

#Create virtual environment and install copernismarine
virtualenv_create(envname = "CopernicusMarine")

virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))

reticulate::use_virtualenv("CopernicusMarine", required = TRUE)

#Store "copernicusmarine" package in variable to use toolbox functions
cmt <- import("copernicusmarine")

#Create login
cmt$login("jindivero1", "Password1")

starts <- seq(ymd(20210501), ymd(20240501), by="1 year")
ends <- seq(ymd(20211030), ymd(20241030), by="1 year")
starts <- as.character(starts)
ends <- as.character(ends)    

##Temp/sal
outdir <- setwd("C:/Users/jindiv/Dropbox/GLORYS/alaska_ts_2") # set work directory

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_myint_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("so", "thetao"),
    minimum_longitude=-180,
    maximum_longitude=-125,
    minimum_latitude=50,
    maximum_latitude=70,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0,
    maximum_depth=1600,
  )
}

##Temp/Sal Alaska2
outdir <- setwd("C:/Users/jindiv/Dropbox/GLORYS/alaska2_ts_2") # set work directory

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_myint_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("so", "thetao"),
    minimum_longitude=170,
    maximum_longitude=180,
    minimum_latitude=49,
    maximum_latitude=60,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0,
    maximum_depth=1600,
  )
}

##Temp/sal
outdir <- setwd("C:/Users/jindiv/Dropbox/GLORYS/wc_ts_2") # set work directory

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_myint_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("so", "thetao"),
    minimum_longitude=-138,
    maximum_longitude=-114,
    minimum_latitude=26,
    maximum_latitude=51,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0,
    maximum_depth=1600,
  )
}

outdir <- setwd("C:/Users/jindiv/Dropbox/GLORYS/bc_ts_2") # set work directory
#BC
for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_myint_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("so", "thetao"),
    minimum_longitude=-150,
    maximum_longitude=-114,
    minimum_latitude=46,
    maximum_latitude=56,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0,
    maximum_depth=1600,
  )
}


outdir <- setwd("C:/Users/jindiv/Dropbox/GLORYS/alaska_o2_2") # set work directory

#####Alaska#######
for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_bgc-bio_anfc_0.25deg_P1D-m",
    dataset_version="202311",
    variables=list("o2"),
    minimum_longitude=-180,
    maximum_longitude=-125,
    minimum_latitude=50,
    maximum_latitude=70,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0,
    maximum_depth=1500,
  )
}

outdir <- setwd("C:/Users/jindiv/Dropbox/GLORYS/wc_o2_2") # set work directory

####WC#####
for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_bgc-bio_anfc_0.25deg_P1D-m",
    dataset_version="202311",
    variables=list("o2"),
    minimum_longitude=-138,
    maximum_longitude=-114,
    minimum_latitude=26,
    maximum_latitude=51,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0,
    maximum_depth=1600,
  )
}

####BC#####
for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_bgc-bio_anfc_0.25deg_P1D-m",
    dataset_version="202311",
    variables=list("o2"),
    minimum_longitude=-150,
    maximum_longitude=-114,
    minimum_latitude=46,
    maximum_latitude=56,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0,
    maximum_depth=1600,
  )
}


outdir <- setwd("C:/Users/jindiv/Dropbox/GLORYS/alaska2_o2_2") # set work directory

####BC#####
for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_bgc-bio_anfc_0.25deg_P1D-m",
    dataset_version="202311",
    variables=list("o2"),
    minimum_longitude=170,
    maximum_longitude=180,
    minimum_latitude=49,
    maximum_latitude=60,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0,
    maximum_depth=1600,
  )
}



