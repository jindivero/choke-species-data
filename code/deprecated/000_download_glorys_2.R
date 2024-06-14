install.packages("reticulate")
library(reticulate)
library(lubridate)



#Create virtual environment and install copernismarine
virtualenv_create(envname = "CopernicusMarine")

virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))

reticulate::use_virtualenv("CopernicusMarine", required = TRUE)

#Store "copernicusmarine" package in variable to use toolbox functions
cmt <- import("copernicusmarine")

#Create login
cmt$login("jindivero1", "Password1")

#Alaska oxygen 2021
outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/Copernicus/o2/alaska/2021") # set work directory
cmt$subset(
  dataset_id="cmems_mod_glo_bgc_my_0.25_P1D-m",
  dataset_version="202112",
  variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
  minimum_longitude=-179.6,
  maximum_longitude=-133,
  minimum_latitude=51,
  maximum_latitude=66,
  start_datetime="2021-05-01",
  end_datetime="2021-09-30",
  minimum_depth=0.4940253794193268,
  maximum_depth=1300,
)

#Other Alaska section
cmt$subset(
  dataset_id="cmems_mod_glo_bgc_my_0.25_P1D-m",
  dataset_version="202112",
  variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
  minimum_longitude=170,
  maximum_longitude=179,
  minimum_latitude=50,
  maximum_latitude=54,
  start_datetime="1993-05-01",
  end_datetime="2021-08-30",
  minimum_depth=0.4940253794193268,
  maximum_depth=1300,
)


#Other Alaska section
cmt$subset(
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  dataset_version="202311",
  variables=list("bottomT", "so", "thetao"),
  minimum_longitude=170,
  maximum_longitude=179,
  minimum_latitude=50,
  maximum_latitude=54,
  start_datetime="1993-05-01T00:00:00",
  end_datetime="2021-06-30T00:00:00",
  minimum_depth=0.4940253794193268,
  maximum_depth=1300,
)

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species") # set work directory

#Northwest
cmt$subset(
  dataset_id="cmems_mod_glo_bgc_my_0.25_P1D-m",
  dataset_version="202112",
  variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
  minimum_longitude=-125,
  maximum_longitude=-117,
  minimum_latitude=31,
  maximum_latitude=48,
  start_datetime="2021-05-01",
  end_datetime="2021-10-31",
  minimum_depth=9,
  maximum_depth=1300,
)

cmt$subset(
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  dataset_version="202311",
  variables=list("so", "thetao"),
  minimum_longitude=-125,
  maximum_longitude=-117,
  minimum_latitude=31,
  maximum_latitude=48,
  start_datetime="2021-05-01",
  end_datetime="2021-10-31",
  minimum_depth=9,
  maximum_depth=1300,
)

#BC
cmt$subset(
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  dataset_version="202311",
  variables=list("so", "thetao"),
  minimum_longitude=-134,
  maximum_longitude=-124,
  minimum_latitude=48,
  maximum_latitude=54,
  start_datetime="2021-05-01",
  end_datetime="2021-09-30",
  minimum_depth=9,
  maximum_depth=1300,
)

cmt$subset(
  dataset_id="cmems_mod_glo_bgc_my_0.25_P1D-m",
  dataset_version="202112",
  variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
  minimum_longitude=-134,
  maximum_longitude=-124,
  minimum_latitude=48,
  maximum_latitude=54,
  start_datetime="2021-05-01",
  end_datetime="2021-09-30",
  minimum_depth=9,
  maximum_depth=1300,
)

#Download Alaska temp/sal data

starts <- seq(ymd(19930501), ymd(20210501), by="1 year")
ends <- seq(ymd(19930830), ymd(20210830), by="1 year")
starts <- as.character(starts)
ends <- as.character(ends)                 

options(device.ask.default = FALSE)

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("bottomT", "so", "thetao"),
    minimum_longitude=-179.6,
    maximum_longitude=-133,
    minimum_latitude=51,
    maximum_latitude=66,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=9,
    maximum_depth=1300
  )
}

#IMPORTANT NOTE: need to manually enter Y in the console for length(starts) to answer the downloading prompt

##Extra bit of BC realized that I think is missing
outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data") # set work directory

starts <- seq(ymd(19930501), ymd(20210501), by="1 year")
ends <- seq(ymd(19930930), ymd(20210930), by="1 year")
starts <- as.character(starts)
ends <- as.character(ends)                 

options(device.ask.default = FALSE)

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("bottomT", "so", "thetao"),
    minimum_longitude=-134,
    maximum_longitude=-122,
    minimum_latitude=48,
    maximum_latitude=55,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=9,
    maximum_depth=1300
  )
}

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_bgc_my_0.25_P1D-m",
    dataset_version="202112",
    variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
    minimum_longitude=-134,
    maximum_longitude=-122,
    minimum_latitude=48,
    maximum_latitude=55,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=9,
    maximum_depth=1300
  )
}

#IMPORTANT NOTE: need to manually enter Y in the console for length(starts) to answer the downloading prompt

#Download missing glorys data
#Read in list of dates and 
setwd("~/Dropbox/choke species/code/choke-species-data/code/wa_state")
missing1 <- readRDS("missing_o2.rds")

#Download--these are just October dates in BC
dates <- missing1$date_gloryso2
lats <- missing1$lat_gloryso2
lons <- missing1$lon_gloryso2

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/Copernicus/o2/bc/missing_bc_oct") # set work directory

for(i in 1:nrow(missing1)){
  cmt$subset(
    dataset_id="cmems_mod_glo_bgc_my_0.25_P1D-m",
    dataset_version="202112",
    variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
    minimum_longitude=lons[i],
    maximum_longitude=lons[i],
    minimum_latitude=lats[i],
    maximum_latitude=lats[i],
    start_datetime=dates[i],
    end_datetime=dates[i],
    minimum_depth=9,
    maximum_depth=1300
  )
}


#Download--these are just October dates in BC
outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/bc/bc3") # set work directory

#BC
cmt$subset(
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  dataset_version="202311",
  variables=list("so", "thetao"),
  minimum_longitude=-134,
  maximum_longitude=-124,
  minimum_latitude=48,
  maximum_latitude=54,
  start_datetime="2021-09-30",
  end_datetime="2021-10-30",
  minimum_depth=9,
  maximum_depth=1300,
)

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/Copernicus/o2/bc/missing_bc_oct") # set work directory

cmt$subset(
  dataset_id="cmems_mod_glo_bgc_my_0.25_P1D-m",
  dataset_version="202112",
  variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
  minimum_longitude=-134,
  maximum_longitude=-124,
  minimum_latitude=48,
  maximum_latitude=54,
  start_datetime="2021-09-30",
  end_datetime="2021-10-30",
  minimum_depth=9,
  maximum_depth=1300,
)
#Need to manually enter Y for number of times of loop

#Download the October dates in BC for temp/sal data

#Download Alaska temp/sal data for 0830--0915
outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/ak_extratime") # set work directory

starts <- seq(ymd(19930831), ymd(20210831), by="1 year")
ends <- seq(ymd(19930930), ymd(20210930), by="1 year")
starts <- as.character(starts)
ends <- as.character(ends)                 

options(device.ask.default = FALSE)

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("so", "thetao"),
    minimum_longitude=-179.6,
    maximum_longitude=-133,
    minimum_latitude=51,
    maximum_latitude=66,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=9,
    maximum_depth=1300
  )
}

#Download Alaska o2 data for 0830--0915
outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/Copernicus/o2/alaska/extra_time") # set work directory

starts <- seq(ymd(19930831), ymd(20210831), by="1 year")
ends <- seq(ymd(19930930), ymd(20210930), by="1 year")
starts <- as.character(starts)
ends <- as.character(ends)                 

options(device.ask.default = FALSE)

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("so", "thetao"),
    minimum_longitude=-179.6,
    maximum_longitude=-133,
    minimum_latitude=51,
    maximum_latitude=66,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=9,
    maximum_depth=1300
  )
}

###Extra West Coast section
starts <- seq(ymd(19930501), ymd(20210501), by="1 year")
ends <- seq(ymd(19931030), ymd(20211030), by="1 year")
starts <- as.character(starts)
ends <- as.character(ends)  

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/wc_extraspace") # set work directory

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("so", "thetao"),
    minimum_longitude=-126,
    maximum_longitude=-117,
    minimum_latitude=33,
    maximum_latitude=49,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=9,
    maximum_depth=1300
  )
}

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/Copernicus/temp_sal/jindivero/wc_extraspace") # set work directory

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_bgc_my_0.25_P1D-m",
    dataset_version="202112",
    variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
    minimum_longitude=-126,
    maximum_longitude=-117,
    minimum_latitude=33,
    maximum_latitude=49,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=9,
    maximum_depth=1300
  )
}

