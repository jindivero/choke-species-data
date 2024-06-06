install.packages("reticulate")
library(reticulate)

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/glorys_ak2_biogeochem") # set work directory


#Create virtual environment and install copernismarine
virtualenv_create(envname = "CopernicusMarine")

virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))

reticulate::use_virtualenv("CopernicusMarine", required = TRUE)

#Store "copernicusmarine" package in variable to use toolbox functions
cmt <- import("copernicusmarine")

#Create login
cmt$login("jindivero1", "Password1")

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

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/glorys_ak2_biogeochem") # set work directory

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

