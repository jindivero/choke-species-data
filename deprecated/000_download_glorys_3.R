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

starts <- seq(ymd(19930501), ymd(20220501), by="1 year")
ends <- seq(ymd(19931030), ymd(20221030), by="1 year")
starts <- as.character(starts)
ends <- as.character(ends)    

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/alaska_new") # set work directory

#Alaska
for(i in 1:length(starts)){
cmt$subset(
  dataset_id="cmems_mod_glo_bgc_my_0.25deg_P1D-m",
  dataset_version="202406",
  variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
  minimum_longitude=170,
  maximum_longitude=222,
  minimum_latitude=46,
  maximum_latitude=70,
  start_datetime=starts[i],
  end_datetime=ends[i],
  minimum_depth=0.4940253794193268,
  maximum_depth=1300,
)
}
outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/alaska_new2") # set work directory

for(i in 1:length(starts)){
  cmt$subset(
    dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
    dataset_version="202311",
    variables=list("so", "thetao"),
    minimum_longitude=170,
    maximum_longitude=222,
    minimum_latitude=46,
    maximum_latitude=70,
    start_datetime=starts[i],
    end_datetime=ends[i],
    minimum_depth=0.4940253794193268,
    maximum_depth=1300,
  )
}

##Washington coast
outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/wc_new") # set work directory

#Northwest
cmt$subset(
  dataset_id="cmems_mod_glo_bgc_my_0.25deg_P1D-m",
  dataset_version="202406",
  variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
  minimum_longitude=-126,
  maximum_longitude=-116,
  minimum_latitude=30,
  maximum_latitude=49,
  start_datetime=starts[i],
  end_datetime=ends[i],
  minimum_depth=9,
  maximum_depth=1300,
)

outdir <- setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/wc_new2") # set work directory
cmt$subset(
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  dataset_version="202311",
  variables=list("so", "thetao"),
  minimum_longitude=-126,
  maximum_longitude=-116,
  minimum_latitude=30,
  maximum_latitude=49,
  start_datetime=starts[i],
  end_datetime=ends[i],
  minimum_depth=9,
  maximum_depth=1300,
)

#BC
cmt$subset(
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  dataset_version="202311",
  variables=list("so", "thetao"),
  minimum_longitude=-135,
  maximum_longitude=-123,
  minimum_latitude=47,
  maximum_latitude=55,
  start_datetime=starts[i],
  end_datetime=ends[i],
  minimum_depth=9,
  maximum_depth=1300,
)

cmt$subset(
  dataset_id="cmems_mod_glo_bgc_my_0.25deg_P1D-m",
  dataset_version="202406",
  variables=list("chl", "no3", "nppv", "o2", "po4", "si"),
  minimum_longitude=-135,
  maximum_longitude=-123,
  minimum_latitude=47,
  maximum_latitude=55,
  start_datetime=starts[i],
  end_datetime=ends[i],
  minimum_depth=9,
  maximum_depth=1300,
)

#IMPORTANT NOTE: need to manually enter Y in the console for length(starts) to answer the downloading prompt
