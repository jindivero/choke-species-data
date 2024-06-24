## Download GLORYS data--taken from here: https://help.marine.copernicus.eu/en/articles/8638253-how-to-download-data-via-the-copernicus-marine-toolbox-in-r
#Install packages
install.packages("reticulate")
library(reticulate)

#Set working directory
outdir <- setwd("")

#Create virtual environment and install copernicussmarine
virtualenv_create(envname = "CopernicusMarine")

virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))

reticulate::use_virtualenv("CopernicusMarine", required = TRUE)

#Store "copernicusmarine" package in variable to use toolbox functions
cmt <- import("copernicusmarine")

#Enter username and password of your Copernicus account: https://data.marine.copernicus.eu/register
cmt$login("username", "password")

#Download just one subset--biogeochemistry data: https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_BGC_001_029/download?dataset=cmems_mod_glo_bgc_my_0.25_P1D-m_202112
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
  ##Note on depth: if you want just surface, can make max depth way shallower, if want bottom values, look at shallowest and deepest depths in data
  minimum_depth=0.4940253794193268,
  maximum_depth=1300 #max depth available is 5900 m
)

#Download just one subset--physical data: https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/download?dataset=cmems_mod_glo_phy_my_0.083deg_P1D-m_202311
cmt$subset(
  dataset_id="cmems_mod_glo_phy_my_0.083deg_P1D-m",
  dataset_version="202311",
  variables=list("so", "thetao"), #only temperature and salinity, there are a bunch of other variable options (including bottom temp--add "bottomT" to list if wanted)
  minimum_longitude=-134,
  maximum_longitude=-124,
  minimum_latitude=48,
  maximum_latitude=54,
  start_datetime="2021-05-01",
  end_datetime="2021-09-30",
  minimum_depth=9,
  maximum_depth=1300,
)

##Download multiple subsets of time 
#Create list of start and end date
#Could obviously also do this for list of longitudes/latitudes to loop through multiple spatial areas
starts <- as.character(seq(ymd(19930501), ymd(20210501), by="1 year"))
ends <- as.character(seq(ymd(19930830), ymd(20210830), by="1 year"))

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
#!!!IMPORTANT NOTE: need to manually enter Y in the console for the number of times of the length of the loop to answer the downloading prompt

