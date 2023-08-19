#Packages
install.packages("dplyr")
library(dplyr)
library(tidyr)
install.packages("readxl")
library(readxl)
library(taxize)

### load helper functions ####
source("util_funs.R")

### Raw Data ###
## Haul and catch data from https://github.com/DFO-NOAA-Pacific/surveyjoin ##
load("~/data/fish_raw/NOAA/afsc_catch.rda")
load("~/data/fish_raw/NOAA/afsc_haul.rda")
load("~/data/fish_raw/NOAA/nwfsc_catch.rda")
load("~/data/fish_raw/NOAA/nwfsc_haul.rda")
afsc_haul_o2 <- readRDS("~/data/fish_raw/NOAA/2012-2016_o2.rds")
pbs_haul <- readRDS("~/data/fish_raw/pbs-haul.rds")
pbs_catch <- readRDS("~data/fish_raw/pbs-catch.rds")

if(surveyjoin) {
  remotes::install_github("DFO-NOAA-Pacific/surveyjoin", dependencies = TRUE)
  library(surveyjoin)
  
  #Download data
  cache_data()
  load_sql_data()
  
  db <- surv_db()
  catch <- tbl(db, "catch")
  haul <- tbl(db, "haul")
}

##Data from IPHC ##
iphc_other <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/IPHC_FISS_other.xlsx")
iphc_halibut <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/IPHC_FISS_set_halibut.xlsx")

### Housekeeping ###
#Make AFSC species lowercase
afsc_catch$scientific_name <- tolower(afsc_catch$scientific_name)
#Make consistent column names and types
colnames(afsc_catch)[6] <- "catch_wt_units"
colnames(afsc_catch)[5] <- "catch_wt"
colnames(nwfsc_catch)[1] <- "event_id"
nwfsc_catch$event_id <- as.numeric(nwfsc_catch$event_id)
afsc_haul$performance <- as.character(afsc_haul$performance)
pbs_haul$performance <- as.character(pbs_haul$performance)
#Clean up AFSC oxygen
afsc_haul_o2 <- afsc_haul_o2[,c("hauljoin", "salinity", "ph", 'turbidity', "o2")]
colnames(afsc_haul_o2)[1] <- "event_id"
#Combine AFSC oxygen with complete AFSC haul data
afsc_haul <- left_join(afsc_haul, afsc_haul_o2, by="event_id")
#Check how many got properly merged
afsc_haul_check <- subset(afsc_haul,afsc_haul$o2>0)
#Pass--yay, all o2 data merged

## For pbs data ##
# Get species names
# List of species
itis <- unique(pbs_catch$itis)
scientific_names <-  classification(itis, db="itis")
itis <- as.data.frame(itis)
names <-lapply(scientific_names, extract_name)
names <- unlist(names)
itis$scientific_name <- names
pbs_catch <- left_join(pbs_catch,itis)

## Merge NOAA and DFO catches and hauls ##
catch_combined <- bind_rows(afsc_catch, nwfsc_catch, pbs_catch)
haul_combined <- bind_rows(afsc_haul, nwfsc_haul, pbs_haul)

#Merge catch and haul together
data_combined <- full_join(catch_combined, haul_combined, by="event_id")

#Save
saveRDS(data_combined, "~/Dropbox/choke species/code/choke-species-data/data_combined.rds")

#test
sci_name <- "Anoplopoma fimbria" 
spc <- "sablefish" 
dat.by.size <- length_expand(sci_name)
dat <- load_data(spc = spc, dat.by.size = dat.by.size)

#Save
saveRDS(data_combined, "~/Dropbox/choke species/code/choke-species-data/data_combined.rds")


### Housekeeping IPHC data ###
#Catch at each station was divided into small halibut (≤ 81 cm) and large (> 81 cm) halibut with each component weighed
#The weight was then divided by the effective number of skates (defined as a 549 m length of groundline with 100 individual hooks) hauled to determine a catch-per-unit-of-effort (CPUE in kg/skate) for each size component of the catch

