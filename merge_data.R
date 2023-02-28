#Packages
install.packages("dplyr")
library(dplyr)
library(tidyr)


##Haul and catch data from https://github.com/DFO-NOAA-Pacific/surveyjoin
load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/afsc_catch.rda")
load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/afsc_haul.rda")
load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/nwfsc_catch.rda")
load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/nwfsc_haul.rda")
afsc_haul_o2 <- readRDS("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/2012-2016_o2.rds")

##Merge NWFSC and AFSC
#Make AFSC species lowercase
afsc_catch$scientific_name <- tolower(afsc_catch$scientific_name)
#Make consistent column names and types
colnames(afsc_catch)[6] <- "catch_wt_units"
colnames(afsc_catch)[5] <- "catch_wt"
colnames(nwfsc_catch)[1] <- "event_id"
nwfsc_catch$event_id <- as.numeric(nwfsc_catch$event_id)
afsc_haul$performance <- as.character(afsc_haul$performance)
#Clean up AFSC oxygen
afsc_haul_o2 <- afsc_haul_o2[,c("hauljoin", "salinity", "ph", 'turbidity', "o2")]
colnames(afsc_haul_o2)[1] <- "event_id"
#Combine AFSC oxygen with complete AFSC haul data
afsc_haul <- left_join(afsc_haul, afsc_haul_o2, by="event_id")
#Check how many got properly merged
afsc_haul_check <- subset(afsc_haul,afsc_haul$o2>0)
#Pass--yay, all o2 data merged

#Merge catches and hauls
catch_combined <- bind_rows(afsc_catch, nwfsc_catch)
haul_combined <- bind_rows(afsc_haul, nwfsc_haul)

#Merge catch and haul together
data_combined <- full_join(catch_combined, haul_combined, by="event_id")

#Save
saveRDS(data_combined, "~/Dropbox/choke species/code/choke-species-data/data_combined.rds")

##Data from IPHC
library(readxl)
iphc_other <- read_excel("Dropbox/choke species/code/choke-species-data/raw/IPHC/IPHC_FISS_other.xlsx")
iphc_halibut <- read_excel("~/Dropbox/choke species/code/choke-species-data/raw/IPHC/IPHC_FISS_set_halibut.xlsx")

#