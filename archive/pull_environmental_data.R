#Packages
install.packages("taxize")
library(taxize)

#Haul and catch data from https://github.com/DFO-NOAA-Pacific/surveyjoin
load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/afsc_catch.rda")
load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/afsc_haul.rda")
load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/nwfsc_catch.rda")
load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/nwfsc_haul.rda")

load("~/Dropbox/choke species/code/choke-species-data/raw/NOAA/species.rda")

#Add species names to AFSC
afsc_catch$scientific_name <-  classification(sci_id=afsc_catch$itis, db="itis")

#Merge catch and haul

#Merge NWFSC and AFSC

#Correct units of catch/density?

#Data from IPHC


###Environmental data
#Pull nwfsc haul data for haul environmental covariates
install.packages("nwfscSurvey")
library(nwfscSurvey)

devtools::install_github("pfmc-assessments/nwfscSurvey")

catch = PullCatch.fn(SurveyName = "NWFSC.Combo")
bio   = PullBio.fn(SurveyName = "NWFSC.Combo")

pull_haul(survey = "NWFSC.Combo", dir = NULL, verbose = TRUE)

#pull awfsc data for haul environmental covariates
library("httr")
library("jsonlite")
# link to the API
api_link <- "https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey/"

#Test
res <- httr::GET(url = api_link, query = list(year = "2018", srvy = "AI"))
# res # Test connection
data <- jsonlite::fromJSON(base::rawToChar(res$content))
x <- data$items
x <- x[,c("srvy", "year", "stratum", "station", "vessel_name", "latitude_dd", "longitude_dd",
          "species_code", "common_name", "scientific_name", "taxon_confidence",
          "cpue_kgha", "cpue_noha", "weight_kg", "count",
          "bottom_temperature_c", "surface_temperature_c", "depth_m")]
