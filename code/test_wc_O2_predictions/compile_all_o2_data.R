## Compile all insitu Oxygen observations and merge into single dataframe for analysis
library(dplyr)
library(tidyverse)
library(sf)

# load all fish survey data

synoptic_dat <- readRDS("data/processed_data/insitu_combined.rds")
synoptic_dat$doy <- as.POSIXlt(synoptic_dat$date, format = "%Y-%b-%d")$yday

synoptic_dat <- synoptic_dat %>%
  rename(temp = temperature_C,
         do = O2_umolkg,
         sigma0 = sigma0_kgm3) %>%
  select(survey, year, doy, X, Y, temp, do, sigma0, depth)


newport_dat <- readRDS("data/processed_data/newportline_processed.rds")
newport_dat <- newport_dat %>%
  rename(temp = temperature_C,
         do = O2_umolkg,
         sigma0 = sigma0_kgm3)  %>%
  select(survey, year, doy, X, Y, temp, do, sigma0, depth)

calcofi_dat <- readRDS("data/processed_data/calCOFI_processed.rds")

calcofi_dat <- calcofi_dat %>%
  rename(temp = temperature_C,
         do = O2_umolkg,
         sigma0 = sigma0_kgm3)  %>%
  select(survey, year, doy, X, Y, temp, do, sigma0, depth)
calcofi_dat <- sf::st_drop_geometry(calcofi_dat)

linep_dat <- readRDS("data/processed_data/lineP_processed.rds")

linep_dat <- linep_dat %>%
  rename(temp = temperature_C,
         do = O2_umolkg,
         sigma0 = sigma0_kgm3)  %>%
  select(survey, year, doy, X, Y, temp, do, sigma0, depth)
linep_dat <- sf::st_drop_geometry(linep_dat)

all_dat <- rbind(synoptic_dat,
                 newport_dat,
                 calcofi_dat,
                 linep_dat)
# save all_dat as all_o2_dat

saveRDS(all_dat, file = "data/processed_data/all_o2_dat.rds")
