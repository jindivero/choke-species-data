## Compile all insitu Oxygen observations and merge into single dataframe for analysis
library(dplyr)
library(tidyverse)
library(sf)


filenames <- c("data/processed_data/insitu_combined.rds",
               "data/processed_data/newportline_processed.rds",
               "data/processed_data/calCOFI_processed.rds",
               "data/processed_data/lineP_processed.rds",
               "data/processed_data/wcoa_processed.rds",
               "data/processed_data/codap_processed.rds",
               "data/processed_data/ocnms2_processed.rds",
               "data/processed_data/ocnms_processed.rds",
               "data/processed_data/ocnms_processed.rds",
               "data/processed_data/hake_oxygen_processed.rds"
               )
simplify_df <- function(input_dat, makedoy = F) {
  # see if DOY is present, if not, add it
  if (makedoy) input_dat$doy <- as.POSIXlt(input_dat$date, format = "%Y-%b-%d")$yday
  output_dat <- input_dat %>%
    rename(temp = temperature_C,
           o2 = O2_umolkg,
           sigma0 = sigma0_kgm3) %>%
    select(survey, year, doy, X, Y, latitude, longitude, temp, o2, sigma0, depth)
  
  # see if geometry is present, if so, delete
  if ("geometry" %in% names(output_dat)) output_dat <- sf::st_drop_geometry(output_dat)
  
  return(output_dat)
}


for (i in 1:length(filenames)) {
  df_tmp <- readRDS(filenames[i])
  df_tmp <- simplify_df(df_tmp)
  # remove depth with error codes -999
  df_tmp <- dplyr::filter(df_tmp, !depth == -999)
  if (i==1) dat <- df_tmp
  if (i>1) dat <- rbind(dat, df_tmp)
}

# convert any longitude on a d=360 scale to -180 to +180
fix_index <- which((dat$longitude) > 180)
dat$longitude[fix_index] <- dat$longitude[fix_index] - 360

# remove rows with missing data
dat <- dat %>%
  drop_na(depth, o2, temp, sigma0)
saveRDS(dat, file = "data/processed_data/all_o2_dat.rds")
