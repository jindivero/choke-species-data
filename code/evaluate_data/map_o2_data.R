library(ggplot2)

basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"
setwd(basewd)

#Upload all processed O2 data
wcoa <- readRDS("data/processed_data/wcoa_processed.rds")
ocnms2 <- readRDS("data/processed_data/ocnms2_processed.rds")
codap <- readRDS("data/processed_data/codap_processed.rds")
calCOFI <- readRDS("data/processed_data/calCOFI_processed.rds")
synoptic <- readRDS("data/processed_data/insitu_combined.rds")
newport <- readRDS("data/processed_data/newportline_processed.rds")
