### load helper functions ####
source("code/util_funs.R")

type <- "hauls"
catch <- combine_all(type=type)
saveRDS(catch, file="hauls_per_species.rds")

type <- "biomass"
biomass <- combine_all(type=type)
saveRDS(biomass, file="biomass_per_species.rds")
