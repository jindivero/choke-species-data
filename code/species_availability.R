### load helper functions ####
source("code/util_funs.R")
library(dplyr)
library(stringr)

#Reports positive catches
type <- "hauls"
catch <- combine_all(type=type)
saveRDS(catch, file="hauls_per_species.rds")

type <- "biomass"
biomass <- combine_all(type=type)
saveRDS(biomass, file="biomass_per_species.rds")

#Subset to species being considered
sci_names <- c("gadus chalcogrammus", "anoplopoma fimbria", "microstomus pacificus", "eopsetta jordani", "gadus macrocephalus", "sebastolobus altivelis", "sebastolobus alascanus", "hippoglossus stenolepis", "sebastes pinniger", "ophiodon elongatus", "sebastes crameri", "oncorhynchus keta", "oncorhynchus tshawytscha", "oncorhynchus gorbuscha", "atheresthes stomias", "merluccius productus", "sebastes entomelas", "squalus suckleyi","sebastes ruberrimus","sebastes maliger", "glyptocephalus zachirus", "hippoglossoides elassodon", "parophrys vetulus", "lepidopsetta bilineata", "sebastes aleutianus")
spcs <- c("walleye pollock", "sablefish", "dover sole", "petrale sole", "pacific cod", "longspine thornyhead", "shortspine thornyhead", "pacific halibut", "canary rockfish", "lingcod", "darkblotched rockfish", "chum salmon", "chinook salmon", "pink salmon", "arrowtooth flounder", "pacific hake", "widow rockfish", "pacific spiny dogfish", "yelloweye rockfish", "quillback rockfish", "rex sole", "flathead sole", "english sole", "southern rock sole", "rougheye rockfish")

sci_names <- c("gadus chalcogrammus"|"anoplopoma fimbria"|"microstomus pacificus"|"eopsetta jordani"|"gadus macrocephalus"|"sebastolobus altivelis"|"sebastolobus alascanus"|"hippoglossus stenolepis"|"sebastes pinniger"|"ophiodon elongatus"|"sebastes crameri"|"oncorhynchus keta"|"oncorhynchus tshawytscha"|"oncorhynchus gorbuscha"|"atheresthes stomias"|"merluccius productus"|"sebastes entomelas"|"squalus suckleyi","sebastes ruberrimus","sebastes maliger"|"glyptocephalus zachirus"|"hippoglossoides elassodon"|"parophrys vetulus"|"lepidopsetta bilineata"|"sebastes aleutianus")

species_biomass <- filter(biomass, scientific_name %in% c("gadus chalcogrammus", "anoplopoma fimbria", "microstomus pacificus", "eopsetta jordani", "gadus macrocephalus", "sebastolobus altivelis", "sebastolobus alascanus", "hippoglossus stenolepis", "sebastes pinniger", "ophiodon elongatus", "sebastes crameri", "oncorhynchus keta", "oncorhynchus tshawytscha", "oncorhynchus gorbuscha", "atheresthes stomias", "merluccius productus", "sebastes entomelas", "squalus suckleyi","sebastes ruberrimus","sebastes maliger", "glyptocephalus zachirus", "hippoglossoides elassodon", "parophrys vetulus", "lepidopsetta bilineata", "sebastes aleutianus"))
species_catch <- filter(catch, scientific_name %in% c("gadus chalcogrammus", "anoplopoma fimbria", "microstomus pacificus", "eopsetta jordani", "gadus macrocephalus", "sebastolobus altivelis", "sebastolobus alascanus", "hippoglossus stenolepis", "sebastes pinniger", "ophiodon elongatus", "sebastes crameri", "oncorhynchus keta", "oncorhynchus tshawytscha", "oncorhynchus gorbuscha", "atheresthes stomias", "merluccius productus", "sebastes entomelas", "squalus suckleyi","sebastes ruberrimus","sebastes maliger", "glyptocephalus zachirus", "hippoglossoides elassodon", "parophrys vetulus", "lepidopsetta bilineata", "sebastes aleutianus"))
