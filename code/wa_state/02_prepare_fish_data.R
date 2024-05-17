#Prepare data
library(rgdal)
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

###Pull species of interest
#One species
sci_name <- "anoplopoma fimbria" 
spc <- "sablefish" 

dat <- prepare_data(spc=spc, sci_name=sci_name)

#How many positive catches vs zero catches, and how many hauls with missing length data?
positive_catches <- positive_catches(dat)

##Multiple species
#Print options
type <- "biomass"
print_species(type)

#Make list
sci_names <- c("gadus chalcogrammus", "anoplopoma fimbria", "microstomus pacificus", "eopsetta jordani", "gadus macrocephalus", "sebastolobus altivelis", "sebastolobus alascanus", "hippoglossus stenolepis", "sebastes pinniger", "ophiodon elongatus", "sebastes crameri", "oncorhynchus keta", "oncorhynchus tshawytscha", "oncorhynchus gorbuscha", "atheresthes stomias", "merluccius productus", "sebastes entomelas", "squalus suckleyi","sebastes ruberrimus","sebastes maliger", "glyptocephalus zachirus", "hippoglossoides elassodon", "parophrys vetulus", "lepidopsetta bilineata", "sebastes aleutianus")
spcs <- c("walleye pollock", "sablefish", "dover sole", "petrale sole", "pacific cod", "longspine thornyhead", "shortspine thornyhead", "pacific halibut", "canary rockfish", "lingcod", "darkblotched rockfish", "chum salmon", "chinook salmon", "pink salmon", "arrowtooth flounder", "pacific hake", "widow rockfish", "pacific spiny dogfish", "yelloweye rockfish", "quillback rockfish", "rex sole", "flathead sole", "english sole", "southern rock sole", "rougheye rockfish")

#Apply to multiple species
dats <- mapply(prepare_data, sci_names, spcs)
