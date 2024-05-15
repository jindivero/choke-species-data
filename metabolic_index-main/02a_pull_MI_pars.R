setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/metabolic_index-main")

source("lookup_taxa/lookup_taxa_helper.R")

#Pull for general "Teleost"
taxa.name <- "Pleuronectiformes" #Order of halibut
test <-lookup_taxa2(taxa.name)
test <-lookup_taxa(taxa.name)
test2 <- lookup_taxa2("Salmoniformes")
#Very little difference between orders of halibut and salmon

#Save 
setwd("Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
saveRDS(test, file="MI_pars.rds")
