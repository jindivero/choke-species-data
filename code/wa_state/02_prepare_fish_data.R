#Prepare data
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")
library(stringr)

setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

##Pull species of interest
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
sci_names <- c("gadus chalcogrammus", "anoplopoma fimbria", "microstomus pacificus", "eopsetta jordani", "gadus macrocephalus", "sebastolobus altivelis", "sebastolobus alascanus", "hippoglossus stenolepis", "sebastes pinniger", "ophiodon elongatus", "sebastes crameri", "atheresthes stomias", "merluccius productus", "sebastes entomelas","sebastes ruberrimus","sebastes maliger", "glyptocephalus zachirus", "hippoglossoides elassodon", "parophrys vetulus", "lepidopsetta bilineata", "sebastes aleutianus")
spcs <- c("walleye pollock", "sablefish", "dover sole", "petrale sole", "pacific cod", "longspine thornyhead", "shortspine thornyhead", "pacific halibut", "canary rockfish", "lingcod", "darkblotched rockfish",  "arrowtooth flounder", "pacific hake", "widow rockfish","yelloweye rockfish", "quillback rockfish", "rex sole", "flathead sole", "english sole", "southern rock sole", "rougheye rockfish")

#Apply to multiple species
dataframes <- list()
for(i in 16:length(sci_names)){
  message( "Preparing ", spcs[i], Sys.time() )
  dat <- prepare_data(spcs[i], sci_names[i])
  name <- paste(spcs[i])
  dataframes[[name]] <- dat
}

#Save
#saveRDS(dataframes, file="dataframes.rds")

###Summary of data 
##Summarize for data with all regions combined
#Create list and loop positive_catches() function through each dataframe of species
dat_available <- list()
for(i in 1:length(dataframes)){
  dat <- positive_catches(dataframes[[i]])
  name <- paste(spcs[i])
  dat_available[[name]] <- as.data.frame(dat)
}
#Bind together into one dataframe
dat_available2 <- dat_available[[1]]
rownames(dat_available2) <- paste(spcs[1])

for(i in 2:length(dat_available)){
  dat <- dat_available[[i]]
  rownames(dat) <- paste(spcs[i])
  dat_available2 <- bind_rows(dat_available2, dat)
}

##Summarize for each species for each region separately
#Make broader region (combine all the BC surveys) if needed
for(i in 1:length(dataframes)){
dataframes[[i]]$region<- ifelse(str_detect(dataframes[[i]]$project, "SYN"), "BC", dataframes[[i]]$project)
}
#Loop through each species and region
dat_available_region <- list()
for(i in 1:length(dataframes)){
  dats <- group_split(dataframes[[i]], region)
  summary <- list()
  species <- spcs[i]
  for(j in 1:length(dats)){
    dat <- as.data.frame(dats[[j]])
    dat2 <- positive_catches(dat)
    name <- paste(unique(dat$region))
    dat2 <- as.data.frame(dat2)
    summary[[name]] <- dat2
  }
  dat_available_region[[species]] <- summary
}

#Combine dataframes of each region for each species
dat_available_region2 <- list()
for(i in 1:length(dat_available_region)){
  dat <- bind_rows(dat_available_region[[i]])
  dat$species <- spcs[[i]]
  dat_available_region2[[i]] <- dat
}
#Combine for all species
dat_available_region2 <- bind_rows(dat_available_region2)
