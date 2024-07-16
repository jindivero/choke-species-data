#Prepare data
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")
source("code/util_funs.R")
library(stringr)
library(ggplot2)

setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

### Set ggplot themes ###
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##Pull species of interest
#One species
sci_name <- "anoplopoma fimbria" 
spc <- "sablefish" 

insitu <- readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/processed_data/insitu_combined.rds")
dat_sablefish <- prepare_data(spc=spc, sci_name=sci_name, ROMS=F, GLORYS=F, in_situ=insitu)

dat_pcod <- prepare_data(spc="pacific cod", sci_name="gadus macrocephalus", ROMS=F, GLORYS=F, in_situ=insitu)
dat_arrowtooth <- prepare_data(spc="arrowtooth flounder", sci_name="atheresthes stomias", ROMS=F, GLORYS=F, in_situ=insitu)
dat_halibut <- prepare_data(spc="pacific halibut", sci_name="hippoglossus stenolepis", ROMS=F, GLORYS=F, in_situ=insitu)

saveRDS(dat_pcod, "data/processed_data/dat_pcod.rds")
saveRDS(dat_arrowtooth, "data/processed_data/dat_aflounder.rds")
saveRDS(dat_halibut, "data/processed_data/dat_phalibut.rds")
saveRDS(dat_sablefish, "data/processed_data/dat_sablefish.rds")

##Multiple species
#Print options
type <- "biomass"
print_species(type)

#Make list
sci_names <- c("gadus chalcogrammus", "anoplopoma fimbria", "microstomus pacificus", "eopsetta jordani", "gadus macrocephalus", "sebastolobus altivelis", "sebastolobus alascanus", "hippoglossus stenolepis", "sebastes pinniger", "ophiodon elongatus", "sebastes crameri", "atheresthes stomias", "merluccius productus", "sebastes entomelas","sebastes ruberrimus","sebastes maliger", "glyptocephalus zachirus", "hippoglossoides elassodon", "parophrys vetulus", "lepidopsetta bilineata", "sebastes aleutianus")
spcs <- c("walleye pollock", "sablefish", "dover sole", "petrale sole", "pacific cod", "longspine thornyhead", "shortspine thornyhead", "pacific halibut", "canary rockfish", "lingcod", "darkblotched rockfish",  "arrowtooth flounder", "pacific hake", "widow rockfish","yelloweye rockfish", "quillback rockfish", "rex sole", "flathead sole", "english sole", "southern rock sole", "rougheye rockfish")

#Apply to multiple species
dataframes <- list()
for(i in 1:length(sci_names)){
  message( "Preparing ", spcs[i], Sys.time() )
  dat <- prepare_data(spcs[i], sci_names[i], ROMS=F, GLORYS=F)
  name <- paste(spcs[i])
  dataframes[[name]] <- dat
}

#Save
#saveRDS(dataframes, file="dataframes.rds")

###Summary of data 

##Summarize for each species for each region separately

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
  if(length(dat_available_region[[i]])==5){
  dat$region <- c("BC", "EBS", "GOA", "NBS", "WC")
  }
  if(length(dat_available_region[[i]])==6){
    dat$region <- c("BC", "EBS", "GOA", "IPHC", "NBS", "WC")
  }
  dat_available_region2[[i]] <- dat
}

#Combine for all species
dat_available_region2 <- bind_rows(dat_available_region2)

#Plot availability
#Make region an ordered factor
dat_available_region2 <- subset(dat_available_region2, region!="IPHC")
dat_available_region2$region <-factor(dat_available_region2$region, c("WC", "BC", "GOA", "EBS", "NBS")) 

#Plot proportion of hauls with positive catch
ggplot(dat_available_region2, aes(y=prop_positive_hauls, x=as.factor(region)))+geom_col()+facet_wrap("species")+xlab("Region")+ylab("Proportion of hauls with positive catch")

#Number of hauls with length data
ggplot(dat_available_region2, aes(y=prop_hauls_length, x=as.factor(region)))+geom_col()+facet_wrap("species")+xlab("Region")+ylab("Prop of positive catches with length data")

#Number of hauls with positive catch
ggplot(dat_available_region2, aes(y=total_positive_hauls, x=as.factor(region)))+geom_col()+geom_col(mapping=aes(x=as.factor(region), y=total_hauls_with_length), fill="orange3")+facet_wrap("species")+xlab("Region")+ylab("Number of hauls with positive catch")

#Plot proportion of positive catch hauls with length data
ggplot(dat_available_region2, aes(y=pos_hauls_length, x=as.factor(region)))+geom_col()+facet_wrap("species")+ylab("Proportion of positive catches with length data")

