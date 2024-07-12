library(ggplot2)
lib
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")

#Load species data
dat_pcod <- readRDS("data/processed_data/dat_pcod.rds")
dat_af <-readRDS("data/processed_data/dat_aflounder.rds")
dat_hal <- readRDS("data/processed_data/dat_phalibut.rds")
dat_sab <- readRDS("data/processed_data/dat_sablefish.rds")

dat <- list(dat_pcod,dat_af,dat_hal, dat_sab)

##Map 
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(-3500000, 1025581)
ylimits = c(3549000, 8000000)

plot_string <-"figures/fish_catch_positive.pdf"
pdf(plot_string, onefile = TRUE)

for (i in 1:length(dat)){
data <- dat[[i]]
species <- unique(data$common_name)
p <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(subset(data, cpue_kg_km2 >0), mapping=aes(x = longitude * 1000, y = latitude * 1000, colour=region), size=0.2)+
  ylim(ylimits[1], ylimits[2]) +
  xlim(xlimits[1], xlimits[2]) +
  xlab("Longitude")+
  ylab("Latitude")+
  facet_wrap("year")+
  theme_bw() +
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        , strip.background = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title= element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom")+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  ggtitle(species)
  print(p)

}

dev.off()







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

#How many positive catches vs zero catches, and how many hauls with missing length data?
positive_catches <- positive_catches(dat)