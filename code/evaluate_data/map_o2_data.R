library(ggplot2)
library(dplyr)

basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"
setwd(basewd)

#Upload all processed O2 data
wcoa <- readRDS("data/processed_data/wcoa_processed.rds")
codap <- readRDS("data/processed_data/codap_processed.rds")
calCOFI <- readRDS("data/processed_data/calCOFI_processed.rds")
synoptic <- readRDS("data/processed_data/insitu_combined.rds")
newport <- readRDS("data/processed_data/newportline_processed.rds")
ocnms <- readRDS("data/processed_data/ocnms_processed.rds")
ocnms2 <- readRDS("data/processed_data/ocnms_processed.rds")
linep <- readRDS("data/processed_data/LineP_processed.rds")

#combine
dat <- bind_rows(wcoa, codap, calCOFI, newport, ocnms, ocnms2)

##Map 
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")

# crop if you want; not needed:
us_coast <- st_crop(map_data,
                    c(xmin = -126, ymin = 31, 
                      xmax = -110, ymax = 50))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(-3500000, 1025581)
ylimits = c(3549000, 8000000)

plot_string <-"o2_obs_available.pdf"
pdf(plot_string, onefile = TRUE)

ggplot(us_coast_proj) + geom_sf() +
 geom_point(data=filter(dat, year>1998), aes(x = X * 1000, y = Y * 1000, colour=survey), size=0.2)+
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
  guides(colour = guide_legend(override.aes = list(size=8)))

dev.off()

plot_string <-"o2_synoptic_available.pdf"
pdf(plot_string, onefile = TRUE)

ggplot(us_coast_proj) + geom_sf() +
  geom_point(data=synoptic, aes(x = X * 1000, y = Y * 1000, colour=survey), size=0.2)+
  geom_point(data=filter(synoptic, survey=="DFO_Pacific"), aes(x=X*1000, y=Y*1000), colour="#7CAE00", size=0.2)+
  geom_point(data=filter(synoptic, survey=="afsc"), aes(x=X*1000, y=Y*1000), colour="#F8766D", size=0.2)+
  ylim(ylimits[1], ylimits[2]) +
  scale_x_continuous(breaks = c(-180, -120), limits = xlimits) +
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
  guides(colour = guide_legend(override.aes = list(size=8)))

dev.off()
