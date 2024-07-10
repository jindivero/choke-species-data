library(tidync)
library(lubridate)
library(sf)
library(dplyr)
library(sdmTMB)
library(seacarb)
library(respR)
library(ggplot2)
library(rnaturalearth)
library(gridExtra)
basewd <- getwd()


### Load newport data
newport_data <- readRDS("test_data/data/newport_bottom.RDS")

# need to add latitude, depth, and convert DO to mmol
newport_lat <- 44.65
newport_data$latitude <- newport_lat
newport_data$year <- year(newport_data$sample_date)
newport_data$doy <- as.POSIXlt(newport_data$sample_date, format = "%Y-%b-%d")$yday
newport_data$depth <- p2d(lat = newport_lat,
                          p = newport_data$pressure..dbar.)


#  get lat and long in UTC coordinates
newport_data <- newport_data %>%
  st_as_sf(coords=c('longitude..degW.','latitude'),crs=4326,remove = F) %>%  
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 
source("test_data/code/convert_funs.R")

newport_data$sigma <- calc_sigma( s = newport_data$practical.salinity,
                                  t = newport_data$temperature..degC.,
                                  p = newport_data$pressure..dbar.)
newport_data$o2 <- convert_o2(newport_data$dissolved.oxygen..ml.L., newport_data$sigma)

newport_data <- as.data.frame(newport_data)

# filter data between May 1 and Oct 15
newport_data <- newport_data %>%
  filter(doy >= as.POSIXlt("2019-05-01")$yday,
         doy <= as.POSIXlt("2019-10-15")$yday)

# get sampling days for each year, 2017 - 2021
yearlist <- 2017:2021
newport_days <- list(year = NULL, doys = NULL)
for (i in 1:length(yearlist)) {
  newport_data_year <- dplyr::filter(newport_data, year == yearlist[i])
  newport_days$year[i] <- yearlist[i]
  newport_days$doys[[i]] <- unique(newport_data_year$doy)
}


### 1) Set WD to folder with raw GLORYS data
setwd("/Users/essing/Library/CloudStorage/Dropbox/for_tim")

### 2) Loop through all years, extract and format glorys data ####
run_all_years <- T
do_threshold <- 0

if (run_all_years) {
  setwd("/Users/essing/Library/CloudStorage/Dropbox/for_tim")
  files <- list.files("/Users/essing/Library/CloudStorage/Dropbox/for_tim")
  convert_glorys <- function(file_name, doy.2.use, do_threshold) {
    nc <- tidync(file_name)
    nc_df <- nc %>%
      hyper_tibble %>%
      group_by(longitude, latitude) %>%
      filter(depth == max(depth)) %>%
      ungroup()
    # replace DO below threshold with the threshold level
    nc_df <- nc_df %>%
      mutate(o2 = case_when(
        o2 < do_threshold ~ do_threshold,
        TRUE ~ o2  # Keep other values unchanged
      ))
    
    # remove large list from memory
    rm(nc)
   
   
    nc_df$time <- (as_datetime("1950-01-01")+hours(nc_df$time))
    nc_df$doy <- as.POSIXlt(nc_df$time, format = "%Y-%b-%d")$yday
    
    nc_df <- nc_df %>%
      filter(doy %in% doy.2.use)
    
    nc_df <- nc_df %>%
      st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
      st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
      mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>% 
      st_set_geometry(NULL)
    
    nc_df$year <- year(nc_df$time)
    return(nc_df)
  }
  # get first year of glorys
  yearly_glorys <- convert_glorys(files[1], doy.2.use = newport_days$doys[[1]], do_threshold)
  # get remaining years of glorys and combine into single data frame
  for (i in 2:length(files)) {
    tmp_glorys <- convert_glorys(files[i], doy.2.use = newport_days$doys[[i]], do_threshold)
    yearly_glorys <- rbind(yearly_glorys, tmp_glorys)
  }
}
# remove unused file
rm(tmp_glorys)

# move back to base directory
setwd(basewd)
glory_years <- unique(yearly_glorys$year)

for (year_index in 1:length(yearlist)) {

### Loop through days of sampling, extract glorys, fit model, and get newport predictions ####

# make plot of glorys predictions on a map for each doy
doy.2.use <- newport_days$doy[[year_index]]

# prepare for plotting ####
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")

# crop if you want; not needed:
us_coast <- st_crop(map_data,
                    c(xmin = -126, ymin = 31, 
                      xmax = -110, ymax = 50))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(282853, 1025581)
ylimits = c(3549000, 5366000)

plot_string <- paste("glory_newport_compare_", yearlist[year_index], ".pdf", sep = "")
pdf(plot_string, onefile = TRUE)
### Begin Loop ####
for (i in 1:length(doy.2.use)) {
# get GLORYS data for each day
red_glorys <- yearly_glorys %>%
  filter(year == glory_years[year_index],
         doy == doy.2.use[i])

# plot GLORYS on map ####

o2_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = red_glorys, aes(x = X * 1000, y = Y * 1000, col = o2), size = 2.0, alpha = 1.0) +
  scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_viridis_c(limits = c(0, 200), oob = scales::squish, name = bquote(O[2]), breaks = c(0, 100, 200)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        , strip.background = element_blank()
        , strip.text = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title= element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom") + 
  guides(colour = guide_colourbar(title.position="top", title.hjust=0.5))


# extract newport line data for this sample date
newport_doy <- newport_data %>%
  filter(year == yearlist[year_index],
         doy == doy.2.use[i])
# plot these data on same map projection as above
o2_plot_with_line <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = newport_doy, aes(x = X * 1000, y = Y * 1000, col = o2), size = 2.0, alpha = 1.0) +
  scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_viridis_c(limits = c(0, 200), oob = scales::squish, name = bquote(O[2]), breaks = c(0, 100, 200)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        , strip.background = element_blank()
        , strip.text = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title= element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom") + 
  guides(colour = guide_colourbar(title.position="top", title.hjust=0.5))
  
  
# combine into two plots
grid.arrange(o2_plot, o2_plot_with_line, ncol = 2)
}
dev.off()
}
