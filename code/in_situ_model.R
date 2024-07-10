library(tidync)
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)
library(sdmTMB)
library(sdmTMB)
library(seacarb)
library(respR)
library(ggplot2)
library(Metrics)
library(rnaturalearth)
library(gridExtra)
library(RColorBrewer)

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Get WD 
basewd <-getwd()
source("code/util_funs_full.R")


## Pull in combined in situ data
env_data <- readRDS("insitu_combined.rds")


#Log O2 and get DOY
env_data$O2_umolkg_ln <- log(env_data$O2_umolkg)
env_data$depth_ln <- log(env_data$depth)
env_data$sigma0_exp <- exp(env_data$sigma0_kgm3)
env_data$doy <- as.POSIXlt(env_data$date, format = "%Y-%b-%d")$yday

#Remove alaska for now, until we figure out what's going on with the units

env_data <- subset(env_data, survey!="afsc")
filtered_data <- subset_DO(dat = env_data)

# set test data
test_survey <- "nwfsc"
test_year <- 2014
test_data <- filtered_data$density_O2 %>%
  filter(survey == test_survey & year == test_year)

train_survey = unique(filtered_data$density_O2$survey)
train_data <- filtered_data$density_O2 %>%
  filter(!(survey == test_survey & year == test_year ))

train_data <- as.data.frame(train_data)
test_data <- as.data.frame(test_data)

spde <- make_mesh(data = train_data, xy_cols = c("X","Y"), cutoff = 30)

#Run alternative models on training data
#+ s(month, bs = "cc", k = 4) + s(depth_ln),

m_8 <- sdmTMB(formula = O2_umolkg_ln  ~ 0  +as.factor(survey) + s(sigma0_exp)+ s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train_data, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")



###RMSE on test data
#Remove NAs
test_data <- test_data %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)
test_data <- as.data.frame(test_data)

#Predict onto test data
O2_model <- m_8

test_predict_O2 <- predict(O2_model, newdata = test_data)
train_predict_O2 <- predict(O2_model, newdata = train_data)
rsq(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)
rsq(train_predict_O2$O2_umolkg_ln, train_predict_O2$est)
rmse(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)/(max(test_predict_O2$O2_umolkg_ln)- min(test_predict_O2$O2_umolkg_ln))
rmse(train_predict_O2$O2_umolkg_ln, train_predict_O2$est)/(max(train_predict_O2$O2_umolkg_ln)- min(train_predict_O2$O2_umolkg_ln))

test_predict_O2$residual = test_predict_O2$O2_umolkg - exp(test_predict_O2$est)
#Plot ####
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")

# crop if you want; not needed:
us_coast <- st_crop(map_data,
                    c(xmin = -126, ymin = 31, 
                      xmax = -110, ymax = 50))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(282853, 1025581)
ylimits = c(3549000, 5366000)

data_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = test_predict_O2, aes(x = X * 1000, y = Y * 1000, col = O2_umolkg), size = 1.0, alpha = 1.0) +
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
data_plot
predict_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = test_predict_O2, aes(x = X * 1000, y = Y * 1000, col = exp(est)), size = 1.0, alpha = 1.0) +
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
predict_plot
residual_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = test_predict_O2, aes(x = X * 1000, y = Y * 1000, col = residual), size = 1.0, alpha = 1.0) +
  scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_distiller(palette = "RdBu", limits = c(-50, 50)) +
#, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
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
residual_plot

grid.arrange(data_plot, predict_plot, residual_plot, ncol = 3)
