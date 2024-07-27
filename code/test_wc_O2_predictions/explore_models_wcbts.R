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

### set mapping parameters ####
# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")

# crop if you want; not needed:
us_coast <- st_crop(map_data,
                    c(xmin = -126, ymin = 31, 
                      xmax = -110, ymax = 50))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(282853, 1025581)
ylimits = c(3549000, 5366000)


source("code/util_funs_full.R")


# Pull in combined in situ data ####
env_data <- readRDS("data/processed_data/insitu_combined.rds")
# log O2 and get DOY #### 
env_data$depth_ln <- log(env_data$depth)
env_data$doy <- as.POSIXlt(env_data$date, format = "%Y-%b-%d")$yday
env_data <- env_data %>%
  drop_na(depth, temperature_C, sigma0_kgm3, O2_umolkg)
# set minimum sigma0
minsigma0 <- 24
env_data$sigma0_kgm3[env_data$sigma0_kgm3<=minsigma0] <- minsigma0



## get range of WCBTS ####
wcbts <- env_data %>% filter(survey == "nwfsc")

maxlat = max(wcbts$latitude)
minlat = min(wcbts$latitude)
maxlon = max(wcbts$longitude)
minlon = min(wcbts$longitude)

# Extract data within lat and lon range
wc_env_data <- env_data %>% 
  filter((latitude >= minlat & latitude<= maxlat) & (longitude >= minlon & longitude <= maxlon),
         survey %in% c("nwfsc", "iphc"),
         year %in% 2010:2015)





wc_env_data <- as.data.frame(wc_env_data)
## Make Mesh and fit model ####
spde <- make_mesh(data = wc_env_data, xy_cols = c("X","Y"), n_knots= 200)

#Run alternative models on training data
#+ s(month, bs = "cc", k = 4) + s(depth_ln),
formula.2.use <- "O2_umolkg  ~ 0 + as.factor(survey) + as.factor(year) + s(temperature_C) + s(sigma0_kgm3) + s(depth_ln) + s(doy)"

m_s <- sdmTMB(formula = as.formula(formula.2.use),
              mesh = spde,
              data = wc_env_data, 
              family = gaussian(), 
              spatial = "on")
formula.2.use <- "O2_umolkg  ~ 0  + s(temperature_C) + s(sigma0_kgm3) + s(depth_ln) + s(doy)"
m_st <- sdmTMB(formula = as.formula(formula.2.use),
              mesh = spde,
              data = wc_env_data, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")
AIC(m_s, m_st)


model.2.use <- m_st
# Evaluate Smoothers ####
temp_test <- tibble(year = 2011,
                    sigma0_kgm3 = mean(wc_env_data$sigma0_kgm3),
                    temperature_C = seq(min(wc_env_data$temperature_C), max(wc_env_data$temperature_C), length.out = 100),
                    depth_ln = mean(wc_env_data$depth_ln),
                    doy = mean(wc_env_data$doy),
                    Y = mean(wc_env_data$Y),
                    X = mean(wc_env_data$X))
depth_test <- tibble(year = 2011,
                     sigma0_kgm3 = mean(wc_env_data$sigma0_kgm3),
                     temperature_C = mean(wc_env_data$temperature_C),
                     depth_ln = seq(min(wc_env_data$depth_ln), max(wc_env_data$depth_ln), length.out = 100),
                     doy = mean(wc_env_data$doy),
                     Y = mean(wc_env_data$Y),
                     X = mean(wc_env_data$X))
doy_test <- tibble(year = 2011,
                   sigma0_kgm3 = mean(wc_env_data$sigma0_kgm3),
                   temperature_C = mean(wc_env_data$temperature_C),
                   depth_ln = mean(wc_env_data$depth_ln),
                   doy = seq(min(wc_env_data$doy), max(wc_env_data$doy), length.out = 100),
                   Y = mean(wc_env_data$Y),
                   X = mean(wc_env_data$X))
sigma0_test <- tibble(year = 2011,
                      sigma0_kgm3 = seq(min(wc_env_data$sigma0_kgm3), max(wc_env_data$sigma0_kgm3), length.out = 100),
                      temperature_C = mean(wc_env_data$temperature_C),
                      depth_ln = mean(wc_env_data$depth_ln),
                      doy = mean(wc_env_data$doy),
                      Y = mean(wc_env_data$Y),
                      X = mean(wc_env_data$X))

temp_predict <- predict(model.2.use, newdata = temp_test)
depth_predict <- predict(model.2.use, newdata = depth_test)
doy_predict <- predict(model.2.use, newdata = doy_test)
sigma0_predict<- predict(model.2.use, newdata = sigma0_test)
tempplot <- ggplot(data = temp_predict, aes(x = temperature_C, y = est)) +
  geom_line()

depthplot <- ggplot(data = depth_predict, aes(x = depth_ln, y = est)) +
  geom_line()

doyplot <- ggplot(data = doy_predict, aes(x = doy, y = est)) +
  geom_line()

sigma0plot <- ggplot(data = sigma0_predict, aes(x = sigma0_kgm3, y = est)) +
  geom_line()

grid.arrange(tempplot, depthplot, doyplot, sigma0plot, ncol = 2, nrow = 2)



# Plot sigma0
ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = env_data, aes(x = X * 1000, y = Y * 1000, col = sigma0_kgm3),  size = 1.0, alpha = 0.5) +
      scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
      ylim(ylimits[1], ylimits[2]) +
      scale_colour_viridis_c(limits = c(25, 28), oob = scales::squish, name = "sigma0", breaks = c(25, 26, 27, 28)) +
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
