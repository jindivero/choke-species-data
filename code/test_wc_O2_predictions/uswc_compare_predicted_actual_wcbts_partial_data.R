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

# get range of WCBTS
wcbts <- env_data %>% filter(survey == "nwfsc")
maxlat = max(wcbts$latitude)
minlat = min(wcbts$latitude)
maxlon = max(wcbts$longitude)
minlon = min(wcbts$longitude)

#Log O2 and get DOY
env_data$O2_umolkg_ln <- log(env_data$O2_umolkg)
env_data$depth_ln <- log(env_data$depth)
env_data$sigma0_exp <- exp(env_data$sigma0_kgm3)
env_data$doy <- as.POSIXlt(env_data$date, format = "%Y-%b-%d")$yday

# Extract data within lat and lon range
wc_env_data <- env_data %>% 
  filter((latitude >= minlat & latitude<= maxlat) & (longitude >= minlon & longitude <= maxlon))


# get sampling tows with DO data
filtered_data <- subset_DO(dat = wc_env_data)

yearlist <- 2010:2015
rsqlist <- rmselist <- rep(NA, length(yearlist))
# set test data
test_survey <- "nwfsc"

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

output <- list()
set.seed(123)
# Loop through years ####

plot_string <-"figures/test_predictions/wconly_wcbts_predict_partial.pdf"
pdf(plot_string, onefile = TRUE)

for (i in 1:length(yearlist)) {
## Separate test and training data ####  
test_year <- yearlist[i]
region_year_data <- filtered_data$density_O2 %>%
  filter(survey == test_survey & year == test_year)

train_data <- filtered_data$density_O2 %>%
  filter(!(survey == test_survey & year == test_year ))

# randomly add one half of the region_year back into training data

n_samples <- floor(0.5 * nrow(region_year_data))
keep_index <- sample(1:nrow(region_year_data), size = n_samples )

test_data <- region_year_data[keep_index,]
train_data <- rbind(train_data, 
                    region_year_data[-keep_index,])

# set minimum sigma0 ####
minsigma0 <- 24 # very few data lower than this, messes up smoother
train_data$sigma0_kgm3[train_data$sigma0_kgm3<=minsigma0] <- minsigma0

train_data <- as.data.frame(train_data)
test_data <- as.data.frame(test_data)

## Make Mesh and fit model ####
spde <- make_mesh(data = train_data, xy_cols = c("X","Y"), n_knots = 200)
m_st <- sdmTMB(formula =  O2_umolkg  ~ 0 + s(temperature_C) + s(sigma0_kgm3) + s(depth_ln) + s(doy),
              mesh = spde,
              data = train_data, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal = "iid")


#Remove NAs
test_data <- test_data %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)
test_data <- as.data.frame(test_data)

## Predict onto test data ####
O2_model <- m_st
# save this in a list
tmp.output <- list(test_data = test_data, train_data = train_data, O2_model = O2_model)


test_predict_O2 <- predict(O2_model, newdata = test_data)

rsqlist[i] <- rsq(test_predict_O2$O2_umolkg, test_predict_O2$est)
rmselist[i] <- rmse(test_predict_O2$O2_umolkg, test_predict_O2$est)

test_predict_O2$residual = test_predict_O2$O2_umolkg - (test_predict_O2$est)

## Plot ####
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

predict_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = test_predict_O2, aes(x = X * 1000, y = Y * 1000, col = (est)), size = 1.0, alpha = 1.0) +
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

## put all plots in one #### 
grid.arrange(data_plot, predict_plot, residual_plot, ncol = 3)
# plot residuals vs. prediction
resid_vs_pred <- ggplot(data = test_predict_O2, aes(x = (est), y = residual, col = latitude)) +
  geom_point() +
  scale_colour_viridis_c(limits = c(31, 50), oob = scales::squish, name = "latitude", breaks = c(35, 40, 45)) +
  ggtitle(test_year) +
  labs(x = "Predicted", y = "Residual") + 
  theme(legend.position="none")
pred_vs_actual <- ggplot(data = test_predict_O2, aes(x = O2_umolkg, y = est, col = latitude)) +
  geom_point() +
  scale_colour_viridis_c(limits = c(31, 50), oob = scales::squish, name = "latitude", breaks = c(35, 40, 45)) +
  ggtitle(test_year) +
  labs(x = "Observed", y = "Predicted")+
  geom_abline(intercept = 0, slope = 1) +
  theme(legend.position="none")
grid.arrange(pred_vs_actual, resid_vs_pred, ncol = 2)

}
dev.off()

