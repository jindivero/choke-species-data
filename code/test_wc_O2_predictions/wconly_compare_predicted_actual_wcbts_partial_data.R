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

fitmodel = T
savemodel = T
plotmodel = T
filename = "code/test_wc_O2_predictions/wconly_wcbts_predict_partial.Rdata"
plot_string <-"figures/test_predictions/wconly_wcbts_predict_partial.pdf"
### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Get WD 
source("code/util_funs.R")
source("code/test_wc_O2_predictions/helper_funs.R")


## Pull in combined in situ data
dat <- readRDS("data/processed_data/insitu_combined.rds")

# get range of WCBTS
wcbts <- dat %>% filter(survey == "nwfsc")
maxlat = max(wcbts$latitude)
minlat = min(wcbts$latitude)
maxlon = max(wcbts$longitude)
minlon = min(wcbts$longitude)

#Log O2 and get DOY
dat$depth_ln <- log(dat$depth)
dat$doy <- as.POSIXlt(dat$date, format = "%Y-%b-%d")$yday

# Extract data within lat and lon range
wc_dat <- dat %>% 
  filter((latitude >= minlat & latitude<= maxlat) & (longitude >= minlon & longitude <= maxlon),
         survey %in% c("iphc", "nwfsc"))


# get sampling tows with DO data
filtered_dat <- wc_dat %>%
  drop_na(O2_umolkg, temperature_C, sigma0_kgm3, depth)


yearlist <- 2010:2015
n_testlist <- rsqlist <- rmselist <- rep(NA, length(yearlist))
# set test data
test_survey <- "nwfsc"

# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")
dat.2.use <- filtered_dat
# crop if you want; not needed:
us_coast <- st_crop(map_data,
                    c(xmin = -126, ymin = 31, 
                      xmax = -110, ymax = 50))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(282853, 1025581)
ylimits = c(3549000, 5366000)

output <- list()
set.seed(789)
# Loop through years ####
if(fitmodel) {
  if(plotmodel) pdf(plot_string, onefile = TRUE)
for (i in 1:length(yearlist)) {
  ## Separate test and training data ####
  test_year <- yearlist[i]
  region_year_data <- dat.2.use %>%
    filter(survey == test_survey & year == test_year)
  
  train_data <- dat.2.use %>%
    filter(!(survey == test_survey & year == test_year))
  
  # randomly add one half of the region_year back into training data
  
  n_samples <- floor(0.5 * nrow(region_year_data))
  keep_index <- sample(1:nrow(region_year_data), size = n_samples)
  
  test_data <- region_year_data[keep_index, ]
  train_data <- rbind(train_data, region_year_data[-keep_index, ])
  
  
  train_data <- as.data.frame(train_data)
  test_data <- as.data.frame(test_data)
  
  ## Make Mesh and fit model ####
  spde <- make_mesh(data = train_data,
                    xy_cols = c("X", "Y"),
                    cutoff = 45)
  
  m_8 <- sdmTMB(
    formula = O2_umolkg  ~ 0  + as.factor(survey) + s(sigma0_kgm3) + s(temperature_C) +  s(depth_ln) + s(doy),
    mesh = spde,
    data = train_data,
    family = gaussian(),
    time = "year",
    spatial = "on",
    spatiotemporal  = "IID"
  )
  
  test_data <- as.data.frame(test_data)
  
  ## Predict onto test data ####
  O2_model <- m_8
  # save this in a list
  tmp.output <- list(test_data = test_data,
                     train_data = train_data,
                     O2_model = O2_model)
  output[[i]] <- tmp.output
  
  # summarize predictions
  test_predict_O2 <- predict(O2_model, newdata = test_data)
  
  rsqlist[i] <- rsq(test_predict_O2$O2_umolkg, test_predict_O2$est)
  rmselist[i] <- rmse(test_predict_O2$O2_umolkg, test_predict_O2$est)
  n_testlist[i] <- nrow(test_predict_O2)
  test_predict_O2$residual = test_predict_O2$O2_umolkg - (test_predict_O2$est)
  
  plot_predict(test_predict_O2, test_year, us_coast_proj)
  
}
if (plotmodel) dev.off()


if (save_model) {
  save(x = output, file = filename)
  
}
}

# get overall RMSE
rmse2 <- rmselist ^ 2
xminusxbarsq <- (n_testlist) * rmse2
rmse_total <- sqrt(sum(xminusxbarsq) / sum(n_testlist))
print(rmse_total)
