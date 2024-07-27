library(tidync)
library(sf)
library(dplyr)
library(tidyr)
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
filename = "code/test_wc_O2_predictions/wconly_wcbts_predict_full.Rdata"
plot_string <-"figures/test_predictions/wconly_wcbts_predict_full.pdf"
plotmodel = T

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Get WD 
source("code/util_funs.R")
source("code/test_wc_O2_predictions/helper_funs.R")


## Pull in combined in situ data
dat <- readRDS("data/processed_data/all_o2_dat.rds")


# remove rows with na
dat <- dat %>%
  drop_na(temp, do, sigma0, depth)

# set minimum sigma0
minsigma0 <- 24
dat$sigma0[dat$sigma0<=minsigma0] <- minsigma0

# get range of WCBTS
wcbts <- dat %>% filter(survey == "nwfsc")
maxlat = max(wcbts$Y*1.1) # added a small buffer to include 
minlat = min(wcbts$Y)
maxlon = max(wcbts$X)
minlon = min(wcbts$X)
maxdepth <- max(wcbts$depth)
mindoy <- min(wcbts$doy) - 14
maxdoy <- min(wcbts$doy) + 14
#Log O2 and get DOY
dat$depth_ln <- log(dat$depth)
#dat$doy <- as.POSIXlt(dat$date, format = "%Y-%b-%d")$yday

survey.2.use <- c("iphc",
                  "nwfsc",
                  "NewportLine",
                  "calCOFI",
                  "LineP",
                  "wcoa",
                  "codap",
                  "ocnms")
# Extract data within lat and lon range
wc_dat <- dat %>% 
  filter((Y >= minlat & Y<= maxlat) & (X >= minlon & X <= maxlon),
         (doy >= mindoy & doy <=maxdoy),
         depth <= maxdepth,
         survey %in% survey.2.use)


tyearlist <- 2010:2015
n_testlist <- rsqlist <- rmselist <- rep(NA, length(yearlist))
# set test data
test_survey <- "nwfsc"

# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")
dat.2.use <- wc_dat
# crop if you want; not needed:
us_coast <- st_crop(map_data,
                    c(xmin = -126, ymin = 31, 
                      xmax = -110, ymax = 50))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(282853, 1025581)
ylimits = c(3549000, 5366000)

output <- list()
# Loop through years ####
if(fitmodel) {
  if(plotmodel) pdf(plot_string, onefile = TRUE)


for (i in 1:length(yearlist)) {
  ## Separate test and training data ####
  test_year <- yearlist[i]
  test_data <- dat.2.use %>%
    filter(survey == test_survey & year == test_year)
  train_data <- dat.2.use %>%
    filter(!(survey == test_survey & year == test_year))
  
  train_data <- as.data.frame(train_data)
  test_data <- as.data.frame(test_data)
  
  ## Make Mesh and fit model ####
  spde <- make_mesh(data = train_data,
                    xy_cols = c("X", "Y"),
                    cutoff = 45)
  
  m_8 <- sdmTMB(
    formula = do  ~ 0 + as.factor(survey) + s(sigma0) + s(temp) +  s(depth_ln) + s(doy),
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
  
  rsqlist[i] <- rsq(test_predict_O2$do, test_predict_O2$est)
  rmselist[i] <- rmse(test_predict_O2$do, test_predict_O2$est)
  n_testlist[i] <- nrow(test_predict_O2)
  test_predict_O2$residual = test_predict_O2$do - (test_predict_O2$est)
  
  plot_predict(test_predict_O2, test_year, us_coast_proj)
  
}
if(plotmodel) dev.off()


if (savemodel) {
  save(x = output, file = filename)
  
}
}

# get overall RMSE
rmse2 <- rmselist ^ 2
xminusxbarsq <- (n_testlist) * rmse2
rmse_total <- sqrt(sum(xminusxbarsq) / sum(n_testlist))
print(rmse_total)
