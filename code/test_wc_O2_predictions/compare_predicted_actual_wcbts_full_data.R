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

fitmodel = TRUE
save_model = TRUE
plotmodel = T

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())

# filename for saving / loading
filename = "code/test_wc_O2_predictions/wcbts_predict_fullyear.Rdata"
plot_string <-"figures/test_predictions/wcbts_predict_fullyear.pdf"
#Get WD
source("code/util_funs.R")
source("code/test_wc_O2_predictions/helper_funs.R")

## Pull in combined in situ data
# Pull in combined in situ data ####
dat <- readRDS("data/processed_data/insitu_combined.rds")

# remove any rows with missing data
dat <- dat %>%
  drop_na(O2_umolkg, temperature_C, sigma0_kgm3, depth)


# log O2 and get DOY ####
dat$depth_ln <- log(dat$depth)
dat$doy <- as.POSIXlt(dat$date, format = "%Y-%b-%d")$yday


# set minimum sigma0
minsigma0 <- 24
dat$sigma0_kgm3[dat$sigma0_kgm3 <= minsigma0] <- minsigma0

yearlist <- 2010:2015
# set test data
test_survey <- "nwfsc"

# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

# crop if you want; not needed:
us_coast <- st_crop(map_data, c(
  xmin = -126,
  ymin = 31,
  xmax = -110,
  ymax = 50
))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(282853, 1025581)
ylimits = c(3549000, 5366000)


n_testlist <- rsqlist <- rmselist <- rep(NA, length(yearlist))
output <- list()
set.seed(1234)

# Loop through years ####
if (fitmodel) {
  if(plotmodel)    pdf(plot_string, onefile = TRUE)
  
  
  for (i in 1:length(yearlist)) {
    ## Separate test and training data ####
    test_year <- yearlist[i]
    test_data <- dat %>%
      filter(survey == test_survey & year == test_year)
    
    train_data <- dat %>%
      filter(!(survey == test_survey & year == test_year))
    
    train_data <- as.data.frame(train_data)
    test_data <- as.data.frame(test_data)
    
    ## Make Mesh and fit model ####
    spde <- make_mesh(data = train_data,
                      xy_cols = c("X", "Y"),
                      cutoff = 45)
    
    #Run alternative models on training data
    #+ s(month, bs = "cc", k = 4) + s(depth_ln),
    
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
    
    #@ Plot #### 
    if (plotmodel) plot_predict(test_predict_O2, test_year, us_coast_proj)
    
  }
  dev.off()
  
  # save model output to speed up plotting later
 
  if (save_model) {
    save(x = output, file = filename)
    
  }
}

# run this code if fitmodel = F ####
if (!fitmodel) {
  load(file = filename)
  if(plotmodel) pdf(plot_string, onefile = TRUE)
  
  for (i in 1:length(yearlist)) {
    test_year <- yearlist[i]
    ## Separate test and training data ####
    train_data <- output[[i]]$train_data
    test_data <- output[[i]]$test_data
    O2_model <- output[[i]]$O2_model
    
    test_predict_O2 <- predict(O2_model, newdata = test_data)
    test_predict_O2$residual = test_predict_O2$O2_umolkg - (test_predict_O2$est)
    n_testlist[i] <- nrow(test_predict_O2)
    rmselist[i] <- rmse(test_predict_O2$O2_umolkg, test_predict_O2$est)
    #@ Plot ####
    plot_predict(test_predict_O2, test_year, us_coast_proj)
    
  }
  
  # get overall rmse
  rmse2 <- rmselist ^ 2
  xminusxbarsq <- (n_testlist) * rmse2
  rmse_total <- sqrt(sum(xminusxbarsq) / sum(n_testlist))
  print(rmse_total)
  
  if(plotmodel)  dev.off()
  
}
