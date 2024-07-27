library(tidync)
library(lubridate)
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
### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### set mapping parameters ####
# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
#xlimits = c(282853, 1025581)
#ylimits = c(3549000, 5366000)

# save WD ####

source("code/util_funs.R")


# Pull in combined in situ data ####
dat <- readRDS("data/processed_data/insitu_combined.rds")

# remove any rows with missing data
dat <- dat %>%
  drop_na(O2_umolkg, temperature_C, sigma0_kgm3, depth)


# log O2 and get DOY #### 
dat$O2_umolkg_ln <- log(dat$O2_umolkg)
dat$depth_ln <- log(dat$depth)
dat$doy <- as.POSIXlt(dat$date, format = "%Y-%b-%d")$yday


# set minimum sigma0
minsigma0 <- 24
dat$sigma0_kgm3[dat$sigma0_kgm3<=minsigma0] <- minsigma0

dat <- as.data.frame(dat)
## Make Mesh and fit model ####
spde <- make_mesh(data = dat, xy_cols = c("X","Y"), cutoff = 30)

#Run alternative models on training data
#+ s(month, bs = "cc", k = 4) + s(depth_ln),
formula.2.use <- "O2_umolkg  ~ 0 + as.factor(year) + s(temperature_C) + s(sigma0_kgm3) + s(depth_ln) + s(doy)"

m_s <- sdmTMB(formula = as.formula(formula.2.use),
              mesh = spde,
              data = dat, 
              family = gaussian(), 
              spatial = "on")

formula.2.use <- "O2_umolkg  ~ 0  + s(temperature_C) + s(sigma0_kgm3) + s(depth_ln) + s(doy)"
m_st <- sdmTMB(formula = as.formula(formula.2.use),
              mesh = spde,
              data = dat, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")

formula.2.use <- "O2_umolkg  ~ 0  + temperature_C + temperature_C^2 + s(sigma0_kgm3) + s(depth_ln) + doy + doy^2"
m_sv <- sdmTMB(formula = as.formula(formula.2.use),
               mesh = spde,
               data = dat, 
               family = gaussian(), 
               spatial_varying = ~0 + temperature_C + temperature_C^2 + doy + doy^2,
               time = "year",
               spatial = "on",
               spatiotemporal  = "IID")

AIC(m_s, m_st, m_sv)

# get mean X and Y of the nwfsc data for plotting purposes
wcbts<- dat %>% 
  filter(survey == "nwfsc")
X.2.use <-mean(wcbts$X)
Y.2.use <- mean(wcbts$Y)


model.2.use <- m_sv
# Evaluate Smoothers ####
temp_test <- tibble(year = 2010,
                    sigma0_kgm3 = mean(dat$sigma0_kgm3),
                    temperature_C = seq(min(dat$temperature_C), max(dat$temperature_C), length.out = 100),
                    depth_ln = mean(dat$depth_ln),
                    doy = mean(dat$doy),
                    Y = Y.2.use,
                    X = X.2.use)
depth_test <- tibble(year = 2010,
                     sigma0_kgm3 = mean(dat$sigma0_kgm3),
                     temperature_C = mean(dat$temperature_C),
                     depth_ln = seq(min(dat$depth_ln), max(dat$depth_ln), length.out = 100),
                     doy = mean(dat$doy),
                     Y = mean(dat$Y),
                     X = mean(dat$X))
doy_test <- tibble(year = 2010,
                   sigma0_kgm3 = mean(dat$sigma0_kgm3),
                   temperature_C = mean(dat$temperature_C),
                   depth_ln = mean(dat$depth_ln),
                   doy = seq(min(dat$doy), max(dat$doy), length.out = 100),
                   Y = Y.2.use,
                   X = X.2.use)
sigma0_test <- tibble(year = 2010,
                      sigma0_kgm3 = seq(min(dat$sigma0_kgm3), max(dat$sigma0_kgm3), length.out = 100),
                      temperature_C = mean(dat$temperature_C),
                      depth_ln = mean(dat$depth_ln),
                      doy = mean(dat$doy),
                      Y = mean(dat$Y),
                      X = mean(dat$X))

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




