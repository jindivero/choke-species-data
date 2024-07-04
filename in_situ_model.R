library(tidync)
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)
install.packages("sdmTMB", type="source")
library(sdmTMB)
library(seacarb)
library(respR)
library(ggplot2)
install.packages("Metrics")
library(Metrics)

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Set WD 
basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"
setwd(basewd)

#Function
rsq <- function (x, y) cor(x, y) ^ 2

## Pull in combined in situ data
density_O2 <- readRDS("insitu_combined.rds")

#Log O2 and get DOY
density_O2$O2_umolkg_ln <- log(density_O2$O2_umolkg)
density_O2$depth_ln <- log(density_O2$depth)
density_O2$sigma0_exp <- exp(density_O2$sigma0_kgm3)
density_O2$doy <- as.POSIXlt(density_O2$date, format = "%Y-%b-%d")$yday

#Remove alaska for now, until we figure out what's going on with the units
density_O2 <- subset(density_O2, survey!="afsc")

#Subset to data with DO available and no DO
density_no_O2 <- density_O2 %>% 
  filter(is.na(do_mlpL)| do_mlpL<=0)

density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)

#Remove NAs (was causing issues with smoother if had NAs)
density_O2 <- density_O2 %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)

#Separate train and test data
set.seed(7)
density_O2$test <- rbinom(n = nrow(density_O2), size = 1, prob = 0.3)
train <- density_O2[density_O2$test == 0,]
test <- density_O2[density_O2$test == 1,]

train <- as.data.frame(train)
test <- as.data.frame(test)

spde <- make_mesh(data = train, xy_cols = c("X","Y"), cutoff = 30)

#Run alternative models on training data
#+ s(month, bs = "cc", k = 4) + s(depth_ln),
#Best-fitting from Thompson (but depth instead of pressure)
m_1 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(temperature_C)+ s(sigma0_exp)+s(depth_ln)+s(month, bs = "cc", k = 4),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")

m_2 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) + s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")


m_3 <- sdmTMB(formula = O2_umolkg_ln  ~ 0+as.factor(year) + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(month, bs = "cc", k = 4),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")

m_4 <- sdmTMB(formula = O2_umolkg_ln  ~ 0+as.factor(year) + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              #time = "year",
              spatial = "on",
              spatiotemporal  = "off")

m_5 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(month, bs = "cc", k = 4),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")

m_6 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")

m_7 <- sdmTMB(formula = O2_umolkg_ln  ~ 0  + s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")

m_8 <- sdmTMB(formula = O2_umolkg_ln  ~ 0  +as.factor(survey)+s(sigma0_exp)+ s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")


#Taking forever
# m_5 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(month, bs = "cc", k = 4),
#               mesh = spde,
#               data = train, 
#               family = gaussian(), 
#               time = "year",
#               spatial = "on",
#               spatiotemporal  = "AR1")

#AIC
AIC(m_1, m_2, m_3, m_4, m_5, m_6)
AIC(m_6, m_7, m_8)

#m_6 is best-fitting

##Refine doy smoother
#Total number of days of year in dataset
length(unique(train$doy))

m_6b <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(doy, k=150),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")

#k=5 worse, 50 slightly better, 150=-6506, 15=-6504
AIC(m_6, m_6b)

###RMSE on test data
#Remove NAs
test <- test %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)
test <- as.data.frame(test)

#Predict onto test data

O2_model <- m_8

test_predict_O2 <- predict(O2_model, newdata = test)
train_predict_O2 <- predict(O2_model, newdata = train)
rsq(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)
rsq(train_predict_O2$O2_umolkg_ln, train_predict_O2$est)
rmse(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)/(max(test_predict_O2$O2_umolkg_ln)- min(test_predict_O2$O2_umolkg_ln))
rmse(train_predict_O2$O2_umolkg_ln, train_predict_O2$est)/(max(train_predict_O2$O2_umolkg_ln)- min(train_predict_O2$O2_umolkg_ln))

#Plot
  ggplot(test_predict_O2, aes(x = O2_umolkg, y = exp(est)))+
  geom_point(size=0.8)+
  geom_abline(slope = 1, intercept = 0, color = 1)+
  ylab("predicted O2")+
  geom_point(train_predict_O2, mapping=aes(x=O2_umolkg, y=exp(est)), colour="blue", size=0.8, alpha=0.7)
  
  test_predict_O2 %>% 
    mutate(resid = scale(O2_umolkg_ln-est)) %>% 
    ggplot(aes(x=O2_umolkg, y = -depth, color = resid))+
    geom_jitter()+
    scale_color_viridis_c()+
    facet_wrap("survey")
  
  test_predict_O2 %>% 
    mutate(resid = scale(O2_umolkg_ln-est)) %>% 
    ggplot(aes(x=O2_umolkg, y = -depth, color = resid))+
    geom_jitter()+
    scale_color_viridis_c()+
    facet_wrap("survey")
  
  
  test_predict_O2 %>% 
    mutate(resid = scale(O2_umolkg_ln-est)) %>% 
    ggplot(aes(x=longitude, y = latitude, color = resid), size=0.2)+
    geom_jitter()+
    scale_color_viridis_c()+
    facet_wrap("year")+
    xlim(-180, -100)
  
  ggplot(test_predict_O2, aes(x = O2_umolkg, y = exp(est)))+
    geom_point(size=0.8, aes(colour=survey))+
    geom_abline(slope = 1, intercept = 0, color = 1)+
    ylab("predicted O2")+
    geom_point(train_predict_O2, mapping=aes(x=O2_umolkg, y=exp(est), colour=survey), size=0.8)+
    facet_wrap("survey")

  ggplot(test_predict_O2, aes(x = O2_umolkg, y = exp(est)))+
    geom_point(size=0.8)+
    geom_abline(slope = 1, intercept = 0, color = 1)+
    ylab("predicted O2")+
    geom_point(train_predict_O2, mapping=aes(x=O2_umolkg, y=exp(est)), size=0.8)+
    facet_wrap("year")

#### Test predicting to "unsampled" data
#Combined in situ data
density_O2 <- readRDS("insitu_combined.rds")
  
#Log O2
density_O2$O2_umolkg_ln <- log(density_O2$O2_umolkg)
density_O2$depth_ln <- log(density_O2$depth)
density_O2$sigma0_exp <- exp(density_O2$sigma0_kgm3)
density_O2$doy <- as.POSIXlt(density_O2$date, format = "%Y-%b-%d")$yday
  
#Remove alaska for now
density_O2 <- subset(density_O2, survey!="afsc")

#Data with oxygen
density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)  

#Remove rows with missing data
density_O2 <- density_O2 %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)

#Separate training and testing years
unique(density_O2$year)
#2008--2022

train <- filter(density_O2, year!=2019)
test <- filter(density_O2, year==2019)

train <- as.data.frame(train)
test <- as.data.frame(test)
spde <- make_mesh(data = train, xy_cols = c("X","Y"), cutoff = 30)

#Fit model
m_6 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              extra_time=c(2019),
              spatial = "on",
              spatiotemporal  = "IID")

#Predict
O2_model <- m_6

test_predict_O2 <- predict(O2_model, newdata = test)

rsq(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)
rmse(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)/(max(test_predict_O2$O2_umolkg_ln)- min(test_predict_O2$O2_umolkg_ln))

#Compare
ggplot(test_predict_O2, aes(x = O2_umolkg, y = exp(est), colour=survey))+
  geom_point(size=0.8)+
  geom_abline(slope = 1, intercept = 0, color = 1)+
  ylab("predicted O2")

###Predict to "unsampled" areas
#Combined in situ data
density_O2 <- readRDS("insitu_combined.rds")

#Log O2
density_O2$O2_umolkg_ln <- log(density_O2$O2_umolkg)
density_O2$depth_ln <- log(density_O2$depth)
density_O2$sigma0_exp <- exp(density_O2$sigma0_kgm3)
density_O2$doy <- as.POSIXlt(density_O2$date, format = "%Y-%b-%d")$yday

#Remove alaska for now
density_O2 <- subset(density_O2, survey!="afsc")

#Data with oxygen
density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)  

#Remove rows with missing data
density_O2 <- density_O2 %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)

#Separate training and testing region
train <- density_O2
train$O2_umolkg_ln <- ifelse(train$latitude>50| train$latitude <45, train$O2_umolkg_ln, NA)
test <- filter(density_O2, latitude<50 & latitude >45)

train <- as.data.frame(train)
test <- as.data.frame(test)
spde <- make_mesh(data = train, xy_cols = c("X","Y"), cutoff = 30)

#Fit model
m_6 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")

#Predict
O2_model <- m_6

test_predict_O2 <- predict(O2_model, newdata = test)

rsq(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)
rmse(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)/(max(test_predict_O2$O2_umolkg_ln)- min(test_predict_O2$O2_umolkg_ln))

ggplot(test_predict_O2, aes(x = O2_umolkg, y = exp(est), colour=survey))+
  geom_point(size=0.8)+
  geom_abline(slope = 1, intercept = 0, color = 1)+
  ylab("predicted O2")

####Train to IPHC only and predict to DFO and NWFSC
#Combined in situ data
density_O2 <- readRDS("insitu_combined.rds")

#Log O2
density_O2$O2_umolkg_ln <- log(density_O2$O2_umolkg)
density_O2$depth_ln <- log(density_O2$depth)
density_O2$sigma0_exp <- exp(density_O2$sigma0_kgm3)
density_O2$doy <- as.POSIXlt(density_O2$date, format = "%Y-%b-%d")$yday

#Remove alaska for now
density_O2 <- subset(density_O2, survey!="afsc")

#Data with oxygen
density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)  

#Remove rows with missing data
density_O2 <- density_O2 %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)

#Separate training and testing region
train <- density_O2
train$O2_umolkg_ln <- ifelse(train$survey=="DFO_Pacific"|train$survey=="nwfsc", NA, train$O2_umolkg_ln)
test <- filter(density_O2, survey=="DFO_Pacific"|survey=="nwfsc")

train <- as.data.frame(train)
test <- as.data.frame(test)
spde <- make_mesh(data = train, xy_cols = c("X","Y"), cutoff = 30)

#Fit model
m_6 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")


###Predict to Newport data
#Combined in situ data
density_O2 <- readRDS("insitu_combined.rds")

#Log O2
density_O2$O2_umolkg_ln <- log(density_O2$O2_umolkg)
density_O2$depth_ln <- log(density_O2$depth)
density_O2$sigma0_exp <- exp(density_O2$sigma0_kgm3)
density_O2$doy <- as.POSIXlt(density_O2$date, format = "%Y-%b-%d")$yday

#Remove alaska for now
density_O2 <- subset(density_O2, survey!="afsc")

#Data with oxygen
density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)  

#Remove rows with missing data
density_O2 <- density_O2 %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)

train <- density_O2
train <- as.data.frame(train)

spde <- make_mesh(data = train, xy_cols = c("X","Y"), cutoff = 30)

#Fit model
m_6 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              extra_time=c(1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007),
              spatiotemporal  = "IID")

###Newport data to predict to
# load Newport data
setwd(basewd)
newport_data <- readRDS("test_data/data/newport_bottom.RDS")

# need to add latitude, depth, and convert DO to mmol
newport_lat <- 44.65
newport_data$latitude <- newport_lat
newport_data$year <- year(newport_data$sample_date)
newport_data$doy <- as.POSIXlt(newport_data$sample_date, format = "%Y-%b-%d")$yday
newport_data$depth <- p2d(lat = newport_lat,
                          p = newport_data$pressure..dbar.)
newport_data$depth_ln <- log(newport_data$depth)


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
newport_data$O2_umolkg_ln <- log(newport_data$o2)

newport_data <- as.data.frame(newport_data)

#Make column names the same
#needs X, Y, year, doy, temperature_C, sigma0_exp, 
newport_data$sigma0_exp <- exp(newport_data$sigma)
newport_data$temperature_C <- newport_data$temperature..degC.

O2_model <- m_6

test_predict_O2 <- predict(O2_model, newdata = newport_data)

rsq(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)
rmse(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)/(max(test_predict_O2$O2_umolkg_ln)- min(test_predict_O2$O2_umolkg_ln))

ggplot(test_predict_O2, aes(x = o2, y = exp(est)))+
  geom_point(size=0.8)+
  geom_abline(slope = 1, intercept = 0, color = 1)+
  ylab("predicted O2")+
  facet_wrap("year")+
  ylim(0,300)

### Fit to all data except 1 year of NWFSC data ##
#Combined in situ data
density_O2 <- readRDS("insitu_combined.rds")

#Log O2
density_O2$O2_umolkg_ln <- log(density_O2$O2_umolkg)
density_O2$depth_ln <- log(density_O2$depth)
density_O2$sigma0_exp <- exp(density_O2$sigma0_kgm3)
density_O2$doy <- as.POSIXlt(density_O2$date, format = "%Y-%b-%d")$yday

#Remove alaska for now
density_O2 <- subset(density_O2, survey!="afsc")

#Data with oxygen
density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)  

#Remove rows with missing data
density_O2 <- density_O2 %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)

train <- density_O2
train$O2_umolkg_ln <- ifelse((train$survey=="nwfsc" & train$year==2011), NA, train$O2_umolkg_ln)
train <- train %>% drop_na(O2_umolkg_ln)
test <- filter(density_O2, year==2011 & survey=="nwfsc")

train <- as.data.frame(train)
test <- as.data.frame(test)
spde <- make_mesh(data = train, xy_cols = c("X","Y"), cutoff = 30)

#Fit model
m_8 <- sdmTMB(formula = O2_umolkg_ln  ~ 0  +as.factor(survey)+s(sigma0_exp)+ s(temperature_C) +  s(depth_ln) + s(doy),
              mesh = spde,
              data = train, 
              family = gaussian(), 
              time = "year",
              spatial = "on",
              spatiotemporal  = "IID")

#Predict
O2_model <- m_8
test_predict_O2 <- predict(O2_model, newdata = test)

rsq(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)
rmse(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)/(max(test_predict_O2$O2_umolkg_ln)- min(test_predict_O2$O2_umolkg_ln))

#Compare
ggplot(test_predict_O2, aes(x = O2_umolkg, y = exp(est), colour=survey))+
  geom_point(size=0.8)+
  geom_abline(slope = 1, intercept = 0, color = 1)+
  ylab("predicted O2")

test_predict_O2 %>% 
  mutate(resid = scale(O2_umolkg_ln-est)) %>% 
  ggplot(aes(x=O2_umolkg, y = -depth, color = resid))+
  geom_jitter()+
  scale_color_viridis_c()

test_predict_O2 %>% 
  mutate(resid = scale(O2_umolkg_ln-est)) %>% 
  ggplot(aes(x=latitude, y = longitude, color = resid))+
  geom_jitter()+
  scale_color_viridis_c()
