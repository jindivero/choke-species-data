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

### Set ggplot themes ###
theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

basewd <-"/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"

#Combined in situ data
density_O2 <- readRDS("insitu_combined.rds")

#Log O2
density_O2$O2_umolkg_ln <- log(density_O2$O2_umolkg)
density_O2$depth_ln <- log(density_O2$depth)
density_O2$sigma0_exp <- exp(density_O2$sigma0_kgm3)
density_O2$doy <- as.POSIXlt(density_O2$date, format = "%Y-%b-%d")$yday

#Subset to data with DO available and no DO
density_no_O2 <- density_O2 %>% 
  filter(is.na(do_mlpL)| do_mlpL<=0)

density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)

#Separate train and test data
set.seed(7)
density_O2$test <- rbinom(n = nrow(density_O2), size = 1, prob = 0.3)
train <- density_O2[density_O2$test == 0,]
test <- density_O2[density_O2$test == 1,]

train <- as.data.frame(train)

#Remove NAs
train <- train %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)

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

m_6 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(doy),
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

###RMSE on test data
#Remove NAs
test <- test %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)
test <- as.data.frame(test)

#Predict onto test data

O2_model <- m_6

test_predict_O2 <- predict(O2_model, newdata = test)
train_predict_O2 <- predict(O2_model, newdata = train)
rsq <- function (x, y) cor(x, y) ^ 2
rsq(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)
rsq(train_predict_O2$O2_umolkg_ln, train_predict_O2$est)
install.packages("Metrics")
library(Metrics)
rmse(test_predict_O2$O2_umolkg_ln, test_predict_O2$est)/(max(test_predict_O2$O2_umolkg_ln)- min(test_predict_O2$O2_umolkg_ln))
rmse(train_predict_O2$O2_umolkg_ln, train_predict_O2$est)/(max(train_predict_O2$O2_umolkg_ln)- min(train_predict_O2$O2_umolkg_ln))

  ggplot(test_predict_O2, aes(x = O2_umolkg, y = exp(est)))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = 1)+
  ylab("predicted O2")+
  geom_point(train_predict_O2, mapping=aes(x=O2_umolkg, y=exp(est)), colour="blue")

####Northwest region, IPHC train and NWFSC test
nw <- subset(density_O2, latitude <49 & (survey=="nwfsc"|survey=="iphc"))
nw <- nw %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)
nw <- as.data.frame(nw)
nw <- subset(nw, year>2008 & year <2016)
nw$O2_umolkg_ln <- ifelse(nw$survey=="nwfsc", NA, nw$O2_umolkg_ln)
spde <- make_mesh(data = nw, xy_cols = c("X","Y"), cutoff = 30)

m_2 <- sdmTMB(formula = O2_umolkg_ln  ~ 0 + s(sigma0_exp) + s(temperature_C) +  s(depth_ln) + s(month, bs = "cc", k = 4),
              mesh = spde,
              data = nw, 
              family = gaussian(), 
            time = "year",
              spatial = "on",
              spatiotemporal  = "off")

NWFSC <- subset(nw, survey=="nwfsc")
tmp_predict <- predict(m_2, newdata = NWFSC)

ggplot(tmp_predict, aes(x=O2_umolkg, y=exp(est)))+geom_point()+xlab("NWFSC Observed Oxygen")+ylab("Predicted Oxygen from IPHC data")+geom_abline(slope=1, intercept=0)+facet_wrap("year")

#Look at years

#Look at latitude