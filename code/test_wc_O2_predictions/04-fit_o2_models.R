library(sdmTMB)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(rnaturalearth)
library(sf)
library(ggpubr)
library(visreg)

#Load functions
source("code/util_funs.R")
source("code/test_wc_O2_predictions/helper_funs.R")

# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)

#Load oxygen data
dat <- as.data.frame(readRDS("data/processed_data/all_o2_dat_filtered.rds"))

#Remove any rows with missing data
dat <- dat %>%
  drop_na(depth, o2, temp, sigma0, doy, X, Y)

#Remove oxygen outliers
dat <- filter(dat, o2<1500)

#Set minimum sigma
minsigma0 <- 24
dat$sigma0[dat$sigma0 <= minsigma0] <- minsigma0

# remove older (earlier than 2000) data
dat <- dplyr::filter(dat, year >=2000)

#Log depth
dat$depth_ln <- log(dat$depth)

#Save model outputs?
savemodel=T
#Plot models and save?
plotmodel = T
#Remove OCNMS?
ocnms =F
#Restrict testing years to just if more than 50 observations?
n_50 =T

#test removing OCNMS
if(ocnms){
dat <- filter(dat, survey!="ocnms")
}

#Function to fit model for a specific region
fit_models <- function(dat, test_region, plot_title){
  ##Set up data
  #Filter to region
  dat.2.use <- as.data.frame(filter(dat, region==test_region))
  if(test_region=="goa") {dat.2.use <- filter(dat.2.use, !(survey=="ai"))}
  #Just trawl survey data
  trawl_dat <- dat.2.use %>%
    filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))
  #Years in trawl data available
  yearlist <- sort(unique(trawl_dat$year))
  if(n_50){
    counts <- count(trawl_dat, year)
    counts <- filter(counts, n>50)
    yearlist <- sort(unique(counts$year))
  }
  
  #Create lists and matrices for storing RMSE and list for storing prediction datasets
  models.2.use <- 4
  other.cols <- 2
  rmse_summary <- matrix(data=NA, nrow=length(yearlist), ncol=models.2.use + other.cols)
  colnames(rmse_summary) <- c("persistent_spatial", "persistent_spatial_year", "year_temp_salinity", "temp_salinity_spatiotemporal", "n_test","n_train")
  output <- list()
 # rsqlist <- rmselist <- rep(NA, length(yearlist))
  ##Fit model for each year of training data
for (i in 1:length(yearlist)) {
  #Separate test and training data
  test_year <- yearlist[i]
  print(test_year)
  test_data <- dat.2.use %>%
    filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc") & year==test_year)
  train_data <- dat.2.use %>%
    filter(!((survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc") & year==test_year)))
  train_data <- as.data.frame(train_data)
  test_data <- as.data.frame(test_data)
  #Determine if extra time is needed
  extra_years <- setdiff(yearlist, unique(train_data$year))
  if(length(extra_years)==0) {extra_years = NULL}   
  ## Make Mesh and fit model ####
  spde <- make_mesh(data = train_data,
                    xy_cols = c("X", "Y"),
                    cutoff = 45)
  print("fitting m1")
  m1 <- try(sdmTMB(
    formula = o2  ~ 1+s(depth_ln) + s(doy),
    mesh = spde,
    data = train_data,
    family = gaussian(),
    spatial = "on",
    spatiotemporal  = "off"
  ))
  if(!is.list(m1)){
    print("fitting m1 no intercept")
    m1 <- try(sdmTMB(
      formula = o2  ~ 0+s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
  }
  print("fitting m2")
  if(length(extra_years)>0) {
    train_data <- train_data %>% add_row(year=extra_years, depth_ln=mean(train_data$depth_ln),
                                         survey=test_region, doy=mean(train_data$doy), temp=mean(train_data$temp),
                                         o2=mean(train_data$o2), sigma0=mean(train_data$sigma0),X=mean(train_data$X),
                                         Y=mean(train_data$Y))
    train_data <- as.data.frame(train_data)
    spde <- make_mesh(data = train_data,
                      xy_cols = c("X", "Y"),
                      cutoff = 45)
  }
  
  m2 <- try(sdmTMB(
    formula = o2  ~ 1+as.factor(year) + s(depth_ln) + s(doy),
    mesh = spde,
    data = train_data,
    family = gaussian(),
    spatial = "on",
    spatiotemporal  = "off",
    extra_time=c(extra_years)
  ))
  if(!is.list(m2)){
    print("fitting m2 no intercept")
    m2 <- try(sdmTMB(
      formula = o2  ~ 0+as.factor(year) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off",
      extra_time=c(extra_years)))
    
  }
  print("fitting m3")
  m3 <- try(sdmTMB(
    formula = o2  ~ 1+as.factor(year)+s(sigma0) + s(temp) +  s(depth_ln) + s(doy),
    mesh = spde,
    data = train_data,
    family = gaussian(),
    spatial = "on",
    spatiotemporal  = "off",
    extra_time=c(extra_years)
  ))
  if(!is.list(m3)){
    print("fitting m3 no intercept")
   m3 <- try(sdmTMB(
      formula = o2  ~ 0 + as.factor(year)+s(sigma0) + s(temp) +  s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off",
      extra_time=c(extra_years)
    ))
  }
  if(length(extra_years)>0) {
    train_data <- dat.2.use %>%
      filter(!((survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc") & year==test_year)))
    train_data <- as.data.frame(train_data)
    spde <- make_mesh(data = train_data,
                      xy_cols = c("X", "Y"),
                      cutoff = 45)
  }
  
  print("fitting m4")
  m4 <- try(sdmTMB(
    formula = o2  ~ 1+s(sigma0) + s(temp) +  s(depth_ln) + s(doy),
    mesh = spde,
    data = train_data,
    family = gaussian(),
    time = "year",
    spatial = "on",
    spatiotemporal  = "iid",
    extra_time=c(extra_years)
  ))
  if(!is.list(m4)){
    print("fitting m4 no intercept")
    m4 <- try(sdmTMB(
      formula = o2  ~ 0+s(sigma0) + s(temp) +  s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      time = "year",
      spatial = "on",
      spatiotemporal  = "ar1",
      extra_time=c(extra_years)
    ))
  }
  models <- list(m1,m2,m3, m4)
  tmp.preds <- list()
  #Predict data from each model and calculate RMSE
  for (j in 1:length(models)){
    # Predict onto data
    test_predict_O2 <- try(predict(models[[j]], newdata = test_data))
    test_predict_O2$residual = try(test_predict_O2$o2 - (test_predict_O2$est))
    rmse_summary[i,j] <- try(rmse(test_predict_O2$o2, test_predict_O2$est), silent=T)
    tmp.preds[[j]] <- test_predict_O2
    #Number of datapoints in each year for calculating overall RMSE late
    if(j==1){
      ncols <- ncol(rmse_summary)
      rmse_summary[i,ncols -1] <- nrow(test_data)
      rmse_summary[i,ncols] <- nrow(train_data)
    }
  }
  tmp.output <- list(train_data, test_data, tmp.preds, models)
  names(tmp.output) <-c("train_data", "test_data", "predictions", "models")
  output[[i]] <- tmp.output
  #Plot
  if(plotmodel){
    print("plots")
    try(plot_simple(tmp.output, dat.2.use))
    if(is.list(models[4])){
      print("marginal effects")
    try(plot_marginal_effects(models, tmp.preds, dat.2.use, 4))
    }
  }
}
  
  #Clean RMSE table
  rmse_summary <- as.data.frame(rmse_summary)
  rmse_summary$year <- yearlist
  rmse_summary$region <- test_region
  rmse_summary$persistent_spatial <- as.numeric(rmse_summary$persistent_spatial)
  rmse_summary$persistent_spatial_year <- as.numeric(rmse_summary$persistent_spatial_year)
  rmse_summary$year_temp_salinity <- as.numeric(rmse_summary$year_temp_salinity)
  rmse_summary$temp_salinity_spatiotemporal <- as.numeric(rmse_summary$temp_salinity_spatiotemporal)
  
  ##Save models
  if (savemodel) {
    save(x = output, file = paste("code/test_wc_O2_predictions/outputs/o2_models_", test_region, ".Rdata", sep=""))
  }

  #Plot RMSE and save
  rmse_long <- pivot_longer(rmse_summary, 1:models.2.use, names_to="model")
  #Remove rows with less than n=50 in test data
  rmse_long <- filter(rmse_summary, n_test>50)
  rmse_long <- pivot_longer(rmse_long, 1:models.2.use, names_to="model")
  ggplot(rmse_long, aes(x=year, y=value))+
    geom_col(aes(fill=model), position="dodge")+
    ylab("RMSE")+
    ggtitle(paste(plot_title))+
    xlab("Year")+
    theme(legend.position="top")+
    theme_set(theme_bw(base_size = 15))+
  theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave(paste("code/test_wc_O2_predictions/outputs/", plot_title, "_rmse_plot.pdf", sep=""))
  
  #Calculate overall RMSE
  rmse_total <- as.data.frame(sapply(rmse_summary[,1:models.2.use], calc_rmse, rmse_summary$n_test))
  colnames(rmse_total) <- "rmse_total"
  print(rmse_total)
  
  #Save
  saveRDS(rmse_summary, file=paste("code/test_wc_O2_predictions/outputs/rmse_years_", test_region, ".rds", sep=""))
  saveRDS(rmse_total, file=paste("code/test_wc_O2_predictions/outputs/rmse_total_", test_region, ".rds", sep=""))
 
 #Return RMSE table
  return(rmse_summary)
}

#Apply to region

rmse_cc <- fit_models(dat, test_region="cc", "California Current")
rmse_bc <- fit_models(dat, "bc", "British Columbia")
rmse_goa <- fit_models(dat, "goa", "Gulf of Alaska")
rmse_ebs <- fit_models(dat, "ebs", "Eastern Bering Sea")
rmse_ai <- fit_models(dat, "ai", "Aleutian Islands")
