library(sdmTMB)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(rnaturalearth)
library(sf)
library(ggpubr)
library(visreg)
library(tidync)
library(marmap)

#Set base WD
basewd <- "/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data"
setwd(basewd)

#Load functions
source("code/util_funs.R")
source("code/test_wc_O2_predictions/helper_funs.R")

#Set WD location of GLORYS files
gloryswd <- "/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/GLORYS"

# load regional polygons
regions.hull <- readRDS("data/processed_data/regions_hull.rds")

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

#Log depth
dat$depth_ln <- log(dat$depth)

#Save model outputs?
savemodel=F
#Plot models and save?
plotmodel = F
#Remove OCNMS?
ocnms =F
#Restrict testing years to just if more than 50 observations?
n_50 =T

#Set do threshold level for GLORYS data
do_threshold <- 0

#Filter time?
filter_time <- F

#Function to fit model for a specific region
glorys_fit <- function(dat, test_region, plot_title){
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
  rmse_summary <- matrix(data=NA, nrow=length(yearlist), ncol=2)
  colnames(rmse_summary) <- c("glorys", "n_test")
  output <- list()
  
  ##For each year of testing data, pull out GLORYS data, fit model, and calculate RMSE
  for (i in 1:length(yearlist)) {
    test_year <- yearlist[i]
    print(test_year)
    ##Trawl testing data
    test_data <- dat.2.use %>%
      filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc") & year==test_year)
    test_data <- as.data.frame(test_data)
    ##Pull GLORYS data
    setwd(gloryswd)
    if(test_region=="cc"){
      files <- list.files("wc_o2", pattern=paste(test_year))
      files <- paste("wc_o2/", files, sep="")
    }
    
    if(test_region=="bc"){
      files <- list.files("bc_o2", pattern=paste(test_year))
      files <- paste("bc_o2/", files, sep="")
    }
    
    if(test_region=="ebs"|test_region=="goa"|test_region=="ai"){
      files <- list.files("alaska_o2_combined", pattern=paste(test_year))
      files <- paste("alaska_o2_combined/", files, sep="")
    }
    
    #Convert GLOYRS file from .nc format to dataframe in right format
    print("converting GLORYS")
    if(length(files)==1){
      glorys <- convert_glorys(files, do_threshold)
    }
    
    #Use function to get data for all files if multiple files in year (this is mostly a thing for Alaska)
    if(length(files)>1){
      # get first year of glorys
      glorys <- convert_glorys(files[1], do_threshold)
      # get remaining years of glorys and combine into single data frame
      for (j in 2:length(files)) {
        tmp_glorys <- convert_glorys(files[j], do_threshold)
        glorys <- rbind(glorys, tmp_glorys)
      }
    }
    
    ##Pull in survey extent polygon from GLORYS data
    # Regional polygon
    poly <- filter(regions.hull, region==test_region)
    #Convert GLORYS to an sf
    glorys_sf <-  st_as_sf(glorys, coords = c("longitude", "latitude"), crs = st_crs(4326))
    # pull out observations within each region
    region_dat  <- st_filter(glorys_sf, poly)
    region_dat <- as.data.frame(region_dat)
    
    #Log depth
    region_dat$depth_ln <- log(region_dat$depth)
    
    #Set working directory back to base for saving
    setwd(basewd)
    
    ##Match nearest GLORYS point
    ##Extract out for each haul the closest matching lat and lon
    # use nn2() to calculate min distance to nearest ROMS lat/long for each date
    for(k in 1:1){
    test_day <- test_data[1,]
    doy <- unique(test_day$doy)
    region.2.use <- filter(region_dat, doy==doy)
    test <- RANN::nn2(region.2.use[, c('Y', 'X')], test_day[, c('Y', "X")],k = 1)
    #Extract GLORYS point
    test_day$est <- region.2.use[c(test$nn.idx),c("o2")]
    test_predict_O2 <- test_day
    }
    
    for(k in 2:nrow(test_data)){
      test_day <- test_data[k,]
      doy <- unique(test_day$doy)
      region.2.use <- filter(region_dat, doy==doy)
      test <- RANN::nn2(region.2.use[, c('Y', 'X')], test_day[, c('Y', "X")],k = 1)
      #Extract GLORYS point
      test_day$est <- region.2.use[c(test$nn.idx),c("o2")]
      test_predict_O2 <- bind_rows(test_predict_O2, test_day)
    }

    #Residuals
    test_predict_O2$residual = test_predict_O2$o2 - test_predict_O2$est
                                                     
    #RMSE
    rmse_summary[i,1] <- try(rmse(test_predict_O2$o2, test_predict_O2$est), silent=T)
    rmse_summary[i,2] <- try(rmse(nrow(test_predict_O2)))
    
   output[[i]] <-test_predict_O2
    }
  
  #Clean RMSE table
  rmse_summary <- as.data.frame(rmse_summary)
  rmse_summary$year <- yearlist
  
  ##Save models
  if (savemodel) {
    save(x = output, file = paste("code/test_wc_O2_predictions/outputs/o2_models_glorys/", test_region, ".Rdata", sep=""))
  }
  
  #Calculate overall RMSE
  rmse_total <- as.data.frame(calc_rmse(rmse_summary$glorys, rmse_summary$n_test))
  colnames(rmse_total) <- "rmse_total"
  rownames(rmse_total) <- "glorys"
  
  #Combine with synoptic table
  file <- list.files("code/test_wc_O2_predictions/outputs", pattern=paste("glorys_rmse_",test_region,".rds", sep=""))
  table <- readRDS(paste("code/test_wc_O2_predictions/outputs/", file, sep=""))
  file2 <- list.files("code/test_wc_O2_predictions/outputs", pattern=paste("glorys_rmsetotal_",test_region,".rds", sep=""))
  table2 <- readRDS(paste("code/test_wc_O2_predictions/outputs/", file2, sep=""))
  
  table$glorys2 <- rmse_summary$glorys
  table2 <- bind_rows(table2,rmse_total)
  
  if(plotmodel){
  #Plot RMSE and save
  rmse_long <- pivot_longer(table, c(1:4,9), names_to="model")
  #Remove rows with less than n=50 in test data
  ggplot(rmse_long, aes(x=year, y=value))+
    geom_col(aes(fill=model), position="dodge")+
    ylab("RMSE")+
    ggtitle(paste(plot_title))+
    xlab("Year")+
    theme(legend.position="top")+
    theme_set(theme_bw(base_size = 15))+
    theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave(paste("code/test_wc_O2_predictions/outputs/glorys_", test_region, "_rmse_plot.pdf", sep=""))
  }
  
    saveRDS(table, file=paste("code/test_wc_O2_predictions/outputs/glorys2_rmse_", test_region, ".rds", sep=""))
    saveRDS(table2, file=paste("code/test_wc_O2_predictions/outputs/glorys2_rmsetotal_", test_region, ".rds", sep=""))
    #saveRDS(rmse_summary, file=paste("code/test_wc_O2_predictions/outputs/glorys2_summary_", test_region, ".rds", sep=""))
  
  #Plot?
  #Return RMSE table
  return(table)
}

#Run
rmse_cc <- glorys_fit(dat, test_region="cc", "California Current")
rmse_bc <- glorys_fit(dat, "bc", "British Columbia")
rmse_goa <- glorys_fit(dat, "goa", "Gulf of Alaska")
rmse_ebs <- glorys_fit(dat, "ebs", "Eastern Bering Sea")
rmse_ai <- glorys_fit(dat, "ai", "Aleutian Islands")
