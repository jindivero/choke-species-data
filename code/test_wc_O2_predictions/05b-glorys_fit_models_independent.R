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
basewd <- "/Users/jindivero/Dropbox/choke species/code/choke-species-data"
setwd(basewd)
  
#Load functions
source("code/util_funs.R")
source("code/test_wc_O2_predictions/helper_funs.R")

#Set WD location of GLORYS files
gloryswd <- "/Users/jindivero/Dropbox/choke species/code/GLORYS"

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
savemodel=T
#Plot models and save?
plotmodel = T
#Remove OCNMS?
ocnms =F
#Restrict testing years to just if more than 50 observations?
n_50 =T

#Set do threshold level for GLORYS data
do_threshold <- 0

#Filter days of GLORYS data to every 10th day?
filter_time <- T

#Function to fit model for a specific region
glorys_fit <- function(dat, test_region){
  ##Set up data
  #Filter to region
  dat.2.use <- as.data.frame(filter(dat, region==test_region))
  if(test_region=="goa") {dat.2.use <- filter(dat.2.use, !(survey=="ai"))}
  #Just trawl survey data
  trawl_dat <- dat.2.use %>%
    filter(!((survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc"))))
  trawl_dat <- filter(trawl_dat, year>1992)
  #Years in trawl data available
  yearlist <- sort(unique(trawl_dat$year))

  #Create lists and matrices for storing RMSE and list for storing prediction datasets
  output <- list()
  
  ##For each year of testing data, pull out GLORYS data, fit model, and calculate RMSE
  for (i in 1:length(yearlist)) {
    test_year <- yearlist[i]
    print(test_year)
    ##Trawl testing data
    test_data <- dat.2.use %>%
      filter((!(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc")) & year==test_year))
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
  
  #Mesh
  spde <- make_mesh(data = region_dat,
                    xy_cols = c("X", "Y"),
                    cutoff = 45)
  
  #Fit model
  print("fitting model")
  m <- try(sdmTMB(formula = o2 ~ 0 +s(depth_ln) + s(doy),
               mesh = spde,
               data = region_dat, 
               family = gaussian(), 
               spatial = "on",
               spatiotemporal  = "off"))
  if(!is.list(m)){
    print("fitting model no intercept")
    m <- try(sdmTMB(formula = o2 ~ 1 +s(depth_ln) + s(doy),
                mesh = spde,
                data = region_dat, 
                family = gaussian(), 
                spatial = "on",
                spatiotemporal  = "off"))
  }
  ### Predictions ###
  #Predict
  test_predict_O2 <- try(predict(m, newdata = test_data))
  #Residuals
  test_predict_O2$residual = try(test_predict_O2$o2 - (test_predict_O2$est))
  output[[i]] <- test_predict_O2
  }
  return(output)
}

#Run
rmse_cc <- glorys_fit(dat, test_region="cc")
rmse_bc <- glorys_fit(dat, "bc")
rmse_goa <- glorys_fit(dat, "goa")
rmse_ebs <- glorys_fit(dat, "ebs")
rmse_ai <- glorys_fit(dat, "ai")
all <- list(rmse_cc, rmse_bc, rmse_goa, rmse_goa, rmse_ebs, rmse_ai)

combine <- function(x){
row_lt2 <- which(sapply(x, is.data.frame))
x <- x[row_lt2]
x <- bind_rows(x)
}

test <- lapply(all, combine)
test <- bind_rows(test)

#Just independent ones
test <- filter(test, survey!="ai")
test <- filter(test, survey!="hake")

saveRDS(test, file="code/test_wc_O2_predictions/outputs/o2_models_glorys/glorys_predictions_independent.rds")

#Calculate RMSE for each survey type
test2 <- filter(test, est>0)
rmse <- test2 %>% group_by(survey) %>% summarize(rmse=rmse(o2,est))
rmse$label <- paste("RMSE=", rmse$rmse, sep="")
rmse$label <- substr(rmse$label, 1,10)

#Labels
labs <- c("CalCOFI", "CODAP", "Newport Line", "OCNMS", "WCOA")
names(labs) <- c("calCOFI", "codap", "NewportLine", "ocnms", "wcoa")

#plot
ggplot(filter(test, est>0), aes(x=o2, y=est))+
  geom_point()+
  geom_abline(intercept=0, slope=1)+
  facet_wrap("survey", labeller = labeller(survey=labs))+
  theme_minimal(base_size=20)+
  ylim(0,450)+
    geom_text(data=rmse, mapping = aes(x = 120, y = 450, label = label))

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/glorys_independent_predsobs.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 7,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)



