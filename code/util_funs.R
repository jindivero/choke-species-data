library(dplyr)
library(devtools)
install_github("ropensci/taxize")
library(taxize)
install.packages("rfishbase")
library(rfishbase)
install.packages("gsw")
library(gsw)
library(readxl)
library(stringr)
library(lubridate)
library(sf)

#Convert a scaled variable back to its original
back.convert <- function(x, mean_orig, sd_orig) (x* sd_orig+mean_orig)

#Get scientific name from itis number
extract_name <- function(x){
  last_row <- x[nrow(x), ]
  name <- last_row$name
  return(name)
}

#Pull and extract ITIS info to get species names
itis_extract <- function(catch){ #catch is dataframe with the itis numbers
itis <- unique(catch$itis)

scientific_names <-  taxize::classification(itis, db="itis")
itis <- as.data.frame(itis)
names <-lapply(scientific_names, extract_name)
names <- unlist(names)
itis$scientific_name <- tolower(names)
}

print_species <- function(type){
  biomass <- combine_all(type)
  species <- biomass[1:2]
  return(species)
}

load_all_hauls <- function() {
  install.packages("remotes")
  remotes::install_github("nwfsc-assess/nwfscSurvey")
  haul = nwfscSurvey::PullHaul.fn(SurveyName = "NWFSC.Combo")
  haul <- plyr::rename(haul, replace=c("salinity_at_gear_psu_der" = "sal", 
                                       "temperature_at_gear_c_der" = "temp", 
                                       "o2_at_gear_ml_per_l_der" = "o2",
                                       "depth_hi_prec_m" = "depth"))
  
  # read in the grid cell data from the survey design
  grid_cells = readxl::read_excel("data/Selection Set 2018 with Cell Corners.xlsx")
  grid_cells = dplyr::mutate(grid_cells,
                             depth_min = as.numeric(unlist(strsplit(grid_cells$Depth.Range,"-"))[1]),
                             depth_max = as.numeric(unlist(strsplit(grid_cells$Depth.Range,"-"))[2]))
  
  # convert grid_cells to sp object
  grid = SpatialPoints(cbind(grid_cells$Cent.Long,grid_cells$Cent.Lat),
                       proj4string = CRS("+proj=longlat +datum=WGS84"))
  r = raster::rasterize(x=grid, y = raster(nrow=length(unique(grid_cells$Cent.Lat)),
                                           ncol=length(unique(grid_cells$Cent.Long))))
  rasterToPoints(r)
  
  raster = aggregate(r, fact = 2)
  raster = projectRaster(raster, crs = "+proj=tmerc +lat_0=31.96 +lon_0=-121.6 +k=1 +x_0=390000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # create matrix of point data with coordinates and depth from raster
  grid = as.data.frame(rasterToPoints(raster))
  
  # Figure out the grid cell corresponding to each tow location
  haul$Cent.Lat = NA
  haul$Cent.Lon = NA
  haul$Cent.ID = NA
  for(i in 1:nrow(haul)) {
    indx = which(grid_cells$NW.LAT > haul$latitude_dd[i] &
                   grid_cells$SW.LAT < haul$latitude_dd[i] &
                   grid_cells$NW.LON < haul$longitude_dd[i] &
                   grid_cells$NE.LON > haul$longitude_dd[i])
    if(length(indx) > 0) {
      haul$Cent.ID[i] = grid_cells$Cent.ID[indx]
      haul$Cent.Lat[i] = grid_cells$Cent.Lat[indx]
      haul$Cent.Lon[i] = grid_cells$Cent.Long[indx]
    }
  }
  
  # project lat/lon to UTM, after removing missing values and unsatisfactory hauls
  haul = haul %>% filter(!is.na(Cent.Lon), performance == "Satisfactory")
  
  haul_trans = haul
  coordinates(haul_trans) <- c("Cent.Lon", "Cent.Lat")
  proj4string(haul_trans) <- CRS("+proj=longlat +datum=WGS84")
  newproj = paste("+proj=utm +zone=10 ellps=WGS84")
  haul_trans <- spTransform(haul_trans, CRS(newproj))
  haul_trans = as.data.frame(haul_trans)
  haul_trans$Cent.Lon = haul_trans$Cent.Lon/10000
  haul_trans$Cent.Lat = haul_trans$Cent.Lat/10000
  haul_trans$year = as.numeric(substr(haul_trans$date_yyyymmdd,1,4))
  
  haul$X = haul_trans$Cent.Lon
  haul$Y = haul_trans$Cent.Lat
  haul$year = haul_trans$year
  #haul$year_centered = haul$year - mean(unique(haul$year))
  
  return(haul)
}

load_data_nwfsc <- function(spc,sci_name, dat.by.size, length=T) {
  dat <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
  names(dat) = tolower(names(dat))
  dat.by.size$trawl_id <- as.character(dat.by.size$trawl_id)
  dat$common_name <- tolower(dat$common_name)
  dat = dplyr::filter(dat, common_name == spc)
  dat <- left_join(dat, dat.by.size, by = "trawl_id")
  # remove tows where there was positive catch but no length measurements
  if(length){
  dat <- dplyr::filter(dat, !is.na(p1))
  }
  # analyze or years and hauls with adequate oxygen and temperature data, within range of occurrence
  
  # get julian day
  dat$julian_day <- rep(NA, nrow(dat))
  for (i in 1:nrow(dat)){ 
    dat$julian_day[i] <- as.POSIXlt(dat$date[i], format = "%Y-%b-%d")$yday
  }
  
  
  #O2 from trawl data is in ml/l 
  # just in case, remove any missing or nonsense values from sensors
  # dat <- dplyr::filter(dat, !is.na(o2), !is.na(sal), !is.na(temp), is.finite(sal))
  # dat <- calc_po2_mi(dat)
  # dat <- dplyr::filter(dat, !is.na(temp), !is.na(mi))
  
  # prepare data and models -------------------------------------------------
  dat$longitude <- dat$longitude_dd
  dat$latitude <- dat$latitude_dd
  dat$event_id <- dat$trawl_id
  dat$date <- as.POSIXct(as.Date(dat$date, format = "%Y-%b-%d"))
  dat <- dplyr::select(dat, event_id, common_name, project, vessel, tow, year, date, longitude_dd, latitude_dd, longitude, latitude, cpue_kg_km2,
                       depth_m, julian_day, nlength, median_weight, haul_weight, pass, p1, p2, p3, p4)
  
  # UTM transformation
  dat_ll = dat
  sp::coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
  sp::proj4string(dat_ll) <- sp::CRS("+proj=longlat +datum=WGS84")
  # convert to utm with spTransform
  dat_utm = sp::spTransform(dat_ll, 
                        sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
  # convert back from sp object to data frame
  dat = as.data.frame(dat_utm)
  dat = dplyr::rename(dat, X = coords.x1,
                      Y = coords.x2)
  dat$scientific_name <- sci_name
  dat$depth <- dat$depth_m
  dat$depth_m <- NULL
  return(dat)
}


# Species of interest and max. juvenile lengths (define ontogenetic classes)
length_expand_nwfsc <- function(spc, sci_name) {
  # load, clean, and join data
  bio <- readRDS("data/fish_raw/NOAA/nwfsc_bio.rds")
  load("data/fish_raw/NOAA/nwfsc_haul.rda")
  haul <- nwfsc_haul
  catch <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
  names(catch) = tolower(names(catch))
  names(bio) = tolower(names(bio))
  names(haul) = tolower(names(haul))
  bio$scientific_name <- tolower(bio$scientific_name)
  bio$common_name <- tolower(bio$common_name)
  catch$common_name <- tolower(catch$common_name)
  
  bio$trawl_id = as.character(bio$trawl_id)
  haul$trawl_id = as.character(haul$event_id)
  catch$trawl_id=as.character(catch$trawl_id)
  haul$year <- as.character(substr(haul$date, start=1, stop=4))
  bio$year <- as.character(bio$year)
  catch$date <- NULL
  bio$date <- NULL
  bio$year <- NULL
  
  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)
  
  #Combine data
  dat = dplyr::left_join(catch[,c("trawl_id","common_name", "subsample_count","area_swept_ha","longitude_dd", "latitude_dd",
                                  "subsample_wt_kg","total_catch_numbers","total_catch_wt_kg","cpue_kg_km2")], haul, relationship = "many-to-many") %>%
    dplyr::left_join(filter(bio[,c("trawl_id", "scientific_name", "common_name", "weight", "ageing_lab", "oto_id", "length_cm", "width_cm", "sex", "age")], !is.na(length_cm)), relationship = "many-to-many") %>%
    filter(performance == "Satisfactory")
  
  # filter out species of interest from joined (catch/haul/bio) dataset
  dat_sub = dplyr::filter(dat, common_name == spc)
  
  #Warning if species not present
  if(nrow(dat_sub)==0){
    warning("species not present in data")
  }
  
  #Add column counting number of length observations per catch
  dat_sub$is_length <- ifelse(!is.na(dat_sub$length_cm), 1,0) 
  dat_sub <- group_by(dat_sub, trawl_id) %>% mutate(nlength=sum(is_length)) %>% ungroup()
  dat_sub$trawl_id <- as.numeric(dat_sub$trawl_id)
  
  # fit length-weight regression by year to predict fish weights that have lengths only.
  # note a rank-deficiency warning may indicate there is insufficient data for some year/sex combinations (likely for unsexed group)
  #Create one set of data with only year/survey combinations with at least some weight data:
 
  if(nrow(dat_sub)>0) {
   fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
    group_by(year) %>%
    mutate(sum = sum(weight, na.rm=T)) %>%
    filter(sum>0) %>%
    ungroup()
  fitted$weight <- ifelse(fitted$weight==0, NA, fitted$weight)
  #And one for those without any weight data:
  not_fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
    group_by(year) %>%
    mutate(sum = sum(weight, na.rm=T)) %>%
    filter(sum==0) %>%
    ungroup()
  
  if(nrow(fitted)>0){
  fitted <-  
   group_nest(fitted, year) %>%
    mutate(
      model = purrr::map(data, ~ lm(log(weight) ~ log(length_cm), data = .x)),
      tidied = purrr::map(model, broom::tidy),
      augmented = purrr::map(model, broom::augment),
      predictions = purrr::map2(data, model, modelr::add_predictions)
    )
  
  # replace missing weights with predicted weights
  dat_pos2 = fitted %>%
    tidyr::unnest(predictions) %>%
    dplyr::select(-data, -model, -tidied, -augmented) %>%
    dplyr::mutate(weight = ifelse(is.na(weight), exp(pred), weight))

  dat_pos <- bind_rows(dat_pos2, not_fitted)
  }
  
  if(nrow(fitted)==0 & nrow(not_fitted)>0){
    dat_pos <- not_fitted
  }
  
  if(nrow(fitted)>0 | nrow(not_fitted)>0){
  #If there is no weight data available to get weight-length empirical interpolation, use fishbase to fill in
  # find length-weight relationship parameters for one species
  pars <- rfishbase::length_weight(
    #convert scientific name to first letter capitalized for fishbase
    species_list = stringr::str_to_sentence(sci_name, locale = "en"))
  #Get mean
  a <- mean(pars$a)
  b <- mean(pars$b)
  #Make NAs where text
  dat_pos$weight <- ifelse(dat_pos$weight=="NaN", NA, dat_pos$weight)
  #Remove any zero weights
  dat_pos$weight <- ifelse(dat_pos$weight==0, NA, dat_pos$weight)
  #Calculate weight (and convert from g to kg)
  dat_pos <- dplyr::mutate(dat_pos, weight = ifelse(is.na(weight), ((a*length_cm^b)*0.001), weight))
  #Add column getting the mean individual weight 
  dat_test <- group_by(dat_pos, trawl_id) %>% mutate(haul_weight=mean(weight)) %>% ungroup()
  
  trawlids <- unique(dat_pos$trawl_id)
  if(length(trawlids!=0)){
  p <- data.frame(trawl_id = trawlids,
                  p1 = 0,
                  p2 = 0,
                  p3 = 0,
                  p4 = 0)
  
  sizethresholds <- quantile(dat_pos$weight, c(0.15, 0.5, 0.85, 1), na.rm = T)
  for (i in 1:length(trawlids)) {
    haul_sample<- dplyr::filter(dat_pos, trawl_id == trawlids[i])
    if(nrow(haul_sample) > 0 | var(haul_sample$weight >0)) {
      # fit kernel density to weight frequency
      smoothed_w <- KernSmooth::bkde(haul_sample$weight, range.x = c(min(dat_pos$weight), max(dat_pos$weight)), bandwidth = 2)
      # make sure smoother predicts positive or zero density
      smoothed_w$y[smoothed_w$y<0] <- 0
      # calculate proportion by biomass and by number
      p_w_byweight <- smoothed_w$y * smoothed_w$x / sum(smoothed_w$x*smoothed_w$y)
      
      p_w_byweight[p_w_byweight<0] <- 0
      #p_w_bynum[p_w_bynum<0] <- 0
      
      p1 <- sum(p_w_byweight[smoothed_w$x<=sizethresholds[1]])
      p2 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[1] & smoothed_w$x <=sizethresholds[2]])
      p3 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[2] & smoothed_w$x <=sizethresholds[3]])
      p4 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[3]])
      
      p[i,2:5] <- c(p1, p2, p3, p4)
      
    }
    else {
      indx <- which(sizethresholds>haul_sample$weight)
      p[i, min(indx)+1] <- 1
    }
  }
  # add hauls with zero catch back in
  absent = filter(dat_sub, cpue_kg_km2 == 0)
  trawlids <- unique(absent$trawl_id)
  absent.df <- data.frame(trawl_id = trawlids,
                          p1 = 0,
                          p2 = 0,
                          p3 = 0,
                          p4 = 0)
  
  all_hauls <- rbind(p, absent.df)
  all_hauls$trawl_id <- as.numeric(all_hauls$trawl_id)
  dat_sub$median_weight <- median(dat_sub$weight, na.rm=T)
  nlengths <- unique(dat_sub[,c("trawl_id","nlength", "median_weight")])
  meanweight <- unique(dat_test[,c("trawl_id","haul_weight")])
  all_hauls2 <- left_join(all_hauls, nlengths)
  all_hauls2 <- left_join(all_hauls2, meanweight)
  return(all_hauls2)
 }
  }
  }
  if(nrow(dat_sub)>0){
  if(nrow(fitted)==0 & nrow(not_fitted)==0){
    trawlids <- unique(dat_sub$trawl_id)
    absent.df <- data.frame(trawl_id = trawlids,
                            p1 = NA,
                            p2 = NA,
                            p3 = NA,
                            p4 = NA,
                            nlength=0,
                            haul_weight=NA, 
                            median_weight=NA)
    return(absent.df)
  }
  }
  if(nrow(dat_sub)>0){
  if(length(trawlids)==0){
    trawlids <- unique(dat_sub$trawl_id)
    absent.df <- data.frame(trawl_id = trawlids,
                            p1 = NA,
                            p2 = NA,
                            p3 = NA,
                            p4 = NA,
                            nlength=0,
                            haul_weight=NA)
    return(absent.df)
  }
  }
  if(nrow(dat_sub)==0){
    return(warning("species not present in data"))
  }
  }

# Species of interest and max. juvenile lengths (define ontogenetic classes)
length_expand_afsc <- function(sci_name) {
  # load, clean, and join data
  bio2 <-readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_all_levels.RDS")
  catch2 <- readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_cpue_zerofilled.RDS")
  
  #Isolate necessary parts of full data to get specimen weights/lengths per haul
  haul <- bio2$haul
  specimen <- bio2$specimen
  species <- bio2$species
  size <- bio2$size
  
  #make lowercase
  names(haul) <- tolower(names(haul))
  names(specimen) <- tolower(names(specimen))
  names(species) <- tolower(names(species))
  names(size) <- tolower(names(bio2$size))
  
  species$species_name <- tolower(species$species_name)
  
  #Combine size and species
  lengths <- dplyr::left_join(size, species)
  lengths <- dplyr::left_join(lengths, haul)
  
  #Expand to make separate row for each measurement
  lengths <- dplyr::filter(lengths, !is.na(frequency))
  lengths2 <- tidyr::uncount(lengths, weights=frequency)
  
  #Combine
  bio <- dplyr::left_join(specimen, species)
  bio <- dplyr::left_join(bio, haul)
  
  #Combine specimen and length data
  bio3 <- dplyr::bind_rows(bio, lengths)
  
  #Convert length from mm to cm
  bio3$length_cm <- bio3$length*0.1
  
  #Convert weight from g to kg
  bio3$weight <- bio3$weight*0.001
  
  #Combine catch data with species data
  names(catch2) <- tolower(names(catch2))
  catch <- dplyr::left_join(catch2, species)
  
  # filter out species of interest from joined (catch/haul/bio) dataset
  catch_sub = dplyr::filter(catch, species_name == sci_name)
  bio_sub = dplyr::filter(bio3, species_name == sci_name)
  
  #Select only necessary columns for joining
  catch4 <- catch_sub[,c("hauljoin", "survey", "year", "depth_m", "latitude_dd_start", "longitude_dd_start", "cpue_kgkm2", "species_name", "common_name")]
  bio4 <- dplyr::filter(bio_sub[,c("hauljoin", "performance", "species_name", "common_name", "length_cm", "weight", "sex", "age")], !is.na(length_cm))
  #Combine data
  dat <-dplyr::left_join(bio4, catch4, relationship = "many-to-many")
  dat <- dplyr::mutate(dat, trawl_id=hauljoin)
  #According to the codebook https://repository.library.noaa.gov/view/noaa/50147, 0 means Good performance, and the other numbers are for "Satisfactory, and then a "but"..."; negative numbers are Unsatisfactory
  #Dataset already includes only Good and Satisfactory hauls, Unsatisfactory are removed
  
  #If years=T in the function, this will subset data to 1999 onward to remove possibly funky data prior to 1999
  years <- F
  if(years){
  dat_sub = dplyr::filter(dat, year>1999)
  }
  
  if(!years){
    dat_sub = dat
  }
  
  #Add column counting number of length observations per catch
  dat_sub$is_length <- ifelse(!is.na(dat_sub$length_cm), 1,0) 
  dat_sub <- group_by(dat_sub, trawl_id) %>% mutate(nlength=sum(is_length)) %>% ungroup()
  dat_sub$trawl_id <- as.numeric(dat_sub$trawl_id)
  
  #Warning if no species present
  if(nrow(dat_sub)==0){
    warning("species not present in data")
  }
  
  # fit length-weight regression by year to predict fish weights that have lengths only.
  # note a rank-deficiency warning may indicate there is insufficient data for some year/sex combinations (likely for unsexed group)
  
  if(nrow(dat_sub)>0){
  fitted = dat_sub
  # #If region=T in function, this will do the length-weight regression for each broad geographic region (EBS, NBS, and GOA)
  # if(region){
  #   # dplyr::select(trawl_id,year,
  #   #               subsample_wt_kg, total_catch_wt_kg, area_swept_ha_der, cpue_kg_km2,
  #   #               individual_tracking_id, sex, length_cm, weight) %>%
  #   
  # #Create one set of data with only year/survey combinations with at least some weight data:
  #   fitted <-  filter(fitted, !is.na(length_cm)) %>%
  #     group_by(year,survey) %>%
  #     mutate(sum = sum(weight, na.rm=T)) %>%
  #     filter(sum>0) %>%
  #     ungroup()
  #   
  #   #Remove erroneous zero weight observations because cause error in lm()
  #   fitted$weight <- ifelse(fitted$weight==0, NA, fitted$weight)
  #   
  #  #And one for those without any weight data:
  #   not_fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
  #     group_by(year,survey) %>%
  #     mutate(sum = sum(weight, na.rm=T)) %>%
  #     filter(sum==0) %>%
  #     ungroup()
  #   
  # #Fit regression model for years with data
  #   
  #   fitted <-
  #   group_nest(fitted, year,survey) %>%
  #   mutate(
  #     model = purrr::map(data, ~ lm(log(weight) ~ log(length_cm), data = .x)),
  #     tidied = purrr::map(model, broom::tidy),
  #     augmented = purrr::map(model, broom::augment),
  #     predictions = purrr::map2(data, model, modelr::add_predictions)
  #   )
  # }

  #If region=F in function, this will do length-weight regression for all of Alaska combined
      # dplyr::select(trawl_id,year,
      #               subsample_wt_kg, total_catch_wt_kg, area_swept_ha_der, cpue_kg_km2,
      #               individual_tracking_id, sex, length_cm, weight) %>%
      
      #Create one set of data with only year/survey combinations with at least some weight data:
      fitted <-  filter(fitted, !is.na(length_cm)) %>%
        group_by(year) %>%
        mutate(sum = sum(weight, na.rm=T)) %>%
        filter(sum>0) %>%
        ungroup()
      #And one for those without any weight data:
      not_fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
        group_by(year,survey) %>%
        mutate(sum = sum(weight, na.rm=T)) %>%
        filter(sum==0) %>%
        ungroup()
      
      if(nrow(fitted)>0){
      #Fit regression model for years with data
      #Remove weird zero weights
      fitted$weight <- ifelse(fitted$weight==0, NA, fitted$weight)
      fitted <-
        group_nest(fitted, year) %>%
        mutate(
          model = purrr::map(data, ~ lm(log(weight) ~ log(length_cm), data = .x)),
          tidied = purrr::map(model, broom::tidy),
          augmented = purrr::map(model, broom::augment),
          predictions = purrr::map2(data, model, modelr::add_predictions)
        )
  # replace missing weights with predicted weights for years with data
  dat_pos2 = fitted %>%
    tidyr::unnest(predictions) %>%
    dplyr::select(-data, -model, -tidied, -augmented) %>%
    try(dplyr::mutate(weight = ifelse(is.na(weight), exp(pred), weight)))
  
  #combine back with data for years without data
  dat_pos <- bind_rows(dat_pos2, not_fitted)
      }
  if(nrow(fitted)==0){
    dat_pos <- not_fitted
  }
  ##Checked that this works by checking number of rows--looks like it passes!
  
  #If there is no weight data available to get weight-length empirical interpolation, use fishbase to fill in
  # find length-weight relationship parameters for one species
   pars <- rfishbase::length_weight(
    #convert scientific name to first letter capitalized for fishbase
    species_list = stringr::str_to_sentence(sci_name, locale = "en"))
   #Get mean
   a <- mean(pars$a)
   b <- mean(pars$b)
   #For longspine thornyhead, which is not in database for some reason, got these values from fishbase direct page
   if(sci_name=="sebastolobus altivelis"){
     a <- 0.00912
     b=3.09
   }
   #Calculate weight (and convert from g to kg)
   dat_pos <- dplyr::mutate(dat_pos, weight = ifelse(is.na(weight), ((a*length_cm^b)*0.001), weight))
  #convert cm-g units
   #haul level weight
   dat_test <- group_by(dat_pos, trawl_id) %>% mutate(haul_weight=mean(weight)) %>% ungroup()
   
  #make column of trawl_id, which is called hauljoin originally in the AFSC data
  trawlids <- unique(dat_pos$trawl_id)
  if(length(trawlids!=0)){
  p <- data.frame(trawl_id = trawlids,
                  p1 = 0,
                  p2 = 0,
                  p3 = 0,
                  p4 = 0)
  
  sizethresholds <- quantile(dat_pos$weight, c(0.15, 0.5, 0.85, 1), na.rm = T)
  for (i in 1:length(trawlids)) {
    haul_sample<- dplyr::filter(dat_pos, trawl_id == trawlids[i])
    if(nrow(haul_sample) > 0 | var(haul_sample$weight >0)) {
      # fit kernel density to weight frequency
      smoothed_w <- KernSmooth::bkde(haul_sample$weight, range.x = c(min(dat_pos$weight), max(dat_pos$weight)), bandwidth = 2)
      # make sure smoother predicts positive or zero density
      smoothed_w$y[smoothed_w$y<0] <- 0
      # calculate proportion by biomass and by number
      p_w_byweight <- smoothed_w$y * smoothed_w$x / sum(smoothed_w$x*smoothed_w$y)
      
      
      p_w_byweight[p_w_byweight<0] <- 0
      #p_w_bynum[p_w_bynum<0] <- 0
      
      p1 <- sum(p_w_byweight[smoothed_w$x<=sizethresholds[1]])
      p2 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[1] & smoothed_w$x <=sizethresholds[2]])
      p3 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[2] & smoothed_w$x <=sizethresholds[3]])
      p4 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[3]])
      
      p[i,2:5] <- c(p1, p2, p3, p4)
      
    }
    else {
      indx <- which(sizethresholds>haul_sample$weight)
      p[i, min(indx)+1] <- 1
    }
  }
  
  # add hauls with zero catch back in
  absent = filter(dat_sub, cpue_kgkm2 == 0)
  if(nrow(absent)>0){
  trawlids <- unique(absent$trawl_id)
  absent.df <- data.frame(trawl_id = trawlids,
                          p1 = 0,
                          p2 = 0,
                          p3 = 0,
                          p4 = 0)
  
  all_hauls <- rbind(p, absent.df)
  }
  if(nrow(absent)==0){
    all_hauls <- p
  }
  all_hauls$trawl_id <- as.numeric(all_hauls$trawl_id)
  dat_sub$median_weight <- median(dat_sub$weight, na.rm=T)
  nlengths <- unique(dat_sub[,c("trawl_id","nlength", "median_weight")])
  meanweight <- unique(dat_test[,c("trawl_id","haul_weight")])
  all_hauls2 <- left_join(all_hauls, nlengths)
  all_hauls2 <- left_join(all_hauls2, meanweight)
  return(all_hauls2)
  }
  }
  if(nrow(dat_sub)>0){
  if(nrow(fitted)==0 & nrow(not_fitted)==0){
    trawlids <- unique(dat_sub$trawl_id)
    absent.df <- data.frame(trawl_id = trawlids,
                            p1 = NA,
                            p2 = NA,
                            p3 = NA,
                            p4 = NA,
                            nlength=0,
                            haul_weight=NA, 
                            median_weight=NA)
    return(absent.df)
  }
  }
  #If there are no hauls at all with any length measurements, do this instead (because caused an error in the kernel density function otherwise)
if(nrow(dat_sub)>0){
  trawlids <- unique(dat_sub$trawl_id)
 if(length(trawlids)==0){
    absent.df <- data.frame(trawl_id = trawlids,
                            p1 = NA,
                            p2 = NA,
                            p3 = NA,
                            p4 = NA,
                            nlength=0,
                            haul_weight=NA)
    return(absent.df)
 }
}
if(nrow(dat_sub)==0){
  return(warning("species not present in data"))
}
}

load_data_afsc <- function(sci_name, spc, dat.by.size, length=T) {
    bio2 <-readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_all_levels.RDS")
    dat <-readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_cpue_zerofilled.RDS")
    species <- bio2$species
    names(dat) = tolower(names(dat))
    names(species) =tolower(names(species))
    species$species_name <- tolower(species$species_name)
    species$common_name <- tolower(species$common_name)
    dat <- dplyr::left_join(dat, species)
    dat.by.size$trawl_id <- as.character(dat.by.size$trawl_id)
    dat$trawl_id <-as.character(dat$hauljoin)
    dat = dplyr::filter(dat, species_name ==sci_name)
    dat <- dplyr::left_join(dat, dat.by.size, by = "trawl_id")
    # remove tows where there was positive catch but no length measurements
    if(length){
      dat <- dplyr::filter(dat, !is.na(p1))
    }
    # analyze or years and hauls with adequate oxygen and temperature data, within range of occurrence
    
    # get julian day
    dat$julian_day <- rep(NA, nrow(dat))
    haul <- bio2$haul
    names(haul) <- tolower(names(haul))
    haul <- haul[,c("hauljoin", "start_time")]
    dat <- left_join(dat, haul)
    for (i in 1:nrow(dat)) dat$julian_day[i] <- as.POSIXlt(dat$start_time[i], format = "%Y-%b-%d")$yday
    
    #O2 from trawl data is in ml/l 
    # just in case, remove any missing or nonsense values from sensors
    # dat <- dplyr::filter(dat, !is.na(o2), !is.na(sal), !is.na(temp), is.finite(sal))
    # dat <- calc_po2_mi(dat)
    # dat <- dplyr::filter(dat, !is.na(temp), !is.na(mi))
    
    # prepare data and models -------------------------------------------------
    dat$longitude_dd <- dat$longitude_dd_start
    dat$latitude_dd <- dat$latitude_dd_start
    dat$longitude <- dat$longitude_dd
    dat$latitude <- dat$latitude_dd
    dat$scientific_name <- dat$species_name
    dat$cpue_kg_km2 <- dat$cpue_kgkm2
    dat$project <- dat$survey
    dat$event_id <- dat$trawl_id
    dat$date <- as.POSIXct(as.Date(dat$start_time, format = "%Y-%b-%d"))
    dat <- dplyr::select(dat, event_id, common_name, scientific_name, project, year, date, bottom_temperature_c, longitude_dd, latitude_dd, longitude, latitude, cpue_kg_km2,
                         depth_m, julian_day, nlength, median_weight, haul_weight, p1, p2, p3, p4)
    
    
    # UTM transformation
    dat_ll = dat
    sp::coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
    sp::proj4string(dat_ll) <- sp::CRS("+proj=longlat +datum=WGS84")
    # convert to utm with spTransform
    dat_utm = sp::spTransform(dat_ll, 
                              sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
    # convert back from sp object to data frame
    dat = as.data.frame(dat_utm)
    dat = dplyr::rename(dat, X = coords.x1,
                        Y = coords.x2)
    dat$depth <- dat$depth_m
    dat$depth_m <- NULL
    return(dat)
  }

length_expand_bc <- function(sci_name, spc) {
  # load, clean, and join data
  itis <- readRDS("data/fish_raw/BC/species-table.rds")
  haul <- readRDS("data/fish_raw/BC/pbs-haul.rds")
  catch <- readRDS("data/fish_raw/BC/pbs-catch.rds")
  bio2 <- readRDS("data/fish_raw/BC/pbs-bio-samples.rds")
  
  #Merge the official BC bio data and the official BC haul data to get metadata (from Sean, or here: https://open.canada.ca/data/en/dataset/86af7918-c2ab-4f1a-ba83-94c9cebb0e6c)
  bio <- dplyr::full_join(bio2, haul, by="event_id", relationship="many-to-many")
  
  #Combine with species data
  bio <- dplyr::full_join(bio, itis, by="itis", relationship="many-to-many")
  
  #Put bio data in the same format as the NOAA bio data
  #Convert g to kg
  bio$weight <- bio$weight*0.001

  #rename columnns
  bio$scientific_name <- bio$species_science_name
  bio$common_name <- bio$species_common_name
  bio$length_cm <- bio$length
      
  #Clean catch data
  names(catch) = tolower(names(catch))

  #Merge with ITIS information to get scientific name
  catch <- left_join(catch,itis)
  
  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)
  
  #Combine catch data with haul data
  dat <- dplyr::left_join(catch, haul, relationship = "many-to-many")
  
  #Rename missing species
  if(sci_name=="sebastes aleutianus"){
    bio$scientific_name <- ifelse(str_detect(bio$scientific_name, "sebastes aleutianus"), "sebastes aleutianus", bio$scientific_name)
    bio$common_name <- ifelse(str_detect(bio$common_name, "rougheye"), "rougheye rockfish", bio$common_name)
  }
  dat$scientific_name <- dat$species_science_name
  dat$common_name <- dat$species_common_name
  dat$species_science_name <- NULL
  dat$species_common_name <- NULL
  #Combine bio/haul data with catch data
  dat <- dplyr::left_join(dat, filter(bio[,c("event_id", "age","length_cm", "weight", "scientific_name", "common_name")], !is.na(length_cm)), relationship = "many-to-many")
  
  # filter out species of interest from joined (catch/haul/bio) dataset
  dat_sub = dplyr::filter(dat, scientific_name==sci_name)

  dat_sub$event_id <- as.numeric(dat_sub$event_id)
  trawlids <- unique(dat_sub$event_id)
  
  if(nrow(dat_sub)>0) {
  #Add column counting number of length observations per catch
  dat_sub$is_length <- ifelse(!is.na(dat_sub$length_cm), 1,0) 
  dat_sub <- group_by(dat_sub, event_id) %>% mutate(nlength=sum(is_length)) %>% ungroup()
  
  # fit length-weight regression by year to predict fish weights that have lengths only.
  # note a rank-deficiency warning may indicate there is insufficient data for some year/sex combinations (likely for unsexed group)
  
  #Create one set of data with only year/survey combinations with at least some weight data:
  fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
    group_by(year) %>%
    mutate(sum = sum(weight, na.rm=T)) %>%
    filter(sum>0) %>%
    ungroup()
  fitted$weight <- ifelse(fitted$weight==0, NA, fitted$weight)
  #And one for those without any weight data:
  not_fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
    group_by(year) %>%
    mutate(sum = sum(weight, na.rm=T)) %>%
    filter(sum==0) %>%
    ungroup()

  #Fit regression model for years with data
  fitted <-
    group_nest(fitted, year) %>%
    mutate(
      model = purrr::map(data, ~ lm(log(weight) ~ log(length_cm), data = .x)),
      tidied = purrr::map(model, broom::tidy),
      augmented = purrr::map(model, broom::augment),
      predictions = purrr::map2(data, model, modelr::add_predictions)
    )
  
  # replace missing weights with predicted weights
  dat_pos2 = fitted %>%
    tidyr::unnest(predictions) %>%
    dplyr::select(-data, -model, -tidied, -augmented) %>%
    try(dplyr::mutate(weight = ifelse(is.na(weight), exp(pred), weight)))
  
  #combine back with data for years without data
  dat_pos <- bind_rows(dat_pos2, not_fitted)
  
  ##Checked that this works by checking number of rows--looks like it passes!
  
  #If there is no weight data available to get weight-length empirical interpolation, use fishbase to fill in
  # find length-weight relationship parameters for one species
  pars <- rfishbase::length_weight(
    #convert scientific name to first letter capitalized for fishbase
    species_list = stringr::str_to_sentence(sci_name, locale = "en"))
  #Get mean
  a <- mean(pars$a)
  b <- mean(pars$b)
  #For longspine thornyhead, which is not in database for some reason, got these values from fishbase direct page
  if(sci_name=="sebastolobus altivelis"){
    a <- 0.00912
    b=3.09
  }
  #Calculate weight (and convert from g to kg)
  dat_pos <- dplyr::mutate(dat_pos, weight = ifelse(is.na(weight), ((a*length_cm^b)*0.001), weight))
  dat_test <- group_by(dat_pos, event_id) %>% mutate(haul_weight=mean(weight)) %>% ungroup()
  
  trawlids <- unique(dat_pos$event_id)
  if(length(trawlids!=0)){
  p <- data.frame(event_id = trawlids,
                  p1 = 0,
                  p2 = 0,
                  p3 = 0,
                  p4 = 0)
  
  sizethresholds <- quantile(dat_pos$weight, c(0.15, 0.5, 0.85, 1), na.rm = T)
  for (i in 1:length(trawlids)) {
    haul_sample<- dplyr::filter(dat_pos, event_id == trawlids[i])
    if(nrow(haul_sample) > 0 | var(haul_sample$weight >0)) {
      # fit kernel density to weight frequency
      smoothed_w <- KernSmooth::bkde(haul_sample$weight, range.x = c(min(dat_pos$weight), max(dat_pos$weight)), bandwidth = 2)
      # make sure smoother predicts positive or zero density
      smoothed_w$y[smoothed_w$y<0] <- 0
      # calculate proportion by biomass and by number
      p_w_byweight <- smoothed_w$y * smoothed_w$x / sum(smoothed_w$x*smoothed_w$y)
      
      p_w_byweight[p_w_byweight<0] <- 0
      #p_w_bynum[p_w_bynum<0] <- 0
      
      p1 <- sum(p_w_byweight[smoothed_w$x<=sizethresholds[1]])
      p2 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[1] & smoothed_w$x <=sizethresholds[2]])
      p3 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[2] & smoothed_w$x <=sizethresholds[3]])
      p4 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[3]])
      
      p[i,2:5] <- c(p1, p2, p3, p4)
      
    }
    else {
      indx <- which(sizethresholds>haul_sample$weight)
      p[i, min(indx)+1] <- 1
    }
  }
  
  # add hauls with zero catch back in
  absent = filter(dat_sub, catch_weight == 0)
  trawlids <- unique(absent$event_id)
  absent.df <- data.frame(event_id = trawlids,
                          p1 = 0,
                          p2 = 0,
                          p3 = 0,
                          p4 = 0)
  
  all_hauls <- rbind(p, absent.df)
  all_hauls$event_id <- as.numeric(all_hauls$event_id)
  dat_sub$median_weight <- median(dat_sub$weight, na.rm=T)
  nlengths <- unique(dat_sub[,c("event_id","nlength", "median_weight")])
  meanweight <- unique(dat_test[,c("event_id","haul_weight")])
  all_hauls2 <- left_join(all_hauls, nlengths)
  all_hauls2 <- left_join(all_hauls2, meanweight)
  return(all_hauls2)
  }
  }
  if(nrow(dat_sub)>0){
  if(nrow(fitted)==0 & nrow(not_fitted)==0){
    trawlids <- unique(dat_sub$trawl_id)
    absent.df <- data.frame(trawl_id = trawlids,
                            p1 = NA,
                            p2 = NA,
                            p3 = NA,
                            p4 = NA,
                            nlength=0,
                            haul_weight=NA, 
                            median_weight=NA)
    return(absent.df)
  }
  }
  if(nrow(dat_sub)>0){
  if(length(trawlids)==0){
    absent.df <- data.frame(trawl_id = trawlids,
                            p1 = NA,
                            p2 =NA,
                            p3 = NA,
                            p4 = NA,
                            nlength=0,
                            haul_weight=NA)
    return(absent.df)
  }
  }
  if(nrow(dat_sub)==0){
    return(warning("species not present in data"))
  }
}

load_data_bc <- function(sci_name,dat.by.size, length=T, spc) {
  catch <- readRDS("data/fish_raw/BC/pbs-catch.rds")
  haul <- readRDS("data/fish_raw/BC/pbs-haul.rds")
  itis  <- readRDS("data/fish_raw/BC/itis_bc.rds")

  catch <- left_join(catch,itis)
  
  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)
  
  #Combine catch data with haul data
  dat <- dplyr::left_join(catch, haul, relationship = "many-to-many")
  
 # dat.by.size$event_id <- as.character(dat.by.size$event_id)
  dat = dplyr::filter(dat, scientific_name == sci_name)
  dat <- left_join(dat, dat.by.size, by = "event_id")
  # remove tows where there was positive catch but no length measurements
  if(length){
    dat <- dplyr::filter(dat, !is.na(p1))
  }
  # analyze or years and hauls with adequate oxygen and temperature data, within range of occurrence
  
  # get julian day
  dat$julian_day <- rep(NA, nrow(dat))
  for (i in 1:nrow(dat)) dat$julian_day[i] <- as.POSIXlt(dat$date[i], format = "%Y-%b-%d")$yday
  
  
  #O2 from trawl data is in ml/l 
  # just in case, remove any missing or nonsense values from sensors
  # dat <- dplyr::filter(dat, !is.na(o2), !is.na(sal), !is.na(temp), is.finite(sal))
  # dat <- calc_po2_mi(dat)
  # dat <- dplyr::filter(dat, !is.na(temp), !is.na(mi))
  
  # prepare data and models -------------------------------------------------
  dat$cpue_kg_km2 <- dat$catch_weight
  dat$longitude_dd <- dat$lon_start
  dat$latitude_dd <- dat$lat_start
  dat$longitude <- dat$lon_start
  dat$latitude <- dat$lat_start
  dat$event_id <- as.character(dat$event_id)
  dat$year <- substr(dat$date, start=1, stop=4)
  dat$year <- as.integer(dat$year)
  dat$date <- as.POSIXct(as.Date(dat$date, format = "%Y-%b-%d"))
  dat$project <- dat$survey_name
  dat$salinity_psu <- dat$salinity_PSU
  dat$salinity_PSU <- NULL
  dat <- dplyr::select(dat, event_id, scientific_name, project, year, date, longitude_dd, latitude_dd, longitude, latitude, cpue_kg_km2,
                       depth_m, julian_day, nlength,median_weight, haul_weight, pass, p1, p2, p3, p4, temperature_C, do_mlpL, salinity_psu)
  dat <- filter(dat, !is.na(latitude_dd))
  
  
  # UTM transformation
  dat_ll = dat
  sp::coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
  sp::proj4string(dat_ll) <- sp::CRS("+proj=longlat +datum=WGS84")
  # convert to utm with spTransform
  dat_utm = sp::spTransform(dat_ll, 
                        sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
  # convert back from sp object to data frame
  dat = as.data.frame(dat_utm)
  dat = dplyr::rename(dat, X = coords.x1,
                      Y = coords.x2)
  dat$common_name <- spc
  dat$depth <- dat$depth_m
  dat$depth_m <- NULL
  
  #convert oxygen mg/L to umol_kg
  SA = gsw_SA_from_SP(dat$salinity_psu,dat$depth,dat$longitude,dat$latitude) #absolute salinity for pot T calc
  pt = gsw_pt_from_t(SA,dat$temperature_C,dat$depth) #potential temp at a particular depth
  CT = gsw_CT_from_t(SA,dat$temperature_C,dat$depth) #conservative temp
  dat$sigma0_kgm3 = gsw_sigma0(SA,CT)
  dat$O2_umolkg = dat$do_mlpL*44660/(dat$sigma0_kgm3+1000) 
  
  return(dat)
}

combine_all <- function(type){
  #BC
  #BC raw data
  bio_qc <- read.csv("data/fish_raw/BC/QCS_biology.csv")
  bio_vi <- read.csv("data/fish_raw/BC/WCVI_biology.csv")
  bio_hs <- read.csv("data/fish_raw/BC/HS_biology.csv")
  bio_hg <- read.csv("data/fish_raw/BC/WCHG_biology.csv")
  itis  <- readRDS("data/fish_raw/BC/itis_bc.rds")
  
  haul <- readRDS("data/fish_raw/BC/pbs-haul.rds")
  catch <- readRDS("data/fish_raw/BC/pbs-catch.rds")
  
  haul_qc <- read.csv("data/fish_raw/BC/QCS_effort.csv")
  haul_vi <- read.csv("data/fish_raw/BC/WCVI_effort.csv")
  haul_hs <- read.csv("data/fish_raw/BC/HS_effort.csv")
  haul_hg <- read.csv("data/fish_raw/BC/WCHG_effort.csv")
  
  #Combine BC bio and haul data to combine together
  bio2 <- rbind(bio_hg, bio_hs, bio_qc, bio_vi)
  haul2 <- rbind(haul_hg, haul_hs, haul_qc, haul_vi)
  
  #Merge the official BC bio data and the official BC haul data to get metadata (from here: https://open.canada.ca/data/en/dataset/86af7918-c2ab-4f1a-ba83-94c9cebb0e6c)
  bio2$Set.number <- bio2$Tow.number
  bio <- dplyr::left_join(bio2, haul2, relationship="many-to-many")
  
  names(bio) = tolower(names(bio))
  names(haul) = tolower(names(haul))
  
  #Put bio data in the same format as the NOAA bio data
  bio$scientific_name <- tolower(bio$scientific.name)
  bio$common_name <- tolower(bio$english.common.name)
  bio$weight <- bio$weight..g.*0.001
  bio$length_cm <- bio$fork.length..mm.*0.1
  bio$length_cm <- ifelse(is.na(bio$length_cm), (bio$total.length..mm.*0.1), bio$length_cm)
  
  #Make column names consistent so can join to surveyjoin haul data to get event_id
  haul$year <- as.character(substr(haul$date, start=1, stop=4))
  bio$year <- as.character(bio$survey.year)
  haul$set.date <- substr(haul$date, start=1, stop=10)
  bio$lat_start <- bio$start.latitude
  bio$lat_end <- bio$end.latitude
  bio$lon_start <- bio$start.longitude
  bio$lon_end <- bio$end.longitude
  
  #Combine BC bio data and haul metadata with surveyjoin metadata
  bio3 <- dplyr::left_join(bio[,c("age", "sex", "length_cm", "weight", "scientific_name", "common_name", "lat_start", "lon_start", "lat_end", "lon_end", "set.date")], haul, relationship="many-to-many")
  bio <- bio3
  
  #Clean catch data
  names(catch) = tolower(names(catch))

  catch <- dplyr::left_join(catch,itis)
  
  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)
  
  #Combine catch data with haul data
  dat_bc <- dplyr::left_join(catch, haul, relationship = "many-to-many")
  #Combine bio/haul data with catch data
  #Only positive catches
  dat_bc <- subset(dat_bc, catch_weight>0)
  dat_bc$cpue_kg_km2 <- dat_bc$catch_weight
  #Biomass
  if(type=="hauls"){
  counts_bc <- aggregate(cpue_kg_km2~scientific_name, dat_bc, FUN=length)
  }
  if(type=="biomass"){
    counts_bc <- aggregate(cpue_kg_km2~scientific_name, dat_bc, FUN=sum)
  }
  
   #Add common name
  counts_bc <- unique(dplyr::left_join(counts_bc, bio[,c("scientific_name", "common_name")]))
  counts_bc <- dplyr::mutate(counts_bc, bc=cpue_kg_km2)
  counts_bc$cpue_kg_km2 <- NULL
  rm(list=setdiff(ls(), "counts_bc"))
  
#NWFSC
  # load, clean, and join data
  bio <- readRDS("data/fish_raw/NOAA/nwfsc_bio.rds")
  load("data/fish_raw/NOAA/nwfsc_haul.rda")
  haul <- nwfsc_haul
  catch <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
  names(catch) = tolower(names(catch))
  names(bio) = tolower(names(bio))
  names(haul) = tolower(names(haul))
  bio$scientific_name <- tolower(bio$scientific_name)
  bio$common_name <- tolower(bio$common_name)
  catch$common_name <- tolower(catch$common_name)
  
  bio$trawl_id = as.character(bio$trawl_id)
  haul$trawl_id = as.character(haul$event_id)
  catch$trawl_id=as.character(catch$trawl_id)
  haul$year <- as.character(substr(haul$date, start=1, stop=4))
  bio$year <- as.character(bio$year)
  catch$date <- NULL
  bio$date <- NULL
  bio$year <- NULL
  
  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)
  
  #Combine data
  dat = dplyr::left_join(catch[,c("trawl_id","common_name", "subsample_count","area_swept_ha","longitude_dd", "latitude_dd",
                                  "subsample_wt_kg","total_catch_numbers","total_catch_wt_kg","cpue_kg_km2")], haul, relationship = "many-to-many") %>%
    dplyr::left_join(filter(bio[,c("trawl_id", "scientific_name", "common_name", "weight", "ageing_lab", "oto_id", "length_cm", "width_cm", "sex", "age")], !is.na(length_cm)), relationship = "many-to-many") %>%
    filter(performance == "Satisfactory")

#Only positive catches
dat_nw <- subset(dat, cpue_kg_km2>0)
#Biomass
if(type=="hauls"){
  counts_nw <- aggregate(cpue_kg_km2~scientific_name, dat_nw, FUN=length)
}
if(type=="biomass"){
  counts_nw <- aggregate(cpue_kg_km2~scientific_name, dat_nw, FUN=sum)
}
counts_nw <- unique(left_join(counts_nw, bio[,c("scientific_name", "common_name")]))
counts_nw <- dplyr::mutate(counts_nw, nw=cpue_kg_km2)
counts_nw$cpue_kg_km2 <- NULL

rm(list=setdiff(ls(), c("counts_bc", "counts_nw")))

##Alaska
bio2 <-readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_all_levels.RDS")
catch2 <- readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_cpue_zerofilled.RDS")

#Isolate necessary parts of full data to get specimen weights/lengths per haul
haul <- bio2$haul
specimen <- bio2$specimen
species <- bio2$species
size <- bio2$size

#make lowercase
names(haul) <- tolower(names(haul))
names(specimen) <- tolower(names(specimen))
names(species) <- tolower(names(species))
names(size) <- tolower(names(bio2$size))

species$species_name <- tolower(species$species_name)

#Combine catch data with species data
names(catch2) <- tolower(names(catch2))
catch <- dplyr::left_join(catch2, species)

#Select only necessary columns for joining
catch4 <- catch[,c("hauljoin", "survey", "year", "depth_m", "latitude_dd_start", "longitude_dd_start", "cpue_kgkm2", "species_name", "common_name")]
dat <- dplyr::filter(catch4, year>1998)
#bio4 <- dplyr::filter(bio4, year>1998)

#Combine data
dat <- dplyr::mutate(dat, trawl_id=hauljoin)
#According to the codebook https://repository.library.noaa.gov/view/noaa/50147, 0 means Good performance, and the other numbers are for "Satisfactory, and then a "but"..."; negative numbers are Unsatisfactory
#Dataset already includes only Good and Satisfactory hauls, Unsatisfactory are removed

#Only positive catches
dat$cpue_kg_km2 <- dat$cpue_kgkm2
dat_ak <- subset(dat, cpue_kg_km2>0)
dat_ak$scientific_name <- dat_ak$species_name
#Biomass
if(type=="hauls"){
  counts_ak <- aggregate(cpue_kg_km2~scientific_name, dat_ak, FUN=length)
}
if(type=="biomass"){
  counts_ak <- aggregate(cpue_kg_km2~scientific_name, dat_ak, FUN=sum)
}
counts_ak <- dplyr::mutate(counts_ak, ak=cpue_kg_km2)
counts_ak$cpue_kg_km2 <- NULL
counts_ak <- dplyr::left_join(counts_ak, species[,c("common_name", "species_name")], by=c("scientific_name"="species_name"))
counts_ak$common_name <- tolower(counts_ak$common_name)

#counts_ak <- unique(left_join(counts_nw, bio4[,c("scientific_name", "common_name")]))

#Combine
counts <- full_join(counts_nw, counts_bc)
counts <- full_join(counts, counts_ak)
return(counts)
}

IPHC <- function (catch, adjustment) {
  
  #Calculate CPUE as U32 and O32 count and weight divided by effective skates hauled
  catch$cpue_O32_count <- catch$`O32 Pacific halibut count`/catch$`Effective skates hauled`
  catch$cpue_U32_count <- catch$`U32 Pacific halibut count`/catch$`Effective skates hauled`
  catch$cpue_O32_weight <- catch$`O32 Pacific halibut weight`/catch$`Effective skates hauled`
  catch$cpue_U32_weight <- catch$`U32 Pacific halibut weight`/catch$`Effective skates hauled`
  
  #make lowercase
  colnames(catch) <- tolower(colnames(catch))
  colnames(adjustment) <- tolower(colnames(adjustment))
  
  #join
  adjustment$stlkey <- as.character(adjustment$stlkey)
  data <- left_join(catch, adjustment, by="stlkey")
  data$h.adj <- as.numeric(data$h.adj)
  
  #Calculate ajustment factor
  data$cpue_o32_count <- data$cpue_o32_count * data$h.adj
  data$cpue_u32_count <- data$cpue_u32_count * data$h.adj
  data$cpue_o32_weight <- data$cpue_o32_weight * data$h.adj
  data$cpue_u32_weight <- data$cpue_u32_weight * data$h.adj
  
  #Extract columns of interest
  dat <- data[,c("year.x", "date.x", "midlat", "midlon", "avgdepth (fm)", "temp c", "salinity psu", "oxygen_ml",  "cpue_o32_weight", "cpue_u32_weight")]

  #Re-name columns to match the NOAA and BC data
  colnames(dat) <- c("year", "date", "latitude", "longitude", "depth", "temperature_C", "salinity_psu", "do_mlpL", "cpue_o32_weight", "cpue_u32_weight")
  
  #Add columns to match the NOAA and BC data
  dat$project <- "iphc"
  dat$common_name <- "pacific halibut"
  dat$scientific_name <- "hippoglossus stenolepis"
  
  #Date and month in right format
  dat$month <- case_when(grepl("May",dat$date) ~5,
                         grepl("Jun",dat$date)  ~6,
                         grepl("Jul",dat$date)  ~7,
                         grepl("Aug",dat$date)  ~8,
                         grepl("Sep",dat$date)  ~9,
                         grepl("Oct",dat$date)  ~10)
  dat$day <- as.numeric(substr(dat$date, 1,2))
  dat$date <-  as.POSIXct(as.Date(with(dat,paste(year,month,day,sep="-")),"%Y-%m-%d"))
  dat$doy <- as.POSIXlt(dat$date, format = "%Y-%b-%d")$yday
  dat$year <- as.numeric(dat$year)
  dat$day <- NULL
  
  
  #convert oxygen mg/L to umol_kg
  SA = gsw_SA_from_SP(dat$salinity_psu,dat$depth,dat$longitude,dat$latitude) #absolute salinity for pot T calc
  pt = gsw_pt_from_t(SA,dat$temperature_C,dat$depth) #potential temp at a particular depth
  CT = gsw_CT_from_t(SA,dat$temperature_C,dat$depth) #conservative temp
  dat$sigma0_kgm3 = gsw_sigma0(SA,CT)
  dat$O2_umolkg = dat$do_mlpL*44660/(dat$sigma0_kgm3+1000) 
  
  #Convert coordinates
  dat <- subset(dat, !is.na(latitude))
  dat <- dat %>%
    st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%  
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>% 
    mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) 
  
  return(dat)
}
   
calc_po2_sat <- function(salinity, temp, depth, oxygen, lat, long, umol_m3, ml_L) {
  # Input:       S = Salinity (pss-78)
  #              T = Temp (deg C) ! use potential temp
  #depth is in meters
  
  #Pena et al. ROMS and GLORYS are in mmol per m^3 (needs to be converted to umol per kg) (o2 from trawl data was in mL/L, so had to do extra conversions)
  gas_const = 8.31
  partial_molar_vol = 0.000032
  kelvin = 273.15
  boltz = 0.000086173324
 
 #lol this was dumb because it is actually equivalent to umol/kg
  #convert mmol to umol
   umol_m3 <- oxygen*1000
  #convert m3 to l
   umol_l <- umol_m3/1000
  #convert from molality (moles per volume) to molarity (moles per mass)
   #1 L of water = 1 kg of water, so no equation needed?? Right??
   o2_umolkg <- umol_l * 1/1
  
  SA = gsw_SA_from_SP(salinity,depth,long,lat) #absolute salinity for pot T calc
  pt = gsw_pt_from_t(SA,temp,depth) #potential temp at a particular depth
  #this is for if using data that has oxygen in ml/L
  #CT = gsw_CT_from_t(SA,temp,depth) #conservative temp
  #sigma0 = gsw_sigma0(SA,CT)
 # o2_umolkg = oxygen*44660/(sigma0+1000) 
  
  O2_Sat0 = gsw_O2sol_SP_pt(salinity,pt)

  #= o2satv2a(sal,pt) #uses practical salinity and potential temp - solubity at p =1 atm
  press = exp(depth*10000*partial_molar_vol/gas_const/(temp+kelvin))
  O2_satdepth = O2_Sat0*press
  
  #solubility at p=0
  sol0 = O2_Sat0/0.209
  sol_Dep = sol0*press
  po2 = o2_umolkg/sol_Dep
  po2 <- po2 * 101.325 # convert to kPa
  return(po2)

}

calc_mi <- function(Eo, Ao, W, n,po2, inv.temp) {
  mi = W^n*Ao*po2 *exp(Eo * inv.temp)
  return(mi)
}

# calc o2 solubility, relies on o2 in umol/kg
gsw_O2sol_SP_pt <- function(sal,pt) {
  x = sal
  pt68 = pt*1.00024
  y = log((298.15 - pt68)/(273.15 + pt68))
  
  a0 =  5.80871
  a1 =  3.20291
  a2 =  4.17887
  a3 =  5.10006
  a4 = -9.86643e-2
  a5 =  3.80369
  b0 = -7.01577e-3
  b1 = -7.70028e-3
  b2 = -1.13864e-2
  b3 = -9.51519e-3
  c0 = -2.75915e-7
  
  O2sol = exp(a0 + y*(a1 + y*(a2 + y*(a3 + y*(a4 + a5*y)))) + x*(b0 + y*(b1 + y*(b2 + b3*y)) + c0*x))
  return(O2sol)
}

prepare_data <- function(spc,sci_name, ROMS, GLORYS, in_situ){
dat.by.size <- try(length_expand_bc(sci_name))
gc()
if(is.data.frame(dat.by.size)){
dat3 <- try(load_data_bc(sci_name = sci_name, spc=spc, dat.by.size = dat.by.size, length=F))
}
gc()
rm(dat.by.size)

dat.by.size <- try(length_expand_nwfsc(spc=spc, sci_name=sci_name))
gc()
if(is.data.frame(dat.by.size)){
dat2 <- try(load_data_nwfsc(spc= spc, sci_name=sci_name, dat.by.size = dat.by.size, length=F))
}

gc()
rm(dat.by.size)
dat.by.size <- try(length_expand_afsc(sci_name))
gc()
if(is.data.frame(dat.by.size)){
dat5 <- try(load_data_afsc(sci_name = sci_name, spc=spc, dat.by.size = dat.by.size, length=F))
}

gc()

#All regions present
if(exists("dat3") & exists("dat2") & exists("dat5")){
dat4 <- bind_rows(dat3, dat2, dat5)
}
#Only BC
if(exists("dat3") & !exists("dat2") & !exists("dat5")){
  dat4 <- dat3
}
#Only AK
if(!exists("dat3") & !exists("dat2") & exists("dat5")){
  dat4 <- dat5
}

#Only NWFSC
if(!exists("dat3") & exists("dat2") & !exists("dat5")){
  dat4 <- dat2
  
}
#BC & NW
if(exists("dat3") & exists("dat2") & !exists("dat5")){
  dat4 <- bind_rows(dat3, dat2)
}
#BC & AK
if(exists("dat3") & !exists("dat2")& exists("dat5")){
  dat4 <- bind_rows(dat3, dat5)
}
#AK & NW
if(!exists("dat3") & exists("dat2")& exists("dat5")){
  dat4 <- bind_rows(dat2, dat5)
}

if(spc=="pacific halibut" & (ROMS==T| GLORYS==T)){
  catch <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/Set and Pacific halibut data.xlsx")
  adjustment <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/iphc-2023-fiss-hadj-20231031.xlsx")
  dat_IPHC <- IPHC(catch, adjustment)
  dat4 <- bind_rows(dat4, dat_IPHC)
}

#List of species--will get to this 
#sci_names <- c("")
#spcs <- c("")

if(ROMS==T){
#Combine with ROMS data
ROMS <- as.data.frame(readRDS("data/haul_combined_ROMS.rds"))
ROMS2 <- as.data.frame(ROMS[,c("event_id","o2_ROMS", "temp_ROMS", "sal_ROMS")])
ROMS2$event_id <- as.character(ROMS2$event_id)
ROMS2$o2_ROMS <- as.numeric(ROMS2$o2_ROMS)
ROMS2$temp_ROMS <- as.numeric(ROMS2$temp_ROMS)
ROMS2$sal_ROMS <- as.numeric(ROMS2$sal_ROMS)

dat <- full_join(dat4, ROMS2, by="event_id")

#Subset to what has ROMS data
dat <- subset(dat, lat>46)
dat <- subset(dat, year<2022)

### Set up environmental data ###
##Set NA for missing ROMS data
dat$o2_ROMS <- ifelse(dat$o2_ROMS>=0, dat$o2_ROMS, NA)
dat$temp_ROMS <- ifelse(dat$temp_ROMS>=0, dat$temp_ROMS, NA)
dat$sal_ROMS <- ifelse(dat$sal_ROMS>=0, dat$sal_ROMS, NA)
}

###Add glorys data
if(GLORYS==T){
glorys <- readRDS("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/data/glorys_combined.rds")
#Isolate just variables of interest
glorys <- glorys[,c("o2_glorys", "temp_glorys", "sal_glorys", "date_haul", "event_id", "stlkey", "depth_gloryso2")]
#Combine stlkey and event_id
glorys$event_id <- ifelse(is.na(glorys$event_id), glorys$stlkey, glorys$event_id)
glorys$stlkey <- NULL
glorys$event_id <- as.character(glorys$event_id)

#Combine glorys with data
dat <- left_join(dat4, glorys, by="event_id")

#Remove before 1993 and after 2020
#dat <- subset(dat, dat$year=="1993"|dat$year=="1994"|dat$year=="1995"|dat$year=="1996"|dat$year=="1997"|dat$year=="1998"|dat$year=="1999"|dat$year=="2000"|dat$year=="2001"|dat$year=="2002"|dat$year=="2003"|dat$year=="2004"|dat$year=="2005"|dat$year=="2006"|dat$year=="2007"|dat$year=="2008"|dat$year=="2009"|dat$year=="2010"|dat$year=="2011"|dat$year=="2012"|dat$year=="2013"|dat$year=="2014"|dat$year=="2015"|dat$year=="2016"|dat$year=="2017"|dat$year=="2018"|dat$year=="2019")

## Convert oxygen from glorys concentration (mmol/m^3) to kPa
dat$po2 <- calc_po2_sat(salinity=dat$sal_glorys, temp=dat$temp_glorys, depth=dat$depth_gloryso2, oxygen=dat$o2_glorys, lat=dat$lat, long=dat$long, umol_m3=T, ml_L=F)

##Calculate inverse temp
kelvin = 273.15
boltz = 0.000086173324
tref <- 12
dat$invtemp <- (1 / boltz)  * ( 1 / (dat$temp_glorys + 273.15) - 1 / (tref + 273.15))

##Calculate Metabolic index 
#Pull parameters ((pulled in code file 2a))
MI_pars <- readRDS("metabolic_index-main/MI_pars.rds")
Eo1 <-  MI_pars[1,3] #Median
Eo2 <-  Eo1-(MI_pars[2,3]*1.28)    #low 90th percentile, close to zero
Eo3 <-  Eo1+(MI_pars[2,3]*1.28)    #High 90th percentile, close to zero
Ao <- 1/exp(MI_pars[1,1])
n <- MI_pars[1,2]
#Average weight across all hauls--in case want to make universal
dat$mean_weight <- mean(dat$median_weight, na.rm=T)

#Calculate metabolic index with each Eo
dat$mi1 <- calc_mi(Eo1, Ao, dat$mean_weight,  n, dat$po2, dat$invtemp)
dat$mi2 <- calc_mi(Eo2, Ao, dat$mean_weight, n, dat$po2, dat$invtemp)
dat$mi3 <- calc_mi(Eo3, Ao, dat$mean_weight, n, dat$po2, dat$invtemp)

#For average body size from each haul
dat$mi5 <- calc_mi(Eo1, Ao, dat$haul_weight,  n, dat$po2, dat$invtemp)

##Scale and such
dat$temp_s <- (scale(dat$temp_glorys))
dat$po2_s <- (scale(dat$o2_glorys))
dat$mi1_s <-(scale(dat$mi1))
dat$mi2_s <-(scale(dat$mi2))
dat$mi3_s <-(scale(dat$mi3))
dat$log_depth_scaled <- scale(log(dat$depth_m))
dat$log_depth_scaled2 <- with(dat, log_depth_scaled ^ 2)
dat$jday_scaled <- scale(dat$julian_day)
dat$jday_scaled2 <- with(dat, jday_scaled ^ 2)
dat$cpue_kg_km2_sub <- dat$cpue_kg_km2 * (dat$p2+dat$p3)
dat$cpue_kg_km2_sub <- ifelse(dat$cpue_kg_km2==0, 0, dat$cpue_kg_km2_sub)
}

#Make broader region for BC
if(GLORYS==F & ROMS==F){
  dat <- dat4
}

###Add in situ data####
if(GLORYS==F & ROMS==F){
#Isolate just NWFSC and EBS/NBS/GOA data for joining
insitu <- filter(insitu, survey!="iphc")
insitu <- filter(insitu, survey!="dfo")
dat6 <- filter(dat4, project=="EBS"|project=="NBS"|project=="NWFSC.Combo"|project=="GOA")
#Just columns of interest
dat6 <- left_join(dat6[,c("event_id", "date", "year", "project", "latitude", "longitude", "depth", "X", "Y", "cpue_kg_km2", "julian_day", "nlength", "median_weight", "haul_weight", "pass", "p1", "p2", "p3", "p4", "common_name", "scientific_name", "depth", "vessel", "tow", "bottom_temperature_c")], insitu, by=c("date", "latitude", "longitude"))
#Edit columns
dat6$event_id <- dat6$event_id.x
dat6$event_id.x <- NULL
dat6$event_id.y <- NULL
dat6$year <- dat6$year.x
dat6$year.y <- NULL
dat6$year.x <- NULL
dat6$depth <- dat6$depth.x
dat6$depth.x <- NULL
dat6$depth.y <- NULL
dat6$survey <- NULL
dat6$X <- dat6$X.x
dat6$Y <- dat6$Y.x
dat6$X.x <- NULL
dat6$X.y <- NULL
dat6$Y.x <- NULL
dat6$Y.y <- NULL
dat6$depth.1 <- NULL
dat6$month <- NULL
dat6$doy <- NULL

#Recombine back with DFO data
if(exists("dat3")){
dat <- bind_rows(dat6, dat3)
}

if(!exists("dat3")){
  dat <- dat6
}

dat$doy <- dat$julian_day
dat$julian_day <- NULL
dat$month <- month(dat$date)

#Add IPHC data
if(spc=="pacific halibut"){
  catch <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/Set and Pacific halibut data.xlsx")
  adjustment <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/iphc-2023-fiss-hadj-20231031.xlsx")
  dat_IPHC <- IPHC(catch, adjustment)
  dat <- bind_rows(dat, dat_IPHC)
}


#Some other columns
dat$survey<- ifelse(str_detect(dat$project, "SYN"), "dfo", dat$project)
dat$cpue_kg_km2_sub <- dat$cpue_kg_km2 * (dat$p2+dat$p3)
dat$cpue_kg_km2_sub <- ifelse(dat$cpue_kg_km2==0, 0, dat$cpue_kg_km2_sub)
dat$log_depth_scaled <- scale(log(dat$depth))
dat$log_depth_scaled2 <- with(dat, log_depth_scaled ^ 2)

##Calculate metabolic index
## Convert oxygen from glorys concentration (mmol/m^3) to kPa

dat$po2 <- calc_po2_sat(salinity=dat$salinity_psu, temp=dat$temperature_C, depth=dat$depth, oxygen=dat$O2_umolkg, lat=dat$latitude, long=dat$longitude, umol_m3=T, ml_L=F)

##Calculate inverse temp
kelvin = 273.15
boltz = 0.000086173324
tref <- 12
dat$invtemp <- (1 / boltz)  * ( 1 / (dat$temperature_C + 273.15) - 1 / (tref + 273.15))

##Calculate Metabolic index 
#Pull parameters ((pulled in code file 2a))
MI_pars <- readRDS("metabolic_index-main/MI_pars.rds")
Eo1 <-  MI_pars[1,3] #Median
Eo2 <-  Eo1-(MI_pars[2,3]*1.28)    #low 90th percentile, close to zero
Eo3 <-  Eo1+(MI_pars[2,3]*1.28)    #High 90th percentile, close to zero
Ao <- 1/exp(MI_pars[1,1])
n <- MI_pars[1,2]
#Average weight across all hauls--in case want to make universal
dat$mean_weight <- mean(dat$median_weight, na.rm=T)

#Calculate metabolic index with each Eo
dat$mi1 <- calc_mi(Eo1, Ao, dat$mean_weight,  n, dat$po2, dat$invtemp)
dat$mi2 <- calc_mi(Eo2, Ao, dat$mean_weight, n, dat$po2, dat$invtemp)
dat$mi3 <- calc_mi(Eo3, Ao, dat$mean_weight, n, dat$po2, dat$invtemp)

#For average body size from each haul
dat$mi5 <- calc_mi(Eo1, Ao, dat$haul_weight,  n, dat$po2, dat$invtemp)

##Scale and such
dat$temp_s <- (scale(dat$temperature_C))
dat$po2_s <- (scale(dat$po2))
dat$mi1_s <-(scale(dat$mi1))
dat$mi2_s <-(scale(dat$mi2))
dat$mi3_s <-(scale(dat$mi3))
dat$mi5_s <- scale(dat$mi5)

#Reorder columns
if(spc!="pacific halibut"){
dat <- relocate(dat, scientific_name, common_name, project, survey, year, date, doy, month, depth, longitude, latitude, cpue_kg_km2, cpue_kg_km2_sub, salinity_psu, temperature_C, po2, sigma0_kgm3,do_mlpL, O2_umolkg, log_depth_scaled, log_depth_scaled2, X, Y, invtemp, mi1, mi2, mi3, mi5, temp_s, po2_s, mi1_s, mi2_s, mi3_s, mi5_s, p1,p2,p3,p4,median_weight, mean_weight, haul_weight, nlength, pass, vessel, tow, bottom_temperature_c)
}
if(spc=="pacific halibut"){
dat <- relocate(dat, scientific_name, common_name, project, survey, year, date, doy, month, depth, longitude, latitude, cpue_kg_km2, cpue_kg_km2_sub,cpue_o32_weight, cpue_u32_weight, salinity_psu, temperature_C, po2, sigma0_kgm3, do_mlpL, O2_umolkg, log_depth_scaled, log_depth_scaled2, X, Y, invtemp, mi1, mi2, mi3, mi5, temp_s, po2_s, mi1_s, mi2_s, mi3_s, mi5_s, p1,p2,p3,p4,median_weight, mean_weight, haul_weight, nlength, pass, vessel, tow, bottom_temperature_c)
  
}
}
try(return(dat))
}

paste_reverse <- function(x, y) {
  item <- paste0(y,x)
  return(item)
}

positive_catches <- function(dat){
  missing <-  subset(dat, cpue_kg_km2==0)
  positive <- subset(dat, cpue_kg_km2>0)
  total <- nrow(dat)
  num_pos <- nrow(positive)
  num_lengths <- nrow(subset(dat, p1>0))
  prop_positive <- nrow(positive)/nrow(dat)
  prop_missing <- nrow(missing)/nrow(dat)
  prop_pos_length <- num_lengths/num_pos
  summary <- matrix(data=c(total, num_pos, num_lengths, prop_positive, prop_missing, prop_pos_length), ncol=6, nrow=1)
  colnames(summary) <- c("total_hauls", "total_positive_hauls", "total_hauls_with_length", "prop_positive_hauls", "prop_zero_hauls", "prop_hauls_length")
  return(summary)
}

#Function to run sdmTMB for one model
run_sdmTMB <- function(formula, dat, start, spc){
  
  # make spde
 mesh <- make_mesh(dat,xy_cols = c('X','Y'), 
                    cutoff = 20)
  
  print('running model.')
  m <- try( sdmTMB(
    formula = as.formula(formula),
    data = dat, 
    spatial = "on",
    mesh=mesh,
    anisotropy=T,
    reml=F,
    time=NULL,
    family =tweedie(link="log")))
    #extra_time=1980:2100))
    #,control = sdmTMBcontrol(
   #   start = list(b_threshold = start))
  #)
  
  if(class(m)=="try-error"){
    print(paste("Error."))
  }else{
    print(paste("Model for",formula,spc,"complete."))
  }
  
  # return(m)
  return(m)
}

##Function to run model and return list of model outputs
run_sdmTMB_noprior <- function(simdat, start, mesh) {
  m2 <- try(sdmTMB(sim ~ -1+as.factor(year)+logistic(mi_s)+log_depth_scaled+log_depth_scaled2, 
                   data = simdat, 
                   spatial = "on",
                   spatiotemporal="off",
                   mesh=mesh,
                   family =tweedie(link="log"),
                   control = sdmTMBcontrol(
                     start = list(b_threshold = start),
                     #lower = list(b_threshold = c(-Inf, -Inf, -Inf, -Inf)), 
                     # upper = list(b_threshold = c(Inf, Inf, 100, Inf)),
                     newton_loops = 2)))
  try(tidy(m2))
  try(return(m2))
}

run_sdmTMB_noprior2 <- function(simdat, start, mesh) {
  m2 <- try(sdmTMB(sim ~ -1+as.factor(year)+logistic(mi_s)+log_depth_scaled+log_depth_scaled2, 
                   data = simdat, 
                   spatial = "off",
                   spatiotemporal="off",
                   mesh=mesh,
                   family =tweedie(link="log"),
                   control = sdmTMBcontrol(
                     start = list(b_threshold = start),
                     #lower = list(b_threshold = c(-Inf, -Inf, -Inf, -Inf)), 
                     # upper = list(b_threshold = c(Inf, Inf, 100, Inf)),
                     newton_loops = 2)))
  try(tidy(m2))
  try(return(m2))
}


simulate_fish<- function(dat,mesh, s50, delta, smax, modelpars) {
  seed <- sample(1:1000, 1)
  sim <- sdmTMB_simulate(formula=~-1+as.factor(year)+logistic(mi_s)+log_depth_scaled+log_depth_scaled2,
                         data=dat,
                         family=tweedie(link="log"),
                         tweedie_p=p,
                         phi=phi,
                         range=range,
                         sigma_O=sigma_O,
                         sigma_E=NULL,
                         mesh=mesh,
                         threshold_coefs=c(s50, delta, smax),
                         B=c(b_years, beta1, beta2),
                         seed=seed)
  dat$sim <- sim$observed
  return(dat)
}

simulate_fish2<- function(dat,mesh, s50, delta, smax, modelpars) {
  seed <- sample(1:1000, 1)
  sim <- sdmTMB_simulate(formula=~-1+as.factor(year)+logistic(mi_s)+log_depth_scaled+log_depth_scaled2,
                         data=dat,
                         family=tweedie(link="log"),
                         tweedie_p=p,
                         phi=phi,
                         range=NULL,
                         sigma_O=NULL,
                         sigma_E=NULL,
                         mesh=mesh,
                         threshold_coefs=c(s50, delta, smax),
                         B=c(b_years, beta1, beta2),
                         seed=seed)
  dat$sim <- sim$observed
  return(dat)
}

## Functions for extracting parameter estimates and diagnostics ##
# Extract parameter estimates #
extract_pars <- function(x){
  if(!is.character(x)){
    par_estimates <- as.data.frame(tidy(x, conf.int = TRUE, effects="fixed"))
    par_estimates_rand <- as.data.frame(tidy(x, conf.int = TRUE, effects="ran_pars"))
    par_estimates <- bind_rows(par_estimates, par_estimates_rand)
    return(par_estimates)
  }
  if(is.character(x)){
    return(NA)
  }
}

# Function to clean up pars for plotting #
clean_pars <- function(pars, fits){
  names(pars) <- c(1:length(fits))
  #Remove models with errors
  pars <- keep(pars, function(x) !is.logical(x))
  #Combine into single dataframe, with column of simulation number
  pars <- bind_rows(pars,.id="id")
  return(pars)
}

#Basic function to calculate logistic threshold
logfun_basic <- function(mi, smax, s50, s95){
  delta <- s95
  s50 <- s50
  a <- log(smax / (log(0.5) + smax) - 1)
  b <- log(smax / (log(0.95) + smax) - 1)
  beta0 <- -a + s50 * (b - a) / delta
  beta1 <- (a - b) / delta
  logmu <- exp(smax * (1 / ( 1 + exp( - beta0 - beta1 * mi)) -1))
  return(logmu)
}


calc_sigma <- function(s, t, p) {
  constants <- list(B0 = 8.24493e-1, B1 = -4.0899e-3, B2 = 7.6438e-5, B3 = -8.2467e-7, B4 = 5.3875e-9, C0 = -5.72466e-3, C1 = 1.0227e-4, C2 = -1.6546e-6, D0 = 4.8314e-4, A0 = 999.842594, A1 = 6.793952e-2, A2 = -9.095290e-3, A3 = 1.001685e-4, A4 = -1.120083e-6, A5 = 6.536332e-9, FQ0 = 54.6746, FQ1 = -0.603459, FQ2 = 1.09987e-2, FQ3 = -6.1670e-5, G0 = 7.944e-2, G1 = 1.6483e-2, G2 = -5.3009e-4, i0 = 2.2838e-3, i1 = -1.0981e-5, i2 = -1.6078e-6, J0 =1.91075e-4, M0 = -9.9348e-7, M1 = 2.0816e-8, M2 = 9.1697e-10, E0 = 19652.21, E1 = 148.4206, E2 = -2.327105, E3 = 1.360477e-2, E4 = -5.155288e-5, H0 = 3.239908, H1 = 1.43713e-3, H2 = 1.16092e-4, H3 = -5.77905e-7, K0 = 8.50935e-5, K1 =-6.12293e-6, K2 = 5.2787e-8)
  
  list2env(constants, environment())
  constant_names <- names(constants)
  t2 = t*t
  t3 = t*t2
  t4 = t*t3
  t5 = t*t4
  #  if (s <= 0.0) s = 0.000001
  s32 = s^ 1.5
  p = p / 10.0 # convert decibars to bars */
  sigma = A0 + A1*t + A2*t2 + A3*t3 + A4*t4 + A5*t5 + (B0 + B1*t + B2*t2 + B3*t3 + B4*t4)*s + (C0 + C1*t + C2*t2)*s32 + D0*s*s
  kw = E0 + E1*t + E2*t2 + E3*t3 + E4*t4
  aw = H0 + H1*t + H2*t2 + H3*t3
  bw = K0 + K1*t + K2*t2
  k = kw + (FQ0 + FQ1*t + FQ2*t2 + FQ3*t3)*s + (G0 + G1*t + G2*t2)*s32 + (aw + (i0 + i1*t + i2*t2)*s + (J0*s32))*p + (bw + (M0 + M1*t + M2*t2)*s)*p*p
  val = 1 - p / k
  sigma = sigma / val - 1000.0
  rm(constant_names)
  return(sigma)
}

# convert ml /l to umol / kg
convert_o2 <- function(o2ml_l, sigma){
  (o2ml_l * 44.660)/((1000 + sigma)/1000)
}