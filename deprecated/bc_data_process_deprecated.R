length_expand_bc <- function(sci_name, spc) {
  # load, clean, and join data
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
  bio <- dplyr::full_join(bio2, haul2, relationship="many-to-many")
  
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
  bio3 <- dplyr::full_join(bio[,c("age", "sex", "length_cm", "weight", "scientific_name", "common_name", "lat_start", "lon_start", "lat_end", "lon_end", "set.date")], haul, relationship="many-to-many")
  bio <- bio3
  
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
  
  #Combine bio/haul data with catch data
  dat <- dplyr::left_join(dat, filter(bio[,c("event_id", "age", "sex","length_cm", "weight", "scientific_name", "common_name")], !is.na(length_cm)), relationship = "many-to-many")
  
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
  
  #Clean catch data
  names(catch) = tolower(names(catch))
  
  catch <- left_join(catch,itis)
  
  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)
  
  #Combine catch data with haul data
  dat <- dplyr::left_join(catch, haul, relationship = "many-to-many")
  
  names(dat) = tolower(names(dat))
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
  dat <- dplyr::select(dat, event_id, scientific_name, project, year, date, longitude_dd, latitude_dd, longitude, latitude, cpue_kg_km2,
                       depth_m, julian_day, nlength,median_weight, haul_weight, pass, p1, p2, p3, p4)
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
  return(dat)
}
