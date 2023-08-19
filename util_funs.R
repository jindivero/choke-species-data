#Get scientific name from itis number
extract_name <- function(x){
  name <- x$name[14]
  return(name)
}
# calc o2 solubility, relies on o2 in umol/kg
gsw_O2sol_SP_pt <- function(sal,pt) {
  #oxygen in mmol/m^3--convert to umol to work in gsw_O2sol_SP_pt function?
  
  #salinity psu
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

load_data <- function(spc,dat.by.size, length=T) {
  dat <- readRDS("data/catch_data.rds")
  dat = dplyr::filter(dat, species == spc, year%in%seq(1993,2020))
  dat <- left_join(dat, dat.by.size, by = "trawl_id")
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
  dat <- dplyr::filter(dat, !is.na(o2), !is.na(sal), !is.na(temp), is.finite(sal))
  dat <- calc_po2_mi(dat)
  dat <- dplyr::filter(dat, !is.na(temp), !is.na(mi))
  
  # prepare data and models -------------------------------------------------
  
  dat <- dplyr::select(dat, trawl_id, species, year, longitude_dd, latitude_dd, cpue_kg_km2,
                       o2, temp, depth, mi, po2, julian_day, pass, p1, p2, p3, p4)
  
  
  # UTM transformation
  dat_ll = dat
  coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
  proj4string(dat_ll) <- CRS("+proj=longlat +datum=WGS84")
  # convert to utm with spTransform
  dat_utm = spTransform(dat_ll, 
                        CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
  # convert back from sp object to data frame
  dat = as.data.frame(dat_utm)
  dat = dplyr::rename(dat, longitude = coords.x1,
                      latitude = coords.x2)
  return(dat)
}


# Species of interest and max. juvenile lengths (define ontogenetic classes)
length_expand <- function(sci_name) {
  # load, clean, and join data
  bio = readRDS("data/wcbts_bio_2019-08-01.rds")
  haul = readRDS("data/wcbts_haul_2019-08-01.rds")
  catch = readRDS("data/wcbts_catch_2019-08-01.rds")
  names(catch) = tolower(names(catch))
  names(bio) = tolower(names(bio))
  names(haul) = tolower(names(haul))
  
  bio$trawl_id = as.character(bio$trawl_id)
  haul$trawl_id = as.character(haul$trawl_id)
  haul$date_yyyymmdd = as.numeric(haul$date_yyyymmdd)
  haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)
  
  dat = dplyr::left_join(catch[,c("trawl_id","scientific_name","year","subsample_count",
                                  "subsample_wt_kg","total_catch_numbers","total_catch_wt_kg","cpue_kg_km2")], haul, relationship = "many-to-many") %>%
    dplyr::left_join(filter(bio, !is.na(length_cm)), relationship = "many-to-many") %>%
    filter(performance == "Satisfactory")  %>%
    mutate(depth_m = depth_hi_prec_m)
  
  
  
  # filter out species of interest from joined (catch/haul/bio) dataset
  dat_sub = dplyr::filter(dat, scientific_name == sci_name)
  
  # fit length-weight regression by year to predict fish weights that have lengths only.
  # note a rank-deficiency warning may indicate there is insufficient data for some year/sex combinations (likely for unsexed group)
  
  fitted = dat_sub %>%
    filter(!is.na(length_cm), !is.na(weight_kg)) %>%
    dplyr::select(trawl_id,year,
                  subsample_wt_kg, total_catch_wt_kg, area_swept_ha_der, cpue_kg_km2,
                  individual_tracking_id, sex, length_cm, weight_kg) %>%
    group_nest(year)  %>%
    mutate(
      model = map(data, ~ lm(log(weight_kg) ~ log(length_cm), data = .x)),
      tidied = map(model, tidy),
      augmented = map(model, augment),
      predictions = map2(data, model, modelr::add_predictions)
    )
  
  # replace missing weights with predicted weights
  dat_pos = fitted %>%
    unnest(predictions) %>%
    dplyr::select(-data, -model, -tidied, -augmented) %>%
    mutate(weight = ifelse(is.na(weight_kg), exp(pred), weight_kg))
  
  trawlids <- unique(dat_pos$trawl_id)
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
      smoothed_w <- bkde(haul_sample$weight, range.x = c(min(dat_pos$weight), max(dat_pos$weight)), bandwidth = 2)
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
  return(all_hauls)
}