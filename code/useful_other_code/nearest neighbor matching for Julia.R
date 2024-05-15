	# matching lat/longs with nearest neighbor

	library(tidyverse); library(RANN); library(data.table); library(here)

	## load ROMS output ####
	
	# this is a file that just has the lat/longs of the centers of unique roms grid cells along with an ID that I gave each one
  roms_grid <- fread(file = here("./data/roms_grid.csv"))

	## load survey grid stations ####
	
	# pollock has the highest number of unique stations so use this df to get 
	# avg lat long of stations over the hindcast period
	
	survey_grid <- fread(file = here("./data/survey_grid.csv"))  %>%
		select(stationid, latitude, longitude) %>%
		group_by(stationid) %>%
		summarise(latitude = mean(latitude),
							longitude = mean(longitude))
	
	# match lat/longs of survey grid to nearest neighbor from ROMS grid	
	
	# use nn2() to calculate min distance to nearest ROMS lat/long
	survey_grid[, c(4, 5)] <- as.data.frame(RANN::nn2(roms_grid[, c('latitude', 'longitude')],
                                                  survey_grid[, c('latitude', 'longitude')],
                                                  k = 1))
	
	# Match nearest lat/long from ROMS
	survey_grid$roms_ID <- pull(roms_grid[c(survey_grid$nn.idx), 'roms_ID'])
	
	# any NAs in matching?
	which(is.na(survey_grid$roms_ID), )
	
	# drop cols from nn2
	survey_grid <- survey_grid %>%
		select(-nn.idx, -nn.dists)
	
	# match survey grid points to ROMS lat/longs - don't really have to even do this step
	
	roms_grid <- roms_grid %>%
		rename(roms_lat = latitude,
					 roms_long = longitude)
	
	survey_grid <- left_join(survey_grid, roms_grid, by = "roms_ID")
	