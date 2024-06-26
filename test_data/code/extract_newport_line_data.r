# load packages
library(ncdf4)
library(chron)
library(cmocean)
library(oce)
library(tidyverse)
library(respR)

# Get list of all files

startpath <- "test_data/data/newport_hydrographic_line_station_data"

# get file names
all_files <- dir(startpath)
# get all dates
newport_dates <- readRDS("test_data/data/newport_dates")

i =1
all_files[1]

bottom_ctd <- tibble(sample_date = newport_dates[1],
                     NHL.station.number= numeric(),
                     longitude..degW. = numeric(),
                     pressure..dbar. = numeric(),           
                     temperature..degC. = numeric(),
                     practical.salinity = numeric(),
                     potential.density..kg.m.3.= numeric(),
                     spiciness..kg.m.3.= numeric(),
                     dissolved.oxygen..ml.L.= numeric()
)


for (i in 1:length(all_files)) {
  sample_date <- newport_dates[i]
  file_string <- paste(startpath, "/", all_files[i], sep = "")
  dat.2.use <- read.csv(file = file_string,
                        skip = 1,
                        header = T)
  stations <- unique(dat.2.use$NHL.station.number)
  n_stations <- length(stations)
  
  for (j in 1:n_stations) {
    station_data <- dplyr::filter(dat.2.use, NHL.station.number == stations[j])
    
    # find lowest depth with O2 measurement
    which_o2 <- which(!station_data$dissolved.oxygen..ml.L. == -9999)
    # only proceed if there are oxygen measurements on this station
    if (length(which_o2) > 0) {
      lowest_data <- station_data[max(which_o2), ]
      # Add sample date to the single data row
      lowest_data <- lowest_data %>%
        add_column(sample_date = sample_date, .before = "NHL.station.number")
      # combine to dataframe
      bottom_ctd <- bottom_ctd %>%
        add_row(lowest_data)
    }
  }
}
    
saveRDS(bottom_ctd, file = "test_data/data/newport_bottom.RDS")
  
