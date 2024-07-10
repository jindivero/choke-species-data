
#Plot available data
#Subset to data with DO available and no DO
density_no_O2 <- density_O2 %>% 
  filter(is.na(do_mlpL)| do_mlpL<=0)

density_O2 <- density_O2 %>% 
  filter(!is.na(do_mlpL), do_mlpL>0)

#Remove NAs (was causing issues with smoother if had NAs)
density_O2 <- density_O2 %>% drop_na(temperature_C, depth, month, salinity_psu)

# prepare for plotting ####
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")

# crop if you want; not needed:
us_coast <- st_crop(map_data,
                    c(xmin = -180, ymin = 31, 
                      xmax = -110, ymax = 75))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)

# * 1000 b/c we are in UTM km for model fitting:
xlimits = c(-282853, 1025581)
ylimits = c(2349000, 7900000)

dat <- density_O2
ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = dat, aes(x = longitude, y = latitude, colour=survey), size = 2.0, alpha = 1.0) +
  ylim(31, 75) +
  facet_wrap("year")

ggplot(us_coast_proj) + geom_sf() +
  geom_point(data = density_no_O2, aes(x = longitude, y = latitude, colour=survey), size = 2.0, alpha = 1.0) +
  facet_wrap("year")




ggplot(data = dat, aes(x = X, y = Y, colour=survey))+geom_point(size = 1, alpha = 1)+facet_grid(year~survey)
ggplot(data = dat, aes(x = X, y = Y, colour=survey))+geom_point(size = 1, alpha = 1)+facet_wrap("year")
ggplot(data = dat, aes(x = X, y = Y, colour=as.factor(year)))+geom_point(size = 1, alpha = 0.3)+facet_wrap("survey")

ggplot(data=dat, aes(x=year, fill=survey))+geom_bar()

#Plot
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent ="North America")

# crop if you want; not needed:
us_coast <- st_crop(map_data,
                    c(xmin = -126, ymin = 31, 
                      xmax = -110, ymax = 50))
us_coast_proj <- sf::st_transform(map_data, crs = 32610)
density_no_O2 <- density_no_O2 %>% drop_na(temperature_C, depth_ln, month, sigma0_exp)

ggplot(density_no_O2, aes(x=X,y=Y, colour=survey))+geom_point()+facet_wrap("year")
ggplot(density_O2, aes(x=X,y=Y, colour=survey))+geom_point()+facet_wrap("year")


#Data availability
#Count number of NAs 
summary <- dat %>%
  group_by(survey, year) %>%
  summarize_at(c("salinity_psu", "depth", "temperature_C", "O2_umolkg"), ~(length(.x)-sum(is.na(.x))))

#Plot
ggplot(summary, aes(x=year, y=O2_umolkg,  group=survey, fill=survey))+geom_col()
ggplot(summary, aes(x=year, y=depth,  group=survey, fill=survey))+geom_col()
ggplot(summary, aes(x=year, y=salinity_psu,  group=survey, fill=survey))+geom_col()
ggplot(summary, aes(x=year, y=temperature_C,  group=survey, fill=survey))+geom_col()


#So what percentage are missing?
summary2 <- dat %>%
  group_by(survey, year) %>%
  summarize_at(c("salinity_psu", "depth", "temperature_C", "O2_umolkg"), ~(1-(sum(is.na(.x)/length(.x)))))

#Plot
ggplot(summary2, aes(x=year, y=O2_umolkg,  group=survey, fill=survey))+geom_col()
ggplot(summary2, aes(x=year, y=depth,  group=survey, fill=survey))+geom_col()
ggplot(summary2, aes(x=year, y=salinity_psu,  group=survey, fill=survey))+geom_col()
ggplot(summary, aes(x=year, y=temperature_C,  group=survey, fill=survey))+geom_col()

summary3 <- dat %>%
  group_by(survey, year) %>%
  summarize_at(c("salinity_psu", "depth", "temperature_C", "do_mlpL"), ~(1-(sum(is.na(.x)/length(.x)))))

summary3 <- as.data.frame(summary3)

summary3 <- summary3 %>% complete(survey, year)
summary3 <- summary3[,c(1:6)]

#Pivot longer
summary3b <- pivot_longer(summary3, 3:6)
summary3b$value <- ifelse(is.na(summary3b$value), 0, summary3b$value)


ggplot(summary3b, aes(x=year, y=value, fill=name))+geom_col()+facet_wrap("survey")+ylab("proportion of in situ observations with data available")



