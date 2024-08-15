library(sdmTMB)
library(dplyr)
library(Metrics)
library(ggplot2)
library(tidyr)

theme_set(theme_bw(base_size = 25))
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Load oxygen data
dat <- as.data.frame(readRDS("data/processed_data/all_o2_dat_filtered.rds"))

dat.2.use <- as.data.frame(filter(dat, region=="cc"))

count <- dat.2.use %>% count(year, survey)

ggplot(dat.2.use, aes(x=year))+stat_count(aes(fill=survey))+xlim(2012,2019)


dat.2.use <- as.data.frame(filter(dat, region=="bc"))

count <- dat.2.use %>% count(year, survey)

dat$region <- case_when(dat$region=="ai"~"Aleutian Islands", 
                        dat$region=="bc"~"British Columbia", 
                        dat$region=="cc"~"California Current", 
                        dat$region=="ebs"~"Eastern Bering Sea", 
                        dat$region=="goa"~"Gulf of Alaska")

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
 theme_minimal()+ylab("Number of oxygen observations")+xlab("Year")

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
  theme_minimal()+
  ylab("Number of oxygen observations")+
  xlab("Year")+
  xlim(1995,2023)

ggplot(dat, aes(x=year))+
  stat_count(aes(fill=survey))+
  facet_wrap("region", ncol=1, nrow=5, scales="free_y")+
  theme_minimal()+
  ylab("Number of oxygen observations")+
  xlab("Year")+
  xlim(2015,2023)

##Diagnose computation problems
dat.2.use <- as.data.frame(filter(dat, region==test_region))
test_data <- dat.2.use %>%
  filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc") & (year==2015|year==2014))
train_data <- dat.2.use %>%
  filter(!((survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc") & (year==2015|year==2014))))
train_data <- as.data.frame(train_data)
test_data <- as.data.frame(test_data)

ggplot(filter(dat.2.use, year>2012),aes(x=year))+stat_count(aes(fill=survey))


##Diagnose computation problems
dat$depth_ln <- log(dat$depth)
dat.2.use <- as.data.frame(filter(dat, region==test_region))
test_data <- dat.2.use %>%
  filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc") & year==test_year)
train_data <- dat.2.use %>%
  filter(!((survey %in% c("nwfsc", "dfo", "goa", "EBS", "iphc") & year==test_year)))
train_data <- as.data.frame(train_data)
test_data <- as.data.frame(test_data)

ggplot(train_data, aes(x=year))+stat_count(aes(fill=survey))

train_data <- filter(train_data, year>2000)
spde <- make_mesh(data = train_data,
                  xy_cols = c("X", "Y"),
                  cutoff = 45)
m2 <- try(sdmTMB(
  formula = o2  ~ 1 +as.factor(year)+s(sigma0) + s(temp) +  s(depth_ln) + s(doy),
  mesh = spde,
  data = train_data,
  family = gaussian(),
  spatial = "on",
  spatiotemporal  = "off"
))



