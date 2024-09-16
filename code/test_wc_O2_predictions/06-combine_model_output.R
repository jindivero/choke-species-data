library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

#Load functions
source("code/test_wc_O2_predictions/helper_funs.R")

###Synoptic only###
##Combine overall RMSE of each region into a single table
#Read files
rmse_total_cc <- readRDS("code/test_wc_O2_predictions/outputs/rmse_total_cc.rds")
rmse_total_bc <- readRDS("code/test_wc_O2_predictions/outputs/rmse_total_bc.rds")
rmse_total_goa <- readRDS("code/test_wc_O2_predictions/outputs/rmse_total_goa.rds")
rmse_total_ebs <- readRDS("code/test_wc_O2_predictions/outputs/rmse_total_ebs.rds")
rmse_total_ai <- readRDS("code/test_wc_O2_predictions/outputs/rmse_total_ai.rds")

#Combine
rmse_totals <- bind_cols(rmse_total_cc, rmse_total_bc, rmse_total_goa, rmse_total_ebs, rmse_total_ai)
colnames(rmse_totals) <- c("cc", "bc", "goa", "ebs", "ai")

saveRDS(rmse_totals, file="code/test_wc_O2_predictions/outputs/rmse_total_combined.rds")

##Plot RMSE vs n_train
rmse_cc <- readRDS("code/test_wc_O2_predictions/outputs/rmse_years_cc.rds")
rmse_bc <- readRDS("code/test_wc_O2_predictions/outputs/rmse_years_bc.rds")
rmse_goa <- readRDS("code/test_wc_O2_predictions/outputs/rmse_years_goa.rds")
rmse_ebs <- readRDS("code/test_wc_O2_predictions/outputs/rmse_years_ebs.rds")
rmse_ai <- readRDS("code/test_wc_O2_predictions/outputs/rmse_years_ai.rds")

#Combine
rmse_combined <-bind_rows(rmse_cc, rmse_bc, rmse_goa, rmse_ebs, rmse_ai)
saveRDS(rmse_combined, file="code/test_wc_O2_predictions/outputs/rmse_combined.rds")

#Plot (all regions together)
ggplot(rmse_combined, aes(x=n_train, y=temp_salinity_spatiotemporal))+
  geom_point(aes(colour=region))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11))

#Re-order
rmse_combined$region <- factor(rmse_combined$region, levels=c("cc", "bc", "goa", "ebs", "ai")) 

###Plot for proto-draft--
ggplot(rmse_combined, aes(x=n_train, y=temp_salinity_spatiotemporal))+
  geom_boxplot(aes(colour=region))+
  geom_point(aes(colour=region))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))+
  theme(legend.title = element_blank())+
  xlab("Number of Observations in Training Data")+
  ylab("RMSE of Spatio-temporal Model")+
  theme(legend.position=c(0.8,0.7))+
  scale_colour_discrete(labels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/rmse_vs_ntrain.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

#Plot (regions separately)
ggplot(rmse_combined, aes(x=n_train, y=temp_salinity_spatiotemporal))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap("region", scales="free")+
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11))+
  xlab("Number of Observations in Training Data")+
  ylab("RMSE of Spatio-temporal Model")

ggplot(rmse_combined, aes(x=n_train, y=year_temp_salinity))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap("region", scales="free")+
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11))+
  xlab("Number of Observations in Training Data")+
  ylab("RMSE of Year+Temp+Salinity Model")

###With Glorys data###
##Total RMSE per region
glorys_rmsetotal_cc <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmsetotal_cc.rds")
glorys_rmsetotal_bc <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmsetotal_bc.rds")
glorys_rmsetotal_goa <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmsetotal_goa.rds")
glorys_rmsetotal_ebs <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmsetotal_ebs.rds")
glorys_rmsetotal_ai <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmsetotal_ai.rds")

rmse_combined2 <-bind_cols(glorys_rmsetotal_cc[,1],glorys_rmsetotal_bc[,1], glorys_rmsetotal_goa[,1], glorys_rmsetotal_ebs[,1], glorys_rmsetotal_ai[,1])
colnames(rmse_combined2) <- c("cc", "bc", "goa", "ebs", "ai")
rownames(rmse_combined2) <- c("persistent_spatial", "persistent_spatial_year", "year_temp_salinity", "temp_salinity_spatiotemporal", "glorys", "glorys_nearest_neighbor")
saveRDS(rmse_combined2, file="code/test_wc_O2_predictions/outputs/rmse_glorys_total_combined.rds")

##RMSE per year
glorys_rmse_cc <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmse_cc.rds")
glorys_rmse_bc <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmse_bc.rds")
glorys_rmse_goa <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmse_goa.rds")
glorys_rmse_ebs <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmse_ebs.rds")
glorys_rmse_ai <- readRDS("code/test_wc_O2_predictions/outputs/glorys2_rmse_ai.rds")

rmse_glorys_combined <-bind_rows(glorys_rmse_cc, glorys_rmse_bc, glorys_rmse_goa, glorys_rmse_ebs, glorys_rmse_ai)

#Convert to nice table and csv for supplement
rmse_glorys_combined <- select(rmse_glorys_combined, region, year, n_train, n_test, persistent_spatial, persistent_spatial_year, year_temp_salinity, temp_salinity_spatiotemporal, glorys2, glorys)
rmse_glorys_combined$region <- case_when(rmse_glorys_combined$region=="cc"~"California Current",
                                  rmse_glorys_combined$region=="bc"~"British Columbia",
                                  rmse_glorys_combined$region=="goa"~"Gulf of Alaska",
                                  rmse_glorys_combined$region=="ebs"~"Eastern Bering Sea", 
                                  rmse_glorys_combined$region=="ai"~"Aleutian Islands")
rmse_glorys_combined[,c(5:10)] <- round(rmse_glorys_combined[,c(5:10)], digits=1)
write.csv(rmse_glorys_combined, file="code/test_wc_O2_predictions/outputs/rmse_glorys_combined_table.csv", row.names=F)

#Convert long
rmse_glorys_combined <- pivot_longer(rmse_glorys_combined, c(1:4,9:10), names_to="model")
labs <- c("Aleutian Islands", "British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska")
names(labs) <- c("ai", "bc", "cc", "ebs", "goa")
rmse_glorys_combined$region <- factor(rmse_glorys_combined$region, levels=c("cc", "bc", "goa", "ebs", "ai")) 

#### Plot for proto-draft ####
#Remove rows with less than n=50 in test data
#Exclude GLORYS
rmse_plot <- subset(rmse_glorys_combined, model!="glorys")
rmse_plot <- subset(rmse_plot, model!="glorys2")
ggplot(filter(rmse_plot), aes(x=year, y=value))+
  geom_line(aes(colour=model))+
  facet_wrap("region", labeller = labeller(region=labs))+
  ylab("RMSE")+
  xlab("Year")+
  theme(legend.position="top")+
  theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme_minimal(base_size=20) +
  scale_x_continuous(breaks=c(2008,2012,2016,2020,2024))+
    theme(legend.title=element_blank())+
    xlab("Year")+
    ylab("RMSE")+
  theme(legend.position=c(0.85,0.2))+
  scale_colour_discrete(labels=c("Persistent", "Annual", "Spatio-temporal", "Covariates"))

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/rmse_years.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##GLORYS 
ggplot(filter(rmse_glorys_combined, model=="glorys"|model=="glorys2"|model=="temp_salinity_spatiotemporal"), aes(x=year, y=value))+
  geom_line(aes(colour=model))+
  facet_wrap("region", scales="free_y", labeller = labeller(region=labs))+
  ylab("RMSE")+
  xlab("Year")+
  theme(legend.position="top")+
  theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=c(2008,2012,2016,2020,2024))+
  theme_minimal(base_size=20) +
  theme(legend.title=element_blank())+
  xlab("Year")+
  ylab("RMSE")+
  theme(legend.position=c(0.85,0.2))+
 scale_colour_discrete(labels=c("GOBH interpolation", "GOBH nearest-neighbor", "In situ spatio-temporal"))

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/rmse_glorys_st.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###Combine residuals for spatio-temporal model across regions
##read in model output files
load("code/test_wc_O2_predictions/outputs/o2_models_cc.Rdata")
cc <- output
load("code/test_wc_O2_predictions/outputs/o2_models_bc.Rdata")
bc <- output
load("code/test_wc_O2_predictions/outputs/o2_models_goa.Rdata")
goa <- output
load("code/test_wc_O2_predictions/outputs/o2_models_ebs.Rdata")
ebs <- output
load("code/test_wc_O2_predictions/outputs/o2_models_ai.Rdata")
ai <- output

#Isolate the predictions for the spatio-temporal model for each year
glorys <- F
ai <- combine_preds(ai, glorys)
bc <- combine_preds(bc, glorys)
cc <- combine_preds(cc, glorys)
ebs <- combine_preds(ebs, glorys)
goa <- combine_preds(goa, glorys)


combined_preds <- bind_rows(cc, bc, goa, ebs, ai)
saveRDS(combined_preds, file="code/test_wc_O2_predictions/outputs/combined_preds.rds")

##How many are above or below 50?
test <- subset(combined_preds, residual>50|residual< -50)
count <- test %>% count(region)
#Proportion within 50
1-(nrow(test)/nrow(combined_preds))

##How many are above or below 25?
test <- subset(combined_preds, residual>25|residual< -25)
count <- test %>% count(region)
#Proportion within 50
1-(nrow(test)/nrow(combined_preds))

#Reorder regions
combined_preds$region <- factor(combined_preds$region, levels=c("cc", "bc", "goa", "ebs", "ai")) 
labs <- c("British Columbia", "California Current", "Eastern Bering Sea", "Gulf of Alaska", "Aleutian Islands")
names(labs) <- c("bc", "cc", "ebs", "goa", "ai")

##Plot density plots
ggplot(combined_preds, aes(x=residual))+
  geom_density(aes(colour=region))+
  scale_colour_discrete(labels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))+
  xlab("Prediction Residual")+
  ylab("Density")+
  xlim(-100,100)+
  theme_bw(base_size=20) +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.8))

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/residual_density.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Plot predictions versus observations, all one plot but colored by region
ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(aes(colour=region))+
  scale_colour_discrete(labels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))+
  xlab("Prediction Residual")+
  ylab("Density")+
  geom_abline(slope=1, intercept=0)+
  theme_bw(base_size=20) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) + 
  scale_y_continuous(expand = c(0, 0), limits=c(0,NA))+
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.position="right")

#Make each region a separate plot
ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(aes(colour=year), alpha=0.9)+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  theme_bw(base_size=20) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) + 
  scale_y_continuous(expand = c(0, 0), limits=c(0,NA))+
  theme_minimal()+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.3))+
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/preds_vs_obs.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

##Isolate just one year and region
test_predict_O2 <- filter(combined_preds, region=="cc"&year==2012)

# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)

xlimits <- c(min(test_predict_O2$X)*1000, max(test_predict_O2$X)*1000)
ylimits <- c(min(test_predict_O2$Y)*1000, max(test_predict_O2$Y)*1000)
lats <- c(round(min(test_predict_O2$latitude)),  round(max(test_predict_O2$latitude)))
lons <- c(round(min(test_predict_O2$longitude)+2), round(max(test_predict_O2$longitude)))

##Plot predictions, residuals
predict_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(
    data = test_predict_O2,
    aes(
      x = X * 1000,
      y = Y * 1000,
      col = (est)
    ),
    size = 1.0,
    alpha = 1.0
  ) +
  scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_viridis_c(
    limits = c(0, 200),
    oob = scales::squish,
    name = bquote(O[2]),
    breaks = c(0, 100, 200)
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom") +
  guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                    0.5))

residual_plot <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(
    data = test_predict_O2,
    aes(
      x = X * 1000,
      y = Y * 1000,
      col = residual
    ),
    size = 1.0,
    alpha = 1.0
  ) +
  scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
  ylim(ylimits[1], ylimits[2]) +
  scale_colour_distiller(palette = "RdBu", limits = c(-50, 50)) +
  #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom") +
  guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                    0.5))

omega <- ggplot(us_coast_proj) + geom_sf() +
  geom_point(test_predict_O2, mapping=aes(x=X*1000, y=Y*1000, col=omega_s),
             size = 1.0,
             alpha = 1.0
  ) +
  scale_colour_distiller(palette = "RdBu") +
  ylim(ylims)+
  scale_x_continuous(breaks=c(-125,-120), limits=xlims)+
  #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.position = "bottom") +
  guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                    0.5))
try(epsilon <- ggplot(us_coast_proj) + geom_sf() +
      geom_point(test_predict_O2, mapping=aes(x=X*1000, y=Y*1000, col=epsilon_st),
                 size = 1.0,
                 alpha = 1.0
      ) +
      scale_colour_distiller(palette = "RdBu") +
      ylim(ylimits)+
      scale_x_continuous(breaks=c(-125,-120), limits=xlimits)+
      #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
      labs(x = "Longitude", y = "Latitude") +
      theme_bw() +
      theme(
        panel.grid.major = element_blank()
        ,
        panel.grid.minor = element_blank()
        ,
        panel.border = element_blank()
        ,
        strip.background = element_blank()
        ,
        strip.text = element_blank()
      ) +
      theme(axis.line = element_line(color = "black")) +
      theme(axis.text = element_text(size = 11)) +
      theme(axis.title = element_text(size = 12)) +
      theme(legend.text = element_text(size = 11)) +
      theme(legend.position = "bottom") +
      guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                        0.5)))

figure <- ggarrange(predict_plot, residual_plot,omega, epsilon, labels=c("A Predictions", "B Residuals", "C Spatial Variation", "D Spatio-temporal Variation"),ncol=4,nrow=1,  font.label = list(size = 10))
print(figure)

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/example_year_preds.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 7.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)

###GLORYS data
load("code/test_wc_O2_predictions/outputs/o2_models_glorys/cc.Rdata")
cc <- output
load("code/test_wc_O2_predictions/outputs/o2_models_glorys/bc.Rdata")
bc <- output
load("code/test_wc_O2_predictions/outputs/o2_models_glorys/goa.Rdata")
goa <- output
load("code/test_wc_O2_predictions/outputs/o2_models_glorys/ebs.Rdata")
ebs <- output
load("code/test_wc_O2_predictions/outputs/o2_models_glorys/ai.Rdata")
ai <- output

glorys <- T
ai <- combine_preds(ai, glorys)
bc <- combine_preds(bc, glorys)
cc <- combine_preds(cc, glorys)
ebs <- combine_preds(ebs, glorys)
goa <- combine_preds(goa, glorys)

combined_preds_glorys <- bind_rows(cc, bc, goa, ebs, ai)
saveRDS(combined_preds_glorys, file="code/test_wc_O2_predictions/outputs/combined_preds_glorys.rds")

ggplot(combined_preds_glorys, aes(x=residual))+
  geom_density(aes(colour=region))+
  scale_colour_discrete(labels=c("California Current", "British Columbia", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands"))+
  xlab("Prediction Residual")+
  ylab("Density")+
  xlim(-100,100)+
  theme_bw(base_size=20) +
  theme(
    panel.grid.major = element_blank()
    ,
    panel.grid.minor = element_blank()
    ,
    panel.border = element_blank()
    ,
    strip.background = element_blank()
    ,
    strip.text = element_blank()
  ) +
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.8))

#Make each region a separate plot
ggplot(combined_preds, aes(x=o2,y=est))+
  geom_point(aes(colour=year), alpha=0.9)+
  facet_wrap("region", labeller = labeller(region=labs))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  theme_bw(base_size=20) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) + 
  scale_y_continuous(expand = c(0, 0), limits=c(0,NA))+
  theme_minimal()+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.3))+
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")

###Isolate just one year in one region from each
test <- filter(combined_preds, region=="cc"&year==2012)
test$type <- "insitu observations spatio-temporal model"
test2 <- filter(combined_preds_glorys, region=="cc"&year==2012)
test2$type <- "Global Ocean Biogeochemistry Hindcast"

test <- bind_rows(test, test2)

test$type <- factor(test$type, levels=c("insitu observations spatio-temporal model", "Global Ocean Biogeochemistry Hindcast"))

p1 <- ggplot(test, aes(x=o2,y=est))+
  geom_point(aes(colour=type))+
  xlab("Observed Bottom Dissolved Oxygen")+
  ylab("Predicted Bottom Dissolved Oxygen")+
  geom_abline(slope=1, intercept=0)+
  theme_minimal(base_size=15) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,NA)) + 
  scale_y_continuous(expand = c(0, 0), limits=c(0,NA))+
  theme(axis.line = element_line(color = "black")) +
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.3,0.8))+
  scale_colour_manual(values=c("black", "grey50"))

p2 <- ggplot(test, aes(x=residual))+
  geom_density(aes(colour=type))+
  xlab("Prediction Residual")+
  ylab("Density")+
  theme_minimal(base_size=15) +
  theme(axis.line = element_line(color = "black")) +
  theme(legend.text=element_text(size=13))+
  theme(legend.title = element_blank())+
  theme(legend.position=c(0.8,0.8))+
  scale_colour_manual(values=c("black", "grey50"))

figure <- ggarrange(p1, p2, labels=c("A", "B"),
                    ncol = 2, nrow = 1, common.legend=T, legend="bottom")

ggsave(
  paste("code/test_wc_O2_predictions/outputs/plots/example_gobh_insitu.png"),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE, bg="white"
)


