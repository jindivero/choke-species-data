library(dplyr)
library(ggplot2)

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

#Plot
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

ggplot(rmse_combined, aes(x=n_train, y=temp_salinity_spatiotemporal))+
  geom_point()+
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
  theme(legend.text = element_text(size = 11))


###With Glorys data###
#Read files
glorys_rmsetotal_cc <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmsetotal_cc.rds")
glorys_rmsetotal_bc <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmsetotal_bc.rds")
glorys_rmsetotal_goa <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmsetotal_goa.rds")
glorys_rmsetotal_ebs <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmsetotal_ebs.rds")
glorys_rmsetotal_ai <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmsetotal_ai.rds")

rmse_combined2 <-bind_cols(glorys_rmsetotal_cc[,1],glorys_rmsetotal_bc[,1], glorys_rmsetotal_goa[,1], glorys_rmsetotal_ebs[,1], glorys_rmsetotal_ai[,1])
colnames(rmse_combined2) <- c("cc", "bc", "goa", "ebs", "ai")
rownames(rmse_combined2) <- c("persistent_spatial", "persistent_spatial_year", "year_temp_salinity", "temp_salinity_spatiotemporal", "glorys")

#Combine
rmse_totals <- bind_cols(rmse_total_cc, rmse_total_bc, rmse_total_goa, rmse_total_ebs, rmse_total_ai)
colnames(rmse_totals) <- c("cc", "bc", "goa", "ebs", "ai")

glorys_rmse_cc <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmse_cc.rds")
glorys_rmse_bc <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmse_bc.rds")
glorys_rmse_goa <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmse_goa.rds")
glorys_rmse_ebs <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmse_ebs.rds")
glorys_rmse_ai <- readRDS("code/test_wc_O2_predictions/outputs/glorys_rmse_ai.rds")

