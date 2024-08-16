
##Combine overall RMSE of each region into a single table
#Read files
rmse_total_cc <- readRDS("~/Dropbox/choke species/code/choke-species-data/code/test_wc_O2_predictions/outputs/rmse_total_cc.rds")
rmse_total_bc <- readRDS("~/Dropbox/choke species/code/choke-species-data/code/test_wc_O2_predictions/outputs/rmse_total_bc.rds")
rmse_total_goa <- readRDS("~/Dropbox/choke species/code/choke-species-data/code/test_wc_O2_predictions/outputs/rmse_total_goa.rds")
rmse_total_ebs <- readRDS("~/Dropbox/choke species/code/choke-species-data/code/test_wc_O2_predictions/outputs/rmse_total_ebs.rds")
rmse_total_ai <- readRDS("~/Dropbox/choke species/code/choke-species-data/code/test_wc_O2_predictions/outputs/rmse_total_ai.rds")

#Combine
rmse_totals <- bind_cols(rmse_total_cc, rmse_total_bc, rmse_total_goa, rmse_total_ebs, rmse_total_ai)
colnames(rmse_totals) <- c("cc", "bc", "goa", "ebs", "ai")

###Just manually look at one year of a region to see what's happening

#Read in output from a region
load("~/Dropbox/choke species/code/choke-species-data/code/test_wc_O2_predictions/outputs/o2_models_ebs.RData")

#Isolate year of interest
test <- output[[5]]
##1) Plot test data
data <- test[[1]]
ggplot(data, aes(x=X, y=Y))+geom_point(aes(shape=survey, colour=o2))

##1) Plot observations versus predictions
pred <- test[[2]][[1]]
pred2 <- test[[2]][[2]]
pred3 <- test[[2]][[3]]
ggplot(pred2, aes(x=o2, y=est))+geom_point(aes(colour=survey))
ggplot(pred2, aes(x=sigma0, y=depth))+geom_point(aes(colour=temp))+scale_colour_viridis()
ggplot(pred2, aes(x=X, y=Y))+geom_point(aes(shape=survey, colour=residual))


##Plot data, predictions, and spatial residuals
# setup up mapping ####
map_data <- rnaturalearth::ne_countries(scale = "large",
                                        returnclass = "sf",
                                        continent = "North America")

us_coast_proj <- sf::st_transform(map_data, crs = 32610)
