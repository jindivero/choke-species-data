plot_predict <- function(test_predict_O2, test_year, us_coast_proj) {
  # Plot ####
  data_plot <- ggplot(us_coast_proj) + geom_sf() +
    geom_point(
      data = test_predict_O2,
      aes(
        x = X * 1000,
        y = Y * 1000,
        col = do
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
  
  ## put all plots in one ####
  grid.arrange(data_plot, predict_plot, residual_plot, ncol = 3)
  # plot residuals vs. prediction
  resid_vs_pred <- ggplot(data = test_predict_O2, aes(x = (est), y = residual, col = Y)) +
    geom_point() +
    scale_colour_viridis_c(
      limits = c(31, 50),
      oob = scales::squish,
      name = "latitude",
      breaks = c(35, 40, 45)
    ) +
    ggtitle(test_year) +
    labs(x = "Predicted", y = "Residual") +
    theme(legend.position = "none")
  pred_vs_actual <- ggplot(data = test_predict_O2, aes(x = o2, y = est, col = Y)) +
    geom_point() +
    scale_colour_viridis_c(
      limits = c(31, 50),
      oob = scales::squish,
      name = "latitude",
      breaks = c(35, 40, 45)
    ) +
    ggtitle(test_year) +
    labs(x = "Observed", y = "Predicted") +
    geom_abline(intercept = 0, slope = 1) +
    theme(legend.position = "none")
  return(grid.arrange(pred_vs_actual, resid_vs_pred, ncol = 2))
}

calc_rmse <- function(rmse_list, n){
  rmse2 <- as.data.frame(rmse_list)
  rmse2$n <- n
  rmse2 <- filter(rmse2, n>50)
  rmse2$rmse2 <- rmse2$rmse_list ^ 2
  rmse2$xminusxbarsq <- rmse2$n * rmse2$rmse2
  rmse2 <- drop_na(rmse2, xminusxbarsq)
  rmse_total<- sqrt(sum(rmse2$xminusxbarsq, na.rm=T) / sum(rmse2$n, na.rm=T))
  return(rmse_total)
}

plot_predict_simple <- function(test_predict_O2, test_year, model_name) {
  # Plot ####
  xmin <- min(test_predict_O2$X)*1.5
  xlimits = c((min(test_predict_O2$X)*1.5*1000), (max(test_predict_O2$X)*1.5*1000))
  ylimits = c((min(test_predict_O2$Y)*1.5*1000), (max(test_predict_O2$Y)*1.5*1000))
  
  data_plot <- ggplot(us_coast_proj) + geom_sf() +
    geom_point(
      data = test_predict_O2,
      aes(
        x = X * 1000,
        y = Y * 1000,
        col = o2
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
  
  ## put all plots in one ####
  grid.arrange(data_plot, predict_plot, residual_plot, ncol = 3)
  # plot residuals vs. prediction
  resid_vs_pred <- ggplot(data = test_predict_O2, aes(x = (est), y = residual, col = Y)) +
    geom_point() +
    scale_colour_viridis_c(
      limits = c(31, 50),
      oob = scales::squish,
      name = "latitude",
      breaks = c(35, 40, 45)
    ) +
    ggtitle(test_year) +
    labs(x = "Predicted", y = "Residual") +
    theme(legend.position = "none")
  pred_vs_actual <- ggplot(data = test_predict_O2, aes(x = o2, y = est, col = Y)) +
    geom_point() +
    scale_colour_viridis_c(
      limits = c(31, 50),
      oob = scales::squish,
      name = "latitude",
      breaks = c(35, 40, 45)
    ) +
    ggtitle(test_year) +
    labs(x = "Observed", y = "Predicted") +
    geom_abline(intercept = 0, slope = 1) +
    theme(legend.position = "none")
  return(grid.arrange(pred_vs_actual, resid_vs_pred, ncol = 2))
}



##still working on this
#function to pull evaluation plots for just one region, model, year
plot_problem_child <- function(region, year, model){
  #Read in output from a region
  load("~/Dropbox/choke species/code/choke-species-data/code/test_wc_O2_predictions/outputs/o2_models_ebs.RData")
  
  test <- output[[2]]
  ##1) Plot test data
  data <- test[[1]]
  ggplot(data, aes(x=X, y=Y))+geom_point(aes(shape=survey, colour=o2))
  
  ##1) Plot observations versus predictions
  pred <- test[[2]][[1]]
  pred2 <- test[[2]][[1]]
  pred3 <- test[[2]][[1]]
  ggplot()
  
}