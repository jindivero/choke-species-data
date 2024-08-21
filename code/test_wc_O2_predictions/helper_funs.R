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

plot_simple <- function(output, dat.2.use){
  #Separate test and training data, predictions, and models from output list
  train_data <- output[[1]]
  test_data <- output[[2]]
  preds <- output[[3]]
  models <- output[[4]]
  #Set latitude and longitude
  xlims <- c(min(dat.2.use$X)*1000, max(dat.2.use$X)*1000)
  ylims <- c(min(dat.2.use$Y)*1000, max(dat.2.use$Y)*1000)
  lats <- c(round(min(dat.2.use$latitude)),  round(max(dat.2.use$latitude)))
  lons <- c(round(min(dat.2.use$longitude)+2), round(max(dat.2.use$longitude)))
  data_map <- 
    ggplot(us_coast_proj) + geom_sf() +
    geom_point(train_data, mapping=aes(x=X*1000, y=Y*1000, col=o2),
                       # data = train_data,
                        #aes(
                        # x = X * 1000,
                        # y = Y * 1000,
                        #  col = o2
                        #  ),
                        size = 1.0,
                        alpha = 1.0
             ) +
            ylim(ylims)+
            scale_x_continuous(breaks=lons, limits=xlims)+
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
             theme(axis.text = element_text(size = 11)) +
             theme(axis.title = element_text(size = 12)) +
             theme(legend.text = element_text(size = 11)) +
             theme(legend.position = "none")
             #guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                            #   0.5))
  
  dat_available <- ggplot(train_data, aes(x=year))+
    stat_count(aes(fill=survey))+
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
    theme(legend.position="bottom")+
    xlab("Year")+
    ylab("Number of \n observations")
  
  model_names <- c("Persistent Model", "Persistent + Year Model", "Spatio-Temporal Model")
  test_year <- unique(test_data$year)
  test_region <- unique(test_data$region)
  pred_plots <- list()
  resid_plots <- list()
  pred_obs <- list()
  for (i in 1:length(preds)){
    try(pred_plots[[i]] <-  ggplot(us_coast_proj) + geom_sf() +
          geom_point(preds[[i]], mapping=aes(x=X*1000, y=Y*1000, col=o2),
                 size = 1.0,
                 alpha = 1.0
      ) +
        ylim(ylims)+
        scale_x_continuous(breaks=lons, limits=xlims)+
      scale_colour_viridis_c(
        limits = c(0, 200),
        oob = scales::squish,
        name = bquote(O[2]~Predictions),
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
      theme(axis.text = element_text(size = 11)) +
      theme(axis.title = element_text(size = 12)) +
      theme(legend.text = element_text(size = 11)) +
      theme(legend.position = "bottom") +
      guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                        0.5)))
    
    try(resid_plots[[i]] <-  ggplot(us_coast_proj) + geom_sf() +
          geom_point(preds[[i]], mapping=aes(x=X*1000, y=Y*1000, col=residual),
                 size = 1.0,
                 alpha = 1.0
      ) +
        scale_colour_distiller(palette = "RdBu", limits = c(-50, 50)) +
        ylim(ylims)+
        scale_x_continuous(breaks=lons, limits=xlims)+
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
    
    try(pred_obs[[i]] <- ggplot(data = preds[[i]], aes(x = o2, y = est, col = latitude)) +
      geom_point() +
      scale_colour_distiller(
       # limits = c(31, 50),
       #oob = scales::squish,
       name = "latitude",
       palette="Greys"
       # breaks = c(35, 40, 45)
      ) +
      theme(legend.position = "bottom") +
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
      labs(x = "Observed", y = "Predicted") +
      geom_abline(intercept = 0, slope = 1)+
      theme(legend.position="bottom"))
  }
  # plot residuals vs. prediction
#  resid_vs_pred <- ggplot(data = test_predict_O2, aes(x = (est), y = residual, col = Y)) +
   # geom_point() +
    #scale_colour_viridis_c(
    #  limits = c(31, 50),
    #  oob = scales::squish,
    #  name = "latitude",
    #  breaks = c(35, 40, 45)
   # ) +
   # ggtitle(test_year) +
   # labs(x = "Predicted", y = "Residual") +
   # theme(legend.position = "none")
  
  figure1 <- ggarrange(data_map, dat_available, labels=c("A", "B"),
                      ncol = 2, nrow = 1)
  figure1 <- annotate_figure(figure1, left="Training Data", fig.lab.size=14, fig.lab.face="bold")
  figure2 <- ggarrange(pred_plots[[1]], resid_plots[[1]], pred_obs[[1]], ncol=3, nrow=1, legend="none", labels=c("C", "D", "E"))
  figure2 <- annotate_figure(figure2, left=paste(model_names[1]), fig.lab.size=14, fig.lab.face="bold")
  figure3 <- ggarrange(pred_plots[[2]], resid_plots[[2]], pred_obs[[2]], ncol=3, nrow=1, legend="none")
  figure3 <- annotate_figure(figure3, left=paste(model_names[2]), fig.lab.size=14, fig.lab.face="bold")
  figure4 <- try(ggarrange(pred_plots[[3]], resid_plots[[3]], pred_obs[[3]], ncol=3, nrow=1))
  figure4 <- try(annotate_figure(figure4, left=paste(model_names[3]), fig.lab.size=14, fig.lab.face="bold"))
  if(!is.list(figure4)){
    figure4 <- ggarrange(ggplot(), ggplot(), ggplot())
  }
  
  figure <- ggarrange(figure1, figure2, figure3, figure4, ncol=1, nrow=4, heights=c(1,1,1,1.25), align="h")
  annotate_figure(figure, top=paste(test_year), fig.lab.size=18, fig.lab.face="bold")
  
  ggsave(
    paste("code/test_wc_O2_predictions/outputs/plots/", test_region, "/preds/preds_", test_year, ".pdf", sep=""),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = 8.5,
    height = 11,
    units = c("in"),
    dpi = 600,
    limitsize = TRUE
  )
  return(figure)
}

##Function to plot marginal effects of spatio-temporal model
plot_marginal_effects <- function(models,preds, dat.2.use, i){
  m <- try(models[[i]])
  if(is.list(m)){ #don't do all this if the model failed anyway
    xlims <- c(min(dat.2.use$X)*1000, max(dat.2.use$X)*1000)
    ylims <- c(min(dat.2.use$Y)*1000, max(dat.2.use$Y)*1000)
    lats <- c(round(min(dat.2.use$latitude)),  round(max(dat.2.use$latitude)))
    lons <- c(round(min(dat.2.use$longitude)+2), round(max(dat.2.use$longitude)))
    #Prediction dataframe for epsilon and omega
    preds <- try(preds[[i]])
    test_region <- unique(preds$region)
    test_year <- unique(preds$year)
  #marginal effects
    pdf(paste("code/test_wc_O2_predictions/outputs/plots/", test_region, "/margeffects/margeffects_", test_year,".pdf", sep=""))
    par(mfrow=c(2,2))
  visreg(m, "sigma0")
  visreg(m, "temp")
  visreg(m, "doy")
  visreg(m, "depth_ln")
  dev.off()
  
  omega <- ggplot(us_coast_proj) + geom_sf() +
        geom_point(preds, mapping=aes(x=X*1000, y=Y*1000, col=omega_s),
                   size = 1.0,
                   alpha = 1.0
        ) +
        scale_colour_distiller(palette = "RdBu") +
        ylim(ylims)+
        scale_x_continuous(breaks=lons, limits=xlims)+
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
        geom_point(preds, mapping=aes(x=X*1000, y=Y*1000, col=epsilon_st),
                   size = 1.0,
                   alpha = 1.0
        ) +
        scale_colour_distiller(palette = "RdBu") +
        ylim(ylims)+
        scale_x_continuous(breaks=lons, limits=xlims)+
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
  
  figure <- ggarrange(omega, epsilon)
  annotate_figure(figure, top=paste(test_year))
  ggsave(
    paste("code/test_wc_O2_predictions/outputs/plots/", test_region, "/spatiotemp_var/spatiotemp_var_", test_year, ".pdf", sep=""),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = 8.5,
    height = 11,
    units = c("in"),
    dpi = 600,
    limitsize = TRUE
  )
  
  }
  if(!is.list(m)){
    return(paste("model not fit"))
  }
}

##still working on this
#function to pull evaluation plots for just one region, model, year
plot_one <- function(region, year, model){
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