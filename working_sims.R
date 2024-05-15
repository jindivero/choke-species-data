run_models <- function(dat, mesh) {
  null <- try(sdmTMB(dat ~ -1+year+log_depth_scaled+log_depth_scaled2, 
                     data = dat, 
                     spatial = "on",
                     mesh=mesh,
                     anisotropy=T,
                     reml=F,
                     time=NULL,
                     family =tweedie(link="log"),
                     extra_time=1980:2100),
              control = sdmTMBcontrol(
                newton_loops = 2,
              )))
print(paste("running breakpt(po2)"))
bp_o2 <- try(sdmTMB(sim ~ 1+year+breakpt(po2_s)+log_depth_scaled+log_depth_scaled2, 
                    data = dat,
                    time = NULL,
                    reml = F,
                    anisotropy = TRUE,
                    spatiotemporal = FALSE,
                    mesh=mesh,
                    family =tweedie(link="log"),
                    # control = sdmTMBcontrol(
                    #start = list(b_threshold = start),
                    #lower = list(b_threshold = lower), 
                    #upper = list(b_threshold = upper),
                    #newton_loops = 2
))
print(paste("running logistic(po2)"))
log_o2 <- try(sdmTMB(sim ~ 1+year+logistic2(po2_s)+log_depth_scaled+log_depth_scaled2, 
                     data = dat,
                     time = NULL,
                     reml = F,
                     anisotropy = TRUE,
                     spatiotemporal = FALSE,
                     mesh=mesh,
                     family =tweedie(link="log"),
                     # control = sdmTMBcontrol(
                     #start = list(b_threshold = start),
                     #lower = list(b_threshold = lower), 
                     #upper = list(b_threshold = upper),
                     #newton_loops = 2
))
print(paste("running breakpt(mi1)"))
bp_mi1 <- try(sdmTMB(sim ~ -1+year+breakpt(mi)+log_depth_scaled+log_depth_scaled2, 
                     data = dat, 
                     time = NULL,
                     reml = F,
                     anisotropy = TRUE,
                     spatiotemporal = FALSE,
                     mesh=mesh,
                     family =tweedie(link="log"),
                     extra_time=1980:2100),
              control = sdmTMBcontrol(
                start = list(b_threshold = start),
                newton_loops = 2,
                nlminb_loops=2)))
print(paste("running breakpt(mi2)"))
bp_mi2 <- try(sdmTMB(sim ~ -1+year+breakpt(mi2)+log_depth_scaled+log_depth_scaled2, 
                     data = dat, 
                     time = NULL,
                     reml = F,
                     anisotropy = TRUE,
                     spatiotemporal = FALSE,
                     mesh=mesh,
                     family =tweedie(link="log"),
                     extra_time=1980:2100),
              control = sdmTMBcontrol(
                start = list(b_threshold = start),
                newton_loops = 2,
                nlminb_loops=2)))
print(paste("running breakpt(mi3)"))
bp_mi3 <- try(sdmTMB(sim ~ -1+year+breakpt(mi3)+log_depth_scaled+log_depth_scaled2, 
                     data = dat, 
                     time = NULL,
                     reml = F,
                     anisotropy = TRUE,
                     spatiotemporal = FALSE,
                     mesh=mesh,
                     family =tweedie(link="log"),
                     extra_time=1980:2100),
              control = sdmTMBcontrol(
                start = list(b_threshold = start),
                newton_loops = 2,
                nlminb_loops=2)))
print(paste("running logistic(mi1)"))
log_mi1 <- try(sdmTMB(sim ~ -1+year+logistic2(mi1)+log_depth_scaled+log_depth_scaled2, 
                      data = dat, 
                      time = NULL,
                      reml = F,
                      anisotropy = TRUE,
                      spatiotemporal = FALSE,
                      mesh=mesh,
                      family =tweedie(link="log"),
                      extra_time=1980:2100),
               control = sdmTMBcontrol(
                 start = list(b_threshold = start),
                 newton_loops = 2,
                 nlminb_loops=2)))
print(paste("running logistic (mi2)"))
log_mi2 <- try(sdmTMB(sim ~ -1+year+logistic2(mi2)+log_depth_scaled+log_depth_scaled2, 
                      data = dat, 
                      time = NULL,
                      reml = F,
                      anisotropy = TRUE,
                      spatiotemporal = FALSE,
                      mesh=mesh,
                      family =tweedie(link="log"),
                      extra_time=1980:2100),
               control = sdmTMBcontrol(
                 start = list(b_threshold = start),
                 newton_loops = 2,
                 nlminb_loops=2)))
print(paste("running logistic (mi3)"))
log_mi3 <- try(sdmTMB(sim ~ -1+year+logistic2(mi3)+log_depth_scaled+log_depth_scaled2, 
                      data = dat, 
                      time = NULL,
                      reml = F,
                      anisotropy = TRUE,
                      spatiotemporal = FALSE,
                      mesh=mesh,
                      family =tweedie(link="log"),
                      extra_time=1980:2100),
               control = sdmTMBcontrol(
                 start = list(b_threshold = start),
                 newton_loops = 2,
                 nlminb_loops=2)))
print(paste("running temp"))
temp <- try(sdmTMB(sim ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s,
                 data = dat, 
                 spatial = "on",
                 mesh=mesh,
                 anisotropy=T,
                 reml=F,
                 time=NULL,
                 extra_time=1980:2100),
          family =tweedie(link="log"),
          control = sdmTMBcontrol(
            newton_loops = 1,
          )))
print(paste("running o2"))
po2 <- try(sdmTMB(sim ~ -1+year+log_depth_scaled+log_depth_scaled2+po2_s,
                 data = dat, 
                 spatial = "on",
                 mesh=mesh,
                 anisotropy=T,
                 reml=F,
                 time=NULL,
                 family =tweedie(link="log"),
                 extra_time=1980:2100),
          control = sdmTMBcontrol(newton_loops = 1,
          )))
print(paste("running temp o2"))
temp_o2 <- try(sdmTMB(sim ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s + po2_s,
                 data = dat, 
                 spatial = "on",
                 mesh=mesh,
                 anisotropy=T,
                 reml=F,
                 time=NULL,
                 family =tweedie(link="log"),
                 extra_time=1980:2100),
          control = sdmTMBcontrol(newton_loops = 1,
          )))
print(paste("running temp o2 interaction"))
temp_o2_int <- try(sdmTMB(sim ~ -1+year+log_depth_scaled+log_depth_scaled2+temp_s * po2_s,
                 data = dat, 
                 spatial = "on",
                 mesh=mesh,
                 anisotropy=T,
                 reml=F,
                 time=NULL,
                 family =tweedie(link="log"),
                 extra_time=1980:2100),
          control = sdmTMBcontrol(newton_loops = 1,
          )))

model_fits <- list(null, bp_o2,log_o2, bp_m1, bp_m2, bp_m3, log_mi1, log_mi2, log_mi3,temp, po2, temp_po2, temp_o2_int)
return(model_fits)

}











### Create dAIC table ###
## Make list of model names##
models <- c("breakpt-pO2", "Eo estimation and logistic po2' (no prior)","logistic-pO2", "Null", "temp", "po2", "temp+po2", "temp * po2")
## Create table and add AIC for each ##
AIC <- as.data.frame(matrix(NA, ncol = 1, nrow =length(models), dimnames = list(models)))
AIC[1,] <- try(AIC(m1))
AIC[2,] <- try(AIC(m2))
#AIC[3,] <- try(AIC(m2a))
AIC[3,] <- try(AIC(m3))
AIC[4,] <- try(AIC(m4))
AIC[5,] <- try(AIC(m5))
AIC[6,] <- try(AIC(m6))
AIC[7,] <- try(AIC(m7))
AIC[8,] <- try(AIC(m8))

## Calculate delta-AIC ##
AIC$V1 <- as.numeric(AIC$V1)
AIC$dAIC <- try(abs(min(AIC$V1, na.rm=T)-(AIC$V1)))
AIC$model <- models
try(return(AIC))
}

### Create dAIC table ###
## Make list of model names##
models <- c("breakpt-pO2", "Eo estimation and logistic po2' (no prior)","logistic-pO2", "Null", "temp", "po2", "temp+po2", "temp * po2")
## Create table and add AIC for each ##
AIC <- as.data.frame(matrix(NA, ncol = 1, nrow =length(models), dimnames = list(models)))
AIC[1,] <- try(AIC(m1))
AIC[2,] <- try(AIC(m2))
#AIC[3,] <- try(AIC(m2a))
AIC[3,] <- try(AIC(m3))
AIC[4,] <- try(AIC(m4))
AIC[5,] <- try(AIC(m5))
AIC[6,] <- try(AIC(m6))
AIC[7,] <- try(AIC(m7))
AIC[8,] <- try(AIC(m8))

## Calculate delta-AIC ##
AIC$V1 <- as.numeric(AIC$V1)
AIC$dAIC <- try(abs(min(AIC$V1, na.rm=T)-(AIC$V1)))
AIC$model <- models
try(return(AIC))
