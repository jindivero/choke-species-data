###Fit suite of retrospective models

source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/wa_state/util_funs.R")
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")


devtools::install_github("pbs-assess/sdmTMB", ref="newlogistic2", dependencies = TRUE, force=TRUE)
library(sdmTMB)

### Make mesh ###
mesh <- make_mesh(dat, xy_cols = c("X", "Y"), n_knots=250)

## Make list of models ##
#Null model
null <- "cpue_kg_km2~-1+year+log_depth_scaled+log_depth_scaled2"
formulas <- list("+breakpt(po2_s)") 
#"+logistic(po2_s)", "+logistic(mi1_s)", "+logistic(mi2_s)", "+logistic(mi3_s)", "+po2_s", "+po2_s+temp_s", "+temp_s", "+po2_s*temp_s", "+s(po2_s)", "+s(mi1_s)", "+s(mi2_s)", "+s(mi3_s)")
formulas <- lapply(formulas, paste_reverse, null)

fits <- lapply(formulas, run_sdmTMB, dat, spc)

p1 <- ggplot(subset(pars, pars$term=="mi-Eo"&pars$analysis=="Unconstrained"), aes(x=estimate)) +
  geom_density(fill="lightblue", adjust = 1.5) +
  geom_vline(data=subset(Eo_values, Eo_values$analysis=="Unconstrained"), aes(xintercept = MLE_avg),linetype="dashed", size=1.2, color="darkorange", show.legend=T)+
  geom_vline(data=subset(Eo_values, Eo_values$analysis=="Unconstrained"), aes(xintercept = true),linetype="dashed", size=1.2)+
  facet_wrap("data", labeller=labeller(data = new_labels))+
  scale_y_continuous(limits=c(0,6))+
  scale_x_continuous(limits=c(-0.7,1.4))+
  xlab(bquote(E[0]~Maximum~Likelihood~Estimate))+
  theme(axis.title.y=element_blank(),
    axis.title.x=element_text(size=25),
    axis.text=element_text(size=20),
    strip.background = element_rect(fill="white", color="white"), 
    strip.text= element_text(face = "bold", size=25))
  



###Multiple species
#Prepare data
spc_to_model <- c("dover sole","sablefish","shortspine thornyhead","longspine thornyhead")
sci_names <- c("")
for(i in 1:length(spc_to_model)){
  spc <- spc_to_model[i]
  sci_name <- sci_names[i]
  dat2 <- prepare_data(spc=spc, sci_name=sci_name)
  m <- model_species(s,data = trawl_roms_utm,use_substrate = F)
  write_rds(m,here::here('model output',paste(s,'models.rds')))
  print(paste(s,'models finished'))
}


for(i in 1:length(spp_to_model)){
  s <- spp_to_model[i]
  m <- model_species(s,data = trawl_roms_utm,use_substrate = F)
  write_rds(m,here::here('model output',paste(s,'models.rds')))
  print(paste(s,'models finished'))
}

###Data diagnostics
#How many positive catches vs zero catches, and how many hauls with missing length data?
positive_catches <- positive_catches(dat)