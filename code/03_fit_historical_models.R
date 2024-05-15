library(sdmTMB)
source("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data/code/util_funs.R")
setwd("/Users/jindiv/Library/CloudStorage/Dropbox/choke species/code/choke-species-data")


# Model Species

Use `sdmTMB` to make ensemble models for each species of interest. If these models have already been run by the user and outputs saved, the cleaned and joined version of the outputs should be accessible by skipping down to the chunk labeled **Load**.

```{r,eval=F}
spp_to_model <- c("dover sole","sablefish","shortspine thornyhead","longspine thornyhead")
```

Run models and save cross-validation model stacking weights. (note: this chunk is turned to eval=F for now)

## Fit Models

```{r,eval=F}
for(i in 1:length(spp_to_model)){
  s <- spp_to_model[i]
  m <- model_species(s,data = trawl_roms_utm,use_substrate = F)
  write_rds(m,here::here('model output',paste(s,'models.rds')))
  print(paste(s,'models finished'))
}
```


