#Retrospective skills testing
#From Barnes et al. https://github.com/cheryl-barnes/SDM-fit-forecast
#iteratively increased the length of the time series until a single year remained for forecasting. 
#Estimates of spearman's correlation coefficient (rho) for each nested submodel to assess correlations between forecasts and observations
#10-yr moving window to estimate mean (and variance) in Spearman's rho, a relative measure of forecast skill

#Prepare data in a previous file

# Loop through species
pI = rI = mI = 1
for( pI in 1:length(Species_Set)){
  
}

# Persistence forecast

#Create vector of years and length of years
years <- unique(dat$years)
N <- length(years)
#n_fut #number of years forecasted 

##Fit to year i, predict to years i--N, repeat i+1
#Create function that subsets to fitting and predicting dataset
r_s_t <- function(year, N){
  #Separate dataset into predicting and fitting
  fit <- filter(dat, year>year)
  predict <- filter(dat, year<=year)

  #Fit models (all alternative, or pick best-fitting from overall best-fitting?)
  
  #Identify best-fitting model?
  
  #Predict to modeled years
  
  #Predict to un-modeled years
  
  #Bind observations with predictions
}

#Generate the COG in the final year


#Performance
# Thorson 2019: compare performance predicting future changes in COG against observed change in COG calculated using the abundance-and-area weighted average

#Change in AAWA when forecasting forward (observations)
dAWAA <- (final_year_of_fitting + predicted_change) - final_year_of_fitting
#Change in centroid (COG) of model
dCOG <- (final_year_of_fitting + predicted_change) - final_year_of_fitting
#Persistence, dAWAA and dCOG equals 0

#Predictive error for each estimator relative to persistence forecast

E <- (dAWAA-dCOG)/dAWAA

#Mean squared error relative to persistence forecast
V <- (1/N-n_fut) sum(E^2) 
#Variance explained
R2 <- 1-V
#R2 outperforms persistence >0, R2 <0 worse than persistence; R2=0.2 means model explained 20% of variance in future changes in observed abundance-and-area-weighted centroid