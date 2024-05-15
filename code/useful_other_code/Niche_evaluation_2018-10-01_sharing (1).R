

# Install package                                    #
if(FALSE){
  devtools::install_github("james-thorson/VAST", ref="development")
  devtools::install_github("kaskr/adcomp/TMB")
}

RootDir = "C:/Users/James.Thorson/Desktop/UW Hideaway/Collaborations/2017 -- niche forecast evaluation/"
#RootDir = "D:/UW Hideaway (SyncBackFree)/Collaborations/2017 -- niche forecast evaluation/"
#TmbDir = "C:/Users/James.Thorson/Desktop/Project_git/VAST/inst/executables/"
TmbDir = system.file("executables", package="VAST")
DataDir = paste0(RootDir,"Data/")
LocalTmbDir = paste0(RootDir,"TMB code/")

# load libraries
library( VAST )
library( TMB )

# directory
#Date = Sys.Date()
  Date = "2018-03-15"
  DateDir = paste0(RootDir,Date,"_AR_devel/")
  dir.create(DateDir)

###############
# Settings
###############

if( "Record.RData" %in% list.files(DateDir) ){
  load( file=paste0(DateDir,"Record.RData"))
  attach(Record)
  message("Loaded Record")
}else{
  Version = "VAST_v4_1_0"
  n_x = 380 # Number of stations
  grid_size_km = 25
  Method = c("Grid", "Mesh")[2]
  FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence; #Epsilon=Spatio-temporal; #Omega=Spatial
  RhoConfig = c(Beta1=2, Beta2=2, Epsilon1=4, Epsilon2=4)      # 2=RW; 4=AR1
  ObsModel = c(1,1)
  # Slot 0: 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB
  # Slot 1: 0=delta-model; 1=Poisson-process-model
  Kmeans_Config = list( "randomseed"=1, "nstart"=25, "iter.max"=1e3 )     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
  Use_REML = FALSE
  BiasCorr = FALSE
  Gamma_Config = c("All_identical", "Separate_by_year")[1]
  Temp_Config = c("Local_Bottom"=1, "Local_Surface"=0, "ColdPool"=0) # 0=Off; 1=On
  OverdispersionConfig = c(0,0)
  if(Gamma_Config%in%c("All_separate","Separate_by_year") & Temp_Config['ColdPool']==1) stop("Regional effects varying by year doesn't make sense")
  Options = c('Calculate_Range'=TRUE, 'Calculate_effective_area'=TRUE )
  Domain = c( "Stations", "EBS" )[2]

  # Determine region
  Region = "Eastern_Bering_Sea"

  # Decide on strata for use when calculating indices
  # In this case, will not restrict the extrapolation domain at all while calculating an index
  strata.limits <- data.frame( 'STRATA'="All_areas")

  # Simulation settings
  ModelSet = c("Temp", "Spatial", "Both", "AWA")
  Npeal = 21

  # Save options for future records
  Record = ThorsonUtilities::bundlelist( c("Version","grid_size_km","n_x","Method","FieldConfig","RhoConfig","ObsModel","Kmeans_Config","Use_REML","BiasCorr","Gamma_Config","Temp_Config","OverdispersionConfig","Region","strata.limits","Options","Domain","ModelSet","Npeal") )
  save( Record, file=paste0(DateDir,"Record.RData"))
  capture.output( Record, file=paste0(DateDir,"Record.txt"))
}

################
# Prepare data
# (THIS WILL VARY FOR DIFFERENT DATA SETS)
################

if( "Data_List.RData" %in% list.files(DateDir) ){
  load( file=paste0(DateDir,"Data_List.RData"))
  attach( Data_List )
  message("Loaded Data")
}else{
  # Download survey data
  Database = FishData::download_catch_rates(survey="Eastern_Bering_Sea", species_set=100, localdir=DataDir )
  Data_Full = ThorsonUtilities::rename_columns( Database[,c('Lat','Long','Year','Wt','Sci')], newname=c('Lat','Lon','Year','Catch_KG','Species') )

  # Exclude infrequent ID
  species_set = unique( Data_Full[,'Species'] )

  # Kick out species with no encounters in any year
  EncProb = tapply( Data_Full[,'Catch_KG'], INDEX=list(factor(Database[,'Sci'],levels=species_set),Database[,'Year']), FUN=function(vec){mean(vec>0)})
  species_set = species_set[ which(apply(EncProb,MARGIN=1,FUN=function(vec){all(vec!=0)})) ]

  # Kick out high-level taxa
  species_set = species_set[ which(sapply(as.character(species_set),FUN=function(Char){length(strsplit(Char,split="_")[[1]])==2})) ]

  # Kick out "sp."
  species_set = setdiff( species_set, species_set[grep("_sp.",species_set,fixed=TRUE)] )
  species_set = setdiff( species_set, species_set[grep("eggs",species_set,fixed=TRUE)] )

  # Identify taxonomy
  species_set = sapply( species_set,FUN=function(char){paste(strsplit(char,"_")[[1]],collapse=" ")} )
  COL = taxize::classification( species_set, db='gbif', row=1)

  # Identify fish
  FishTF = sapply(COL, FUN=function(List){ !is.na(List) && List[which(List[,'rank']=="phylum"),'name']=="Chordata"})

  # Identify crabs
  CrabTF = sapply(COL, FUN=function(List){ !is.na(List) && List[which(List[,'rank']=="order"),'name']=="Decapoda"})

  # Cchoose species set
  species_set = species_set[which(FishTF | CrabTF)]

  # Choose species set
  species_set = species_set[1:20]

  #
  Data_Geostat = Data_Full[ which(Data_Full$Species=="Gadus chalcogrammus"), ]

  # Restrict to common years
  Data_Geostat = Data_Geostat[ which(Data_Geostat[,'Year']<=2015), ]
  Year_Set = sort(unique(Data_Geostat[,'Year']))

  # Get bottom temperature
  BottomTemp = read.table( paste0(DataDir,"grid_NearNeighbor_temp_1982_2015_V2.dat") )
  colnames(BottomTemp) = c("Year","Lon","Lat","Temp_C")
  BottomTemp[,'Lon'] = BottomTemp[,'Lon'] - 360

  # Get surface temperature
  SurfaceTemp = read.table( paste0(DataDir,"surf_temp_grid.dat") )
  colnames(SurfaceTemp) = c("Year","Lon","Lat","Temp_C")
  SurfaceTemp[,'Lon'] = SurfaceTemp[,'Lon'] - 360
  plot( x=BottomTemp[,'Temp_C'], y=SurfaceTemp[,'Temp_C'])

  # Get cold pool
  ColdPoolData = read.csv( paste0(DataDir,"cp_areas.csv"), skip=0, header=TRUE )
  ColdPoolData = cbind( ColdPoolData, "Area_Used"=ColdPoolData[,'AREA_MINUS1']+ColdPoolData[,'AREA_0'])

  # Get extrapolation data
  if( Domain=="EBS" ){
    Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )
    # Generate spatial info
    Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=DateDir )
  }
  if( Domain=="Stations" ){
    Extrapolation_List = list( "zone"=32, "flip_around_dateline"=TRUE )
    Extrapolation_List$Data_Extrap = data.frame( unique(DF[which(DF$Year==2006),c('Long','Lat')]) )
    names(Extrapolation_List$Data_Extrap) = c("Lon", "Lat")
    Extrapolation_List$Area_km2_x = rep(1, nrow(Extrapolation_List$Data_Extrap))
    Extrapolation_List$a_el = matrix( Extrapolation_List$Area_km2_x, ncol=1 )
    Extrapolation_List$Data_Extrap = cbind( Extrapolation_List$Data_Extrap, "Area_in_survey_km2"=Extrapolation_List$Area_km2_x )

    # Extra junk
    tmpUTM = Convert_LL_to_UTM_Fn( Lon=Extrapolation_List$Data_Extrap[,'Lon'], Lat=Extrapolation_List$Data_Extrap[,'Lat'], zone=Extrapolation_List$zone, flip_around_dateline=Extrapolation_List$flip_around_dateline)                                                         #$
    Extrapolation_List$Data_Extrap = cbind( Extrapolation_List$Data_Extrap, 'Include'=1)
    Extrapolation_List$Data_Extrap[,c('E_km','N_km')] = tmpUTM[,c('X','Y')]

    # Generate spatial info
    Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Extrapolation_List$Data_Extrap[,'Lon'], Lat=Extrapolation_List$Data_Extrap[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=DateDir )
  }

  # Calculate spatial information for SPDE mesh, strata areas, and AR1 process
  MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
  if(Domain=="Stations") MapDetails_List$Cex = 1

  # Add Lat/Long to Spatial_List
  Tmp = cbind("X"=Spatial_List$loc_x[,1], "Y"=Spatial_List$loc_x[,2])
  attr(Tmp,"projection") = "UTM"
  attr(Tmp,"zone") = MapDetails_List[["Zone"]]
  tmpLL = PBSmapping::convUL(Tmp)                                                         #$
  Spatial_List$loc_x = cbind( Spatial_List$loc_x, "Lon"=tmpLL[,1], "Lat"=tmpLL[,2])

  # Associate knots with temperature
  #Data_Geostat = cbind(Data_Geostat, "Temp_C"=NA)
  #for( i in 1:length(Year_Set)){
  #  Which1 = which( Data_Geostat[,'Year']==Year_Set[i] )
  #  Which2 = which( BottomTemp[,'Year']==Year_Set[i] )
  #  NN_temp = RANN::nn2( data=BottomTemp[Which2,c('Lon','Lat')], query=Data_Geostat[Which1,c('Lon','Lat')], k=1 )
  #  Data_Geostat[Which1,'Temp_C'] = BottomTemp[Which2,'Temp_C'][NN_temp$nn.idx]
  #}

  ### Associate knots with temperature
  # Bottom temperature by knot and year
  BottomTemp_xt = array(NA, dim=c(n_x,length(Year_Set)))
  for( i in 1:length(Year_Set)){
    Which = which( BottomTemp[,'Year']==Year_Set[i] )
    NN_temp = RANN::nn2( data=BottomTemp[Which,c('Lon','Lat')], query=Spatial_List$loc_x[,c('Lon','Lat')], k=1 )
    BottomTemp_xt[,i] = BottomTemp[Which,'Temp_C'][NN_temp$nn.idx]
  }
  # Surface temperature by knot and year
  SurfaceTemp_xt = array(NA, dim=c(n_x,length(Year_Set)))
  for( i in 1:length(Year_Set)){
    Which = which( SurfaceTemp[,'Year']==Year_Set[i] )
    NN_temp = RANN::nn2( data=SurfaceTemp[Which,c('Lon','Lat')], query=Spatial_List$loc_x[,c('Lon','Lat')], k=1 )
    SurfaceTemp_xt[,i] = SurfaceTemp[Which,'Temp_C'][NN_temp$nn.idx]
  }
  # Cold pool _by_ location for each knot and year
  ColdPool_t = ColdPoolData[match(Year_Set,ColdPoolData[,'YEAR']),'Area_Used']
  ColdPool_xtp = aperm( outer(Spatial_List$loc_x[,c('E_km','N_km')]-outer(rep(1,n_x),colMeans(Spatial_List$loc_x[,c('E_km','N_km')])), ColdPool_t-mean(ColdPool_t)), c(1,3,2) )

  ### Make covariate array
  X_xtp = array(0, dim=c(n_x,length(Year_Set),0))
  if( Temp_Config['Local_Bottom']==1 ){
    X_xtp = abind::abind( X_xtp, BottomTemp_xt-array(mean(BottomTemp_xt),dim=dim(BottomTemp_xt)), (BottomTemp_xt-array(mean(BottomTemp_xt),dim=dim(BottomTemp_xt)))^2, along=3)
  }
  # Average temperature _by_ location interaction
  if( Temp_Config['Local_Surface']==1 ){
    X_xtp = abind::abind( X_xtp, SurfaceTemp_xt-array(mean(SurfaceTemp_xt),dim=dim(SurfaceTemp_xt)), (SurfaceTemp_xt-array(mean(SurfaceTemp_xt),dim=dim(SurfaceTemp_xt)))^2, along=3)
  }
  # Average temperature _by_ location interaction
  if( Temp_Config['ColdPool']==1 ){
    X_xtp = abind::abind( X_xtp, ColdPool_xtp, along=3)
  }
  # No temperature effect
  if( all(Temp_Config==0) ){
    X_xtp = array(0, dim=c(n_x,length(Year_Set),1))
  }

  # Save
  Data_List = list("Year_Set"=Year_Set, "species_set"=species_set, "Data_Geostat"=Data_Geostat, "X_xtp"=X_xtp, "ColdPool_t"=ColdPool_t, "Extrapolation_List"=Extrapolation_List, "Spatial_List"=Spatial_List, "SurfaceTemp_xt"=SurfaceTemp_xt, "BottomTemp_xt"=BottomTemp_xt, "BottomTemp"=BottomTemp, "SurfaceTemp"=SurfaceTemp)
  save( Data_List, file=paste0(DateDir,"Data_List.RData"))

  # Plot annual temperature
  f = function(Num, zlim=NULL){
    if( is.null(zlim)) Return = ((Num)-min((Num),na.rm=TRUE))/diff(range((Num),na.rm=TRUE))
    if( !is.null(zlim)) Return = ((Num)-zlim[1])/diff(zlim)
    return(Return)
  }
  Col = colorRampPalette(colors=c("darkblue","blue","lightblue","lightgreen","yellow","orange","red"))
  Dim = c("Nrow"=ceiling(sqrt(length(Year_Set)))); Dim = c(Dim,"Ncol"=ceiling(length(Year_Set)/Dim['Nrow']))
  for( j in seq(1,dim(X_xtp)[3],length=dim(X_xtp)[3]) ){
    ThorsonUtilities::save_fig( file=paste0(DateDir,"Covariate_",j), width=Dim[1]*2, height=Dim[2]*2)
      par( mfrow=Dim, mar=c(0,0,2,0) )
      for( i in 1:length(Year_Set)){
        plot( x=Spatial_List$loc_x[,'Lon'], y=Spatial_List$loc_x[,'Lat'], col=Col(50)[ceiling(f(X_xtp[,,j])[,i]*49)+1], main=Year_Set[i], xlab="", ylab="", xaxt="n", yaxt="n", pch=20, cex=3 )
        library(maps)
        map( "world", add=TRUE )
      }
    dev.off()
  }

  # Plot annual temperature
  Dim = c("Nrow"=ceiling(sqrt(length(Year_Set)))); Dim = c(Dim,"Ncol"=ceiling(length(Year_Set)/Dim['Nrow']))
  for( j in 1:1 ){
    ThorsonUtilities::save_fig( file=paste0(DateDir,"Covariate_",j,"_Domain"), width=Dim[1]*2, height=Dim[2]*2)
      par( mfrow=Dim, mar=c(0,0,2,0), oma=c(4,4,0,0) )
      Zlim = range( X_xtp[,,j] )
      for( i in 1:length(Year_Set) ){
        plot_maps( MappingDetails=MapDetails_List[["MappingDetails"]], Mat=X_xtp[,,j][,i,drop=FALSE], zlim=Zlim, add=TRUE, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName="", Year_Set=Year_Set[i], Rescale=FALSE, Rotate=MapDetails_List[["Rotate"]], Format="", Res=NA, zone=MapDetails_List[["Zone"]], Cex=MapDetails_List[["Cex"]], textmargin=textmargin, pch=20)
      }
    dev.off()
  }

  ThorsonUtilities::save_fig( file=paste0(DateDir,"Annual_covariates--cold_pool"), width=4, height=3, res=c(200,600), suffix=c("LO","HI"), FUN=function(){
    par( mfcol=c(1,1), mar=c(3,3,1,1), mgp=c(2,0.5,0), tck=-0.02 )
    plot( x=Year_Set, y=ColdPool_t, xlab="Year", ylab="Cold pool size", type="b", lwd=3, pch=20 )
  })
}


################
# Make and Run TMB model
# (THIS WILL BE SIMILAR FOR EVERY DATA SET)
################

# Download survey data
DF_full = FishData::download_catch_rates(survey="Eastern_Bering_Sea", species_set=species_set, localdir=DataDir )
  DF_full = DF_full[ which(DF_full[,'Year']<=2015), ]
Species_Set = unique(DF_full[,'Sci'])

# Temporary settings
plot_results = FALSE
plot_diagnostics = FALSE

# Loop through species
  pI = rI = mI = 1
for( pI in 1:length(Species_Set)){
#for( pI in 19:20){
#for( pI in 16:20){

  # Species
  SpeciesDir = paste0(DateDir,Species_Set[pI],"/")
  dir.create(SpeciesDir)

  # Data
  DF = DF_full[ which(DF_full[,'Sci']==Species_Set[pI]), ]
  Data_Geostat = data.frame( "spp"=DF[,"Sci"], "Year"=DF[,"Year"], "Catch_KG"=DF[,"Wt"], "AreaSwept_km2"=0.01, "Vessel"=0, "Lat"=DF[,"Lat"], "Lon"=DF[,"Long"] )
  Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )

  # Add Eastings / Northings
  loc_i = Convert_LL_to_UTM_Fn( Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], zone=Extrapolation_List$zone, flip_around_dateline=Extrapolation_List$flip_around_dateline )                                                         #$
  Data_Geostat = cbind( Data_Geostat, "E_km"=loc_i[,'X'], "N_km"=loc_i[,'Y'] )

  # Add knot number to Data_Geostat
  #NN = RANN::nn2( data=Spatial_List$loc_x[,c("Lat","Lon")], query=Data_Geostat[,c("Lat","Lon")], k=1 )
  #Data_Geostat = cbind( Data_Geostat, "knot_i"=NN$nn.idx[,1] )

  # Data availability
  tapply( Data_Geostat$Catch_KG, INDEX=Data_Geostat$Year, FUN=function(vec){mean(vec>0)} )

  for( rI in 1:Npeal ){
  for( mI in 1:length(ModelSet) ){

    # Model settings
    Model = ModelSet[mI]
    RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=",Model,"/")
    dir.create( RunDir, recursive=TRUE )

    # Run model-based forecast
    if( Model %in% c("Temp","Spatial","Both") ){

      # Load past results or re-run
      if( "Save.RData" %in% list.files(RunDir) ){
        load(file=paste0(RunDir,"Save.RData"))
        load(file=paste0(RunDir,"Results.RData"))
        Cov_tt = Results$Cov_tt
      }else{
      
        # Settings -- spatial variation
        if( Model %in% c("Spatial","Both")){
          FieldConfig_input = FieldConfig
        }else{
          FieldConfig_input = c("Omega1"=0, "Epsilon1"=0, "Omega2"=0, "Epsilon2"=0)
        }
        # Settings -- temperature link
        if( Model %in% c("Temp","Both")){
          X_xtp_input = X_xtp
        }else{
          X_xtp_input = NULL
        }
        # Settings -- temperature link
        if( Model %in% "Temp"){
          RhoConfig_input = RhoConfig
          RhoConfig_input[c("Epsilon1","Epsilon2")] = 0
        }else{
          RhoConfig_input = RhoConfig
        }
        # Settings -- years to include
        PredTF_i = ifelse( Data_Geostat[,'Year']<=max(Year_Set)-rI+1, 0, 1 )
        # Settings -- AR to be changed later if necessary
  
        # Make TMB data list
        TmbData = Data_Fn("Version"=Version, "PredTF_i"=PredTF_i, "FieldConfig"=FieldConfig_input, "RhoConfig"=RhoConfig_input, "OverdispersionConfig"=OverdispersionConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "c_i"=rep(0,nrow(Data_Geostat)), "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year']-min(Data_Geostat[,'Year']), "v_i"=rep(0,nrow(Data_Geostat)), "a_xl"=Spatial_List$a_xl, "X_xtp"=X_xtp_input, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )
  
        # Make TMB object
        TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=TmbDir, "Version"=Version, "loc_x"=Spatial_List$loc_x, "RhoConfig"=RhoConfig_input, "TmbDir"=TmbDir, "Use_REML"=Use_REML)
                                                  # "Parameters"=Save$ParHat,
        # Change default settings
        Map = TmbList$Map
        # Covariates
        if( Gamma_Config=="Separate_by_year" ){
          Var_tp = apply( TmbData[["X_xtp"]], MARGIN=2:3, FUN=var )
          Map = TmbList$Map
          Map[["gamma1_ctp"]] = Map[["gamma2_ctp"]] = array( 1:(TmbData$n_c*TmbData$n_t*TmbData$n_p), dim=c(TmbData$n_c,TmbData$n_t,TmbData$n_p) )
          for(rI in 1:TmbData$n_t){
          for(p in 1:ncol(Var_tp)){
            if( any(Var_tp[,p]>0) ){
              Map[["gamma1_ctp"]][,rI,p] = rep( Map[["gamma1_ctp"]][1,rI,p], TmbData$n_c )
              Map[["gamma2_ctp"]][,rI,p] = rep( Map[["gamma2_ctp"]][1,rI,p], TmbData$n_c )
            }
          }}
          Map[["gamma1_ctp"]] = factor(Map[["gamma1_ctp"]])
          Map[["gamma2_ctp"]] = factor(Map[["gamma2_ctp"]])
        }
        if( Gamma_Config=="All_identical" ){
          message("No changes needed")
        }
  
        # Start from previous save
        SaveDir = paste0( SpeciesDir,"Peal=",rI-2,"/Model=",Model,"/")
        Parameters = "generate"
        if( "Save.RData" %in% list.files(SaveDir) ){
          load( paste0(SaveDir,"Save.RData") )
          if( all(sapply(Save$ParHat,length)==sapply(TmbList$Parameters,length)) ){
            Parameters = Save$ParHat
          }
        }

        # Rebuild                                                                                 # "Parameters"=Save$ParHat,
        TmbList = Build_TMB_Fn("TmbData"=TmbData, "Parameters"=Parameters, "RunDir"=TmbDir, "Version"=Version, "Map"=Map, "loc_x"=Spatial_List$loc_x, "RhoConfig"=RhoConfig_input, "TmbDir"=TmbDir, "Use_REML"=Use_REML)

        # Extract objects
        #Obj$env$inner.control = 50
        Obj = TmbList[["Obj"]]
        Obj$fn( Obj$par )
  
        # Run                               # startpar=Obj$env$last.par.best[-Obj$env$random],
        Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=FALSE, savedir=RunDir, bias.correct=FALSE, newtonsteps=0 )
        file.copy( from=paste0(RunDir,"parameter_estimates.txt"), to=paste0(RunDir,"parameter_estimates_V1.txt"), overwrite=TRUE )

        # If necessary, fix some variances at zero
        MaxGrad = 0.001
        if( any(abs(Opt$diagnostics$final_gradient)>=MaxGrad) ){
          Repeat = TRUE
          LoopNum = 0
        }else{
          Repeat = FALSE
        }
        while( Repeat==TRUE ){
          dyn.unload( paste0(TmbDir,"/",TMB::dynlib(Version)) )
          # Increment LoopNum
          LoopNum = LoopNum + 1
          # Extract previous MLE
          ParHat = Obj$env$parList( Opt$par )
          # Change stuff
          if( ParHat$logsigmaB1<log(0.001) ){
            RhoConfig_input[["Beta1"]] = 3  # Change to constant among years
            ParHat$beta1_ct[] = mean(ParHat$beta1_ct)
          }
          if( ParHat$logsigmaB2<log(0.001) ){
            RhoConfig_input[["Beta2"]] = 3  # Change to constant among years
            ParHat$beta2_ct[] = mean(ParHat$beta2_ct)
          }
          if( ParHat$Epsilon_rho1>0.99 ) RhoConfig_input[["Epsilon1"]] = 2   # Change to RW
          if( ParHat$Epsilon_rho2>0.99 ) RhoConfig_input[["Epsilon2"]] = 2   # Change to RW
          if( abs(ParHat$L_omega1_z)<0.0001 ) FieldConfig_input[["Omega1"]] = 0
          if( abs(ParHat$L_omega2_z)<0.0001 ) FieldConfig_input[["Omega2"]] = 0
          if( abs(ParHat$L_epsilon1_z)<0.0001 ){
            FieldConfig_input[["Epsilon1"]] = 0
            RhoConfig_input[["Epsilon1"]] = 0
          }
          if( abs(ParHat$L_epsilon2_z)<0.0001 ){
            FieldConfig_input[["Epsilon2"]] = 0
            RhoConfig_input[["Epsilon2"]] = 0
          }
          # Make TMB data list
          TmbData = Data_Fn("Version"=Version, "PredTF_i"=PredTF_i, "FieldConfig"=FieldConfig_input, "RhoConfig"=RhoConfig_input, "OverdispersionConfig"=OverdispersionConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "c_i"=rep(0,nrow(Data_Geostat)), "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year']-min(Data_Geostat[,'Year']), "v_i"=rep(0,nrow(Data_Geostat)), "a_xl"=Spatial_List$a_xl, "X_xtp"=X_xtp_input, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )
          # Make TMB object
          TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=TmbDir, "Parameters"=ParHat, "Version"=Version, "RhoConfig"=RhoConfig_input, "loc_x"=Spatial_List$loc_x, "TmbDir"=TmbDir, "Use_REML"=Use_REML)
          Obj = TmbList[["Obj"]]  # "TmbDir"=TmbDir, "Parameters"=ParHat,
          # Optimize model                        # lower=TmbList[["Lower"]], upper=TmbList[["Upper"]],
          Opt = try( TMBhelper::Optimize(obj=Obj, newtonsteps=ifelse(LoopNum<=2,0,1), getsd=FALSE, savedir=RunDir, bias.correct=BiasCorr) )
          file.copy( from=paste0(RunDir,"parameter_estimates.txt"), to=paste0(RunDir,"parameter_estimates_V2",letters[LoopNum],".txt"), overwrite=TRUE )
          # Exit conditions for loop
          if( all(abs(Opt$diagnostics$final_gradient)<MaxGrad) | LoopNum>4 ) Repeat = FALSE
        }
  
        # Get hessian
        Hess = optimHess( par=Opt$par, fn=Obj$fn, gr=Obj$gr )
  
        # get standard errors and bias-corrected predictions
        Opt[["SD"]] = sdreport( obj=Obj, par.fixed=Opt$par, hessian.fixed=Hess, bias.correct=BiasCorr, bias.correct.control=list(sd=FALSE,split=NULL,nsplit=100,vars_to_correct="Index_cyl"), getReportCovariance=TRUE  )
        file.copy( from=paste0(RunDir,"parameter_estimates.txt"), to=paste0(RunDir,"parameter_estimates_final.txt"), overwrite=TRUE )
        file.remove( from=paste0(RunDir,"parameter_estimates.txt") )

        # Extract reporting
        Report = Obj$report()
        ParHat = Obj$env$parList(Opt$par)
  
        # Labeling
        if( "cov" %in% names(Opt$SD) ){
          rownames(Opt$SD$cov) = colnames(Opt$SD$cov) = names(Opt$SD$value)
        }
        Cov_tt = Opt$SD$cov[names(Opt$SD$value)=="mean_Z_cym",names(Opt$SD$value)=="mean_Z_cym"]

        # Save stuff
        Save = list("Opt"=Opt, "Report"=Report, "ParHat"=ParHat, "TmbData"=TmbData, "Obj"=Obj, "Map"=TmbList$Map, "RhoConfig_input"=RhoConfig_input, "FieldConfig_input"=FieldConfig_input)
        save(Save, file=paste0(RunDir,"Save.RData"))
        dyn.unload( paste0(TmbDir,"/",TMB::dynlib(Version)) )
      }

      # Index
      Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
      Index = plot_biomass_index( DirName=RunDir, TmbData=Save$TmbData, Sdreport=Save$Opt$SD, Year_Set=Year_Set, Years2Include=Years2Include, use_biascorr=TRUE )

      # COG
      COG = plot_range_index(Report=Save$Report, TmbData=Save$TmbData, Sdreport=Save$Opt$SD, Znames=colnames(Save$TmbData$Z_xm), PlotDir=RunDir, Year_Set=Year_Set)

      if( plot_results==TRUE ){
        # Plot Anisotropy
        plot_anistropy( FileName=paste0(RunDir,"Aniso.png"), Report=Save$Report )

        # Plot overdispersion
        Plot_Overdispersion( filename1=paste0(RunDir,"Overdispersion"), filename2=paste0(RunDir,"Overdispersion--panel"), Data=Save$TmbData, ParHat=Save$ParHat, Report=Save$Report, ControlList1=list("Width"=10, "Height"=10, "Res"=200, "Units"='in'), ControlList2=list("Width"=Save$TmbData$n_c, "Height"=Save$TmbData$n_c, "Res"=200, "Units"='in') )

        # Plot surface
        MapDetails_List = get_map_info( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
        if(Domain=="Stations") MapDetails_List$Cex = 1

        # Plot data
        plot_data(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=RunDir )

        # Check encounter probabilities
        Enc_prob = plot_encounter_diagnostic( Report=Save$Report, Data_Geostat=Data_Geostat, DirName=RunDir)

        # Map density
        plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Save$Report, Sdreport=Save$Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=RunDir, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

        # Inspect covariates
        if( "gamma1_ctp" %in% rownames(summary(Save$Opt$SD)) ){
          GammaHat = lapply( Save$ParHat[c("gamma1_ctp","gamma2_ctp")], FUN=function(a){apply(a,MARGIN=3,FUN=mean)} )
          capture.output( list("GammaHat"=GammaHat, Save$ParHat[c("gamma1_ctp","gamma2_ctp")]), file=paste0(RunDir,"GammaHat.txt"))

          # Plot
          ThorsonUtilities::save_fig( file=paste0(RunDir,"Temperature_response"), width=sum(Temp_Config==1)*4, height=4 )
            par( mfrow=c(1,sum(Temp_Config==1)), mar=c(2,2,1,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(1,1,0,0))
            if( Temp_Config['Local_Bottom']==1 ){
              Xlim = range(BottomTemp_xt)
              Xcenter = mean(BottomTemp_xt)
            }
            # Average temperature _by_ location interaction
            if( Temp_Config['Local_Surface']==1 ){
              Xlim = range(SurfaceTemp_xt)
              Xcenter = mean(SurfaceTemp_xt)
            }
            # Average temperature _by_ location interaction
            if( Temp_Config['ColdPool']==1 ){
              stop("Make ColdPool plot")
            }
            X = seq(Xlim[1], Xlim[2], length=1e4)
            Y1 = (X-Xcenter)*GammaHat$gamma1_ctp[1] + (X-Xcenter)^2*GammaHat$gamma1_ctp[2]
            Y2 = (X-Xcenter)*GammaHat$gamma2_ctp[1] + (X-Xcenter)^2*GammaHat$gamma2_ctp[2]
            matplot( x=X, y=cbind(Y1,Y2), col=c("black","grey"), type="l", lty="solid", lwd=2, xlab="", ylab="")
            legend("bottom", bty="n", fill=c("black","grey"), legend=c("#1","#2"))
            mtext( side=1:2, text=c("Temperature","Log-linear response"), outer=TRUE )
          dev.off()
        } # End covariate plots plots
      }

      # Plot residuals
      if( plot_diagnostics==TRUE ){
        # QQ
        Q = plot_quantile_diagnostic( TmbData=Save$TmbData, Report=Save$Report, FileName_PP=paste0(RunDir,"Posterior_Predictive.jpg"), FileName_Phist=paste0(RunDir,"Posterior_Predictive-Histogram.jpg"), FileName_QQ=paste0(RunDir,"Q-Q_plot.jpg"), FileName_Qhist=paste0(RunDir,"Q-Q_hist.jpg"))
        
        # Pearson residuals
        plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=Save$TmbData, Report=Save$Report, Q=Q, savedir=RunDir, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=RunDir, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)
      }
      
      # Save results
      Results = list("Index"=Index, "COG"=COG, "Cov_tt"=Cov_tt)
      save(Results, file=paste0(RunDir,"Results.RData"))
      
    } # End model-based analyses

    # Sanity check
    if( FALSE ){
      # Reduce data set
      Data_Geostat = Data_Geostat[ which(Data_Geostat$Year>=2011), ]
      X_xtp = X_xtp[,dim(X_xtp)[2]-4:0,]

      # Run nonspatial using TMB
      TmbData = Data_Fn("Version"=Version, "FieldConfig"=c(Omega1=0,Epsilon1=0,Omega2=0,Epsilon2=0), "OverdispersionConfig"=OverdispersionConfig, "ObsModel"=c(1,0), "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "c_i"=rep(0,nrow(Data_Geostat)), "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year']-min(Data_Geostat[,'Year']), "v_i"=rep(0,nrow(Data_Geostat)), "a_xl"=Spatial_List$a_xl, "X_xtp"=X_xtp, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )
      TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=TmbDir, "Version"=Version, "loc_x"=Spatial_List$loc_x, "TmbDir"=TmbDir, "Use_REML"=Use_REML)
      Obj = TmbList$Obj
      Opt = TMBhelper::Optimize( obj=Obj, startpar=rep(0,length(Obj$par)), lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=RunDir, bias.correct=FALSE, newtonsteps=1 )

      # Run two components of delta-model using GLM in R
      DF = data.frame(b_i=TmbData$b_i, t_i=TmbData$t_i+1, Temp=X_xtp[cbind(TmbData$s_i+1,TmbData$t_i+1,1)], Temp2=X_xtp[cbind(TmbData$s_i+1,TmbData$t_i+1,2)])
      LM0 = glm( ifelse(b_i>0,1,0) ~ 0 + factor(t_i) + Temp + Temp2, data=DF, family=binomial(link="logit") )
      LM1 = glm( ifelse(b_i>0,log(b_i),NA) ~ 0 + factor(t_i) + Temp + Temp2, data=DF )
    }

    if( FALSE ){
      # Reduce data set
      Data_Geostat = Data_Geostat[ which(Data_Geostat$Year>=2011), ]

      # Run Spatio-temporal using TMB
      TmbData = Data_Fn("Version"=Version, "PredTF_i"=ifelse(Data_Geostat$Year>=2011,0,1), "FieldConfig"=c(Omega1=1,Epsilon1=1,Omega2=1,Epsilon2=1), "OverdispersionConfig"=OverdispersionConfig, "ObsModel"=c(1,0), "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "c_i"=rep(0,nrow(Data_Geostat)), "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year']-min(Data_Geostat[,'Year']), "v_i"=rep(0,nrow(Data_Geostat)), "a_xl"=Spatial_List$a_xl, "X_xtp"=X_xtp, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )
      TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=TmbDir, "Version"=Version, "loc_x"=Spatial_List$loc_x, "TmbDir"=TmbDir, "Use_REML"=Use_REML)
      Obj = TmbList$Obj
      Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=RunDir, bias.correct=FALSE, newtonsteps=1 )

      # Run two components of delta-model using GLM in R
      library(mgcv)
      DF = data.frame(b_i=TmbData$b_i, t_i=TmbData$t_i+1, Lat_i=Spatial_List$loc_x[TmbData$s_i+1,'Lat'], Long_i=Spatial_List$loc_x[TmbData$s_i+1,'Lon'], Temp=X_xtp[cbind(TmbData$s_i+1,TmbData$t_i+1,1)], Temp2=X_xtp[cbind(TmbData$s_i+1,TmbData$t_i+1,2)])
      GAM0 = gam( ifelse(b_i>0,1,0) ~ 0 + te(Lat_i,Long_i) + te(Lat_i,Long_i,by=factor(t_i)) + factor(t_i) + Temp + Temp2, data=DF, family=binomial(link="logit") )
      GAM1 = gam( ifelse(b_i>0,log(b_i),NA) ~ 0 + te(Lat_i,Long_i) + te(Lat_i,Long_i,by=factor(t_i))  + factor(t_i) + Temp + Temp2, data=DF )
    }

    # Sample-based forecasts
    if( Model %in% "AWA" ){
      if( "Results.RData" %in% list.files(RunDir) ){
        load(file=paste0(RunDir,"Results.RData"))
      }else{
        AWA_tz = array(NA, dim=c(length(Year_Set),2), dimnames=list(Year_Set,c("E_km","N_km")))
        COG_tz = array(NA, dim=c(length(Year_Set)*2,4), dimnames=list(NULL,c("m","Year","COG_hat","SE")))
        Temp_t = rep(NA, length(Year_Set))
        Cov_tt = array(0, dim=2*rep(length(Year_Set),2) )
        for(tI in 1:length(Year_Set)){
          Which_temp = which( BottomTemp[,'Year']==Year_Set[tI] )
          NN_temp = RANN::nn2( data=BottomTemp[Which_temp,c("Lat","Lon")], query=Extrapolation_List$Data_Extrap[,c('Lat','Lon')], k=1 )
          Temp_t[tI] = weighted.mean(x=BottomTemp[Which_temp,'Temp_C'][NN_temp$nn.idx], w=Extrapolation_List$Data_Extrap$Area_in_survey_km2 )
        }
        for( AxisI in 1:2 ){
          Axis = c("E_km", "N_km")[AxisI]
          for(tI in 1:(length(Year_Set)-rI+1)){
            Which = which( Data_Geostat[,'Year']==Year_Set[tI] )
            NN = RANN::nn2( data=Data_Geostat[Which,c("Lat","Lon")], query=Extrapolation_List$Data_Extrap[,c('Lat','Lon')], k=1 )
            AWA_tz[tI,AxisI] = weighted.mean(x=Extrapolation_List$Data_Extrap[,Axis], w=Data_Geostat[Which,'Catch_KG'][NN$nn.idx]*Extrapolation_List$Data_Extrap$Area_in_survey_km2 )
            # NN = RANN::nn2( data=Data_Geostat[Which,c("Lat","Lon")], query=Extrapolation_List$Data_Extrap[,c('Lat','Lon')], k=1 )
            # weighted.mean(x=Extrapolation_List$Data_Extrap[,Axis], w=Save$Report$D_xcy[,1,tI]*Extrapolation_List$Data_Extrap$Area_in_survey_km2 )
          }
          # Using LM function -- doesn't get covariance
          if(FALSE){
            Lm = lm( AWA_tz[,AxisI] ~ 1 + Temp_t )
            Pred = predict( Lm, newdata=data.frame("Temp_t"=Temp_t), se.fit=TRUE )
            Rows = 1:length(Year_Set)+(AxisI-1)*length(Year_Set)
            COG_tz[Rows,] = cbind( AxisI, Year_Set, Pred$fit, Pred$se.fit)
          }
          # Using TMB to get covariance
          Data = list( "y_t"=AWA_tz[,AxisI], "x_t"=Temp_t )
          Params = list( "beta0"=0, "beta"=0, "log_sd"=0 )
          setwd(LocalTmbDir)
          compile("AWA_v1.cpp")
          dyn.load("AWA_v1")
          Obj = MakeADFun( parameters=Params, data=Data )
          Opt = TMBhelper::Optimize( obj=Obj, getReportCovariance=TRUE )
          dyn.unload("AWA_v1")
          Rows = 1:length(Year_Set) + (AxisI-1)*length(Year_Set)
          COG_tz[Rows,] = cbind( AxisI, Year_Set, Opt$SD$value, Opt$SD$sd)
          Cov_tt[Rows,Rows] = Opt$SD$cov
        }
        Results = list("COG"=list("COG_Table"=COG_tz), "Cov_tt"=Cov_tt)
        save( Results, file=paste0(RunDir,"Results.RData") )
      }
    }
  }} # Loop through peals and models

  ####################
  # Load and plot results for a given species
  ####################
  
  Axis = c("E_km", "N_km")[2]
  Model_Names = sapply( ModelSet, FUN=switch, "Temp"="Habitat envelope", "Spatial"="VAST", "Both"="VAST with temp.", "AWA"="Annual regression" )
  Model_Names2 = sapply( ModelSet, FUN=switch, "Temp"="Habitat envelope", "Spatial"="VAST", "Both"="VAST with temperature", "AWA"="Annual regression" )

  # Calculate COG for all fits
  COG_rmtz = array( NA, dim=c(Npeal,length(ModelSet),length(Year_Set),3) )
  dimnames(COG_rmtz) = list(NULL,ModelSet,NULL,c('Year','COG_hat','SE'))

  for( rI in 1:Npeal ){
  for( mI in 1:length(ModelSet) ){
      
    # Model settings
    Model = ModelSet[mI]
    RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=",Model,"/")
  
    if( "Results.RData" %in% list.files(RunDir) ){
      # Load results
      load(file=paste0(RunDir,"Results.RData"))
      COG_Table = Results$COG$COG_Table
      COG_rmtz[rI,mI,,] = COG_Table[which(COG_Table[,'m']==switch(Axis,"E_km"=1,"N_km"=2)),c('Year','COG_hat','SE')]
    }
  }}

  COG_mt0t1z = array( NA, dim=c(length(ModelSet),length(Year_Set),length(Year_Set),2) )
  dimnames(COG_mt0t1z) = list(ModelSet,paste0("Last_fit=",Year_Set),paste0("Year=",Year_Set),c('COG_hat','SE'))
  for( rI in 1:Npeal ){
  for( mI in 1:length(ModelSet) ){
  for( tI in 1:length(Year_Set) ){
     COG_mt0t1z[mI,length(Year_Set)-rI+1,tI,c('COG_hat','SE')] = c( COG_rmtz[rI,mI,tI,'COG_hat'], COG_rmtz[rI,mI,tI,'SE'] )
  }}}

  deltaCOG_mt0t1t2z = array( NA, dim=c(length(ModelSet),length(Year_Set),length(Year_Set),length(Year_Set),2) )
  dimnames(deltaCOG_mt0t1t2z) = list(ModelSet,paste0("Last_fit=",Year_Set),paste0("Ref_year=",Year_Set),paste0("Pred_year=",Year_Set),c('COG_hat','SE'))
  for( mI in 1:length(ModelSet) ){
  for( t0 in 1:length(Year_Set) ){
  for( t1 in 1:length(Year_Set) ){
  for( t2 in t1:length(Year_Set) ){
    #deltaCOG_mt0t1t2z[mI,t0,t1,t2,] = c( COG_mt0t1z[mI,t0,t2,'COG_hat']-COG_mt0t1z[mI,t0,t1,'COG_hat'], COG_mt0t1z[mI,t0,t2,'SE'] )

    # Model settings
    rI = length(Year_Set) - t0 + 1
    Model = ModelSet[mI]
    RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=",Model,"/")

    if( "Results.RData" %in% list.files(RunDir) ){
      # Load results
      load(file=paste0(RunDir,"Results.RData"))
      deltaCOG_mt0t1t2z[mI,t0,t1,t2,'COG_hat'] = COG_mt0t1z[mI,t0,t2,'COG_hat'] - COG_mt0t1z[mI,t0,t1,'COG_hat']
      RowNums = switch(Axis,"E_km"=0,"N_km"=1) * length(Year_Set) + c(t1,t2)
      #deltaCOG_mt0t1t2z[mI,t0,t1,t2,'SE'] = sqrt( Results$Cov_tt[t1,t1] + Results$Cov_tt[t2,t2] - 2*Results$Cov[t1,t2] )
      deltaCOG_mt0t1t2z[mI,t0,t1,t2,'SE'] = sqrt( Results$Cov_tt[RowNums[1],RowNums[1]] + Results$Cov_tt[RowNums[2],RowNums[2]] - 2*Results$Cov[RowNums[1],RowNums[2]] )
    }
  }}}}

  # Calculate "true" for use in comparison
  WhichTrue = c("AWA", "NN", "None")[1]
  if( WhichTrue=="AWA" ){
    AWA_t = tapply( 1:nrow(Data_Geostat), INDEX=Data_Geostat[,'Year'], FUN=function(index,x,w){weighted.mean(x=x[index],w=w[index])}, x=Spatial_List$loc_i[,Axis], w=Data_Geostat[,'Catch_KG'])
    COG_t = AWA_t
  }
  if( WhichTrue=="NN" ){
    NN_t = rep(NA,length(Year_Set))
    for(tI in 1:length(Year_Set)){
      Which = which( Data_Geostat[,'Year']==Year_Set[tI] )
      NN = RANN::nn2( data=Data_Geostat[Which,c("Lat","Lon")], query=Extrapolation_List$Data_Extrap[,c('Lat','Lon')], k=1 )
      NN_t[tI] = weighted.mean(x=Extrapolation_List$Data_Extrap[,Axis], w=Data_Geostat[Which,'Catch_KG'][NN$nn.idx]*Extrapolation_List$Data_Extrap$Area_in_survey_km2 )
    }
    COG_t = NN_t
  }
  if( WhichTrue=="None" ){
    COG_t = rep(NA, length(Year_Set))
  }

  # Calculate quantile for true in 
  Qself_rmt = Qtrue_rmt = array( NA, dim=c(Npeal,length(ModelSet),length(Year_Set)) )
  dimnames(Qself_rmt) = dimnames(Qtrue_rmt) = list(NULL,ModelSet,NULL)
  
  for( rI in 1:Npeal ){
  for( mI in 1:length(ModelSet) ){
    Qtrue_rmt[rI,mI,] = pnorm( COG_t, mean=COG_rmtz[rI,mI,,'COG_hat'], sd=COG_rmtz[rI,mI,,'SE'] )
    if(rI!=1) Qself_rmt[rI,mI,] = pnorm( COG_rmtz[1,mI,,'COG_hat'], mean=COG_rmtz[rI,mI,,'COG_hat'], sd=COG_rmtz[rI,mI,,'SE'] )
  }}    
  
  # Plot retrospective peals 
  for( zI in 1:2 ){
    ThorsonUtilities::save_fig( paste0(SpeciesDir,"Retrospectives-all",c("","--Error")[zI]), width=4*length(ModelSet), height=4 )
      par( mfrow=c(1,length(ModelSet)), mar=c(3,3,2,0), mgp=c(2,0.5,0), tck=-0.02 )
      Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
      Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
      for( mI in 1:length(ModelSet) ){
        Ybounds = COG_rmtz[,,,'COG_hat']%o%c(1,1)+COG_rmtz[,,,'SE']%o%c(-1,1)  - rep(1,Npeal)%o%rep(1,length(ModelSet))%o%COG_t%o%c(1,1)*ifelse(zI==1,0,1)
        Ylim = range(Ybounds,na.rm=TRUE)
        if(zI==1) Ylim = range(c(Ylim,COG_t))
        plot( x=1, y=1, xlim=range(Year_Set), ylim=Ylim, main=Model_Names2[mI], ylab="", xlab="" )
        for( rI in Npeal:1 ){
          y = COG_rmtz[rI,mI,,'COG_hat'] - COG_t*ifelse(zI==1,0,1)
          ybounds = Ybounds[rI,mI,,]
          plot_lines( x=Year_Set, y=y, ybounds=ybounds, bounds_type="shading", col_bounds=Col_bounds(Npeal)[rI], col=Col_lines(Npeal)[rI] )
        }
        if(zI==1) lines( x=Year_Set, y=COG_t, lty="dotted", lwd=3)
        if(zI==2) abline(h=0, lwd=3)
      }  
    dev.off()
  }
  
  # Plot retrospective peals
  if( Npeal==21 ){
    ThorsonUtilities::save_fig( paste0(SpeciesDir,"Fig_2_Retrospectives_"), width=6, height=6, type=c("png","png","pdf"), res=c(200,600,200), suffix=c("LO","HI","PDF"), FUN=function(){
      par( mfrow=c(2,2), mar=c(1,1,2,0), mgp=c(2,0.5,0), tck=-0.02, xaxs="i", oma=c(2.5,2.5,0,1) )
      Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
      Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
      for( mI in 1:length(ModelSet) ){
        Ybounds = COG_rmtz[,,,'COG_hat']%o%c(1,1)+COG_rmtz[,,,'SE']%o%c(-1,1)
        Ylim = range(Ybounds,na.rm=TRUE)
        if(zI==1) Ylim = range(c(Ylim,COG_t))
        plot( x=1, y=1, xlim=range(Year_Set), ylim=Ylim, main=Model_Names2[mI], ylab="", xlab="", xaxt="n", yaxt="n" )
        for( rI in rev(seq(1,21,by=4)) ){
          y = COG_rmtz[rI,mI,,'COG_hat']
          ybounds = Ybounds[rI,mI,,]
          plot_lines( x=Year_Set, y=y, ybounds=ybounds, bounds_type="shading", border=Col_lines(Npeal)[rI], col_bounds=Col_bounds(Npeal)[rI], col=Col_lines(Npeal)[rI], border_lty="dotted" )
        }
        lines( x=Year_Set, y=COG_t, lwd=2, lty="dotted" )
        if(mI==4) legend( "topleft", bty="n", title="Fitted to data through year...", legend=rev(Year_Set)[seq(1,21,by=4)], fill=Col_lines(Npeal)[seq(1,21,by=4)], ncol=3 )
        if(mI %in% c(1,3)) axis(2)
        if(mI %in% c(3,4)) axis(1)
        mtext( side=1:2, text=c("Year","Centroid (km north of equator)"), outer=TRUE, line=1 )
      }
    })
  }

  if( Npeal==21 ){
    for( zI in 1:2 ){
      ThorsonUtilities::save_fig( paste0(SpeciesDir,"Forecast",c("","--Error")[zI]), width=3*3, height=3*4 )
        par( mfcol=c(4,1), mar=c(0,1,1,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(3,3,0,0), xaxs="i" )
        Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
        Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
        tset = dim(COG_rmtz)[3] - 20:1 + 1
        for( mI in 1:length(ModelSet) ){
          Ybounds = COG_rmtz[,,,'COG_hat']%o%c(1,1)+COG_rmtz[,,,'SE']%o%c(-1,1)  - rep(1,Npeal)%o%rep(1,length(ModelSet))%o%COG_rmtz[1,3,,'COG_hat']%o%c(1,1)*ifelse(zI==1,0,1)
          Ylim = range(Ybounds,na.rm=TRUE)
          if(zI==1) Ylim = range(c(Ylim,COG_t))
          plot( x=1, y=1, xlim=c(1995,2015)+c(-0.2,0.2), ylim=Ylim, main="", ylab="", xlab="", xaxt="n", yaxt="n" )
          for( rI in 1:20 ){
            r = rI + 1
            tproj = (dim(COG_rmtz)[3]-rI) : (min(dim(COG_rmtz)[3],dim(COG_rmtz)[3]-rI+2))
            y = COG_rmtz[r,mI,tproj,'COG_hat'] - COG_rmtz[1,3,tproj,'COG_hat']*ifelse(zI==1,0,1)
            ybounds = Ybounds[r,mI,tproj,]
            matplot( x=Year_Set[tproj], y=cbind(y,ybounds), lwd=c(3,1,1), col=Col_lines(20)[rI], type="l", lty="solid", add=TRUE )
          }
          #if(zI==1) lines( x=Year_Set, y=COG_rmtz[1,3,,'COG_hat'], lty="dotted", lwd=5)
          if(zI==1) lines( x=Year_Set, y=COG_t, lty="dotted", lwd=5)
          if(zI==2) abline(h=0, lwd=3)
          axis(2, cex=1.5)
          if( mI==4) axis(1, cex=1.5)
          mtext( side=2, text=ModelSet[mI], line=2 )
        }
      dev.off()
    }
  }

  if( Npeal==21 ){
    Start_Years = (length(Year_Set)-Npeal+1):(length(Year_Set)-1)
    ThorsonUtilities::save_fig( paste0(SpeciesDir,"Forecast_change"), width=3*3, height=3*4 )
      par( mfcol=c(4,1), mar=c(0,1,1,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(3,3,0,0), xaxs="i" )
      Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
      Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
      tset = dim(COG_rmtz)[3] - 20:1 + 1
      for( mI in 1:length(ModelSet) ){
        Ylim = c(-200, 200)
        plot( x=1, y=1, xlim=c(1995,2015)+c(-0.2,0.2), ylim=Ylim, main="", ylab="", xlab="", xaxt="n" )
        for( t0 in Start_Years ){
          y = deltaCOG_mt0t1t2z[mI,t0,t0,t0+0:1,'COG_hat']
          #ybounds = y%o%c(1,1) + deltaCOG_mt0t1t2z[mI,t0,t0,t0+0:1,'SE']%o%c(-1,1)
          # Extract ybounds
          rI = length(Year_Set) - t0 + 1
          Model = ModelSet[mI]
          RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=",Model,"/")
          if( "Results.RData" %in% list.files(RunDir) ){
            # Load results
            load(file=paste0(RunDir,"Results.RData"))
            if( "Cov_tt" %in% names(Results)){
              RowNums = switch(Axis,"E_km"=0,"N_km"=1) * length(Year_Set) + t0 + 0:1
              ybounds = y%o%c(1,1) + c(0,sqrt( Results$Cov_tt[RowNums[1],RowNums[1]] + Results$Cov_tt[RowNums[2],RowNums[2]] - 2*Results$Cov[RowNums[1],RowNums[2]] ))%o%c(-1,1)
            }else{
              stop("Something is wrong")
            }
          }
          # Make plot
          matplot( x=Year_Set[t0-1+1:length(y)], y=cbind(y,ybounds), lwd=c(3,1,1), col=Col_lines(20)[t0-min(Start_Years)+1], type="l", lty="solid", add=TRUE )
        }
        lines( x=Year_Set[-length(Year_Set)], y=diff(COG_t), lty="dotted", lwd=5)
        if( mI==4) axis(1, cex=1.5)
        mtext( side=2, text=ModelSet[mI], line=2 )
      }
    dev.off()
  }

  # Panel figure of forecasted vs. observed change in AWA/COG
  # x-axis: forecasted year
  # Column: forecast step (1, 2, 3, or 4 years forward)
  # Row: forecast model
  if( Npeal==21 ){
    ThorsonUtilities::save_fig( paste0(SpeciesDir,"Forecast_short-term"), width=3*3, height=3*length(ModelSet) )
      par( mfcol=c(4,3), mar=c(0,1,1,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(3,3,2,0), xaxs="i" )
      Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
      Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
      tset = dim(COG_rmtz)[3] - 20:1 + 1
      for( pI in 1:3 ){
      for( mI in 1:length(ModelSet) ){
        Ybounds = COG_rmtz[,,,'COG_hat']%o%c(1,1) + COG_rmtz[,,,'SE']%o%c(-1.96,1.96)
        Ylim = range(Ybounds,na.rm=TRUE)
        plot( x=1, y=1, xlim=c(1990,2015)+c(-1,1), ylim=Ylim, main="", ylab="", xlab="", xaxt="n", yaxt="n" )
        for( rI in (pI+1):dim(COG_rmtz)[1] ){
          plot_lines( x=COG_rmtz[rI,mI,rev(tset)[rI-pI],'Year'], y=COG_rmtz[rI,mI,rev(tset)[rI-pI],'COG_hat'], ybounds=matrix(COG_rmtz[rI,mI,rev(tset)[rI-pI],'COG_hat']+c(-1.96,1.96)*COG_rmtz[rI,mI,rev(tset)[rI-pI],'SE'],ncol=2), col_bounds="black", col="black", lwd=2 )
          points( x=COG_rmtz[rI,mI,rev(tset)[rI-pI],'Year'], y=COG_rmtz[rI,mI,rev(tset)[rI-pI],'COG_hat'] )
        }
        lines( x=COG_rmtz[1,3,tset,'Year'], y=COG_rmtz[1,3,tset,'COG_hat'], lty="solid", lwd=3)
        if( pI==1) axis(2, cex=1.5)
        if( mI==4) axis(1, cex=1.5)
        if( pI==1) mtext( side=2, text=ModelSet[mI], line=2 )
        if( mI==1) mtext( side=3, text=paste0(pI," year forecast"), line=0.5 )
      }}
    dev.off()
  }

  if( Npeal==21 ){
    Width = 1.0
    for( zI in 1:2 ){
      ThorsonUtilities::save_fig( paste0(SpeciesDir,"Fig_",c(3,4)[zI],"_Forecast_change_short-term",c("_","_scatterplot_")[zI]), width=6.5, height=6.5, type=c("png","png","pdf"), res=c(200,600,200), suffix=c("LO","HI","PDF"), FUN=function(){
        par( mfcol=c(4,3), mar=c(0,1,1,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(4,3,2,2), xaxs="i" )
        Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
        Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
        for( fI in 1:3 ){
        for( mI in 1:length(ModelSet) ){
          Ybounds = c(-200,200)
          Ylim = range(Ybounds,na.rm=TRUE)
          if(zI==1) plot( x=1, y=1, xlim=c(1995,2014)+c(-1,1), ylim=Ylim, main="", ylab="", xlab="", xaxt="n", yaxt="n", type="n" )
          if(zI==2) plot( x=1, y=1, xlim=Ylim, ylim=Ylim, main="", ylab="", xlab="", xaxt="n", yaxt="n", type="n" )
          t0set = (dim(COG_rmtz)[3]-20) : (dim(COG_rmtz)[3]-fI)
          for( t0 in t0set ){
            y = deltaCOG_mt0t1t2z[mI,t0,t0,t0+fI,'COG_hat']
            # ybounds = y + c(-1,1)*Width*deltaCOG_mt0t1t2z[mI,t0,t0,t0+fI,'SE']
            # Extract ybounds
            rI = length(Year_Set) - t0 + 1
            Model = ModelSet[mI]
            RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=",Model,"/")
            if( "Results.RData" %in% list.files(RunDir) ){
              # Load results
              load(file=paste0(RunDir,"Results.RData"))
              if( "Cov_tt" %in% names(Results)){
                RowNums = switch(Axis,"E_km"=0,"N_km"=1) * length(Year_Set) + t0 + c(0,fI)
                ybounds = y%o%c(1,1) + sqrt( Results$Cov_tt[RowNums[1],RowNums[1]] + Results$Cov_tt[RowNums[2],RowNums[2]] - 2*Results$Cov[RowNums[1],RowNums[2]] )%o%c(-1,1)
              }else{
                stop("Something is wrong")
              }
            }
            # Make plot
            if(zI==1){
              plot_lines( x=Year_Set[t0], y=y, ybounds=matrix(ybounds,ncol=2), col_bounds="black", col="black", lwd=2 )
              points( x=Year_Set[t0], y=y )
            }
            if(zI==2){
              plot_lines( x=COG_t[t0+fI]-COG_t[t0], y=y, ybounds=matrix(ybounds,ncol=2), col_bounds="black", col="black", lwd=2 )
              points( x=COG_t[t0+fI]-COG_t[t0], y=y )
            }
          }
          if(zI==1) lines( x=Year_Set[t0set], y=COG_t[t0set+fI]-COG_t[t0set], lty="solid", lwd=3)
          if(zI==2){
            abline( a=0, b=1, lty="solid", lwd=3)
            Corr = cor( COG_t[t0set+fI]-COG_t[t0set], deltaCOG_mt0t1t2z[mI,,,,'COG_hat'][cbind(t0set,t0set,t0set+fI)] )
            legend( "topleft", legend=paste0("Corr. = ",formatC(Corr,format="f",digits=2)), bty="n" )
          }
          if( fI==1) axis(2, cex=1.5)
          if( mI==4) axis(1, cex=1.5)
          if( fI==3) mtext( side=4, text=Model_Names[mI], line=0.5 )
          if( mI==1) mtext( side=3, text=paste0(fI," year forecast"), line=0.5 )
          if( zI==1) mtext( side=1:2, text=c("Forecast fitted to data through...","Forecasted/Observed poleward movement (km north of equator)"), outer=TRUE, line=c(2.5,1.5))
          if( zI==2) mtext( side=1:2, text=c("Observed poleward movement (km north of equator)","Forecasted poleward movement (km north of equator)"), outer=TRUE, line=c(2.5,1.5))
        }}
      })
    }
  }

  # Plot quantiles of full-data prediction from retrospective forecasts
  for(zI in 1:2){
    ThorsonUtilities::save_fig( paste0(SpeciesDir,"Quantiles-",c("True","Self")[zI]), width=4*length(ModelSet), height=4 )
      par( mfrow=c(1,length(ModelSet)), mar=c(3,3,2,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i" )
      for( mI in 1:length(ModelSet) ){
        ThorsonUtilities::Hist_Fn( list(Qtrue_rmt,Qself_rmt)[[zI]][,mI,], xlim=c(0,1), main=ModelSet[mI], freq=FALSE, xlab="", ylab="" )
      }
    dev.off()
  }
}




