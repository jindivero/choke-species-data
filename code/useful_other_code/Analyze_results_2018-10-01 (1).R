

###################################
# Analyze all results
###################################

RootDir = "C:/Users/James.Thorson/Desktop/UW Hideaway/Collaborations/2017 -- niche forecast evaluation/"
#RootDir = "D:/UW Hideaway (SyncBackFree)/Collaborations/2017 -- niche forecast evaluation/"
DataDir = paste0(RootDir,"Data/")

# Choose results
#DateDir =  paste0(RootDir,"2017-10-14_AR_devel/")
DateDir =  paste0(RootDir,"2018-03-15_AR_devel/")

# Load settings
load( file=paste0(DateDir,"Record.RData"))
attach(Record)

# Load data
load( file=paste0(DateDir,"Data_List.RData"))
attach( Data_List )

# Download survey data
DF_full = FishData::download_catch_rates(survey="Eastern_Bering_Sea", species_set=species_set, localdir=DataDir )
  DF_full = DF_full[ which(DF_full[,'Year']<=2015), ]
Species_Set = unique(DF_full[,'Sci'])

# Names
Model_Names = sapply( ModelSet, FUN=switch, "Temp"="Habitat envelope", "Spatial"="VAST", "Both"="VAST with temp.", "AWA"="Annual regression" )
Species_Names = sapply( as.character(Species_Set), FUN=function(char){paste(strsplit(char,"_")[[1]],collapse=" ")} )
Species_Names2 = sapply( as.character(Species_Set), FUN=function(char){paste(strsplit(char,"_")[[1]],collapse="\n")} )

###################################
# Load predictions
###################################

# What to save
Axis = c("E_km", "N_km")[2]
WhichTrue = c("AWA", "NN", "None")[1]

# Calculate COG for all fits
COG_prmtz = array( NA, dim=c(length(Species_Set),Npeal,length(ModelSet)+1,length(Year_Set),3) )
  dimnames(COG_prmtz) = list(Species_Set,NULL,c(ModelSet,"True"),NULL,c('Year','COG_hat','SE'))
COG_pmt0t1z = array( NA, dim=c(length(Species_Set),length(ModelSet)+1,length(Year_Set),length(Year_Set),2) )
  dimnames(COG_pmt0t1z) = list(Species_Set,c(ModelSet,"True"),paste0("Last_fit=",Year_Set),paste0("Year=",Year_Set),c('COG_hat','SE'))
deltaCOG_pmt0t1t2z = array( NA, dim=c(length(Species_Set),length(ModelSet)+1,length(Year_Set),length(Year_Set),length(Year_Set),2) )
  dimnames(deltaCOG_pmt0t1t2z) = list(Species_Set,c(ModelSet,"True"),paste0("Last_fit=",Year_Set),paste0("Ref_year=",Year_Set),paste0("Pred_year=",Year_Set),c('COG_hat','SE'))

# Loop through species
  pI = rI = mI = 1
for( pI in 1:length(Species_Set)){

  # Species
  SpeciesDir = paste0(DateDir,Species_Set[pI],"/")

  # Data
  DF = DF_full[ which(DF_full[,'Sci']==Species_Set[pI]), ]
  Data_Geostat = data.frame( "spp"=DF[,"Sci"], "Year"=DF[,"Year"], "Catch_KG"=DF[,"Wt"], "AreaSwept_km2"=0.01, "Vessel"=0, "Lat"=DF[,"Lat"], "Lon"=DF[,"Long"] )
  Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )

  # Add Eastings / Northings
  loc_i = FishStatsUtils::Convert_LL_to_UTM_Fn( Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], zone=Extrapolation_List$zone, flip_around_dateline=Extrapolation_List$flip_around_dateline )                                                         #$
  Data_Geostat = cbind( Data_Geostat, "E_km"=loc_i[,'X'], "N_km"=loc_i[,'Y'] )

  ####################
  # Load and plot results for a given species
  ####################

  for( rI in 1:Npeal ){
  for( mI in 1:length(ModelSet) ){

    # Model settings
    Model = ModelSet[mI]
    RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=",Model,"/")

    if( "Results.RData" %in% list.files(RunDir) ){
      # Load results
      load(file=paste0(RunDir,"Results.RData"))
      COG_Table = Results$COG$COG_Table
      COG_prmtz[pI,rI,mI,,] = COG_Table[which(COG_Table[,'m']==switch(Axis,"E_km"=1,"N_km"=2)),c('Year','COG_hat','SE')]
    }
  }}

  # Calculate "true" for use in comparison
  if( WhichTrue=="AWA" ){
    AWA_t = tapply( 1:nrow(Data_Geostat), INDEX=Data_Geostat[,'Year'], FUN=function(index,x,w){weighted.mean(x=x[index],w=w[index])}, x=Spatial_List$loc_i[,Axis], w=Data_Geostat[,'Catch_KG'])
    for( rI in 1:Npeal ) COG_prmtz[pI,rI,"True",,'COG_hat'] = AWA_t
  }
  if( WhichTrue=="NN" ){
    NN_t = rep(NA,length(Year_Set))
    for(tI in 1:length(Year_Set)){
      Which = which( Data_Geostat[,'Year']==Year_Set[tI] )
      NN = RANN::nn2( data=Data_Geostat[Which,c("Lat","Lon")], query=Extrapolation_List$Data_Extrap[,c('Lat','Lon')], k=1 )
      NN_t[tI] = weighted.mean(x=Extrapolation_List$Data_Extrap[,Axis], w=Data_Geostat[Which,'Catch_KG'][NN$nn.idx]*Extrapolation_List$Data_Extrap$Area_in_survey_km2 )
    }
    for( rI in 1:Npeal ) COG_prmtz[pI,rI,"True",,'COG_hat'] = NN_t
  }

  for( rI in 1:Npeal ){
  for( mI in 1:length(ModelSet) ){
  for( tI in 1:length(Year_Set) ){
    t0 = length(Year_Set) - rI + 1
    COG_pmt0t1z[pI,mI,t0,tI,c('COG_hat','SE')] = c( COG_prmtz[pI,rI,mI,tI,'COG_hat'], COG_prmtz[pI,rI,mI,tI,'SE'] )
  }}}
  for( rI in 1:Npeal ){
  for( tI in 1:length(Year_Set) ){
    t0 = length(Year_Set) - rI + 1
    COG_pmt0t1z[pI,"True",t0,tI,'COG_hat'] = COG_prmtz[pI,rI,"True",tI,'COG_hat']
  }}

  for( mI in 1:length(ModelSet) ){
  for( t0 in 1:length(Year_Set) ){
  for( t1 in 1:length(Year_Set) ){
  for( t2 in t1:length(Year_Set) ){
    #deltaCOG_pmt0t1t2z[pI,mI,t0,t1,t2,] = c( COG_pmt0t1z[pI,mI,t0,t2,'COG_hat']-COG_pmt0t1z[pI,mI,t0,t1,'COG_hat'], COG_pmt0t1z[pI,mI,t0,t2,'SE'] )

    # Model settings
    rI = length(Year_Set) - t0 + 1
    Model = ModelSet[mI]
    RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=",Model,"/")

    if( "Results.RData" %in% list.files(RunDir) ){
      # Load results
      load(file=paste0(RunDir,"Results.RData"))
      if( "Cov_tt" %in% names(Results)){
        deltaCOG_pmt0t1t2z[pI,mI,t0,t1,t2,'COG_hat'] = COG_pmt0t1z[pI,mI,t0,t2,'COG_hat'] - COG_pmt0t1z[pI,mI,t0,t1,'COG_hat']
        RowNums = switch(Axis,"E_km"=0,"N_km"=1) * length(Year_Set) + c(t1,t2)
        deltaCOG_pmt0t1t2z[pI,mI,t0,t1,t2,'SE'] = sqrt( Results$Cov_tt[RowNums[1],RowNums[1]] + Results$Cov_tt[RowNums[2],RowNums[2]] - 2*Results$Cov[RowNums[1],RowNums[2]] )
      }else{
        deltaCOG_pmt0t1t2z[pI,mI,t0,t1,t2,c('COG_hat','SE')] = c( COG_pmt0t1z[pI,mI,t0,t2,'COG_hat']-COG_pmt0t1z[pI,mI,t0,t1,'COG_hat'], COG_pmt0t1z[pI,mI,t0,t2,'SE'] )
        stop("Something is wrong")
      }
    }
  }}}}
  for( t0 in 1:length(Year_Set) ){
  for( t1 in 1:length(Year_Set) ){
  for( t2 in t1:length(Year_Set) ){
    deltaCOG_pmt0t1t2z[pI,"True",t0,t1,t2,'COG_hat'] = COG_pmt0t1z[pI,"True",t0,t2,'COG_hat'] - COG_pmt0t1z[pI,"True",t0,t1,'COG_hat']
  }}}
}
# deltaCOG_pmt0t1t2z[,3,,33,34,2]
# deltaCOG_pmt0t1t2z[,"True",,33,34,'COG_hat']
# COG_pmt0t1z[,"True",,33,'COG_hat']
# COG_prmtz[,rI,"True",,'COG_hat']

##############################
# Plots
##############################

Width = 1.0
ThorsonUtilities::save_fig( paste0(DateDir,"Fig_S1_Retrospectives--ALL"), type="pdf", onefile=TRUE, width=6.5, height=6.5 )
  for( pI in 1:length(Species_Set)){
    par( mfrow=c(2,2), mar=c(1,1,2,0), mgp=c(2,0.5,0), tck=-0.02, xaxs="i", oma=c(4,3,4,2) )
    Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
    Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
    COG_t = COG_prmtz[pI,1,'True',,'COG_hat']
    for( mI in 1:length(ModelSet) ){
      Ybounds = COG_prmtz[pI,,,,'COG_hat']%o%c(1,1)+COG_prmtz[pI,,,,'SE']%o%c(-1,1)
      Ylim = range(Ybounds,na.rm=TRUE)
      Ylim = range(c(Ylim,COG_t))
      plot( x=1, y=1, xlim=range(Year_Set), ylim=Ylim, main=Model_Names[mI], ylab="", xlab="", xaxt="n", yaxt="n" )
      for( rI in rev(seq(1,21,by=4)) ){
        y = COG_prmtz[pI,rI,mI,,'COG_hat']
        ybounds = Ybounds[rI,mI,,]
        FishStatsUtils::plot_lines( x=Year_Set, y=y, ybounds=ybounds, bounds_type="shading", border=Col_lines(Npeal)[rI], col_bounds=Col_bounds(Npeal)[rI], col=Col_lines(Npeal)[rI], border_lty="dotted" )
      }
      lines( x=Year_Set, y=COG_t, lwd=2, lty="dotted" )
      if(mI==4) legend( "topleft", bty="n", title="Fitted to data through year...", legend=rev(Year_Set)[seq(1,21,by=4)], fill=Col_lines(Npeal)[seq(1,21,by=4)], ncol=3 )
      if(mI %in% c(1,3)) axis(2)
      if(mI %in% c(3,4)) axis(1)
      mtext( side=1:3, text=c("Year","Centroid (km north of equator)",Species_Names[pI]), outer=TRUE, line=c(1,1,2) )
    }
  }
dev.off()

Width = 1.0
ThorsonUtilities::save_fig( paste0(DateDir,"Fig_S2_Forecast_change_short-term--ALL"), type="pdf", onefile=TRUE, width=6.5, height=6.5 )
  for( pI in 1:length(Species_Set)){
    par( mfcol=c(4,3), mar=c(0,1,1,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(4,3,4,2), xaxs="i" )
    Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
    Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
    for( fI in 1:3 ){
    for( mI in 1:length(ModelSet) ){
      Ybounds = c(-200,200)
      Ylim = range(Ybounds,na.rm=TRUE)
      plot( x=1, y=1, xlim=c(1995,2014)+c(-1,1), ylim=Ylim, main="", ylab="", xlab="", xaxt="n", yaxt="n", type="n" )
      t0set = (dim(COG_pmt0t1z)[3]-20) : (dim(COG_pmt0t1z)[3]-fI)
      for( t0 in t0set ){
        y = deltaCOG_pmt0t1t2z[pI,mI,t0,t0,t0+fI,'COG_hat']
        ybounds = y + c(-1,1)*Width*deltaCOG_pmt0t1t2z[pI,mI,t0,t0,t0+fI,'SE']
        FishStatsUtils::plot_lines( x=Year_Set[t0], y=y, ybounds=matrix(ybounds,ncol=2), col_bounds="black", col="black", lwd=2 )
        points( x=Year_Set[t0], y=y )
      }
      lines( x=Year_Set[t0set], y=deltaCOG_pmt0t1t2z[pI,"True",,,,'COG_hat'][cbind(t0set,t0set,t0set+fI)], lty="solid", lwd=3)
      if( fI==1) axis(2, cex=1.5)
      if( mI==4) axis(1, cex=1.5)
      if( fI==3) mtext( side=4, text=Model_Names[mI], line=0.5 )
      if( mI==1) mtext( side=3, text=paste0(fI," year forecast"), line=0.5 )
      mtext( side=c(1,2,3), text=c("Forecast fitted to data through...","Forecasted/Observed poleward movement (km north of equator)",Species_Names[pI]), outer=TRUE, line=c(2.5,1.5,2))
    }}
  }
dev.off()

# Scatterplot of short-term forecast errors
Table2 = Table1 = Table = array( NA, dim=c(length(Species_Set),3*length(ModelSet)), dimnames=list(Species_Names,rep(ModelSet,3)) )
if( Npeal==21 ){
  ThorsonUtilities::save_fig( paste0(DateDir,"Fig_5_Forecast_change_short-term--scatterplot_"), width=6.5, height=6.5, type=c("png","png","pdf"), res=c(200,600,200), suffix=c("LO","HI","PDF"), FUN=function(){
    par( mfcol=c(4,3), mar=c(0,1,1,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(4,3,2,2), xaxs="i" )
    Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
    Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
    for( fI in 1:3 ){
    for( mI in 1:length(ModelSet) ){
      Ybounds = c(-200,200)
      Ylim = range(Ybounds,na.rm=TRUE)
      t0set = (dim(COG_prmtz)[4]-20) : (dim(COG_prmtz)[4]-fI)
      True_t0p = Est_t0p = array(NA, dim=c(length(t0set),length(Species_Set)))
      for( pI in 1:length(Species_Set)){
        True_t0p[,pI] = deltaCOG_pmt0t1t2z[pI,"True",,,,'COG_hat'][cbind(t0set,t0set,t0set+fI)]
        Est_t0p[,pI] = deltaCOG_pmt0t1t2z[pI,mI,,,,'COG_hat'][cbind(t0set,t0set,t0set+fI)]
      }
      plot( x=Est_t0p, y=True_t0p, xlim=Ybounds, ylim=Ybounds, xaxt="n", yaxt="n" )
      abline( a=0, b=1, lty="dotted" )
      Corr = cor( as.vector(True_t0p), as.vector(Est_t0p) )
      VarExpl_Fn = function( true, est ) 1 - var(as.vector(true)-as.vector(est)) / var(as.vector(true))
      VarExpl = VarExpl_Fn( true=True_t0p, est=Est_t0p )
      legend( "topleft", legend=paste0("Corr. = ",formatC(Corr,format="f",digits=2)), bty="n" )
      #legend( "bottomright", legend=paste0("Var. Expl. = ",formatC(VarExpl,format="f",digits=2)), bty="n" )
      if( fI==1) axis(2, cex=1.5)
      if( mI==4) axis(1, cex=1.5)
      if( fI==3) mtext( side=4, text=Model_Names[mI], line=0.5 )
      if( mI==1) mtext( side=3, text=paste0(fI," year forecast"), line=0.5 )
      mtext( side=1:2, text=c("Forecasted poleward movement","Observed poleward movement"), outer=TRUE, line=c(2.5,1.5))
      # Save stuff in Table
      for( pI in 1:length(Species_Set)){
        Table[pI,(fI-1)*length(ModelSet)+mI] = VarExpl_Fn( true=True_t0p[,pI], est=Est_t0p[,pI] )
        Table1[pI,(fI-1)*length(ModelSet)+mI] = var(as.vector(True_t0p[,pI]))
        Table2[pI,(fI-1)*length(ModelSet)+mI] = var(as.vector(True_t0p[,pI]) - as.vector(Est_t0p[,pI]))
      }
      assign("Table", value=Table, envir = .GlobalEnv)
      assign("Table1", value=Table1, envir = .GlobalEnv)
      assign("Table2", value=Table2, envir = .GlobalEnv)
    }}
  })
}
Table3 = rbind( "Median"=apply(Table,MARGIN=2,FUN=median), Table )
Table3 = data.frame(formatC(Table3,digits=2,format="f"))
write.csv( Table3, file=paste0(DateDir,"Table3.csv"), row.names=TRUE )

# Plot change in variance with forecast length
ThorsonUtilities::save_fig( paste0(DateDir,"Fig_S3_Variance_and_forecast_interval_"), width=4, height=4, type=c("png","png","pdf"), res=c(200,600,200), suffix=c("LO","HI","PDF"), FUN=function(){
  Ylim = sqrt( range(Table1,Table2) )
  par( mar=c(3,3,1,1), mgp=c(2,0.5,0), tck=-0.02, xaxs="i" )
  plot( 1, type="n", xlim=c(0.6,3.4), ylim=Ylim, xaxt="n", xlab="Years ahead to forecast", ylab="SD of error in COG forecast (kilometers)", log="y" )
  axis( 1, at=1:3, labels=1:3 )
  for( xI in 1:3 ){
  for( mI in 1:5 ){
    if(mI==1) Y = sqrt(Table1[, (xI-1)*4+1]) # / 1000
    if(mI>=2) Y = sqrt(Table2[, (xI-1)*4+mI-1]) # / 1000
    Col = c("black","blue","red","brown","green")[mI]
    points( x=xI+seq(-0.2,0.2,by=0.1)[mI], y=median(Y), pch=20, col=Col )
    lines( x=rep(xI+seq(-0.2,0.2,by=0.1)[mI],2), y=range(Y), lwd=1, lty="dotted", col=Col )
    lines( x=rep(xI+seq(-0.2,0.2,by=0.1)[mI],2), y=quantile(Y,c(0.25,0.75)), lwd=2, col=Col )
  }}
})

# Predictive probability for short-term forecatss
if( Npeal==21 ){
  ThorsonUtilities::save_fig( paste0(DateDir,"Fig_6_Forecast_change_short-term--probability_"), width=6.5, height=6.5, type=c("png","png","pdf"), res=c(200,600,200), suffix=c("LO","HI","PDF"), FUN=function(){
    par( mfcol=c(4,3), mar=c(0,1,1,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(4,3,2,2), xaxs="i", yaxs="i" )
    Col_bounds = colorRampPalette(colors = c(rgb(1,0,0,0.1),rgb(0,0,1,0.1)), alpha=TRUE)
    Col_lines = colorRampPalette(colors = c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), alpha=FALSE)
    for( fI in 1:3 ){
    for( mI in 1:length(ModelSet) ){
      Ybounds = c(-200,200)
      Ylim = range(Ybounds,na.rm=TRUE)
      t0set = (dim(COG_prmtz)[4]-20) : (dim(COG_prmtz)[4]-fI)
      SE_t0p = True_t0p = Est_t0p = array(NA, dim=c(length(t0set),length(Species_Set)))
      for( pI in 1:length(Species_Set)){
        True_t0p[,pI] = deltaCOG_pmt0t1t2z[pI,"True",,,,'COG_hat'][cbind(t0set,t0set,t0set+fI)]
        Est_t0p[,pI] = deltaCOG_pmt0t1t2z[pI,mI,,,,'COG_hat'][cbind(t0set,t0set,t0set+fI)]
        SE_t0p[,pI] = deltaCOG_pmt0t1t2z[pI,mI,,,,'SE'][cbind(t0set,t0set,t0set+fI)]
      }
      P_t0p = pnorm( True_t0p, mean=Est_t0p, sd=SE_t0p )
      ThorsonUtilities::Hist_Fn( P_t0p, breaks=seq(0,1,by=0.05), xaxt="n", yaxt="n", main="", xlab="", ylab="", freq=FALSE )
      box()
      abline( h=1, lty="dotted" )
      if( fI==1) axis(2, cex=1.5)
      if( mI==4) axis(1, cex=1.5)
      if( fI==3) mtext( side=4, text=Model_Names[mI], line=0.5 )
      if( mI==1) mtext( side=3, text=paste0(fI," year forecast"), line=0.5 )
      mtext( side=1:2, text=c("Quantile for observed poleward shift given forecast distribution","Density"), outer=TRUE, line=c(2.5,1.5))
    }}
  })
}

# Plot temperature responses
if( sum(Temp_Config)==1 ){
  rI = 1
  Max_pz = array(NA, dim=c(length(Species_Set),4), dimnames=list(NULL,c("Temp1","Temp2","VAST1","VAST2")) )

  ThorsonUtilities::save_fig( paste0(DateDir,"Fig_1_Temperature_response_"), width=6.5, height=6.5*5/4, type=c("png","png","pdf"), res=c(200,600,200), suffix=c("LO","HI","PDF"), FUN=function(){
    par( mfcol=c(5,4), mar=c(0,1,3,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(4,3,1,1), xaxs="i" )
    for( pI in 1:length(Species_Set) ){
      # Species
      SpeciesDir = paste0(DateDir,Species_Set[pI],"/")
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
      plot( 1, type="n", xlim=range(X), ylim=exp(c(-2,1)), xaxt="n", yaxt="n", xlab="", ylab="", main=Species_Names2[pI], log="y" )
      # Plot Temp
      RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=Temp/")
      load( paste0(RunDir,"Save.RData") )
      GammaHat = lapply( Save$ParHat[c("gamma1_ctp","gamma2_ctp")], FUN=function(a){apply(a,MARGIN=3,FUN=mean)} )
      Y1 = (X-Xcenter)*GammaHat$gamma1_ctp[1] + (X-Xcenter)^2*GammaHat$gamma1_ctp[2]
      Y2 = (X-Xcenter)*GammaHat$gamma2_ctp[1] + (X-Xcenter)^2*GammaHat$gamma2_ctp[2]
      matplot( x=X, y=exp(cbind(Y1-max(Y1),Y2-max(Y2))), col=c("black","grey"), add=TRUE, type="l", lty="dotted", lwd=2 )
      Max_pz[pI,c("Temp1","Temp2")] = X[c( which.max(Y1), which.max(Y2) )]
      # Plot Both
      RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=Both/")
      load( paste0(RunDir,"Save.RData") )
      GammaHat = lapply( Save$ParHat[c("gamma1_ctp","gamma2_ctp")], FUN=function(a){apply(a,MARGIN=3,FUN=mean)} )
      Y1 = (X-Xcenter)*GammaHat$gamma1_ctp[1] + (X-Xcenter)^2*GammaHat$gamma1_ctp[2]
      Y2 = (X-Xcenter)*GammaHat$gamma2_ctp[1] + (X-Xcenter)^2*GammaHat$gamma2_ctp[2]
      matplot( x=X, y=exp(cbind(Y1-max(Y1),Y2-max(Y2))), col=c("black","grey"), add=TRUE, type="l", lty="solid", lwd=2 )
      Max_pz[pI,c("VAST1","VAST2")] = X[c( which.max(Y1), which.max(Y2) )]
      # Plot boundaries
      if((pI%%5)==0) axis(1)
      if(pI<=5) axis(2)
      #if(pI==(length(Species_Set)-1)) legend("top", bty="n", fill=c("black","grey"), legend=c("#1","#2"), lty="dotted", title="Temp. only", ncol=2 )
      #if(pI==length(Species_Set)) legend("top", bty="n", fill=c("black","grey"), legend=c("#1","#2"), lty="solid", title="VAST with temp.", ncol=2 )
    }
    mtext( side=1:2, text=c("Temperature","Expected density relative to optimal temperature"), outer=TRUE, line=c(2,1.5) )
    assign("Max_pz", value=Max_pz, envir = .GlobalEnv)
  })

  #
  sum( Max_pz[,c("Temp1","Temp2")]==min(X) )
  sum( Max_pz[,c("Temp1","Temp2")]==max(X) )

  #
  sum( Max_pz[,c("VAST1","VAST2")]==min(X) )
  sum( Max_pz[,c("VAST1","VAST2")]==max(X) )
}

# Table of estimated variance parameters
TableB = Table = array( NA, dim=c(length(Species_Set),8), dimnames=list(Species_Names,c('Epsilon_rho1','L_omega1_z','L_epsilon1_z','sigmaB1','Epsilon_rho2','L_omega2_z','L_epsilon2_z','sigmaB2')) )
rI = 1
for( pI in 1:length(Species_Set) ){
  # Species
  SpeciesDir = paste0(DateDir,Species_Set[pI],"/")
  # VAST with temperature
  RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=Both/")
  load( paste0(RunDir,"Save.RData") )
  Table[ pI, c('Epsilon_rho1','L_omega1_z','L_epsilon1_z','Epsilon_rho2','L_omega2_z','L_epsilon2_z') ] = abs(unlist(Save$ParHat[c('Epsilon_rho1','L_omega1_z','L_epsilon1_z','Epsilon_rho2','L_omega2_z','L_epsilon2_z')]))
  Table[ pI, c('sigmaB1','sigmaB2') ] = exp(unlist(Save$ParHat[c('logsigmaB1','logsigmaB2')]))
  # VAST
  RunDir = paste0( SpeciesDir,"Peal=",rI-1,"/Model=Spatial/")
  load( paste0(RunDir,"Save.RData") )
  TableB[ pI, c('Epsilon_rho1','L_omega1_z','L_epsilon1_z','Epsilon_rho2','L_omega2_z','L_epsilon2_z') ] = abs(unlist(Save$ParHat[c('Epsilon_rho1','L_omega1_z','L_epsilon1_z','Epsilon_rho2','L_omega2_z','L_epsilon2_z')]))
  TableB[ pI, c('sigmaB1','sigmaB2') ] = exp(unlist(Save$ParHat[c('logsigmaB1','logsigmaB2')]))
}
Table1 = data.frame(formatC(Table,digits=2,format="f"))
write.csv( Table1, file=paste0(DateDir,"Table1.csv"), row.names=TRUE )
capture.output( summary(Table), file=paste0(DateDir,"Table1_summary.txt") )
Table1B = data.frame(formatC(TableB,digits=2,format="f"))
write.csv( Table1B, file=paste0(DateDir,"Table1B.csv"), row.names=TRUE )

TableS1 = data.frame( TableB[,c('L_epsilon1_z','L_epsilon2_z')], Table[,c('L_epsilon1_z','L_epsilon2_z')] )
TableS1 = cbind( TableS1, (TableS1[,1:2] - TableS1[,3:4]) / TableS1[,1:2] )
TableS1[,1:4] = formatC(unlist(TableS1[,1:4]),digits=2,format="f")
TableS1[,5:6] = paste0( formatC(unlist(TableS1[,5:6])*100,digits=0,format="f"), "%" )
write.csv( TableS1, file=paste0(DateDir,"TableS1.csv"), row.names=TRUE )
