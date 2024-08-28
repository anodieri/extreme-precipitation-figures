#########################################################################
## Climate signal for EURO-CORDEX MODELS
## Calculate mean return level for 1h, 12h, 24h for periods of
## +1.5 degrees warming and +3 degrees warming
## for PRUDENCE regions
## both mean before relative difference (results1.5_...RData and results3_...RData) 
## and  relative difference before mean (relative_mean_...RData) are saved 
#########################################################################
# Note: for 1 run no +3°C period is given
# this is used from the other runs and then deleted when plotting
#########################################################################
### Load the R-packages below
#####################################################
#EDIT PATH IF NEEDED
library('spam', lib.loc = '../lib')
library(fields, lib.loc="../lib")
library(maps, lib.loc="../lib")
library(ggplot2, lib.loc="../lib")
library(sp, lib.loc="../lib")
library(ncdf4, lib.loc="../lib")
library(readxl, lib.loc="../lib")
library(stringr, lib.loc="../lib")
library("ismev",lib.loc="../lib")
library("evd",lib.loc="../lib")
library("lmomco",lib.loc="../lib")


##############################################################
## Defining functions to calculate return levels 
##############################################################

T=10 #period for the return level, 10 years
p=1/T #probability associated to this period

#function to fit a gev to given data and return parameters using mle estimation
gev_param_mle<-function(data){
  gevfit=gev.fit(data,show = FALSE)#fit gev distribution
  param=gevfit$mle #save parameters
  return(param)
}

#function to fit a gev to given data and return parameters using l-moments
gev_param_lmoments<-function(data){
  param<-lmr2par(data,type="gev")$para #lmoments
  param[3]=-param[3] #different convention: extra minus in shape parameter
  return(param)
}

#function calculates the return level for the given parameters
#if parameters are NULL, fits a gev to specified data
calculate_returnlevel <- function(data=NULL, param=NULL) {
  if(is.null(param)){param=gev_param_mle(data)}#if the parameters are not specified, fit a gev
  #return levels calculated as quantile, lower.tail to start from right side
  returnlevel=qgev(p, loc=param[1], scale = param[2], shape=param[3], lower.tail = FALSE)
  return(returnlevel)
}


##############################################################################
#read in excel file with periods per GCM and run for 1.5 and 3 degrees warming
##############################################################################
#EDIT PATH IF NEEDED
periods=as.data.frame(read_excel("periods.xlsx"))

####################################################################
# loop over prudence regions
####################################################################

regions=c('BI', 'IP','FR','ME','SC','AL','MD','EA') #list all region abbreviations

for(k in 1:length(regions)){
  
  ##############################################################################
  #load mask of the prudence region
  ##############################################################################
  prudence_region=regions[k]
  
  #EDIT PATH IF NEEDED
  load(paste("masks/mask_0.11_",prudence_region,".RData", sep = '')) #load mask of the PRUDENCE region
  
  points.in.region <- mask
  points.in.region <- as.matrix(points.in.region,nrow(points.in.region),2)#convert to matrix
  
  ###############################################################################
  ### Load Annual maxima data from Regional Climate Models (EURO-CORDEX)
  ################################################################################
  dir <- "/mnt/HDS_CORDEXBE_RMIB/hvijver/EURO-CORDEX_pr_EUR11_1hr_AM/"  ### Directory with NetCDF-files
  nc_files <- list.files(dir)                                           ### Show all the files of the given drectory "dir"
  nc_files <- nc_files[str_detect(nc_files, "historical", negate = TRUE)]   ### Select non historical runs            ### Select those files with historical simulations (i.e. filenames containing the substring "historical")
  n.files <- length(nc_files)                                           ### Number of selected files
  
  
  ##############################################################
  ## Prepare dataframes to save results
  ##############################################################
  results1.5<-data.frame(matrix(nrow=n.files, ncol=6))
  colnames(results1.5)<-c('gcm', 'rcm', 'run','1h','12h','24h')
  
  results3<-data.frame(matrix(nrow=n.files, ncol=6))
  colnames(results3)<-c('gcm', 'rcm', 'run','1h','12h','24h')
  
  relative_difference<-data.frame(matrix(nrow=n.files, ncol=6))
  colnames(relative_difference)<-c('gcm', 'rcm', 'run','1h','12h','24h')
  
  ########################################################
  ### LOOP OVER REGIONAL CLIMATE MODELS 
  ########################################################
  
  for (j in 1:n.files) {
    file=nc_files[j] #get filename
    
    print(j) #print progress
    print(file)
    
    nc <- nc_open(paste(dir,nc_files[j],sep=""))      ### Open NetCDF with filename "nc_files[j]"
    
    years <- ncvar_get(nc,varid="years")             ### Extract the years of the simulation
    n.y <- length(years)
    
    am1h <- ncvar_get(nc, varid="am1h")               ### Extract annual maximum 1-h precipitation
    am12h <- ncvar_get(nc, varid="am12h")/12          ### And so on... ncvar_get functie leest data bij variabele varid in van netcdf file
    am24h <- ncvar_get(nc, varid="am24h")/24
    
    if (grepl("HadREM",file)) { 
      lon <- ncvar_get(nc,varid="longitude")          ### Extract longitude of the gridpoints of regional climate model HadREM, blijkbaar anders opgeslagen
      lat <- ncvar_get(nc,varid="latitude")           ### Extract latitude  of the gridpoints of regional climate model HadREM
    } else {
      lon <- ncvar_get(nc,varid="lon")                ### Extract longitude of the gridpoints
      lat <- ncvar_get(nc,varid="lat")                ### Extract latitude  of the gridpoints     
    }
    
  
    lon.vec <- as.vector(lon)                        ### Convert matrix lon (dim: 424×412) to vector lon.vec (length: 174688)
    lat.vec <- as.vector(lat)                        ### Convert matrix lat (dim: 424×412) to vector lat.vec (length: 174688)
    
    if(grepl("REMO",file)){
      lon.vec[lon.vec>180]<-lon.vec[lon.vec>180]-360 #for REMO, west of 0 meridian is encoded as 180-360 degrees while in mask this is -180 to 0 degrees
    }
    
    run <- ncatt_get(nc,varid=0, attname="driving_model_ensemble_member")$value # extract run
    gcm <- ncatt_get(nc,varid=0, attname="driving_model_id")$value #extract gcm
    rcm <- ncatt_get(nc,varid=0, attname="model_id")$value #extract rcm
    
    nc_close(nc)                                     ### Close NetCDF (Please, do not forget, these files are really heavy!)
  
    period_gcmrun <- periods[periods$gcmrun==paste(gcm, run, sep='-'),] #get begin and end years for periods for this gcm and run
    
    period1.5 <- (years>=period_gcmrun$begin1.5 & years<=period_gcmrun$end1.5) #construct 1.5 degree period in true/false of full years
    period3 <- (years>=period_gcmrun$begin3 & years<=period_gcmrun$end3) #construct 3 degree period
    
    ###################################################################################################
    ### Select gridpoints of the Regional Climate Model that are in European countries
    ###################################################################################################
    grid_rcm <- cbind(lon.vec,lat.vec) ### Longitude/Latitude of gridpoints of the Regional Climate Model
    colnames(grid_rcm) <- c("longitude","latitude")
    
    nearest_in_rcm <- apply(rdist(points.in.region, grid_rcm), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask
    
    ############################################################################################
    ### "nearest_in_rcm" is a vector, so in order to select annual maximum data in the UK, 
    ### we convert also the array am1h with dim = 424×412×55 to a matrix with dim = 174688×55
    ############################################################################################
    am1h <- sapply(1:n.y, function(i){ as.vector(am1h[,,i]) })
    am12h <- sapply(1:n.y, function(i){ as.vector(am12h[,,i]) })
    am24h <- sapply(1:n.y, function(i){ as.vector(am24h[,,i]) })
    
    #############################################################################################
    # Select the annual maxima for the two periods
    ############################################################################################
    
    am1hperiod1.5 <- am1h[, period1.5]
    am1hperiod3 <- am1h[, period3]
    
    am12hperiod1.5 <- am12h[, period1.5]
    am12hperiod3 <- am12h[, period3]
    
    am24hperiod1.5 <- am24h[, period1.5]
    am24hperiod3 <- am24h[, period3]
    
    ########################################################################################
    # initialise variables to save sums and sums of rel differences
    ########################################################################################
    mean1h1.5=0
    mean1h3=0
    mean_rel_diff1h=0
    
    mean12h1.5=0
    mean12h3=0
    mean_rel_diff12h=0
    
    
    mean24h1.5=0
    mean24h3=0
    mean_rel_diff24h=0
    
    ########################################################################################
    # Loop over all gridpoints in the mask
    ########################################################################################
    for (i in 1:length(nearest_in_rcm)) {
      
      #1h, 1.5 degrees
      param=gev_param_lmoments(am1hperiod1.5[nearest_in_rcm[i], ])
      RL1h1.5=calculate_returnlevel(param = param)#calculate 1h return level
      mean1h1.5=mean1h1.5+RL1h1.5 #add to the mean
      #1h, 3 degrees
      param=gev_param_lmoments(am1hperiod3[nearest_in_rcm[i], ])
      RL1h3=calculate_returnlevel(param = param)#calculate 1h return level
      mean1h3=mean1h3+RL1h3
      #calculate relative difference in this gridpoint
      reldiff1h=(RL1h3-RL1h1.5)/RL1h1.5
      mean_rel_diff1h=mean_rel_diff1h+reldiff1h
      
      
      #12h, 1.5 degrees
      param=gev_param_lmoments(am12hperiod1.5[nearest_in_rcm[i], ])
      RL12h1.5=calculate_returnlevel(param = param)#calculate 12h return level
      mean12h1.5=mean12h1.5+RL12h1.5
      #12h, 3 degrees
      param=gev_param_lmoments(am12hperiod3[nearest_in_rcm[i], ])
      RL12h3=calculate_returnlevel(param = param)#calculate 12h return level
      mean12h3=mean12h3+RL12h3
      #calculate relative difference in this gridpoint
      reldiff12h=(RL12h3-RL12h1.5)/RL12h1.5
      mean_rel_diff12h=mean_rel_diff12h+reldiff12h
      
      
      #24h, 1.5 degrees
      param=gev_param_lmoments(am24hperiod1.5[nearest_in_rcm[i], ])
      RL24h1.5=calculate_returnlevel(param = param)#calculate 24h return level
      mean24h1.5=mean24h1.5+RL24h1.5
      #24h, 3 degrees
      param=gev_param_lmoments(am24hperiod3[nearest_in_rcm[i], ])
      RL24h3=calculate_returnlevel(param = param)#calculate 24h return level
      mean24h3=mean24h3+RL24h3
      #calculate relative difference in this gridpoint
      reldiff24h=(RL24h3-RL24h1.5)/RL24h1.5
      mean_rel_diff24h=mean_rel_diff24h+reldiff24h
      
    }
    
    #####################################################################
    ## save the mean returnlevels for both periods after normalising
    #####################################################################
    
    l=length(nearest_in_rcm) # Divide  by total number of gridpoints in used
    
    #save results for +1.5°C
    results1.5$gcm[j]<-gcm
    results1.5$rcm[j]<-rcm
    results1.5$run[j]<-run
    results1.5$'1h'[j]<-mean1h1.5/l
    results1.5$'12h'[j]<-mean12h1.5/l
    results1.5$'24h'[j]<-mean24h1.5/l
    
    #save results for +3°C
    results3$gcm[j]<-gcm
    results3$rcm[j]<-rcm
    results3$run[j]<-run
    results3$'1h'[j]<-mean1h3/l
    results3$'12h'[j]<-mean12h3/l
    results3$'24h'[j]<-mean24h3/l
    
    #save results for precomputed relative differences
    relative_difference$gcm[j]<-gcm
    relative_difference$rcm[j]<-rcm
    relative_difference$run[j]<-run
    relative_difference$'1h'[j]<-mean_rel_diff1h/l
    relative_difference$'12h'[j]<-mean_rel_diff12h/l
    relative_difference$'24h'[j]<-mean_rel_diff24h/l
    
  
  }  
  
  ###############################################################
  ## Save the results
  ###############################################################
  
  #EDIT PATH IF NEEDED
  save(results1.5, file=paste("results1.5_",prudence_region,".RData", sep=''))
  save(results3, file=paste("results3_",prudence_region,".RData", sep=''))
  save(relative_difference, file=paste("relative_difference_",prudence_region,".RData", sep=''))

}
