#########################################################################
## Climate signal for EURO-CORDEX MODELS
## Similar to Fischer (2016) "Observed heavy precipitation increase confirms
## theory and early models "
## for 1h and 24h
#########################################################################

#########################################################################
### Load the R-packages below
#####################################################
#EDIT LIBRARY PATHS IF NEEDED
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

p=c(0.10,0.05,0.025,0.01,0.005,0.0025,0.001) #probabilities
T=1/p #periods associated to probabilities


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

#function that calculates the return levels for the given parameters
#if parameters are NULL, fits a gev to specified data
calculate_returnlevel <- function(data=NULL, param=NULL) {
  if(is.null(param)){param=gev_param_mle(data)}#if the parameters are not specified, fit a gev
  #return levels calculated as quantile, lower.tail to start from right side
  returnlevel=qgev(p, loc=param[1], scale = param[2], shape=param[3], lower.tail = FALSE)
  return(returnlevel)
}

#given parameters of a gev distribution, calculate frequency of exceeding given value 
calculate_frequency <- function(param, value) {
  freq=pgev(value, loc=param[1], scale = param[2], shape=param[3], lower.tail = FALSE)
  return(freq)
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
  prudence_region=regions[k] #select prudence region name
  
  #EDIT PATH IF NEEDED
  load(paste("masks/mask_0.11_",prudence_region,".RData", sep = ''))  #load mask of the PRUDENCE region
  
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
  ## Prepare dataframe to save results
  ##############################################################
  resultsfischer<-data.frame(matrix(nrow=0, ncol=14))
  colnames(resultsfischer)<-c('freq','gcm', 'rcm', 'run','1h1.5','1h3',
                              '1hCC','1h2CC','1h4CC','24h1.5','24h3','24hCC','24h2CC','24h4CC')

  
  ########################################################
  ### LOOP OVER REGIONAL CLIMATE MODELS 
  ########################################################
  
  for (j in 1:n.files) {
    
    #prepare dataframe to save results of this file
    fischer_j<-data.frame(matrix(nrow=length(p), ncol=14))
    colnames(fischer_j)<-c('freq','gcm', 'rcm', 'run','1h1.5','1h3','1hCC','1h2CC','1h4CC','24h1.5','24h3','24hCC','24h2CC','24h4CC')
    fischer_j$freq<-1-p
    
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
    # initialise variables to save frequencies
    ########################################################################################
    mean1h1.5=0
    mean1h3=0
    mean1hCC=0
    mean1h2CC=0
    mean1h4CC=0
    
    mean12h1.5=0
    mean12h3=0
    mean12hCC=0
    
    mean24h1.5=0
    mean24h3=0
    mean24hCC=0
    mean24h2CC=0
    mean24h4CC=0
    
    
    ########################################################################################
    # Loop over all gridpoints closest to the mask
    ########################################################################################
    for (i in 1:length(nearest_in_rcm)) {
      
      #1h, 1.5 degrees
      param1.5=gev_param_lmoments(am1hperiod1.5[nearest_in_rcm[i], ]) #get parameters for 1.5
      quantiles1.5=calculate_returnlevel(param = param1.5)#calculate quantiles for +1.5
      freq1.5=calculate_frequency(param=param1.5, value = quantiles1.5) #calculate frequencies of these quantiles
      mean1h1.5=mean1h1.5+freq1.5 #add current value to mean
      
      #1h, 3 degrees
      param3=gev_param_lmoments(am1hperiod3[nearest_in_rcm[i], ])
      freq3=calculate_frequency(param=param3, value= quantiles1.5) #calculate freq quantiles 1.5 for gev from +3
      mean1h3=mean1h3+freq3
      
      #1h, CC reference
      paramCC=gev_param_lmoments(1.105*am1hperiod1.5[nearest_in_rcm[i], ]) #param gev for cc increased (7%increase per degree, 1.5 degree increase so 10.5% increase)
      freqCC=calculate_frequency(param=paramCC, value= quantiles1.5) #calculate freq quantiles 1.5 for gev CC
      mean1hCC=mean1hCC+freqCC
      #1h, 2 x CC reference
      param2CC=gev_param_lmoments(1.21*am1hperiod1.5[nearest_in_rcm[i], ]) #param gev for 2xCC increased (14%increase per degree, 1.5 degree increase so 21% increase)
      freq2CC=calculate_frequency(param=param2CC, value= quantiles1.5) #calculate freq quantiles 1.5 for gev 2 x CC
      mean1h2CC=mean1h2CC+freq2CC
      #1h, 4 x CC reference
      param4CC=gev_param_lmoments(1.42*am1hperiod1.5[nearest_in_rcm[i], ]) #param gev for 4xCC increased (28%increase per degree, 1.5 degree increase so 42% increase)
      freq4CC=calculate_frequency(param=param4CC, value= quantiles1.5) #calculate freq quantiles 1.5 for gev 4 x CC
      mean1h4CC=mean1h4CC+freq4CC
      
      
      #24h, 1.5 degrees
      param1.5=gev_param_lmoments(am24hperiod1.5[nearest_in_rcm[i], ])
      quantiles1.5=calculate_returnlevel(param = param1.5)#calculate quantiles for +1.5
      freq1.5=calculate_frequency(param=param1.5, value = quantiles1.5) #calculate frequencies of these quantiles
      mean24h1.5=mean24h1.5+freq1.5
      
      #24h, 3 degrees
      param3=gev_param_lmoments(am24hperiod3[nearest_in_rcm[i], ])
      freq3=calculate_frequency(param=param3, value= quantiles1.5) #calculate freq quantiles 1.5 for gev from +3
      mean24h3=mean24h3+freq3
      
      #24h, CC reference
      paramCC=gev_param_lmoments(1.105*am24hperiod1.5[nearest_in_rcm[i], ]) #param gev for cc increased (7%increase per degree, 1.5 degree increase so 10.5% increase)
      freqCC=calculate_frequency(param=paramCC, value= quantiles1.5) #calculate freq quantiles 1.5 for gev CC
      mean24hCC=mean24hCC+freqCC
      #24h, 2 x CC reference
      param2CC=gev_param_lmoments(1.21*am24hperiod1.5[nearest_in_rcm[i], ]) #param gev for 2xCC increased (14%increase per degree, 1.5 degree increase so 21% increase)
      freq2CC=calculate_frequency(param=param2CC, value= quantiles1.5) #calculate freq quantiles 1.5 for gev 2 x CC
      mean24h2CC=mean24h2CC+freq2CC
      #24h, 4 x CC reference
      param4CC=gev_param_lmoments(1.42*am24hperiod1.5[nearest_in_rcm[i], ]) #param gev for 4xCC increased (28%increase per degree, 1.5 degree increase so 42% increase)
      freq4CC=calculate_frequency(param=param4CC, value= quantiles1.5) #calculate freq quantiles 1.5 for gev 4 x CC
      mean24h4CC=mean24h4CC+freq4CC
    }
    
    #####################################################################
    ## save the mean returnlevels for both periods after normalising
    #####################################################################
    
    l=length(nearest_in_rcm) # Divide  by total number of gridpoints in used
    
    #save model specs
    fischer_j$gcm<-gcm
    fischer_j$run<-run
    fischer_j$rcm<-rcm
    
    #save calculated frequencies
    fischer_j$'1h1.5'<-mean1h1.5/l
    fischer_j$'1h3'<-mean1h3/l
    fischer_j$'1hCC'<-mean1hCC/l
    fischer_j$'1h2CC'<-mean1h2CC/l
    fischer_j$'1h4CC'<-mean1h4CC/l
    fischer_j$'24h1.5'<-mean24h1.5/l
    fischer_j$'24h3'<-mean24h3/l
    fischer_j$'24hCC'<-mean24hCC/l
    fischer_j$'24h2CC'<-mean24h2CC/l
    fischer_j$'24h4CC'<-mean24h4CC/l
    
    
    resultsfischer<-rbind(resultsfischer,fischer_j) #merge dataframe of this file into large dataframe
  
  }  
  
  
  ###############################################################
  ## Save the results
  ###############################################################
  
  #EDIT PATH IF NEEDED
  save(resultsfischer, file=paste("fischer/results_fischer_",prudence_region,".RData", sep=''))
  
}
