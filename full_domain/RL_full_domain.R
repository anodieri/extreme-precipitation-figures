#########################################################################
### 10-year RL for full EURO-CORDEX domain
### for 1h and 24h
########################################################################
### Load the R-packages
#########################################################################

#EDIT PATH IF NEEDED
library('spam', lib.loc = '../lib')#locatie package specifiëren is essentieel
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

############################################################################
## Division of files into the ones belonging to rcm-gcm pairs with a single run (rest), the "single models"
## and the ones with multiple runs (multiple)
############################################################################

#vectors containing the names of the files where the rcm and gcm are the same,
#only the run is different
#hardcoded
cosmo1=c("AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
         "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
         "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc")
cosmo2=c("AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
         "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r2i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
         "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r3i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc")
rca1=c("AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_SMHI-RCA4_v1.nc",
       "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_SMHI-RCA4_v1.nc",
       "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_SMHI-RCA4_v1.nc")
rca2=c("AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_SMHI-RCA4_v1a.nc",
       "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r2i1p1_SMHI-RCA4_v1.nc",
       "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r3i1p1_SMHI-RCA4_v1.nc")
### for these models, there are multiple runs with the same rcm-gcm pair
### these runs have to be pasted together in loop below


#making a list containing also other the filenames, where there is just one run for the gcm-rcm combination
#to make sure all can be done in one loop
all_files=list("AM_pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CNRM-ALADIN63_v2.nc",
               "AM_pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_GERICS-REMO2015_v2.nc",
               "AM_pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_ICTP-RegCM4-6_v2.nc",
               "AM_pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_MOHC-HadREM3-GA7-05_v2.nc",
               "AM_pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v1.nc",
               "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_ICTP-RegCM4-6_v1.nc",
               "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_MOHC-HadREM3-GA7-05_v1.nc",
               "AM_pr_EUR-11_IPSL-IPSL-CM5A-MR_historical_r1i1p1_GERICS-REMO2015_v1.nc",
               "AM_pr_EUR-11_IPSL-IPSL-CM5A-MR_historical_r1i1p1_SMHI-RCA4_v1.nc",
               "AM_pr_EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_CNRM-ALADIN63_v1.nc",
               "AM_pr_EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_MOHC-HadREM3-GA7-05_v1.nc",
               "AM_pr_EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_SMHI-RCA4_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CNRM-ALADIN63_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_ICTP-RegCM4-6_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_MOHC-HadREM3-GA7-05_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r3i1p1_GERICS-REMO2015_v1.nc",
               "AM_pr_EUR-11_NCC-NorESM1-M_historical_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_NCC-NorESM1-M_historical_r1i1p1_CNRM-ALADIN63_v1.nc",
               "AM_pr_EUR-11_NCC-NorESM1-M_historical_r1i1p1_ICTP-RegCM4-6_v1.nc",
               "AM_pr_EUR-11_NCC-NorESM1-M_historical_r1i1p1_MOHC-HadREM3-GA7-05_v1.nc",
               "AM_pr_EUR-11_NCC-NorESM1-M_historical_r1i1p1_SMHI-RCA4_v1.nc", 
               cosmo1,cosmo2,rca1,rca2)



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
  if(is.null(param)){param=gev_param_lmoments(data)}#if the parameters are not specified, fit a gev
  #return levels calculated as quantile, lower.tail to start from right side
  returnlevel=qgev(p, loc=param[1], scale = param[2], shape=param[3], lower.tail = FALSE)
  return(returnlevel)
}



##############################################################################
#load mask for the whole of europe
##############################################################################

#EDIT PATH IF NEEDED
load("full_domain_mask_1.RData")

points.in.EU <- mask
points.in.EU <- as.matrix(points.in.EU,nrow(points.in.EU),2)#omzetten naar matrix

###############################################################################
### Load Annual maxima data from Regional Climate Models (EURO-CORDEX)
################################################################################
dir <- "/mnt/HDS_CORDEXBE_RMIB/hvijver/EURO-CORDEX_pr_EUR11_1hr_AM/"  ### Directory with NetCDF-files
nc_files <- list.files(dir)                                           ### Show all the files of the given drectory "dir"
nc_files <- nc_files[str_detect(nc_files, "historical")]   ### Select  historical runs            ### Select those files with historical simulations (i.e. filenames containing the substring "historical")
n.files <- length(nc_files)                                           ### Number of selected files


##############################################################
## Prepare dataframe to save results
##############################################################
output=data.frame(matrix(nrow=nrow(points.in.EU),ncol= 2))
colnames(output)<- c("lon","lat")
output$lon<-points.in.EU[,1] #save lon mask
output$lat<-points.in.EU[,2] #save lat mask

for(pair in all_files){ #loop over all rcm-gcm pairs
  
  am1h_full<-NULL  #initialise vectors to save the full annual maxima series in
  am24h_full<-NULL
  
  run<-NULL #initialise string to save the names of the runs in
  
  print("new rcm-gcm pair")
  for(file in pair){ #if there are multiple runs of a single pair, these are looped over
    print(paste('new file in same pair:',file))
    nc <- nc_open(paste(dir,file,sep=""))      ### Open NetCDF with filename "nc_files[j]"
    
    #print(nc) #to get information on file, vars, attributes, ...
    
    #save general information on the model
    run<-paste(run,ncatt_get(nc,varid=0, attname="driving_model_ensemble_member")$value, sep = '_') #to obtain string that has all runs
    gcm=ncatt_get(nc,varid=0, attname="driving_model_id")$value
    rcm= ncatt_get(nc,varid=0, attname="model_id")$value
    
    am1h <- ncvar_get(nc, varid="am1h")               ### Extract annual maximum 1-h precipitation
    am24h <- ncvar_get(nc, varid="am24h")/24
    
    if (grepl("HadREM",file)) { #grepl=zoekfunctie
      lon <- ncvar_get(nc,varid="longitude")          ### Extract longitude of the gridpoints of regional climate model HadREM, blijkbaar anders opgeslagen
      lat <- ncvar_get(nc,varid="latitude")           ### Extract latitude  of the gridpoints of regional climate model HadREM
    } else {
      lon <- ncvar_get(nc,varid="lon")                ### Extract longitude of the gridpoints
      lat <- ncvar_get(nc,varid="lat")                ### Extract latitude  of the gridpoints     
    }
    
    #lat en lon zijn nu matrices: raster van lon en lat over europa, omzetten naar 2 vectoren
    lon.vec <- as.vector(lon)                        ### Convert matrix lon (dim: 424×412) to vector lon.vec (length: 174688)
    lat.vec <- as.vector(lat)                        ### Convert matrix lat (dim: 424×412) to vector lat.vec (length: 174688)
    
    if(grepl("REMO",file)){
      lon.vec[lon.vec>180]<-lon.vec[lon.vec>180]-360 #for REMO, west of 0 meridian is encoded as 180 to 360 degrees while in mask this is -180 to 0 degrees
    }
    
    years <- ncvar_get(nc,varid="years")             ### Extract the years of the simulation
    n.y <- length(years)                             ### Number of years of the simulation
    nc_close(nc)                                     ### Close NetCDF (Please, do not forget, these files are really heavy!)
    #print('closed file again')
    
    ###################################################################################################
    ### Select gridpoints of the Regional Climate Model that are in belgium
    ###################################################################################################
    
    grid_rcm <- cbind(lon.vec,lat.vec) ### Longitude/Latitude of gridpoints of the Regional Climate Model
    colnames(grid_rcm) <- c("longitude","latitude")
    
    #rdist berekent distance matrix, apply (..., 1, ...)=> over iedere rij index min teruggeven
    nearest_in_rcm <- apply(rdist(points.in.EU, grid_rcm), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask
    
    
    ######################################################################################################################
    ### Make just a check if the above lines did work well
    #######################################################################################################################
    #rcm.in.belgium <- cbind(lon.vec[nearest_in_rcm], lat.vec[nearest_in_rcm])
    #check <- cbind(rcm.in.belgium, points.in.EU)  ### 1st/2nd column: longitude/latitude gridpoints regional climate model over belgium, 3th/4th column: longitude/latitude gridpoints mask belgium.
    #print("check")
    #print(check)
    
    ############################################################################################
    ### "nearest_in_rcm" is a vector, so in order to select annual maximum data in the belgium, 
    ### we convert also the array am1h with dim = 424×412×55 to a matrix with dim = 174688×55
    ### 55= aantal jaar, dus matrix van maxima per gridpt voor ieder jaar, omgezet naar vector per jaar
    ############################################################################################
    am1h <- sapply(1:n.y, function(i){ as.vector(am1h[,,i]) })
    am24h <- sapply(1:n.y, function(i){ as.vector(am24h[,,i]) })
    
    #####################################################################################
    ## add these to the vector containing the full series
    ##################################################################################
    am1h_full<-cbind(am1h_full,am1h)
    am24h_full<-cbind(am24h_full,am24h)
    #if there was only a single run, this is just the same as am1h,...
    #end loop over set of gcm-rcm runs (so same rcm and gcm) done
  }
  
  #######################################################################################
  #### Calculate returnlevels
  #######################################################################################
  
  returnlevels1h<-vector()#initialize empty vectors to save return levels
  returnlevels24h<-vector()

  for (i in 1:length(nearest_in_rcm)) {
    
    #only try to calculate RL if no 0/NA is contained in AM
    #these appear at the borders of the domain, are set to NA
    
    if(!anyNA(am1h_full[nearest_in_rcm[i], ])&!0%in%am1h_full[nearest_in_rcm[i], ]){
      RL1h=calculate_returnlevel(am1h_full[nearest_in_rcm[i], ])#calculate 1h return level
      returnlevels1h=append(returnlevels1h, RL1h)#add to vector
    }
    else{
      returnlevels1h=append(returnlevels1h, NA) #add NA to vector if AM contain 0/NA
    }
    if(!anyNA(am24h_full[nearest_in_rcm[i], ])&!0%in%am24h_full[nearest_in_rcm[i], ]){
      RL24h=calculate_returnlevel(am24h_full[nearest_in_rcm[i], ])
      returnlevels24h=append(returnlevels24h, RL24h)
    }
    else{
      returnlevels24h=append(returnlevels24h, NA)
    }
  }
  
  #save the errors and returnlevels for 1h and 24h 
  output[,paste("returnlevels1h",gcm, rcm, run, sep="#")]<-returnlevels1h
  output[,paste("returnlevels24h",gcm, rcm, run, sep="#")]<-returnlevels24h
  
  #save the results into the file
  #EDIT PATH IF NEEDED
  save(output,file="output_full_domain_lmoments.RData")
  print(warnings())
}


  
  