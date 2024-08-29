#########################################################################################################
## Calculate mean 10 y return levels for durations 1h, 12h, 24h for 35 EURO-CORDEX models
## when a pair of rcm-gcm is used for multiple runs, these data are treated as one long timeseries
## For United Kingdom and add observations CEH-GEAR, for which return levels are also computed
##########################################################################################################
### Load the R-packages below
#####################################################
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

###############################################################################
### Loading observations for uk 
###############################################################################

dir= "/mnt/HDS_CORDEXBE_RMIB/hvijver/CEH-GEAR-AM/"
file="AM_pr_1km_hourly_CEH-GEAR.nc"

nc <- nc_open(paste(dir,file,sep=""))      ### Open NetCDF 

lon <- ncvar_get(nc,varid="lon")                ### Extract longitude of the gridpoints
lat <- ncvar_get(nc,varid="lat")                ### Extract latitude  of the gridpoints

years <- ncvar_get(nc,varid="year")             ### Extract the years of the observations
n.y <- length(years)   

GEAR1hr <- ncvar_get(nc, varid="am1h")       ### Extract annual maximum 1-h precipitation
GEAR12hr <- ncvar_get(nc, varid="am12h")/12     ### Extract annual maximum 12-h precipitation
GEAR24hr <- ncvar_get(nc, varid="am24h")/24       ### Extract annual maximum 24-h precipitation

nc_close(nc)

lon.vec <- as.vector(lon)                        ### Convert matrix lon (dim: 424×412) to vector lon.vec (length: 174688)
lat.vec <- as.vector(lat)                        ### Convert matrix lat (dim: 424×412) to vector lat.vec (length: 174688)

grid_obs<- cbind(lon.vec,lat.vec) ### Longitude/Latitude of gridpoints of the observations
colnames(grid_obs) <- c("longitude","latitude")


###############################################################################
### Selecting grid points observations closest to grid points in mask for uk
###############################################################################

#EDIT PATH IF NEEDED
load("../masks/UK_mask.RData")  ### Longitude/Latitude of gridpoints in uk, produced by "make_raster.R" (grid has resolution 0.11°)

points.in.UK <- UK_mask
points.in.UK <- as.matrix(points.in.UK,nrow(points.in.UK),2)#convert to matrix

nearest_in_obs <- apply(rdist(points.in.UK, grid_obs), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask

######################################################################################################################
### Make  a check if the above lines did work well
#######################################################################################################################
obs.in.UK <- cbind(lon.vec[nearest_in_obs], lat.vec[nearest_in_obs])
#check <- cbind(obs.in.UK, points.in.UK)  ### 1st/2nd column: longitude/latitude gridpoints regional climate model over UK, 3th/4th column: longitude/latitude gridpoints mask UK.
#print("check")
#print(check)

##############################################################
## Defining functions to calculate return levels 
##############################################################

T=10 #period for the return level, 10 years
p=1/T #probability associated to this period

#function to fit a gev to given data and return parameters
gev_param<-function(data){
  gevfit=gev.fit(data,show = FALSE)#fit gev distribution
  param=gevfit$mle #save parameters
  return(param)
}

#function calculates the return level for the given parameters
#if parameters are NULL, fits a gev to specified data
calculate_returnlevel <- function(data=NULL, param=NULL) {
  if(is.null(param)){param=gev_param(data)}#if the parameters are not specified, fit a gev
  #return levels calculated as quantile, lower.tail to start from right side
  returnlevel=qgev(p, loc=param[1], scale = param[2], shape=param[3], lower.tail = FALSE)
  return(returnlevel)
}


###############################################################################
### Prepare dataframe to save return levels and errors
###############################################################################

output=data.frame(matrix(nrow=length(nearest_in_obs),ncol= 5))
colnames(output)<- c("lon","lat","obs_1h_CEH-GEAR","obs_12h_CEH-GEAR","obs_24h_CEH-GEAR")

output$lon<-points.in.UK[,1]
output$lat<-points.in.UK[,2]


############################################################################################
### "nearest_in_rcm" is a vector, so in order to select annual maximum data in the UK, 
### we convert also the array am1h with dim = 424×412×55 to a matrix with dim = 174688×55
############################################################################################
GEAR1hr <- sapply(1:n.y, function(i){ as.vector(GEAR1hr[,,i]) })
GEAR12hr <- sapply(1:n.y, function(i){ as.vector(GEAR12hr[,,i]) })
GEAR24hr <- sapply(1:n.y, function(i){ as.vector(GEAR24hr[,,i]) })


############################################################################################################
### Calculate return levels for points closest to observations grid
############################################################################################
print("calculating returnlevels observations")

returnlevels1h<-vector()#initialize empty vectors to save return levels
returnlevels12h<-vector()
returnlevels24h<-vector()

xi1h<-vector()#initialise empty vectors for shape parameters
xi12h<-vector()
xi24h<-vector()


for (i in 1:length(nearest_in_obs)) {
  #for each of the gridpoints, calculate shape parameter and return level for 1h,12h and 24h
  #for some gridpoints, the observations for all years are NA,
  #check this beforehand as this results in error when fitting GEV
  
  if(!anyNA(GEAR1hr[nearest_in_obs[i], ])){
    param=gev_param(GEAR1hr[nearest_in_obs[i], ]) #calculate parameters
    RL1h=calculate_returnlevel(param=param)#calculate 1h return level
    returnlevels1h=append(returnlevels1h, RL1h)#add to vector
    xi1h=append(xi1h,round(param[3],digits = 4)) #save the shape parameter
  }
  else{
    returnlevels1h=append(returnlevels1h, NA) #add NA is observations are NA
    xi1h=append(xi1h,NA) #in this case the shape parameter is also set to NA
  }
  
  
  if(!anyNA(GEAR12hr[nearest_in_obs[i], ])){
    param=gev_param(GEAR12hr[nearest_in_obs[i], ])
    RL12h=calculate_returnlevel(param=param)#calculate 12h return level
    returnlevels12h=append(returnlevels12h, RL12h)#add to vector
    xi12h=append(xi12h,round(param[3],digits = 4))
  }
  else{
    returnlevels12h=append(returnlevels12h, NA)
    xi12h=append(xi12h,NA)
  }
  
  if(!anyNA(GEAR24hr[nearest_in_obs[i], ])){
    param=gev_param(GEAR24hr[nearest_in_obs[i], ])
    RL24h=calculate_returnlevel(param=param)#calculate 24h return level
    returnlevels24h=append(returnlevels24h, RL24h)#add to vector
    xi24h=append(xi24h,round(param[3],digits = 4))
  }
  else{
    returnlevels24h=append(returnlevels24h, NA)
    xi24h=append(xi24h,NA)
  }
}

#save the errors and returnlevels for 1h,12h and 24h in netcdf file
output$"obs_1h_CEH-GEAR"<-returnlevels1h
output$"obs_12h_CEH-GEAR"<-returnlevels12h
output$"obs_24h_CEH-GEAR"<-returnlevels24h
output$obs_1h_xi<-xi1h
output$obs_12h_xi<-xi12h
output$obs_24h_xi<-xi24h


###############################################################################
### Load Annual maxima data from Regional Climate Models (EURO-CORDEX)
################################################################################
dir <- "/mnt/HDS_CORDEXBE_RMIB/hvijver/EURO-CORDEX_pr_EUR11_1hr_AM/"  ### Directory with NetCDF-files
nc_files <- list.files(dir)                                           ### Show all the files of the given drectory "dir"
nc_files <- nc_files[str_detect(nc_files, "historical")]              ### Select those files with historical simulations (i.e. filenames containing the substring "historical")

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



##########################################################################
### LOOP OVER REGIONAL CLIMATE MODELS (HISTORICAL RUNS) 
##########################################################################

for(pair in all_files){ #loop over all rcm-gcm pairs
 
  am1h_full<-NULL  #initialise vectors to save the full annual maxima series in
  am12h_full<-NULL
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
    am12h <- ncvar_get(nc, varid="am12h")/12          ### And so on... ncvar_get functie leest data bij variabele varid in van netcdf file
    am24h <- ncvar_get(nc, varid="am24h")/24
    
    if (grepl("HadREM",file)) { 
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
    ### Select gridpoints of the Regional Climate Model that are in UK
    ###################################################################################################
    
    grid_rcm <- cbind(lon.vec,lat.vec) ### Longitude/Latitude of gridpoints of the Regional Climate Model
    colnames(grid_rcm) <- c("longitude","latitude")
    
    #rdist berekent distance matrix, apply (..., 1, ...)=> over iedere rij index min teruggeven
    nearest_in_rcm <- apply(rdist(points.in.UK, grid_rcm), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask
    
    
    ######################################################################################################################
    ### Make just a check if the above lines did work well
    #######################################################################################################################
    #rcm.in.UK <- cbind(lon.vec[nearest_in_rcm], lat.vec[nearest_in_rcm])
    #check <- cbind(rcm.in.UK, points.in.UK)  ### 1st/2nd column: longitude/latitude gridpoints regional climate model over UK, 3th/4th column: longitude/latitude gridpoints mask UK.
    #print("check")
    #print(check)
    
    ############################################################################################
    ### "nearest_in_rcm" is a vector, so in order to select annual maximum data in the UK, 
    ### we convert also the array am1h with dim = 424×412×55 to a matrix with dim = 174688×55
    ### 55= aantal jaar, dus matrix van maxima per gridpt voor ieder jaar, omgezet naar vector per jaar
    ############################################################################################
    am1h <- sapply(1:n.y, function(i){ as.vector(am1h[,,i]) })
    am12h <- sapply(1:n.y, function(i){ as.vector(am12h[,,i]) })
    am24h <- sapply(1:n.y, function(i){ as.vector(am24h[,,i]) })

    #####################################################################################
    ## add these to the vector containing the full series
    ##################################################################################
    am1h_full<-cbind(am1h_full,am1h)
    am12h_full<-cbind(am12h_full,am12h)
    am24h_full<-cbind(am24h_full,am24h)
    #if there was only a single run, this is just the same as am1h,...
    #end loop over set of gcm-rcm runs (so same rcm and gcm) done
  }
  
  #######################################################################################
  #### Calculate returnlevels
  #######################################################################################
  
  returnlevels1h<-vector()#initialize empty vectors to save return levels
  returnlevels12h<-vector()
  returnlevels24h<-vector()
  
  xi1h<-vector()#initialise empty vectors for shape parameters
  xi12h<-vector()
  xi24h<-vector()
  

  for (i in 1:length(nearest_in_rcm)) {
    #for each of the gridpoints, calculate return level for 1h, 12h and 24h and difference with obs and save shape param
    param=gev_param(am1h_full[nearest_in_rcm[i], ])
    RL1h=calculate_returnlevel(param = param)#calculate 1h return level
    returnlevels1h=append(returnlevels1h, RL1h)#add to vector
    xi1h<-append(xi1h,round(param[3],digits = 4))#add xi parameter
    
    param=gev_param(am12h_full[nearest_in_rcm[i], ])
    RL12h=calculate_returnlevel(param=param)#calculate 1h return level
    returnlevels12h=append(returnlevels12h, RL12h)#add to vector
    xi12h<-append(xi12h,round(param[3],digits = 4))#add xi parameter
    
    param=gev_param(am24h_full[nearest_in_rcm[i], ])
    RL24h=calculate_returnlevel(param=param)
    returnlevels24h=append(returnlevels24h, RL24h)
    xi24h<-append(xi24h,round(param[3],digits = 4))#add xi parameter
    
  }
  
  #save the errors, shape parameters and returnlevels for 1h, 12h and 24h in netcdf file
  output[,paste("returnlevels1h",gcm, rcm, run, sep="#")]<-returnlevels1h
  output[,paste("returnlevels12h",gcm, rcm, run, sep="#")]<-returnlevels12h
  output[,paste("returnlevels24h",gcm, rcm, run, sep="#")]<-returnlevels24h
  output[,paste("error1h",gcm, rcm, run, sep="#")]<-output$"obs_1h_CEH-GEAR"-returnlevels1h
  output[,paste("error12h",gcm, rcm, run, sep="#")]<-output$"obs_12h_CEH-GEAR"-returnlevels12h
  output[,paste("error24h",gcm, rcm, run, sep="#")]<-output$"obs_24h_CEH-GEAR"-returnlevels24h
  output[,paste("xi1h",gcm, rcm, run, sep="#")]<-xi1h
  output[,paste("xi12h",gcm, rcm, run, sep="#")]<-xi12h
  output[,paste("xi24h",gcm, rcm, run, sep="#")]<-xi24h

}

#EDIT PATH IF NEEDED
save(output,file="output_UK_merge.RData")


##########################################
### END LOOP OVER REGIONAL CLIMATE MODELS 
##########################################




