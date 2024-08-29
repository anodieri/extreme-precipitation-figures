#########################################################################################################
## Calculate mean 10 y return levels for durations 1h, 12h, 24h for 35 EURO-CORDEX models
## For Belgium and compare observations
## when a pair of rcm-gcm is used for multiple runs, these data are treated as one long timeseries
## saved as RData file instead of .nc
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
### Loading return levels with coordinates for belgium 
###############################################################################
#EDIT PATH IF NEEDED
dir= "/home/andie/fileserver/pub/Hans/EVT.precip.gridded/"

#the number after D is the duration in minutes
#available durations (not in order): D0010 D0120 D1440 D0020 D0180 D2880 D0030 D0360 D4320 D0060 D0720, all separate files

#EDIT PATHS IF NEEDED
data=load(paste(dir,"StatRR_BEL-RMI-2013_D0060.RData",sep='')) #little construction because these were all named RL when they were saved
kmi1hr=get(data)$T_10yr #select 10 year return level, also in this file: 2,5,10,20,30,50,100,200

data=load(paste(dir,"StatRR_BEL-RMI-2013_D0720.RData",sep=''))
kmi12hr=get(data)$T_10yr

data=load(paste(dir,"StatRR_BEL-RMI-2013_D1440.RData",sep=''))
kmi24hr=get(data)$T_10yr

grid_kmi=cbind(get(data)$lon, get(data)$lat)#construct grid

RL10yr_OBS_all<-matrix(NA,nrow(grid_kmi),3)#create matrix to save return levels from observations
colnames(RL10yr_OBS_all)<-c("d = 1h","d = 12h","d = 24h") 
RL10yr_OBS_all[,1]<-kmi1hr
RL10yr_OBS_all[,2]<-kmi12hr/12 #divide by duration
RL10yr_OBS_all[,3]<-kmi24hr/24


###############################################################################
### Selecting grid points KMI closest to grid points in mask for Belgium
###############################################################################

#EDIT PATH IF NEEDED
load("../masks/BE_mask_Improved.RData")  ### Longitude/Latitude of gridpoints in belgium, produced by "make_raster.R" (grid has resolution 0.11°)
Belgium_mask=BE_mask_0.1
#not the boxed version: plot using ggplot, not panoply so no need for pairs of coordinates that make a box like done for Germany

points.in.belgium <- Belgium_mask
points.in.belgium <- as.matrix(points.in.belgium,nrow(points.in.belgium),2)#convert to matrix

nearest_in_obs <- apply(rdist(points.in.belgium, grid_kmi), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask


######################################################################################################################
### Make  a check if the above lines did work well
#######################################################################################################################
obs.in.belgium <- cbind(get(data)$lon[nearest_in_obs], get(data)$lat[nearest_in_obs])
check <- cbind(obs.in.belgium, points.in.belgium)  
### 1st/2nd column: longitude/latitude gridpoints kmi, 3th/4th column: longitude/latitude gridpoints mask belgium.
#print("check")
#print(check)

############################################################################################################
### Select the observed return levels at these closest gridpoints from the full observations matrix
############################################################################################
RL10yr_OBS<-RL10yr_OBS_all[nearest_in_obs,]


###############################################################################
### Prepare dataframe to save return levels and errrors
###############################################################################

output=data.frame(matrix(nrow=length(nearest_in_obs),ncol= 5))
colnames(output)<- c("lon","lat","obs_1h_KMI","obs_12h_KMI","obs_24h_KMI")

#save lon and lat
output$lon<-points.in.belgium[,1]
output$lat<-points.in.belgium[,2]
#save observations
output$obs_1h_KMI<-RL10yr_OBS[,1]
output$obs_12h_KMI<-RL10yr_OBS[,2]
output$obs_24h_KMI<-RL10yr_OBS[,3]

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


##############################################################
## Defining functions to calculate return levels 
##############################################################

T=10 #period for the return level
p=1/T #probability associated to this period

#function that fits a gev to given data (for one gridpoint) and calculates a return level
#using maximum likelihood estimation
calculate_returnlevel <- function(data) {
  gevfit=gev.fit(data,show = FALSE)#fit gev distribution
  param=gevfit$mle #save parameters
  #return levels calculated as quantile, lower.tail to start from right side
  returnlevel=qgev(p, loc=param[1], scale = param[2], shape=param[3], lower.tail = FALSE)
  return(returnlevel)
}

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
    
    lon.vec <- as.vector(lon)                        ### Convert matrix lon (dim: 424×412) to vector lon.vec (length: 174688)
    lat.vec <- as.vector(lat)                        ### Convert matrix lat (dim: 424×412) to vector lat.vec (length: 174688)
    
    
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
    nearest_in_rcm <- apply(rdist(points.in.belgium, grid_rcm), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask
    
    
    ######################################################################################################################
    ### Make just a check if the above lines did work well
    #######################################################################################################################
    #rcm.in.belgium <- cbind(lon.vec[nearest_in_rcm], lat.vec[nearest_in_rcm])
    #check <- cbind(rcm.in.belgium, points.in.belgium)  ### 1st/2nd column: longitude/latitude gridpoints regional climate model over belgium, 3th/4th column: longitude/latitude gridpoints mask belgium.
    #print("check")
    #print(check)
    
    ############################################################################################
    ### "nearest_in_rcm" is a vector, so in order to select annual maximum data in the belgium, 
    ### we convert also the array am1h with dim = 424×412×55 to a matrix with dim = 174688×55
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
  error1h<-vector()#empty vectors to save difference between model and obs
  error12h<-vector()
  error24h<-vector()
  
  for (i in 1:length(nearest_in_rcm)) {
    #for each of the gridpoints, calculate return level for 1h and 24h and difference with obs
    #the first column of RL10yr_OBS contains 1h
    RL1h=calculate_returnlevel(am1h_full[nearest_in_rcm[i], ])#calculate 1h return level
    returnlevels1h=append(returnlevels1h, RL1h)#add to vector
    error1h=append(error1h, RL10yr_OBS[i,1]-RL1h)#add error=observation-model to vector
    
    #the second column contains 12h
    RL12h=calculate_returnlevel(am12h_full[nearest_in_rcm[i], ])#calculate 1h return level
    returnlevels12h=append(returnlevels12h, RL12h)#add to vector
    error12h=append(error12h, RL10yr_OBS[i,2]-RL12h)#add error=observation-model to vector
    
    #the third column of RL10yr_OBS contains 24h
    RL24h=calculate_returnlevel(am24h_full[nearest_in_rcm[i], ])
    returnlevels24h=append(returnlevels24h, RL24h)
    error24h=append(error24h, RL10yr_OBS[i,3]-RL24h)
  }
  
  #save the errors and returnlevels for 1h and 24h 
  output[,paste("returnlevels1h",gcm, rcm, run, sep="#")]<-returnlevels1h
  output[,paste("returnlevels12h",gcm, rcm, run, sep="#")]<-returnlevels12h
  output[,paste("returnlevels24h",gcm, rcm, run, sep="#")]<-returnlevels24h
  output[,paste("error1h",gcm, rcm, run, sep="#")]<-error1h
  output[,paste("error12h",gcm, rcm, run, sep="#")]<-error12h
  output[,paste("error24h",gcm, rcm, run, sep="#")]<-error24h

}

#EDIT PATH IF NEEDED
save(output,file="output_bel_merge.RData")


##########################################
### END LOOP OVER REGIONAL CLIMATE MODELS 
##########################################




