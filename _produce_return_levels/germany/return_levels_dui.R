#########################################################################################################
## Calculate mean 10 y return levels for durations 1h, 12h, 24h for 35 EURO-CORDEX models
## For Germany and compare to KOSTRA data
## when a pair of rcm-gcm is used for multiple runs, these data are treated as one long timeseries
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
### Loading return levels with coordinates for  Germany from KOSTRA
###############################################################################
#EDIT PATH IF NEEDED
kostra_data <- read_excel("/home/andie/fileserver/pub/Hans/KOSTRA2010/KOSTRA-DWD-2010R_geog_Bezug.xlsx",sheet="Raster_geog_Bezug") 

grid_kostra<-cbind(kostra_data$X_CENT_GEO,kostra_data$Y_CENT_GEO)#grid containing coordinates for kostra data

#EDIT PATHS IF NEEDED
kostra1hr<-as.vector(read.csv("/home/andie/fileserver/pub/Hans/KOSTRA2010/StatRR_KOSTRA-DWD-2010R_D0060.csv",sep=";"))#loading return levels for selected durations
kostra12hr<-as.vector(read.csv("/home/andie/fileserver/pub/Hans/KOSTRA2010/StatRR_KOSTRA-DWD-2010R_D0720.csv",sep=";"))
kostra24hr<-as.vector(read.csv("/home/andie/fileserver/pub/Hans/KOSTRA2010/StatRR_KOSTRA-DWD-2010R_D1440.csv",sep=";"))
#more durations: see RCM_ERR_GERMANY.R

RL10yr_OBS_all<-matrix(NA,nrow(grid_kostra),3)#create matrix to save return levels from observations
colnames(RL10yr_OBS_all)<-c("d = 1h","d = 12h","d = 24h") 
RL10yr_OBS_all[,1]<-kostra1hr$HN_010A
RL10yr_OBS_all[,2]<-kostra12hr$HN_010A/12 #divide by duration
RL10yr_OBS_all[,3]<-kostra24hr$HN_010A/24
RL10yr_OBS_all[RL10yr_OBS_all<0]<-NA #remove negative return levels


###############################################################################
### Selecting gridpoints KOSTRA closest to gridpoints in mask for Germany
###############################################################################

#EDIT PATH IF NEEDED
load("../masks/Germany_box_mask.RData")  ### Longitude/Latitude of gridpoints in germany, produced by "make_raster.R" (grid has resolution 0.11°)
#use the boxed version of the mask to be able to plot using panoply
#panoply takes a set of lat coord and lon coord and plots all possible combinations
#so this results in a box

points.in.germany <- Germany_box_mask
points.in.germany <- as.matrix(points.in.germany,nrow(points.in.germany),2) #convert to matrix

nearest_in_obs <- apply(rdist(points.in.germany, grid_kostra), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask


######################################################################################################################
### Make  a check if the above lines did work well
#######################################################################################################################
obs.in.germany <- cbind(kostra_data$X_CENT_GEO[nearest_in_obs], kostra_data$Y_CENT_GEO[nearest_in_obs])
check <- cbind(obs.in.germany, points.in.germany)  ### 1st/2nd column: longitude/latitude gridpoints Kostra, 3th/4th column: longitude/latitude gridpoints mask Germany.
#print("check")
#print(check)

############################################################################################################
### Select the observed return levels at these closest gridpoints
############################################################################################
RL10yr_OBS<-RL10yr_OBS_all[nearest_in_obs,]


###############################################################################
### Prepare netcdf file to save errors in
###############################################################################

#save coordinates of the mask file, to use as general coordinates
#for each model we selected points closest to the points of the mask
lonvec=unique(points.in.germany[,1]) #all unique longitude coordinates, in masks coordinates are saved as pairs
latvec=unique(points.in.germany[,2]) #all unique latitude coordinates

#define dimensions using these coordinates
latdim=ncdim_def("lat", units = "degrees", longname = "latitude", vals=latvec)
londim=ncdim_def("lon", units = "degrees", longname = "longitude", vals=lonvec)

#create a variable to save KOSTRA observations in, to plot these nicely
obs1=ncvar_def("obs_1h_KOSTRA", dim = list(londim, latdim), units = "mm/h", missval = NA)#uitvoerig getest dat dit juiste volgorde lat lon is
obs12=ncvar_def("obs_12h_KOSTRA", dim = list(londim, latdim), units = "mm/h", missval = NA)
obs24=ncvar_def("obs_24h_KOSTRA", dim = list(londim, latdim), units = "mm/h", missval = NA)
#error1h_var=ncvar_def("error1h",dim = list(londim, latdim), units = "mm", missval = NA)

#create an nc-file to save the errors
#EDIT PATH IF NEEDED
errors=nc_create( "output_Dui_merge.nc", vars=list(obs1,obs12,obs24), force_v4=FALSE )

#insert the observations
vals= matrix(RL10yr_OBS[,1], nrow=length(lonvec), ncol=length(latvec))
ncvar_put(errors, "obs_1h_KOSTRA", vals=vals, start=NA, count=NA)
vals= matrix(RL10yr_OBS[,2], nrow=length(lonvec), ncol=length(latvec))
ncvar_put(errors, "obs_12h_KOSTRA", vals=vals, start=NA, count=NA)
vals= matrix(RL10yr_OBS[,3], nrow=length(lonvec), ncol=length(latvec))
ncvar_put(errors, "obs_24h_KOSTRA", vals=vals, start=NA, count=NA)


###############################################################################
### Load Annual maxima data from Regional Climate Models (EURO-CORDEX)
################################################################################
dir <- "/mnt/HDS_CORDEXBE_RMIB/hvijver/EURO-CORDEX_pr_EUR11_1hr_AM/"  ### Directory with NetCDF-files
nc_files <- list.files(dir)                                           ### Show all the files of the given drectory "dir"
nc_files <- nc_files[str_detect(nc_files, "historical")]              ### Select those files with historical simulations (i.e. filenames containing the substring "historical")
n.files <- length(nc_files)                                           ### Number of selected files

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
## Defining functions to calculate return levels and save them
##############################################################

T=10 #period for the return level
p=1/T #probability associated to this period

#function that fits a gev to given data (for one gridpoint) and calculates a return level
calculate_returnlevel <- function(data) {
  gevfit=gev.fit(data,show = FALSE)#fit gev distribution
  param=gevfit$mle #save parameters
  #return levels calculated as quantile, lower.tail to start from right side
  returnlevel=qgev(p, loc=param[1], scale = param[2], shape=param[3], lower.tail = FALSE)
  return(returnlevel)
}

#function to save values from a vector values_vec to a variable in the output netcdf file
save_var <- function(name, values_vec){
  #create a variable for the netcdf file of the dimensions of the grid
  #the name of the variable is made by pasting name, the duration, the gcm, the rcm and the run together
  var=ncvar_def(paste(name,gcm, rcm, run, sep="#"),dim = list(londim, latdim), units = "mm/h", missval = NA)
  vals= matrix(values_vec, nrow=length(lonvec), ncol=length(latvec))#convert vector to matrix of size of spatial grid
  errors=ncvar_add(errors, var)#add variable to the netcdf file
  ncvar_put(errors,var, vals=vals, start=NA, count=NA) #put values in variable
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
    ### Select gridpoints of the Regional Climate Model that are in Germany
    ###################################################################################################
    
    grid_rcm <- cbind(lon.vec,lat.vec) ### Longitude/Latitude of gridpoints of the Regional Climate Model
    colnames(grid_rcm) <- c("longitude","latitude")
    
    #rdist berekent distance matrix, apply (..., 1, ...)=> over iedere rij index min teruggeven
    nearest_in_rcm <- apply(rdist(points.in.germany, grid_rcm), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask
    
    
    ######################################################################################################################
    ### Make just a check if the above lines did work well
    #######################################################################################################################
    #rcm.in.germany <- cbind(lon.vec[nearest_in_rcm], lat.vec[nearest_in_rcm])
    #check <- cbind(rcm.in.germany, points.in.germany)  ### 1st/2nd column: longitude/latitude gridpoints regional climate model over germany, 3th/4th column: longitude/latitude gridpoints mask germany.
    #print("check")
    #print(check)
    
    ############################################################################################
    ### "nearest_in_rcm" is a vector, so in order to select annual maximum data in the germany, 
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
    if(!is.na(RL10yr_OBS[i,1])){#where the observation is NA, no return level has to be calculated 
      #(these are outside the borders of Germany), this saves calculation time: 2000 less points
      RL1h=calculate_returnlevel(am1h_full[nearest_in_rcm[i], ])#calculate 1h return level
      returnlevels1h=append(returnlevels1h, RL1h)#add to vector
      error1h=append(error1h, RL10yr_OBS[i,1]-RL1h)#add error=observation-model to vector
    }else{
      #if the observation is NA, then just add NA for the return level and error at this gridpoint as well
      returnlevels1h=append(returnlevels1h, NA)#add NA
      error1h=append(error1h,NA)#add NA
    }
    #the second column of RL10yr_OBS contains 12h
    if(!is.na(RL10yr_OBS[i,3])){
      RL12h=calculate_returnlevel(am12h_full[nearest_in_rcm[i], ])
      returnlevels12h=append(returnlevels12h, RL12h)
      error12h=append(error12h, RL10yr_OBS[i,2]-RL12h)
    }else{
      returnlevels12h=append(returnlevels12h, NA)
      error12h=append(error12h,NA)
    }
    #the third column of RL10yr_OBS contains 24h
    if(!is.na(RL10yr_OBS[i,3])){
      RL24h=calculate_returnlevel(am24h_full[nearest_in_rcm[i], ])
      returnlevels24h=append(returnlevels24h, RL24h)
      error24h=append(error24h, RL10yr_OBS[i,3]-RL24h)
    }else{
      returnlevels24h=append(returnlevels24h, NA)
      error24h=append(error24h,NA)
    }
  }
  
  #save the errors and returnlevels for 1h and 24h in netcdf file
  save_var("error1h",error1h)
  save_var("error12h",error12h)
  save_var("error24h",error24h)
  save_var("returnlevels1h", returnlevels1h )
  save_var("returnlevels12h", returnlevels12h )
  save_var("returnlevels24h", returnlevels24h )
  
}

nc_close(errors)#make sure to close netcdf file!
print("file closed again")

##########################################
### END LOOP OVER REGIONAL CLIMATE MODELS 
##########################################




