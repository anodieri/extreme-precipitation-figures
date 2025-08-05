###########################################################################
## Script to save the annual maxima of the historical runs of the ensemble
## in a dataframe for a specific year
###########################################################################

########################################################################
### Load the R-packages
#########################################################################

library('spam')
library(fields)
library(maps)
library(sp)
library(ncdf4)
library(readxl)
library(stringr)

############################################################################
## Division of files into the ones belonging to rcm-gcm pairs with a single run (rest), the "single models"
## and the ones with multiple runs (multiple)
############################################################################

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
               "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r2i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r3i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1.nc",
               "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_SMHI-RCA4_v1.nc",
               "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_SMHI-RCA4_v1.nc",
               "AM_pr_EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_SMHI-RCA4_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_SMHI-RCA4_v1a.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r2i1p1_SMHI-RCA4_v1.nc",
               "AM_pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r3i1p1_SMHI-RCA4_v1.nc"
               )

###############################################################################
### Load Annual maxima data from Regional Climate Models (EURO-CORDEX)
################################################################################
dir <- "C:/Users/dierickx/OneDrive - vki.ac.be/Documents/master/MP/extreme precipitation figures/EURO-SUPREME/mnt/HDS_CORDEXBE_RMIB/hvijver/"     ### Directory with NetCDF-files, PATH TO BE EDITED
nc_files <- list.files(dir)                               ### Show all the files of the given directory "dir"
nc_files <- nc_files[str_detect(nc_files, "historical")]   ### Select  historical runs    
n.files <- length(nc_files)                                           ### Number of selected files

##############################################################################
#load mask for the whole of europe
##############################################################################

#EDIT PATH IF NEEDED
load("../full_domain/full_domain_mask_1.RData")

points.in.EU <- mask
points.in.EU <- as.matrix(points.in.EU,nrow(points.in.EU),2)#omzetten naar matrix

##############################################################
## Prepare dataframe to save annual maxima
##############################################################
output=data.frame(matrix(nrow=nrow(points.in.EU),ncol= 2))
colnames(output)<- c("lon","lat")
output$lon<-points.in.EU[,1] #save lon mask
output$lat<-points.in.EU[,2] #save lat mask

###############################################################
# Save the raw annual maxima in this dataframe
###############################################################


for(file in all_files[1:5]){ #loop over all rcm-gcm pairs
  

  print(paste('new file:',file))
  nc <- nc_open(paste(dir,file,sep=""))      ### Open NetCDF with filename "nc_files[j]"
  
  #print(nc) #to get information on file, vars, attributes, ...
  
  #save general information on the model
  run<-ncatt_get(nc,varid=0, attname="driving_model_ensemble_member")$value #to obtain string that has all runs
  gcm=ncatt_get(nc,varid=0, attname="driving_model_id")$value
  rcm= ncatt_get(nc,varid=0, attname="model_id")$value
  
  am1h <- ncvar_get(nc, varid="am1h")               ### Extract annual maximum 1-h precipitation
  #am24h <- ncvar_get(nc, varid="am24h")
  
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
  
  
  ############################################################################################
  ### "nearest_in_rcm" is a vector, so in order to select annual maximum data in the belgium, 
  ### we convert also the array am1h with dim = 424×412×55 to a matrix with dim = 174688×55
  ### 55= aantal jaar, dus matrix van maxima per gridpt voor ieder jaar, omgezet naar vector per jaar
  ############################################################################################
  am1h <- sapply(1:n.y, function(i){ as.vector(am1h[,,i]) })
  #am24h <- sapply(1:n.y, function(i){ as.vector(am24h[,,i]) })
    
  #######################################################################################
  #### Save the annual maxima for the selected year
  #######################################################################################
  year = -1 #this is the index of the year, for historical it goes from 1951 till 2005
  am1h_y = am1h[, year]
  #am24h_y = am24h[, year]
  
  #save the annual maxima for 1h and 24h for the points nearest to the grid
  output[,paste("am1h",gcm, rcm, run, sep="#")]<-am1h[nearest_in_rcm]
  #output[,paste("am24h",gcm, rcm, run, sep="#")]<-am24h[nearest_in_rcm]
  
  #save the results into the file
  #EDIT PATH IF NEEDED
  save(output,file="am_full_domain_1h_all.RData")
  print(warnings())
  
}  



