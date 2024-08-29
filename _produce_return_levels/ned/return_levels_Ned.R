#########################################################################################################
## Calculate mean 10 y return levels for durations 1h, 12h, 24h for 35 EURO-CORDEX models
## Mean taken over all gridpoints of a country
## For countries Netherlands, Denmark and Finland
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


#making a list containing also the other filenames, where there is just one run for the gcm-rcm combination
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

n.files <- length(all_files)                  ### Number of selected files


###############################################
### Create dataframe to save output
##############################################
output=data.frame(matrix(nrow=n.files,ncol= 8))
colnames(output)<- c("model","rcm","gcm","run","returnlevel1h", "returnlevel12h",  "returnlevel24h", "npoints")

########################################################
### LOOP OVER REGIONAL CLIMATE MODELS (HISTORICAL RUNS)
########################################################

j=0#initialise counter j

for(pair in all_files){ #loop over all rcm-gcm pairs
  j=j+1#increase counter
  print(paste("GCM-RCM pair number",j, sep=" "))
  
  output$model[j]=j#add the number of the model to the output
  
  am1h_full<-NULL  #initialise vectors to save the full annual maxima series in: for pairs with multiple runs this is needed
  am2h_full<-NULL
  am6h_full<-NULL
  am12h_full<-NULL
  am24h_full<-NULL
  am48h_full<-NULL
  am72h_full<-NULL
  
  run<-NULL #initialise string to save the names of the runs in
  
  for(file in pair){ #if there are multiple runs of a single pair, these are looped over
    
   print(paste('new file in same pair:',file))
    
   nc <- nc_open(paste(dir,file,sep=""))  ### Open NetCDF with filename "file"

  
   #print(nc) #to get information on file, vars, attributes, ...
   
   #save general information on the model
   run=paste(run,ncatt_get(nc,varid=0, attname="driving_model_ensemble_member")$value, sep = '_') #all runs are concatenated
   gcm=ncatt_get(nc,varid=0, attname="driving_model_id")$value
   rcm= ncatt_get(nc,varid=0, attname="model_id")$value
   
   #put this info in the output of the model
   output$run[j]=run
   output$gcm[j]=gcm
   output$rcm[j]=rcm
   
   am1h <- ncvar_get(nc, varid="am1h")               ### Extract annual maximum 1-h precipitation
   am2h <- ncvar_get(nc, varid="am2h")/2             ### And so on... ncvar_get functie leest data bij variabele varid in van netcdf file
   am6h <- ncvar_get(nc, varid="am6h")/6
   am12h <- ncvar_get(nc, varid="am12h")/12
   am24h <- ncvar_get(nc, varid="am24h")/24
   am48h <- ncvar_get(nc, varid="am48h")/48
   am72h <- ncvar_get(nc, varid="am72h")/72
  
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
   
 
   ###################################################################################################
   ### Select gridpoints of the Regional Climate Model that lie in a given country (for example: UK)
   ###################################################################################################
   #ned, finland, den
   #EDIT PATH IF NEEDED
   #edit name country if needed
   load("masks/Finland_mask.RData")  ### Longitude/Latitude of gridpoints in UK, produced by "make_raster.R" (grid has resolution 0.11°)
   #after loading this has the name UK_mask ( named like this in make_raster.R)
   points.in.uk <- UK_mask
   points.in.uk <- as.matrix(points.in.uk,nrow(points.in.uk),2)#convert to matrix
   #plot(points.in.uk)
     
   grid_rcm <- cbind(lon.vec,lat.vec) ### Longitude/Latitude of gridpoints of the Regional Climate Model
   colnames(grid_rcm) <- c("longitude","latitude")
   
   nearest_in_set2 <- apply(rdist(points.in.uk, grid_rcm), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask
   
   ######################################################################################################################
   ### Make just a check if the above lines did work well
   #######################################################################################################################
   rcm.in.uk <- cbind(lon.vec[nearest_in_set2], lat.vec[nearest_in_set2])
   check <- cbind(rcm.in.uk, points.in.uk)  ### 1st/2nd column: longitude/latitude gridpoints regional climate model over UK, 3th/4th column: longitude/latitude gridpoints mask UK.
   #print("check")
   #print(check)
   ############################################################################################
   ### "nearest_in_set2" is a vector, so in order to select annual maximum data in the UK, 
   ### we convert also the array am1h with dim = 424×412×55 to a matrix with dim = 174688×55
   ############################################################################################
   am1h <- sapply(1:n.y, function(i){ as.vector(am1h[,,i]) })
   am2h <- sapply(1:n.y, function(i){ as.vector(am2h[,,i]) })
   am6h <- sapply(1:n.y, function(i){ as.vector(am6h[,,i]) })
   am12h <- sapply(1:n.y, function(i){ as.vector(am12h[,,i]) })
   am24h <- sapply(1:n.y, function(i){ as.vector(am24h[,,i]) })
   am48h <- sapply(1:n.y, function(i){ as.vector(am48h[,,i]) })
   am72h <- sapply(1:n.y, function(i){ as.vector(am72h[,,i]) })
 
  
   #####################################################################################
   ## add these to the vector containing the full series
   ##################################################################################
   
   am1h_full<-cbind(am1h_full,am1h)
   am2h_full<-cbind(am2h_full,am2h)
   am6h_full<-cbind(am6h_full,am6h)
   am12h_full<-cbind(am12h_full,am12h)
   am24h_full<-cbind(am24h_full,am24h)
   am48h_full<-cbind(am48h_full,am48h)
   am72h_full<-cbind(am72h_full,am72h)
   
   #if there was only a single run, this is just the same as am1h,...
   #end loop over set of gcm-rcm runs (so same rcm and gcm) done
  }
 
 #######################################################################################
 #### Calculate returnlevels
 #######################################################################################
 T=10 #period for the return level
 p=1/T #chance
 returnlevels1h<-vector()#initialize empty vector to save return levels
 returnlevels12h<-vector()
 returnlevels24h<-vector()
 
 #function that fits a gev to each gridpoint and calculates a return level
 calculate_returnlevel <- function(data) {
       gevfit=gev.fit(data,show = FALSE)#fit gev distribution
       param=gevfit$mle #save parameters
       #return levels berekenen via kwantielen, lower.tail om juiste kant te bekijken
       returnlevel=qgev(p, loc=param[1], scale = param[2], shape=param[3], lower.tail = FALSE)
       return(returnlevel)
 }
 
 for (i in 1:length(nearest_in_set2)) {
   #for each of the gridpoints, calculate returnlevel for 1h, 12h, and 24h
  returnlevels1h=append(returnlevels1h, calculate_returnlevel(am1h[nearest_in_set2[i], ]))#voeg returnlevel toe aan vector van returnlevels, geen effect als er error was
  returnlevels24h=append(returnlevels24h, calculate_returnlevel(am24h[nearest_in_set2[i], ]))#voeg returnlevel toe aan vector van returnlevels, geen effect als er error was
  returnlevels12h=append(returnlevels12h, calculate_returnlevel(am12h[nearest_in_set2[i], ]))#voeg returnlevel toe aan vector van returnlevels, geen effect als er error was
 }
 
 #for each model, save means of returnlevels over all gridpoints in output 
 output$returnlevel1h[j]=mean(returnlevels1h)
 output$returnlevel24h[j]=mean(returnlevels24h)
 output$returnlevel12h[j]=mean(returnlevels12h)
 output$npoints[j]=length(nearest_in_set2) #save total number of gridpoints, to check whether right mask was used
 }

#EDIT PATH AND FILENAME IF NEEDED
save(output,file="output_fin_merged.RData")

##########################################
### END LOOP OVER REGIONAL CLIMATE MODELS 
##########################################


