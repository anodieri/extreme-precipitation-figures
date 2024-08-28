##############################################################
## Get times of the annual maxima of hourly precipitation
## for EUROCORDEX models COSMO and RCA4
## for Belgium, historical runs
##############################################################

###############################################
#load libraries
################################################
library(ncdf4, lib.loc = '../lib')
library(stringr, lib.loc = '../lib')
library('spam', lib.loc = '../lib')
library(fields, lib.loc="../lib")


###############################################################################
### Loading mask for Belgium
###############################################################################

load("C:/Users/Anouk/Documents/cursussen/MP/europa/masks/BE_mask_Improved.RData")  ### Longitude/Latitude of gridpoints in belgium, produced by "make_raster.R" (grid has resolution 0.11°)
Belgium_mask=BE_mask_0.1

points.in.belgium <- Belgium_mask
points.in.belgium <- as.matrix(points.in.belgium,nrow(points.in.belgium),2)#convert to matrix


#################################################
# get times at which annual maxima occurred
#################################################

#directory of files with k largest hourly precipitations depths per year
dir="/mnt/HDS_CORDEXBE_RMIB/hvijver/EURO-CORDEX_pr_EUR11_1hr_KLarge"
rcms=list.dirs(dir, recursive = FALSE) #first files are in folders by rcm


#construct dataframe to save times in 
max_length=dim(points.in.belgium)[1]*57 #number of rows is number of gridpoints mask * max number of years =57
times=data.frame(matrix(nrow=max_length,ncol=0)) #construct data frame


#nested loops to run over all folders
#first rcm, then gcm, then run, then files for each year
for(rcm in rcms){
  
  gcms=list.dirs(rcm, recursive = FALSE) #get list of directories by gcm
  
  for(gcm in gcms){
    
    runs=list.dirs(gcm, recursive = FALSE) #get list of directories by run
    
    for(run in runs){
      
      print(run)
      
      files=list.files(run) #get all files for this run
      files= files[str_detect(files, "historical")] #select only historical 
      
      times_annual_max_belgium<-vector() #create vector to save the times (expressed as days from 1949-12-1 00:00:00)
      
      #loop over files, one for each year
      for(file in files){
        
        nc<-nc_open(paste(run,file,sep = '/')) #open netcdf file
        
        #extract longitude, latitude and timing
        lon <- ncvar_get(nc,varid="lon")                
        lat <- ncvar_get(nc,varid="lat") 
        time <- ncvar_get(nc, varid = "when")
        #value <-ncvar_get(nc, varid = "klargest") #extract value, just to check
        
        nc_close(nc) #close file again
        
        
        lon.vec <- as.vector(lon)                        ### Convert matrix lon (dim: 424×412) to vector lon.vec (length: 174688)
        lat.vec <- as.vector(lat)                        ### Convert matrix lat (dim: 424×412) to vector lat.vec (length: 174688)
        
        grid_rcm <- cbind(lon.vec,lat.vec) ### Longitude/Latitude of gridpoints of the Regional Climate Model
        colnames(grid_rcm) <- c("longitude","latitude")
        
        nearest_in_rcm <- apply(rdist(points.in.belgium, grid_rcm), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask
        
        #convert from matrix lat x lon X rank to matrix location x rank
        time <- sapply(1:200, function(i){ as.vector(time[,,i]) })
        
        #loop over gridpoints mask
        for (i in 1:length(nearest_in_rcm)){
          times_annual_max_belgium=append(times_annual_max_belgium,time[nearest_in_rcm[i],1])
        }
      }
      
      
      parts=str_split(run, pattern="/")[[1]][6:8] #split up the final filename
      varname=paste(parts[2],parts[1], parts[3], sep='#') #construct variable name using run, gcm and rcm name
      
      #add NAs to make vector right length (of max number of years)
      times_annual_max_belgium<-append(times_annual_max_belgium, rep(NA, max_length - length(times_annual_max_belgium)))
      
      #add vector to dataframe
      times[,varname]<-times_annual_max_belgium
      
    }
    
  }
  
}


#save the dataframe
save(times,file="times_bel_am.RData")

