#########################################################################################################
## Add elevation to Rdata file with return levels for Belgium
##########################################################################################################
### Load the R-packages 
#####################################################
library('spam', lib.loc = '../lib')#locatie package specifiëren is essentieel
library(fields, lib.loc="../lib")

###############################################################################
### Loading elevations with coordinates for belgium 
###############################################################################
#EDIT PATH IF NEEDED
dir= "/home/andie/fileserver/pub/Hans/EVT.precip.gridded/"

#the number after D is the duration in minutes
#available durations (not in order): D0010 D0120 D1440 D0020 D0180 D2880 D0030 D0360 D4320 D0060 D0720, all separate files

data=load(paste(dir,"StatRR_BEL-RMI-2013_D0060_with_altitude.RData",sep='')) #little construction because these were all named RL when they were saved
H1hr=get(data)$H #select altitude

data=load(paste(dir,"StatRR_BEL-RMI-2013_D0360_with_altitude.RData",sep=''))
H6hr=get(data)$H

data=load(paste(dir,"StatRR_BEL-RMI-2013_D0720_with_altitude.RData",sep=''))
H12hr=get(data)$H

data=load(paste(dir,"StatRR_BEL-RMI-2013_D1440_with_altitude.RData",sep=''))
H24hr=get(data)$H

data=load(paste(dir,"StatRR_BEL-RMI-2013_D2880_with_altitude.RData",sep=''))
H48hr=get(data)$H

data=load(paste(dir,"StatRR_BEL-RMI-2013_D4320_with_altitude.RData",sep=''))
H72hr=get(data)$H

#get lon and lat grid
grid_kmi=cbind(get(data)$lon, get(data)$lat)

H_OBS_all<-matrix(NA,nrow(grid_kmi),6)#create matrix to save altitudes from observations
colnames(H_OBS_all)<-c("d = 1h","d = 6h","d = 12h","d = 24h", "d = 48h","d = 72h") 
H_OBS_all[,1]<-H1hr
H_OBS_all[,2]<-H6hr
H_OBS_all[,3]<-H12hr
H_OBS_all[,4]<-H24hr
H_OBS_all[,5]<-H48hr
H_OBS_all[,6]<-H72hr

###############################################################################
### Selecting grid points KMI closest to grid points in mask for Belgium
###############################################################################

load("../masks/BE_mask_Improved.RData")  ### Longitude/Latitude of gridpoints in belgium, produced by "make_raster.R" (grid has resolution 0.11°)
Belgium_mask=BE_mask_0.1

points.in.belgium <- Belgium_mask
points.in.belgium <- as.matrix(points.in.belgium,nrow(points.in.belgium),2)#convert to matrix

nearest_in_obs <- apply(rdist(points.in.belgium, grid_kmi), 1, which.min)  ### Select the nearest gridpoints (of regional climate model) to the gridpoints of the mask


######################################################################################################################
### Make  a check if the above lines did work well
#######################################################################################################################
obs.in.belgium <- cbind(get(data)$lon[nearest_in_obs], get(data)$lat[nearest_in_obs])
check <- cbind(obs.in.belgium, points.in.belgium)  
### 1st/2nd column: longitude/latitude gridpoints kmi, 3th/4th column: longitude/latitude gridpoints mask belgium.
print("check")
print(check)

############################################################################################################
### Select the elevations at these closest gridpoints from the full observations matrix
############################################################################################
H_OBS<-H_OBS_all[nearest_in_obs,]


###############################################################################
### Load dataframe to save elevations
###############################################################################

#EDIT PATH IF NEEDED
load("output_bel_merge_2year.RData")

# Upon inspection, elevations for different durations are the same, so just saving one is enough
output$H<-H_OBS[,1]


#save file
save(output,file="output_bel_2yr_elevation.RData")


