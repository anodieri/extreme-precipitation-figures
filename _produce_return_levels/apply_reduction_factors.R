############################################################################
## Applying areal and time reduction factors to the observational data
## directly on return levels
## for files with _ARF in the name this script was already applied
############################################################################

#load reduction factors
#from Berg, 2019 (table 2) these combine the areal and temporal reduction factors
#EDIT PATH IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/reductionfactors.Rdata")


############################################
## the Netherlands, Finland, Denmark
############################################

#load observational return levels for the Netherlands, Finland, Denmark (from Poschlod 2021)
#EDIT PATH IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/countries1mean/obs.RData")

#apply reduction factors: divide each column by correct reduction factor
obs$"1h"<-obs$"1h"/reductionfactors['1h',]
obs$"3h"<-obs$"3h"/reductionfactors['3h',]
obs$"6h"<-obs$"6h"/reductionfactors['6h',]
obs$"12h"<-obs$"12h"/reductionfactors['12h',]
obs$"24h"<-obs$"24h"/reductionfactors['24h',]

#EDIT PATH IF NEEDED
setwd("C:/Users/Anouk/Documents/cursussen/MP/europa/")#set working directory
save(obs, file="countries1mean/obs_ARF.RData") #save observations with reduction factors

#################################################
## Belgium and United Kingdom
#################################################

#load return levels Belgium
#EDIT PATH IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/output_bel_merge.RData")

#apply reduction factors to observation columns
#for 48h and 72h, no reduction factor is needed
output$obs_1h_KMI<-output$obs_1h_KMI/reductionfactors['1h',]
output$obs_6h_KMI<-output$obs_6h_KMI/reductionfactors['6h',]
output$obs_12h_KMI<-output$obs_12h_KMI/reductionfactors['12h',]
output$obs_24h_KMI<-output$obs_24h_KMI/reductionfactors['24h',]

save(output, file="belgium/output_bel_merge_ARF.RData")

#load return levels Belgium with elevation
#EDIT PATH IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/output_bel_merge_with_altitude.RData")

#apply reduction factors to observation columns
#for 48h and 72h, no reduction factor is needed
output$obs_1h_KMI<-output$obs_1h_KMI/reductionfactors['1h',]
output$obs_6h_KMI<-output$obs_6h_KMI/reductionfactors['6h',]
output$obs_12h_KMI<-output$obs_12h_KMI/reductionfactors['12h',]
output$obs_24h_KMI<-output$obs_24h_KMI/reductionfactors['24h',]

save(output, file="belgium/output_bel_merge_with_altitude_ARF.RData")

#load data UK
#EDIT PATH IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/UK/output_UK_merge.RData")

output$`obs_1h_CEH-GEAR`<-output$`obs_1h_CEH-GEAR`/reductionfactors['1h',]
output$`obs_6h_CEH-GEAR`<-output$`obs_6h_CEH-GEAR`/reductionfactors['6h',]
output$`obs_12h_CEH-GEAR`<-output$`obs_12h_CEH-GEAR`/reductionfactors['12h',]
output$`obs_24h_CEH-GEAR`<-output$`obs_24h_CEH-GEAR`/reductionfactors['24h',]

save(output, file="UK/output_UK_merge_ARF.RData")

###################################################
## Germany
###################################################

#load package to handle netcdf files
library(ncdf4)

#load return levels Germany from netcdf file
#EDIT PATH IF NEEDED
file="C:/Users/Anouk/Documents/cursussen/MP/europa/Germany/output_Dui_merge_1-12-24_ARF.nc"
nc=nc_open(file, write = TRUE)

#apply reduction factors
arf1h<-ncvar_get(nc,'obs_1h_KOSTRA')/reductionfactors['1h',]
arf6h<-ncvar_get(nc,'obs_6h_KOSTRA')/reductionfactors['6h',]
arf12h<-ncvar_get(nc,'obs_12h_KOSTRA')/reductionfactors['12h',]
arf24h<-ncvar_get(nc,'obs_24h_KOSTRA')/reductionfactors['24h',]

#save updated return levels in netcdf file
ncvar_put(nc, "obs_1h_KOSTRA", vals=arf1h, start=NA, count=NA)
ncvar_put(nc, "obs_6h_KOSTRA", vals=arf6h, start=NA, count=NA)
ncvar_put(nc, "obs_12h_KOSTRA", vals=arf12h, start=NA, count=NA)
ncvar_put(nc, "obs_24h_KOSTRA", vals=arf24h, start=NA, count=NA)


#close netcdf file
nc_close(nc)

