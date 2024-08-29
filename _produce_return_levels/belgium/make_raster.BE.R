################################################################################################
### Produce mask for Belgium
### this requires a specific procedure since there is a bug 
### for resolution 0.11°, the mask forgets a part of Belgium
################################################################################################
library(sp)      ### Load R-package "sp": see https://cran.r-project.org/web/packages/sp/index.html
library(raster)  ### Load R-package "raster": see https://cran.r-project.org/web/packages/raster/index.html

############################################################
## Create mask (this one shows the bug)
############################################################
BE0 <- raster::getData("GADM", country = "Belgium", level = 0)
BE0grid <- sp::makegrid(BE0, cellsize=0.11)
colnames(BE0grid) <- c('Longitude', 'Latitude')
BE0grid_points <- sp::SpatialPoints(coords = BE0grid, proj4string=CRS(proj4string(BE0)))
BE0grid_points_in <- BE0grid_points[BE0, ]
plot(BE0grid_points_in)  ### Forgets a part of Luxembourg
#######################################################
### Convert data to format data.frame and save to disk
#######################################################
BE_mask<-data.frame(coordinates(BE0grid_points_in))
colnames(BE_mask) <- c('lon','lat')

print("Aantal gridpunten:")
print(nrow(BE_mask))


##################################################################################
### SOLUTION HANS
###################################################################
##### I. Generate first a finer grid (0.05°) ######################
###################################################################
BE0 <- raster::getData("GADM", country = "Belgium", level = 0)   ### Dit werkt wel! (waarom weet ik niet)
BE0grid <- sp::makegrid(BE0, nsig=2,cellsize=0.05)
colnames(BE0grid) <- c('Longitude', 'Latitude')
BE0grid_points <- sp::SpatialPoints(coords = BE0grid, proj4string=CRS(proj4string(BE0)))
BE0grid_points_in <- BE0grid_points[BE0, ]
plot(BE0grid_points_in)
BE_mask<-data.frame(coordinates(BE0grid_points_in))
colnames(BE_mask) <- c('lon','lat')
print("Aantal gridpunten:")
print(nrow(BE_mask))

longitude_fine <- BE_mask[,1]
latitude_fine <- BE_mask[,2]
#############################################
### II Select a coarse sub-grid (0.1°)
#############################################
###
longitude_fine_min <- longitude_fine-min(longitude_fine)  ### zet om naar decimalen die een vijf- of tienvoud zijn
latitude_fine_min <- latitude_fine-min(latitude_fine)  

dec_longitude_fine <- as.integer(round((longitude_fine_min%%1)*100))  ## decimal part, rounding needed otherwise as.integer sometimes flips a 10 to a 9
dec_latitude_fine <- as.integer(round((latitude_fine_min%%1)*100))   ## decimal part

select <- ( (dec_longitude_fine%%2) == 0 ) & ( (dec_latitude_fine%%2) == 0 )  ### Neem enkel longitude en latitude waarvan decimaal een een tienvoud is (dus even, en vijfvoud oneven).
BE_mask_0.1 <- BE_mask[select, ]
plot(BE_mask_0.1)

print("Aantal gridpunten:")
print(nrow(BE_mask_0.1))

#######################################################
### Convert data to format data.frame and save to disk
#######################################################
save(BE_mask_0.1,file="BE_mask_Improved.RData")



