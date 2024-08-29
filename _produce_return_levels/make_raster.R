################################################################################################
### This program produces a grid for a given country (for example, UK), and a given resolution
### The output is "UK_mask", a table with the coordinates (longitude/latitude)
### Saved to disk in RData-format.
### also possibility to create mask for the whole of Europe (including and excluding seas)
### and for PRUDENCE regions
################################################################################################
library(sp)  ### Load R-package "sp": see https://cran.r-project.org/web/packages/sp/index.html
library(raster)  ### Load R-package "raster": see https://cran.r-project.org/web/packages/raster/index.html
######################################################################################################

######################################################
## Mask for individual countries
######################################################
#getData('ISO3') to get list of countries

UK0 <- raster::getData("GADM", country = c("Belgium"), level = 0) #get coordinates country
UK0grid <- sp::makegrid(UK0, cellsize=0.88) #make grid with size 0.11 for each cell 
colnames(UK0grid) <- c('Longitude', 'Latitude')
UK0grid_points <- sp::SpatialPoints(coords = UK0grid, proj4string=CRS(proj4string(UK0))) #convert to class SpatialPoints
UK0grid_points_in <- UK0grid_points[UK0, ] #grenzen uitknippen, anders box rond land, deze stap overslaan om box te maken
plot(UK0grid_points_in)

UK_mask<-data.frame(coordinates(UK0grid_points_in)) #convert to dataframe and save
plot(UK_mask)


colnames(UK_mask) <- c('lon','lat')
#EDIT PATH IF NEEDED
setwd("C:/Users/Anouk/Documents/cursussen/MP/europa/masks") #set working directory: here the file will be saved

Belgium_mask_0.04=UK_mask #to be able to load under this name in other R-files
save(Belgium_mask_0.04,file="Belgium_0.04.RData")


##############################
#remove Ireland for the UK
##############################
Ireland_mask<-UK_mask[!(UK_mask$Latitude<55.26&UK_mask$Latitude>54.04&UK_mask$Longitude<(-5.41)&UK_mask$Longitude>(-8.27)),]
plot(Ireland_mask)
UK_mask<-Ireland_mask
colnames(UK_mask) <- c('lon','lat')
save(UK_mask,file="UK_mask.RData")



##################################################
# Mask for the whole of Europe (excluding seas)
##################################################

#set working directory
#EDIT PATH IF NEEDED
setwd('C:/Users/Anouk/Documents/cursussen/MP/europa/climate change')

countries<-ccodes() #all countries' names
european_countries<-countries[countries$continent=='Europe',]$NAME #select names of the European countries

europe_mask=data.frame(matrix(ncol = 2, nrow=0)) #initalise empty dataframe to save lat and lon for all european countries
colnames(europe_mask)<-c('lon','lat')

#loop over countries
for(country in european_countries){
  #for 0.11° resolution
  #if(!country=='Monaco'& !country=='Russia'&!country=="Svalbard and Jan Mayen"& !country=='San Marino'& !country=='Jersey'){
  #for >0.11° resolution
  if(!country=='Luxembourg'&!country=='Monaco'& !country=='Russia'&!country=="Svalbard and Jan Mayen"&!country=='Belgium'){ #these are excluded as they are (partially) outside of the eurocordex domain and monaco gives a problem
    print(country)
    coord <- raster::getData("GADM", country =country, level = 0) #get coordinates country
    grid <- sp::makegrid(coord, cellsize=0.11) #make grid with size 0.11 for each cell 
    colnames(grid) <- c('Longitude', 'Latitude')
    grid_points <- sp::SpatialPoints(coords = grid, proj4string=CRS(proj4string(coord))) #convert to class SpatialPoints
    grid_points_in <- grid_points[coord, ] #grenzen uitknippen, anders box rond land, deze stap overslaan om box te maken
    plot(grid_points_in)
    mask<-data.frame(coordinates(grid_points_in)) #convert to dataframe
    colnames(mask) <- c('lon','lat')
    europe_mask<-rbind(europe_mask,mask) #add to europe_mask
  }
}

#due to a bug, belgium should be added separately for 0.11° resolution
#mask has already been constructed
#EDIT PATH IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/masks/BE_mask_Improved.RData") #load belgian mask, this is special because of the bug (no south luxemburg)
europe_mask=rbind(europe_mask, BE_mask_0.1) #add belgium

#luxembourg suffers from the same problem for 0.11° resolution
Lux <- raster::getData("GADM", country = c("Luxembourg"), level = 0) #get coordinates country
Luxgrid <- sp::makegrid(Lux, cellsize=0.10) #make grid with size 0.11 for each cell 
colnames(Luxgrid) <- c('Longitude', 'Latitude')
Luxgrid_points <- sp::SpatialPoints(coords = Luxgrid, proj4string=CRS(proj4string(Lux))) #convert to class SpatialPoints
Luxgrid_points_in <- Luxgrid_points[Lux, ] #grenzen uitknippen, anders box rond land, deze stap overslaan om box te maken
plot(Luxgrid_points_in)

Lux_mask<-data.frame(coordinates(Luxgrid_points_in)) #convert to dataframe
colnames(Lux_mask) <- c('lon','lat')

europe_mask=rbind(europe_mask, Lux_mask) #add Luxembourg


europe_mask<-europe_mask[!(europe_mask$lat<40&europe_mask$lon<(-10)),] #remove Azores and Canaric islands


#plot this European mask

library(ggplot2)
plot_mask<-function(mask){
  g<-ggplot(mask)+
    geom_point(aes(x=lon, y=lat),shape=3, size=1)+
    geom_point(x=-21,y=64, color='red')+ #rejkjavik to test 
    annotation_map(map_data("world"), alpha=0,fill=NA, colour='black')+ 
    xlab('Longitude (°E)')+
    ylab('Latitude (°N)')+
    coord_equal(ratio=3/2)+
    ggtitle('')+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  #theme_minimal() #to avoid grey background but keep axes and grid
  return(g)
}

g<-plot_mask(europe_mask)
g


#save plot
ggsave("europe_mask_0.11.pdf", g,  width=11, height =11, units = 'in')

#save mask
save(europe_mask, file="europe_mask_0.11.RData")


###################################################### 
# Masks for PRUDENCE regions
######################################################

#load file containing borders of the rectangles
#EDIT PATH IF NEEDED
prudence_borders<-read.table("C:/Users/Anouk/Documents/cursussen/MP/europa/climate change/Prudence.txt")

regions=rownames(prudence_borders)

#produce mask of the prudence region with number region in the rownames above
mask_prudence<-function(region){
  regionname=regions[region] #get the name of the region from the number
  borders=prudence_borders[regionname,] #select the borders of this region
  mask=europe_mask[(europe_mask$lat>borders$lat_south&europe_mask$lat<borders$lat_north&
                       europe_mask$lon>borders$lon_west&europe_mask$lon<borders$lon_east),] #select the parts of the mask in this region
  print(dim(mask)) #print dimensions
  save(mask, file=paste('mask_0.11_',substring(regionname,2,3),".RData", sep='')) #save the mask
  return(mask)
}

#produce mask for each region
for(i in 1: length(regions)){
  mask<-mask_prudence(i)
}


#france without the overlap with ME:
borders=prudence_borders["(FR) France",] #select the borders of this region
mask_FR=europe_mask[(europe_mask$lat>borders$lat_south&europe_mask$lat<borders$lat_north&
                    europe_mask$lon>borders$lon_west&europe_mask$lon<borders$lon_east),] #select the parts of the mask in this region
plot_mask(mask_FR)
print(dim(mask_FR))
#remove overlap zone
mask_FR=mask_FR[!(mask_FR$lat>48&mask_FR$lat<50&
                  mask_FR$lon>2&mask_FR$lon<5),]
plot_mask(mask_FR)

mask=mask_FR
save(mask, file=paste('mask_0.11_FR.RData', sep='')) #save the mask


##############################################################
## Mask full EURO-CORDEX domain (including the sea)
##############################################################

res=1#choose resolution

#corners and centers of the EURO-CORDEX domain (from cordex website)
trc=c(64.4,66.65)
brc=c(36.30,25.36)
blc=c(-10.064,22.2)
tlc=c(-44.14,60.21)

#construct different values of lon and lat using min and max of points above
lat1=seq(22,72, res) 
lon1=seq(-44,64, res)

#construct dataframe to save mask
mask=data.frame(matrix(ncol=2, nrow= length(lat1)*length(lon1)))
colnames(mask)<-c('lon','lat')
mask$lon<-rep(lon1,length(lat1))
mask$lat<-rep(lat1, each=length(lon1))

plot_mask(mask)
#EDIT PATH IF NEEDED
save(mask, file='C:/Users/Anouk/Documents/cursussen/MP/europa/full_domain/full_domain_mask_1.RData')
