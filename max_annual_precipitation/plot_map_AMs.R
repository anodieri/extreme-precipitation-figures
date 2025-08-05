#########################################################################
## Script to plot a map of the raw annual maxima
#########################################################################

#########################################################################
## Load the necessary packages
#########################################################################

library(ggplot2) #to plot
library(ncdf4) #to read ncdf files
library(reshape2) #to use function melt
library(cowplot) #to plot multiple figures in a grid
library(stringr) #to search in strings
library(maps)#for basic map borders
library(plyr) #for the join function
library(scico) #color palettes
library(maps) # map borders
library(raster)

##########################################################################
## Load the data
##########################################################################

# load the dataframe produced by the save_AMs.R script
# was saved as 'output' there
load("am_full_domain.RData")


##########################################################################
## constructing dataframe containing borders of countries for the plots
##########################################################################

map<-map_data('world')#map data
countries<-ccodes() #all countries' names
european_countries<-countries[countries$continent=='Europe',]$NAME #select names of the European countries
#add countries that should be on the map but are not eu
european_countries<-c(european_countries,'UK', 'Syria','Lebanon','Algeria',
                      'Cyprus','Israel', 'Egypt', 'Turkey','Morocco','Libya','Tunisia')
map$eu<-map$region %in% european_countries #col that determines whether region should be plotted
map<-map[map$eu==TRUE,] #select only those rows with this column = TRUE
sampling=9 #choose how much this dataset will be sampled down
row_odd <- seq_len(nrow(map)) %% sampling #select 1/sampling of rownumbers
map<-map[row_odd == 1, ] #select only the chosen rows

##########################################################################
## prepare the data 
##########################################################################

# set the data
data = output

#add columns to data to go from gridpoints to grid squares
resolution=1 #the resolution of the mask is 1 degree
#determine the size of the squares to be drawn
data$xmin=data$lon-resolution/2
data$xmax=data$lon+resolution/2
data$ymin=data$lat-resolution/2
data$ymax=data$lat+resolution/2

##########################################################################
## choose the variable we want to plot
##########################################################################

# set the name of the variable we want to plot
# see the list of variable names by executing
# colnames(data)
varname = "am1h#CNRM-CERFACS-CNRM-CM5#CLMcom-ETH-COSMO-crCLIM-v1-1#r1i1p1"

##########################################################################
## actually create the plot 
##########################################################################

g<-ggplot(data=data, aes(x=lon, y=lat,fill=data[,  varname]))+
  geom_rect(xmin=data$xmin, xmax=data$xmax,ymin=data$ymin, ymax=data$ymax)+ #plot colors
  annotation_map(map,alpha=0, fill=NA, colour='black')+ #plot the map borders
  theme_bw()+#to avoid grey background
  ggtitle('Annual maximum 1 h precipitation in 2005 \nfor COSMO-crCLIM downscaling CNRM-CERFACS-CNRM-CM5')+
  theme(text=element_text(size=16), #specify sizes of different plot elements
        plot.title = element_text(size=16, hjust = 0.5), 
        legend.key.width = unit(2, 'cm'),
        legend.key.height = unit(2,'cm'),
        axis.title = element_blank(),
        legend.title = element_text(angle = -90))+#remove axis names+
  scale_x_continuous(breaks=c(0,15, 30),labels=c("0°","15° E", "30° E"))+
  scale_y_continuous(breaks=c(40,50,60),labels=c("40° N", "50° N", "60° N"))+
  scale_fill_scico('AM 1 h precipitation (mm)', palette = 'managua', na.value = "grey", 
                n.breaks=8)+
  guides(fill = guide_colorbar(title.position = "right"))

 
#set the plot to have the right projection and set the x and y limits
#to only include the EURO-CORDEX domain
g=g+coord_map(projection="orthographic",xlim=c(-8,34), ylim = c(35,67))
g

# save the plot
ggsave('example_AM_2005.pdf', g,width=10, height = 6, units = 'in', dpi = 300)

