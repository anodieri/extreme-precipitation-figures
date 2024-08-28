###########################################################
## Plot the return levels for the full EURO-CORDEX domain
## for all model combinations in a big raster
###########################################################


# load libraries ----------------------------------------------------------


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



# load data ---------------------------------------------------------------

#load the return levels for the full domain
#EDIT PATH IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/full_domain/output_full_domain_lmoments.RData")
data=output
#each variable corresponds to a model run (except lat and lon )


# prepare data ------------------------------------------------------------


#remove the runs from the variable names, not important and makes it more complex to reconstruct names
#different runs of same model combination are merged anyway
colnames(data)<-sub("#[^#]+$", "", colnames(data)) 

#add columns to data to go from gridpoints to grid squares
resolution=1 #the resolution of the mask is 1 degree
#determine the size of the squares to be drawn
data$xmin=data$lon-resolution/2
data$xmax=data$lon+resolution/2
data$ymin=data$lat-resolution/2
data$ymax=data$lat+resolution/2


#function to select the variable names that contain all of the search_strings in a vector/list
#also works with just one string
select_names<-function(search_strings){
  names=colnames(data) #save the names of all variables in the file
  for(s in search_strings){ #for each of the given strings
    names=names[str_detect(names, s)] #select the names that contain this string
  }
  return(names) #return the final selection
}

#convert from precipitation intensities to precipitation depths: multiply with duration
#not needed for 1h
data[,select_names('24h')]<-data[,select_names('24h')]*24


# constructing titles -----------------------------------------------------

#get the shortened rcm name to use as column title
make_title_rcm<-function(varname){
  title=varname
  if(grepl("returnlevel",varname)){ #if the name of the variable contains returnlevel
    rcm=strsplit(varname, split='#', fixed=TRUE)[[1]][3] #select rcm name
    if(grepl('ETH', varname)){
      rcm=strsplit(rcm, split='-')[[1]][3] # in this case, the institute name contains a - so take the third part
    }else{
      rcm=strsplit(rcm, split='-')[[1]][2] #otherwise the second part is the rcm name
    }
    title=rcm #construct the final title
  }
  return(title)
}

#get the shortened gcm name to use as row title
make_title_gcm<-function(varname){
  title=varname
  if(grepl("returnlevel",varname)){ #if the name of the variable contains returnlevel
    gcm=strsplit(varname, split='#', fixed=TRUE)[[1]][2] #select the name of the gcm
    #now manual since all abbreviations are different
    if(gcm=="MPI-M-MPI-ESM-LR"){title='MPI-ESM'}
    else if(gcm=='MOHC-HadGEM2-ES'){title='HadGEM2'}
    else if(gcm=='ICHEC-EC-EARTH'){title='EC-EARTH'}
    else if(gcm=='NCC-NorESM1-M'){title='NorESM'}
    else if(gcm=='IPSL-IPSL-CM5A-MR'){title='IPSL-CM5A'}
    else if(gcm=='CNRM-CERFACS-CNRM-CM5'){title='CNRM-CM5'}
  }
  return(title)
}


# construct map borders ---------------------------------------------------

#constructing dataframe containing borders of countries
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



# plotting ----------------------------------------------------------------


#actually plotting the data from one variable
#varname: the name of the variable in the dataframe to be plotted
#range: min and max of the color scale
#colours and palette: determine colorscale, colours are only used when palette =NULL
#palette options: scico_palette_show()
#legend, xlabs, ylabs determine whether the legend, xlabs, ylabs are plotted
plot_var<-function(varname, range=NULL, legend=FALSE, save=FALSE, rcm_name=FALSE,
                   colours=c("lightblue1","red"), palette=NULL, xlabs=FALSE, ylabs=FALSE){
  
  #if this variable does not exist in the dataframe, return blank square of same specs as map
  #this is used for rcm-gcm combinations that do not exist
  if(!varname %in% colnames(data)){
    g=ggplot(data,aes(x=lon, y=lat))+
      geom_blank()+ # plot a blank rectangle
      theme_bw()+#to avoid grey background
      theme(text=element_text(size=18), #set sizes of plot elements
            plot.title = element_text(size=18, hjust = 0.5), 
            legend.key.width = unit(2, 'cm'),
            legend.key.height = unit(2,'cm'),
            axis.title = element_blank(),
            panel.grid = element_blank())
  }
  
  else{ #if the variable does exist: make an actual plot
    
    #set the range if needed: to use a uniform range over all plots
    if(is.null(range)){ 
      range=range(data[,varname], na.rm=TRUE) #if the range is not specified, just use max and min given by range function
      #na.rm is crucial here, otherwise the range is just NA, NA
    }
    
    #actually create the plot
    g=ggplot(data=data, aes(x=lon, y=lat,fill=data[,varname]))+
      geom_rect(xmin=data$xmin, xmax=data$xmax,ymin=data$ymin, ymax=data$ymax)+ #plot colors
      annotation_map(map,alpha=0, fill=NA, colour='black')+ #plot the map borders
      theme_bw()+#to avoid grey background
      theme(text=element_text(size=18), #specify sizes of different plot elements
            plot.title = element_text(size=18, hjust = 0.5), 
            legend.key.width = unit(2, 'cm'),
            legend.key.height = unit(2,'cm'),
            axis.title = element_blank())#remove axis names
    
    #color scale depending on palette or colors are given
    #if no palette is given, use the colors (that are given or default):
    if(is.null(palette)){
      g<-g+scale_fill_binned('Return level (mm)', show.limits=FALSE,
                             limits=c(floor(range[1]), ceiling(range[2])),n.breaks=10,
                             low=colours[1], high=colours[2])
    }else{ #otherwise use the specified palette from the scico package
      g<-g+scale_fill_scico('Return level (mm)', palette = palette, na.value = "grey", 
                       limits=c(floor(range[1]), ceiling(range[2])),n.breaks=8)
    }
  }
  
  #set axis ticks
  g<-g+
    scale_x_continuous(breaks=c(0,15, 30),labels=c("0°","15° E", "30° E"))+
    scale_y_continuous(breaks=c(40,50,60),labels=c("40° N", "50° N", "60° N"))
    
  #remove the legend if needed
  if(legend==FALSE){
    g=g+theme(legend.position = "none")
  }
  
  #remove ylabs if needed
  if(ylabs==FALSE){
    g=g+theme(axis.text.y = element_blank(), #remove y axis labels
              axis.ticks.y = element_blank()) #remove y axis ticks
  }else{ #if ylabels are needed, add the gcm name as axis title
    g=g+ylab(make_title_gcm(varname))+
      theme(axis.title.y = element_text())
  }
  
  #remove xlabels if needed
  if(xlabs==FALSE){
    g=g+theme(axis.text.x = element_blank(), #remove x axis labels
              axis.ticks.x = element_blank()) #remove x axis ticks
  }
  
  #add the rcm name at the top if needed
  if(rcm_name==TRUE){
    g=g+ggtitle(make_title_rcm(varname))
  }
  
  #set the plot to have the right projection and set the x and y limits
  #to only include the EURO-CORDEX domain
  #this takes quite some time, so for testing comment this line
  g=g+coord_map(projection="orthographic",xlim=c(-8,34), ylim = c(35,67))
  
  #save if needed
  if(save){
    ggsave(paste0("oneplot_",palette,".pdf"), g,width=5, height = 3.75, units = 'in', dpi = 300)
    
  }
  return(g)
}

plot_var("returnlevels24h#MOHC-HadGEM2-ES#MOHC-HadREM3-GA7-05", 
         legend=TRUE, xlabs = TRUE, ylabs = TRUE, range=c(0,201),
         palette = 'davos')


#set ranges to use for all plots
#round numbers result in the nicest scales for palettes
#these values cut off some outliers to avoid the scale being fully determined by them
c1h=50
c24h=200
range1h=c(0,c1h)
range24h=c(0,c24h)
#calculate the percentage of data that are outside the scale
sum(data[,select_names('24h')]>c24h, na.rm=TRUE)/(nrow(data)*length(select_names('24h')))
sum(data[,select_names('1h')]>c1h, na.rm=TRUE)/(nrow(data)*length(select_names('1h')))


#lists of all rcm and gcm names
rcms=c("CNRM-ALADIN63","CLMcom-ETH-COSMO-crCLIM-v1-1",
       "MOHC-HadREM3-GA7-05", "SMHI-RCA4","ICTP-RegCM4-6","GERICS-REMO2015")
gcms=c( "NCC-NorESM1-M","MPI-M-MPI-ESM-LR", "MOHC-HadGEM2-ES",
        "IPSL-IPSL-CM5A-MR","ICHEC-EC-EARTH","CNRM-CERFACS-CNRM-CM5")

#construct the names of the rcm gcm combinations in the data for given duration and rcm
#all possible gcms are included, also if the combination does not exist
#duration should be '1h' or '24h'
#rcm should be a number from 1 to 6 (then this index is selected in rcms)
construct_names<-function(dur, rcm){
  rcmname=rcms[rcm] #get name of the rcm linked to the number
  names=paste("returnlevels",dur,"#",gcms,"#",rcmname, sep='') #construct the variable names as in the data
  return(names)
}


#plot a column for an RCM for a specified duration ('1h' or '24h')
#rcm is again a number from 1 to 6
#palette can be a scico palette 
#ylabs determines whether ylabels are drawn
plot_grid_rcm<-function(rcm, dur, colours=c("lightblue1","red"), palette=NULL,ylabs=FALSE){
  print(paste('starting rcm',rcm))
  
  #determine the range depending on the duration
  if(dur=='1h'){rng=range1h}
  else{rng=range24h}
  
  #add slight offset to range to avoid having marks at the end of scale if no palette is specified
  if(is.null(palette)){
    rng=c(rng[1],rng[2]+1)
  }
  
  #construct the variable names to use
  varnames<-construct_names(dur=dur,rcm)
  
  #construct list of all plots to be plotted in column
  #!with sapply or a for-loop this does not work
  plotlist=lapply(varnames[-6], plot_var, range=c(rng[1],rng[2]), legend=FALSE,
                  colours=colours,ylabs=ylabs, palette=palette)
  
  #separately plot the top plot: this one needs the rcm name as a title
  topplot=plot_var(varname = varnames[1],range=c(rng[1],rng[2]), legend=FALSE, colours=colours,
                      ylabs=ylabs, xlabs=FALSE,rcm_name = TRUE, palette=palette)
  #separately plot the bottom plot: this one needs xlabs
  bottomplot=plot_var(varname = varnames[6],range=c(rng[1],rng[2]), legend=FALSE, colours=colours,
                      ylabs=ylabs, xlabs=TRUE, palette=palette)
  #add the topplot and bottomplot to the list
  plotlist[[1]]<-topplot
  plotlist[[length(varnames)]]=bottomplot #replace the bottom plot
  #put all plots in a grid
  plot_grid(plotlist=plotlist,ncol=1, rel_heights = c(1.1,1,1,1,1,1.1))
}

#plot_grid_rcm(rcm = 1, dur='1h',ylabs=TRUE)


#the full plot, containing all gcm and rcms
# dur should be '1h' or '24h'
# if palette= NULL, colours determines the colorscale, otherwise a scico palette is used e.g. 'roma'
# save determines whether the plot is saved
full_plot<-function(dur, colours=c("lightblue1","red"), palette=NULL, save=FALSE){
  
  #select colors depending on duration
  if(dur=='1h'){colours=c('#D0E9C6','#3166A5')}
  else if(dur=='24h'){colours=c('lightcyan1','purple4')}
  else{colours=c("gold","darkred")}
  
  leftcol=plot_grid_rcm(1, dur, ylabs=TRUE, palette=palette, colours=colours) #construct left column with labels on y-axis
  plotlist=lapply(1:length(rcms), plot_grid_rcm, dur, palette=palette, colours=colours) #plot all other columns without
  plotlist[[1]]<-leftcol #replace the left column
  
  varnames<-construct_names(dur,1)#just to have some varnames to use one to extract legend from
  if(dur=='1h'){rng=range1h} #ranges
  else{rng=range24h}
  #add slight offset to range to avoid having marks at the end of scale if no palette is specified
  if(is.null(palette)){
    rng=c(rng[1],rng[2]+1)
  }
  #extract legend
  legend=get_legend(plot_var(varnames[1], colours=colours,range = rng, legend = TRUE, palette=palette))
  plotlist[[length(rcms)+1]]=legend #add legend to plotlist
  
  p<-plot_grid(plotlist = plotlist, ncol=length(rcms)+1, rel_widths = c(1.3,1,1,1,1,1,0.9))
  
  if(save){#if specified, save the plot as a pdf
    ggsave(paste0("fullplot_",dur,'_',palette,".pdf"), p, width=20, height = 13, units = 'in', dpi = 300)
  }
  return(p)
}

#set working directory
#EDIT PATH IF NEEDED
setwd("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/full_domain/")


#save plots (this takes a while due to the projection calculations)
#comment out projection line in plot_var for quick testing
full_plot(dur = '1h', save=TRUE)
full_plot(dur = '24h', save=TRUE)#, palette = 'glasgow')

# a few palette options
palettes=c('batlow','managua','roma','romaO', NULL) #not very blue

full_plot('1h', palette = 'navia', save=TRUE) #more blue
full_plot('24h', palette = 'cork', save=TRUE) #more blue

#use a website to convert to png for acceptable file size
#e.g. https://pdf2png.com/ 




# outliers specifics ------------------------------------------------------


#calculate the maximum values of the outliers inside the plotted domain
data=data[data$lon<=34&data$lon>=-8&data$lat>=35&data$lat<=67,]
print(range(data[,select_names('1h')], na.rm = TRUE) )
print(range(data[,select_names('24h')], na.rm = TRUE))

#absolute amount of  outliers
sum(data[,select_names('24h')]>c24h, na.rm=TRUE) #178
sum(data[,select_names('1h')]>c1h, na.rm=TRUE) #53

#percentage outliers
sum(data[,select_names('24h')]>c24h, na.rm=TRUE)/(nrow(data)*length(select_names('24h')))
#0.004645943
sum(data[,select_names('1h')]>c1h, na.rm=TRUE)/(nrow(data)*length(select_names('1h')))
#0.001383342

