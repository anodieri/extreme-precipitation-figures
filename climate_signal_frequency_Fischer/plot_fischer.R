###########################################################################
## Fischer plotting
## plotting change in frequencies, relative and absolute
###########################################################################

#load libraries
library(ggplot2)
library(scico)
library(reshape2)
library(dplyr)
library(cowplot)
library(MASS)

############################################################################
# Data preparation: for all regions
# to obtain uniform ranges to use in figures for each of the regions
############################################################################

#construct dataframe for all regions
data=data.frame()

#list of region abbreviations
regions=c('BI','EA','IP','FR','SC','AL','MD','ME')

#fill this dataframe with the files for each region
for(region in regions){
  #load file for each region
  #PATH: to edit
  file=paste("C:/Users/Anouk/Documents/cursussen/MP/europa/climate change/results/fischer/results_fischer_",region,".RData", sep='')
  data_region=get(load(file)) 
  data_region <- subset(data_region, !(run=='r1i1p1' & gcm=='ICHEC-EC-EARTH')) #remove the runs for which no period for +3°C was given
  data_region$region=region #set the region specifier
  data=rbind(data,data_region) #merge
}


#shorten name rcm
data$rcm=sub(".*?-","",data$rcm) #remove institute name from rcm name
data$rcm[data$rcm=='ETH-COSMO-crCLIM-v1-1']<-sub(".*?-","",data$rcm[data$rcm=='ETH-COSMO-crCLIM-v1-1']) #cut off ETH 

#remove 4CC, was just a test
data$'1h4CC'<-NULL
data$'24h4CC'<-NULL

#adapt colnames for easier working
colnames(data)[-c(1,2,3,4)]<-paste('f', colnames(data)[-c(1,2,3,4)], sep='') #for easier working below
colnames(data)[colnames(data) == 'freq'] <- 'percentile' #correct name

#calculate ensemble mean, max and min for each frequency
means=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3 ,f24hCC,f24h2CC)~ percentile, data, mean )
maxs=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3 ,f24hCC, f24h2CC)~ percentile, data, max )
mins=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3 ,f24hCC,f24h2CC)~ percentile, data, min)

#the same for the change in frequency: calculate relative difference
rel_data<-data
rel_data[,c("f1h1.5","f1h3","f1hCC","f1h2CC")]<-(rel_data[,c("f1h1.5","f1h3","f1hCC","f1h2CC")]-rel_data$"f1h1.5")/rel_data$"f1h1.5"
rel_data[,c("f24h1.5","f24h3","f24hCC","f24h2CC")]<-(rel_data[,c("f24h1.5","f24h3","f24hCC","f24h2CC")]-rel_data$"f24h1.5")/rel_data$"f24h1.5"
#calculate relative ensemble mean, max and min for each frequency
rel_means=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3 ,f24hCC,f24h2CC)~ percentile, rel_data, mean )
rel_max=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3 ,f24hCC,f24h2CC)~ percentile, rel_data, max )
rel_min=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3 ,f24hCC,f24h2CC)~ percentile, rel_data, min)

############################################################################
# Plotting: one region at a time
############################################################################

#set region names for plot titles
regionnames <- c(
  'BI' = 'British Isles',
  'EA' = 'Eastern Europe',
  'IP' = 'Iberian Peninsula',
  'FR' = 'France',
  'ME' = 'Mid-Europe',
  'SC' = 'Scandinavia',
  'AL' = 'Alps',
  'MD' = 'Mediterranean'
)

#calculate overall range from dataframe with all regions
#percentile_threshold: how extreme are the percentiles allowed to be
#corresponds to limiting return period, 1 means include all up to 1000 years
#and 0.999 removes 1000 years, 0.9975 removes 400 and 1000 years and so on
# probability p = 1/T with T the return period
#but starting from 'upper side' so percentile_threshold is of the shape 1-p=1-1/T
#include 2CC: should CC and super CC be included?
get_range<-function(percentile_threshold=1, include_2CC, relative){
  
  #apply the percentile threshold in dataframes that are used
  rel_max<-rel_max[means$percentile<percentile_threshold,]
  maxs<-maxs[maxs$percentile<percentile_threshold,]
  rel_min<-rel_min[mins$percentile<percentile_threshold,]
  
  #calculate the range, selecting cols depending on rel/abs and 2CC included
  if(relative){
    if(include_2CC){
      range=range(rel_min[c("f1h1.5","f1h3","f1hCC","f1h2CC","f24h3","f24hCC","f24h2CC")], 
                  rel_max[c("f1h1.5","f1h3","f1hCC","f1h2CC","f24h3","f24hCC","f24h2CC")])*100 
      # actual range, neg also possible (change in frequency: can increase or decrease)
    }else{
      range=range(rel_min[c("f1h1.5","f1h3","f24h3")], 
                  rel_max[c("f1h1.5","f1h3","f24h3")])*100 
    }
  }else{
    if(include_2CC){
      range=c(0,max(maxs[c("f1h1.5","f1h3","f1hCC","f1h2CC","f24h3","f24hCC","f24h2CC")])*100) 
      #starting from 0 to get nice plots, frequencies are never below zero
    }else{
      range=c(0,max(maxs[c("f1h1.5","f1h3","f24h3")])*100) 
    }
  }
  return(range)
}

get_range(relative=TRUE, percentile_threshold = 0.9975, include_2CC = TRUE)

get_range(relative=TRUE, percentile_threshold = 0.9975, include_2CC = FALSE)


#plot frequencies for the percentiles for the selected region
#Options for: including 4CC, title, legend, log scale, saving plot, relative or absolute
#WARNING: log scale does not work for negative numbers, which appears in e.g. relative IP 
#percentile_threshold: remove the highest percentile, corresponding to the longest durations, 
#by default:0.9975=> 400 and 1000 years removed, explanation see function above
plot_frequency_region<-function(dur,region, include2CC=FALSE, save=FALSE, relative=FALSE, log=FALSE,
                                legend=TRUE, title=TRUE, range=NULL, percentile_threshold=0.9975){
  
  #load results for the right region
  file=paste("C:/Users/Anouk/Documents/cursussen/MP/europa/climate change/results/fischer/results_fischer_",region,".RData", sep='')
  data=get(load(file)) 
  
  #remove 4CC, was just a test
  data$'1h4CC'<-NULL
  data$'24h4CC'<-NULL
  
  #remove the runs for which no period for +3°C was given
  data <- subset(data, !(run=='r1i1p1' & gcm=='ICHEC-EC-EARTH')) 
  
  #shorten name rcm
  data$rcm=sub(".*?-","",data$rcm) #remove institute name from rcm name
  data$rcm[data$rcm=='ETH-COSMO-crCLIM-v1-1']<-sub(".*?-","",data$rcm[data$rcm=='ETH-COSMO-crCLIM-v1-1']) #cut off ETH 
  
  colnames(data)[-c(1,2,3,4)]<-paste('f', colnames(data)[-c(1,2,3,4)], sep='') #for easier working below
  colnames(data)[colnames(data) == 'freq'] <- 'percentile' #correct name
  
  #convert to relative if needed
  if(relative){
    rel_data<-data #make a copy
    #convert to relative
    rel_data[,c("f1h1.5","f1h3","f1hCC","f1h2CC")]<-(rel_data[,c("f1h1.5","f1h3","f1hCC","f1h2CC")]-rel_data$"f1h1.5")/rel_data$"f1h1.5"
    rel_data[,c("f24h1.5","f24h3","f24hCC","f24h2CC")]<-(rel_data[,c("f24h1.5","f24h3","f24hCC","f24h2CC")]-rel_data$"f24h1.5")/rel_data$"f24h1.5"
    data<-rel_data #rename
  }
  
  #calculate ensemble mean, max and min for each frequency
  means=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3,f24hCC,f24h2CC)~ percentile, data, mean )
  maxs=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3,f24hCC, f24h2CC)~ percentile, data, max )
  mins=aggregate(cbind(f1h1.5,f1h3,f1hCC,f1h2CC,f24h1.5,f24h3,f24hCC,f24h2CC)~ percentile, data, min)
  
  #remove columns that only contain zeros (this happens for the relative data, ref cols)
  #otherwise problems with log scale (infinite values)
  means[which(colSums(means==0) == nrow(means))]<-NULL
  maxs[which(colSums(maxs==0) == nrow(maxs))]<-NULL
  mins[which(colSums(mins==0) == nrow(mins))]<-NULL
  
  #if needed, remove the highest percentiles, corresponding to return periods of 400 and 1000 years or ...
  means<-means[means$percentile<percentile_threshold,]
  maxs<-maxs[maxs$percentile<percentile_threshold,]
  mins<-mins[mins$percentile<percentile_threshold,]
  
  #set range if needed
  if(is.null(range)){
    range=get_range(relative = relative, percentile_threshold = percentile_threshold, 
                    include_2CC = include2CC)
  }
  
  #selected the columns that are needed according to the duration and whether 2CC is included
  if(include2CC){
    means=dplyr::select(means, c(contains(dur), contains('percentile')))
    maxs=dplyr::select(maxs, c(contains(dur), contains('percentile')))
    mins=dplyr::select(mins, c(contains(dur), contains('percentile')))
  }else{
    means=dplyr::select(means, c(contains(dur), contains('percentile'))&!contains('CC'))
    maxs=dplyr::select(maxs, c(contains(dur), contains('percentile'))& !contains('CC'))
    mins=dplyr::select(mins, c(contains(dur), contains('percentile'))& !contains('CC'))
  }
  
  #melt into long format
  means=melt(means, id=c('percentile'), value.name = 'freq_mean')
  maxs=melt(maxs, id=c('percentile'), value.name = 'freq_max')
  mins=melt(mins, id=c('percentile'), value.name = 'freq_min')
  
  colnames(means)[colnames(means) == 'variable'] <- 'type' #edit variable name
  means$freq_mean=means$freq_mean*100#percentages
  #means$percentile=means$percentile*100
  frac=fractions(means$percentile) #convert percentile to fraction
  means$return_period=sapply(strsplit(attr(frac,"fracs"), split='/', fixed=TRUE), function(x) (x[2]))
  
  #create equally spaced axis ticks for the x axis
  means$ticks=1:length(unique(means$return_period))
  
  #add min and max to means dataframe to plot bands
  means$freq_max=maxs$freq_max*100 #in percentage
  means$freq_min=mins$freq_min*100
  
  #construct ylabel depending on relative or not
  if(relative){
    ylabel='Change in frequency (%)'
  }else{
    ylabel='Frequency (%)'
  }
  
  #actual plotting
  g<-ggplot(means)+
    geom_line(mapping = aes(x=ticks, y=freq_mean, color=type, group=type, linetype=type),linewidth=0.9)+ #lines
    geom_point(mapping = aes(x=ticks, y=freq_mean, color=type, group=type), size=2)+ #points
    geom_ribbon(mapping = aes(ymin= freq_min, ymax=freq_max, x=ticks, fill=type, group=type), alpha=0.1, show.legend =FALSE, color=NA)+ #bands
    ylab(ylabel)+ #add ylabel that was constructed above
    xlab('Return period (year)')+
    #alternative color options:
    #scale_color_scico_d('', palette = 'batlow',labels=c('+1.5°C','+3°C','CC','Super CC','4 x CC'))+
    #scale_fill_scico_d('', palette = 'batlow')+
    #scales for color, fill and linetype
    scale_color_manual('', breaks=c(paste0('f',dur,'1.5'),paste0('f',dur,'3'),paste0('f',dur,'CC'),paste0('f',dur,'2CC')),
                       values=c('black', 'lightseagreen','red','green4'), 
                       labels=c('+1.5°C','+3°C','CC','Super CC'))+
    scale_fill_manual('', breaks=c(paste0('f',dur,'1.5'),paste0('f',dur,'3'),paste0('f',dur,'CC'),paste0('f',dur,'2CC')),
                      values=c('black', 'lightseagreen','red','green4'))+
    scale_linetype_manual('', breaks=c(paste0('f',dur,'1.5'),paste0('f',dur,'3'),paste0('f',dur,'CC'),paste0('f',dur,'2CC')),
                          values=c('solid','dashed','dotdash','dotted'),labels=c('+1.5°C','+3°C','CC','Super CC'))+
    scale_x_continuous(labels=means$return_period, breaks=means$ticks)+ #set y axis ticks
    ylim(range)+  #apply the uniform range
    theme(text=element_text(size=16),#set sizes of various plot elements
          axis.text = element_text(size=16), 
          axis.title = element_text(size=17),
          legend.text = element_text(size = 18),
          legend.key.size = unit(45, 'pt')) 
  
  #remove legend if needed
  if(!legend){
    g<-g+theme(legend.position = 'none')
  }
  
  #add title if needed
  if(title){
    if(relative){
      if(dur=='1h'){
        title=paste(sub('h',' h', dur),sep='') #region name and duration
      }else{
        title=paste(sub('h',' h', dur), sep='') #only duration to not duplicate region name
      }
    }else{
      if(dur=='1h'){
        title=paste('\n\n', sub('h',' h', dur),sep='') #region name and duration
      }else{
        title=paste('\n\n', sub('h',' h', dur), sep='') #only duration to not duplicate region name
      }
    }
    
    g<-g+ggtitle(title)
  }
  
  #apply log scale if needed
  if(log){
    g<-g+scale_y_continuous(trans='log10', limits=range)
  }
  
  #save if needed
  if(save){
    ggsave(paste("Fischer",dur,region,".pdf", sep=''),g, width=10.5, height = 7, units = 'in')
  }
  return(g)
}

#PATH: to edit
setwd("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/climate_signal_frequency_Fischer/presaved figures/")

plot_frequency_region(dur='24h',region='ME', include2CC = FALSE, save = FALSE,
                      relative = FALSE, log=FALSE)




#plot both durations 1h and 24h in one plot with two panels
#parameters: see function above, are just handed over
plot_freq_both_dur_region<-function(save=FALSE, relative, region, percentile_threshold=0.9975, include2CC=FALSE, log=FALSE, title=TRUE){
  
  plotlist=lapply(c('1h','24h'), plot_frequency_region,region=region, include2CC=include2CC,percentile_threshold=percentile_threshold,
                  legend=FALSE, title=title, relative=relative, log=log) #create list of plots
  legend=get_legend(plot_frequency_region(dur='1h', region=region, relative = relative, include2CC = include2CC)) #get legend
  plotlist[[3]]=legend #add legend to plotlist
  g=plot_grid(plotlist = plotlist, nrow=1, rel_widths = c(1,1,0.3)) #plot in grid
  
  #save if needed
  if(save){
    if(relative){rel="_relative"} #specifiers for the name of the file
    else{rel=''}
    if(log){log="_log"}
    else{log=''}
    ggsave(paste0("freq_both_dur_", region, rel,log,".pdf"), g, width=15, height = 7, units = 'in')
  }
  return(g)
}

#a test
plot_freq_both_dur_region(save=FALSE, region='BI', relative = TRUE)

#little loop to plot and save all regions at once
regions=c('BI','EA','IP','FR','SC','AL','MD','ME')
for(i in regions){
  plot_freq_both_dur_region(save=TRUE, region=i, relative = TRUE, 
                            include2CC = FALSE, percentile_threshold = 0.995)
  plot_freq_both_dur_region(save=TRUE, region=i, relative = FALSE, 
                            include2CC = FALSE, percentile_threshold = 0.995)
}

#plot the absolute and relative frequencies in one panel (so contains 4 plots: abs and rel for 1h and 24h)
#again, parameters are just handed over to the fucntions above
plot_freq_both_dur_region_abs_rel<-function(save=FALSE, region,log=FALSE, percentile_threshold=0.995, include2CC=FALSE){
  #plot the relative version
  rel<-plot_freq_both_dur_region(region=region, relative = TRUE, 
                                 percentile_threshold = percentile_threshold,
                                 include2CC = include2CC, log=log, title = TRUE)
  #plot the absolute version
  abs<-plot_freq_both_dur_region(region=region, relative = FALSE, 
                                 percentile_threshold = percentile_threshold,
                                      include2CC = include2CC, log=FALSE)
  #set hjust depending on region name, to center title
  if(region=='ME'|region=='SC'){hjust=-3.6}
  else if(region=='BI'){hjust=-3.8}
  else if(region=='FR'){hjust=-6.6}
  else if(region=='MD'){hjust=-3}
  else if(region=='IP'){hjust=-2.4}
  else if(region=='EA'){hjust=-2.7}
  else if(region=='AL'){hjust=-10.9}
    
  g=plot_grid(abs, rel, ncol=1, rel_heights = c(1.1,1), labels=paste(regionnames[region]),
              label_fontface ='plain', label_size = 22, hjust = hjust) #put these in grid
  
  #save if needed
  if(save){
    if(log){log="_log"} #specifier for file name: log or not
    else{log=''}
    ggsave(paste0("freq_both_dur_", region,"_rel_and_abs",log,".pdf"), g, 
            width=15, height = 10, units = 'in')
  }
  return(g)
}
plot_freq_both_dur_region_abs_rel(save = TRUE, region='AL', log=FALSE, 
                                  percentile_threshold = 0.995)


#loop over all regions
for(i in regions){
  plot_freq_both_dur_region_abs_rel(save = TRUE, region=i)
}

