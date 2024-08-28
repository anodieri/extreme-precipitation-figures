#################################################################
## Plotting the climate signal for EURO-CORDEX
## Plots boxplots per region of change in 10-year return level
## for both mean before relative difference and the other
## way around
#################################################################
## every path is marked with "PATH", these should be edited 
#################################################################

# load libraries ----------------------------------------------------------

library(ggplot2)#to plot
library(reshape2)#to merge datasets
library(scico) #color blind friendly palettes for ggplot
scico_palette_show()

# plot precomputed percentages over all regions ---------------------------------------------------------
# precomputed= first relative difference, then mean over gridpoints

#initialise dataframe to merge all regions
data_all_regions=data.frame()

#list all region abbreviations
regions=c('BI','EA','IP','FR','SC','AL','MD','ME')

#fill this dataframe
for(region in regions){
  #load file for each region
  #PATH: to edit
  file=paste("C:/Users/Anouk/Documents/cursussen/MP/europa/climate change/results/relative_difference_",region,".RData", sep='')
  data=get(load(file)) #get data
  data <- subset(data, !(run=='r1i1p1' & gcm=='ICHEC-EC-EARTH')) #remove the runs for which no period for +3°C was given
  data$region=region #set the region specifier
  data_all_regions=rbind(data_all_regions,data) #add current region to dataframe
}

#possibility to save this
#save(data_all_regions, file='C:/Users/Anouk/Documents/cursussen/MP/europa/climate change/results/relative_difference_all_regions.RData')

#melt data into long format
data_all_regions_long<-data_all_regions #rename data
data_all_regions_long$'12h'<-NULL #remove 12h
data_all_regions_long<-melt(data_all_regions_long, id=c('gcm','rcm','run','region'), value.name = 'rl')
names(data_all_regions_long)[names(data_all_regions_long) == 'variable'] <- 'dur' #melt into long format
data_all_regions_long$dur<-sub('h',' h',data_all_regions_long$dur) #add space between dration and h


#plot all regions, boxplots for both 1h and 24h
#precomputed percentages: first relative difference, then mean over gridpoints
plot_all_regions_both_dur<-function(save=FALSE){
  
  g<-ggplot(data = data_all_regions_long)+
    geom_hline( aes(yintercept = 7,linetype='CC'))+ #plotting CC reference line
    #geom_hline(aes(yintercept = 14, linetype='Super CC'))+ #plotting super CC reference line
    geom_boxplot(mapping=aes(x=region,y=data_all_regions_long[,'rl']*100/1.5, fill=dur),  
                 alpha=1)+ #converted to % per °C
    xlab('PRUDENCE region')+
    ylab(expression(paste('Change in 10-year return level (% per °C warming)')))+
    scale_fill_scico_d('Duration',palette = 'nuuk')+ #set color palette
    scale_linetype_manual('', values=c('dashed','solid'))+ #little hack to add legend for CC, dashed line
    theme(text=element_text(size=16), #set text size
          legend.key.size = unit(1, 'cm'))+ #set legend key size 
    guides(colour = guide_legend(order = 2), linetype = guide_legend(order = 1)) #set legend order
  
  #save if needed
  if(save){
    ggsave(paste("climate_signal_all_regions_both_dur_rel_diff_before_mean_no_super_CC.pdf", sep=''),g, width=12, height = 6, units = 'in' )
  }
  return(g)
}

#set working directory where plots will be saved
#PATH: to edit
setwd("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/climate_signal_intensity/presaved figures")

plot_all_regions_both_dur()

plot_all_regions_both_dur(save = TRUE)


# compare percentages computed after mean over all regions ---------------------------------------------------------

#similar set up: make new dataframe containing all regions
data_all_regions_rel_after_mean=data.frame()

#list region abbreviations
regions=c('BI','EA','IP','FR','SC','AL','MD','ME')

#fill this dataframe
for(region in regions){
  #load data
  #PATH: to edit
  load(paste("C:/Users/Anouk/Documents/cursussen/MP/europa/climate change/results/results1.5_",region,".RData", sep=''))
  load(paste("C:/Users/Anouk/Documents/cursussen/MP/europa/climate change/results/results3_",region,".RData", sep=''))
  results1.5 <- subset(results1.5, !(run=='r1i1p1' & gcm=='ICHEC-EC-EARTH')) #remove the runs for which no period for +3°C was given
  results3 <- subset(results3, !(run=='r1i1p1' & gcm=='ICHEC-EC-EARTH')) #remove the runs for which no period for +3°C was given
  percentage=results3 #copy data
  percentage$rcm=sub(".*?-","",percentage$rcm) #remove institute name from rcm name
  percentage$rcm[percentage$rcm=='ETH-COSMO-crCLIM-v1-1']<-sub(".*?-","",percentage$rcm[percentage$rcm=='ETH-COSMO-crCLIM-v1-1']) #cut off ETH 
  percentage[4:6]=(percentage[4:6]-results1.5[4:6])/results1.5[4:6]*100 #calculate percentage change
  percentage$region=region#set the region specifier
  data_all_regions_rel_after_mean=rbind(data_all_regions_rel_after_mean,percentage) #merge
}

#option to save this dataframe
#save(data_all_regions_rel_after_mean, file='C:/Users/Anouk/Documents/cursussen/MP/europa/climate change/results/relative_difference_after_mean_all_regions.RData')

#melt data into long format for next plot
data_all_regions_rel_long<-data_all_regions_rel_after_mean #copy data
data_all_regions_rel_long$'12h'<-NULL #remove 12h
#melt into long format
data_all_regions_rel_long<-melt(data_all_regions_rel_long, id=c('gcm','rcm','run','region'), value.name = 'rl')
names(data_all_regions_rel_long)[names(data_all_regions_rel_long) == 'variable'] <- 'dur'#rename variable
data_all_regions_rel_long$dur<-sub('h',' h',data_all_regions_rel_long$dur) #add space between duration and h

#plot all regions, both durations for relative differences computed after mean over gridpoints
plot_all_regions_both_dur<-function(save=FALSE){
  #plotting
  g<-ggplot(data = data_all_regions_rel_long)+
    geom_hline( aes(yintercept = 7,linetype='CC'))+ #CC reference line
    #geom_hline(aes(yintercept = 14, linetype='Super CC'))+ #super CC reference line
    geom_boxplot(mapping=aes(x=region,y=data_all_regions_rel_long[,'rl']/1.5, fill=dur),  #boxplots 
                 alpha=1)+ #converted to % per °C
    xlab('PRUDENCE region')+
    ylab(expression(paste('Change in 10-year return level (% per °C warming)')))+
    scale_fill_scico_d('Duration',palette = 'nuuk')+ #color palette
    scale_linetype_manual('', values=c('dashed','solid'))+ #little hack to add legend for CC, dashed line
    theme(text=element_text(size=16), #set text size
          legend.key.size = unit(1, 'cm'))+ #set legend key size 
    guides(colour = guide_legend(order = 2), #set legend order
           linetype = guide_legend(order = 1))
  
  #save if needed
  if(save){
    ggsave(paste("climate_signal_all_regions_both_dur_rel_diff_after_mean_no_super_CC.pdf", sep=''),g, width=12, height = 6, units = 'in' )
  }
  
  return(g)
}

plot_all_regions_both_dur()

plot_all_regions_both_dur(save = TRUE)

