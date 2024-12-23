#########################################################
## Plotting tables of global statistics for returnlevels
## for EURO-CORDEX models
#########################################################

########################################################
#load required libraries
#########################################################

library(ggplot2) # to plot
library(colorspace) # for color scales
library(cowplot) # to make grids of plots

#######################################################
#Preliminary names 
#######################################################

#dictionary with full names countries to use in titles of plots
country_names <- c(
  'uk' = 'United Kingdom',
  'bel' = 'Belgium',
  'dui' = 'Germany',
  'ned' = 'The Netherlands',
  'den' = 'Denmark',
  'fin' = 'Finland'
)

##############################################################
## Plot one big table containing all countries for bias (%)
##############################################################

#load summaries for all countries
#EDIT PATHS IF NEEDED
load("summaries/output_uk_merge_summary_ARF.RData")
load("summaries/output_bel_merge_summary_ARF.RData")
load("summaries/output_dui_merge_summary_ARF.RData")
data_dui<-summary
load("summaries/output_fin_merge_summary_ARF.RData")
load("summaries/output_den_merge_summary_ARF.RData")
load("summaries/output_ned_merge_summary_ARF.RData")

#calculate limits for the scales of all tables
range_bias_percentage=range(data_uk$bias_percentage1h, data_uk$bias_percentage24h, data_bel$bias_percentage1h,data_bel$bias_percentage24h,
      data_den$bias_percentage1h, data_den$bias_percentage24h, data_fin$bias_percentage1h, data_fin$bias_percentage24h,
      data_ned$bias_percentage1h, data_ned$bias_percentage24h, data_dui$bias_percentage1h, data_dui$bias_percentage24h)

#symmetrize the scale
range_bias_percentage=c(-max(abs(range_bias_percentage)),max(abs(range_bias_percentage)))

#function to get the shortened gcm name to use as row title
make_title_gcm<-function(gcm){
  #manual since all abbreviations are different
  if(gcm=="MPI-M-MPI-ESM-LR"){title='MPI-ESM'}
  else if(gcm=='MOHC-HadGEM2-ES'){title='HadGEM2'}
  else if(gcm=='ICHEC-EC-EARTH'){title='EC-EARTH'}
  else if(gcm=='NCC-NorESM1-M'){title='NorESM'}
  else if(gcm=='IPSL-IPSL-CM5A-MR'){title='IPSL-CM5A'}
  else if(gcm=='CNRM-CERFACS-CNRM-CM5'){title='CNRM-CM5'}
  return(title)
}


#plot tables as part of big table for eurocordex models for specified duration and country
#country should be uk, bel, ned, den, fin or dui
#dur should be 1h, 12h or 24h
plot_table_partial<-function(dur, country,legend=FALSE, ylabs=TRUE){
  
  #select summary dataframe depending on country
  if(country=='uk'){
    summary<-data_uk
  }else if(country=='bel'){
    summary<-data_bel
  }else if(country=='dui'){
    summary<-data_dui
  }else if(country=='fin'){
    summary<-data_fin
  }else if(country=='den'){
    summary<-data_den
  }else if(country=='ned'){
    summary<-data_ned
  }
  
  #get the name of the column from the summary frame that will be used to create the table
  colname=paste('bias_percentage', dur, sep = '')
  
  #shorten the rcm names to not include institute anymore
  summary$rcm<-sub(".*?-","",summary$rcm)
  summary$rcm[summary$rcm=='ETH-COSMO-crCLIM-v1-1']<-sub(".*?-","",summary$rcm[summary$rcm=='ETH-COSMO-crCLIM-v1-1'])
  summary$rcm<-sub("-.*","",summary$rcm) #shorten names even further
   
  #construct gcm names using function defined above
  summary$gcm<-sapply(summary$gcm, FUN = make_title_gcm)
  
  #construct title
  if(dur=='1h'){
    #country_names[country],
    title=paste(  "\n                  ", sub('h',' h', dur), sep="")
  }else{
    title=paste("\n               ", sub('h',' h', dur))
  }
  
  #plot the table 
  g<-ggplot(summary, aes(rcm, gcm)) +   
    geom_tile(aes(fill = summary[,colname]))+ #plot tiles
    geom_text(aes(label = round(summary[,colname],digits = 1)), size=7.5)+ #add numbers in  tiles
    coord_equal()+ #make tiles square
    ggtitle(title)+ #add title
    theme(text=element_text(size=28), legend.key.size = unit(4, 'cm'),  #set size of various plot elements
          plot.title = element_text(size = 38),
          axis.text.x = element_text(angle=20, vjust = 0.9, hjust = 0.8),
          panel.grid.major = element_blank(), #remove grid
          axis.title = element_blank(), #remove axis title
          panel.grid.minor = element_blank(), #remove minor grid
          panel.background = element_rect(fill = 'gray99'), #white background
          legend.title = element_text(size=32),
          legend.text = element_text(size=28))+
    #add color scale
    scale_fill_binned_diverging(name='Bias (%)', rev=TRUE, #reverse color palette
                                 palette = "Blue-Red2", n.breaks=14,
                                 limits=c(round(range_bias_percentage[1], digits = 1), 
                                          round(range_bias_percentage[2], digits = 1)))
  
  
  #remove legend if needed
  if(legend==FALSE){
    g=g+theme(legend.position = "none")
  }
  
  #remove y labels if needed
  if(ylabs==FALSE){
    g=g+theme(axis.text.y=element_blank(),
              axis.ticks.y = element_blank())
  }
  
  return(g)
}

plot_table_partial(dur='1h', country='bel', ylabs = TRUE, legend = TRUE)


#plot the full table, containing all rcm-gcm combinations
#save: should the plot be saved?
plot_mega_table<-function(save=FALSE){
  
  #create list of tables for all countries for both durations
  #only ylabels for left bordering plots
  countries=c('bel','dui','uk','den','ned','fin')#list all countries
  plotlist1h=c(lapply(countries[1:3],plot_table_partial,dur='1h'),
               lapply(countries[4:6],plot_table_partial,dur='1h', ylabs=FALSE))
  plotlist24h=lapply(countries, plot_table_partial, dur='24h', ylabs=FALSE)
  
  #reorder to 1h next to 24h and no labels at left side
  plotlist=c(plotlist1h[1], plotlist24h[1], plotlist1h[5], plotlist24h[5], plotlist1h[3], plotlist24h[3],
             plotlist1h[4], plotlist24h[4], plotlist1h[2], plotlist24h[2], plotlist1h[6], plotlist24h[6])
  
  #plot in raster
  raster<-plot_grid(plotlist= plotlist, 
                    rel_widths = rep(c(1.28,1,1,1),4), ncol=4) #adapt rel heights s.t. without labels same size
  #get legend
  legend <- get_legend(plot_table_partial('bel',dur='1h',legend = TRUE))#get legend from the first plot in the list
  #plot legend and raster
  g<-plot_grid(raster,legend, nrow = 1, rel_widths = c(0.9,0.1))
  
  #save if needed
  if(save){
      ggsave(paste("table_blank_middle.pdf", sep=''),g, width=30, height =25 , units = 'in')
  }
  return(g)
}
plot_mega_table(save=FALSE)

#EDIT PATH IF NEEDED
setwd("C:/Users/dierickx/OneDrive - vki.ac.be/Documents/master/MP/extreme precipitation figures/mosaic_tables")

#save the mega table
plot_mega_table(save=TRUE)


