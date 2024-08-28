################################################
## Calculate correlations between elevation
## and return levels for Belgium, for EUROCORDEX
## models and observations and plot them
################################################

# load libraries ----------------------------------------------------------

library(ggplot2) #to plot
library(cowplot) #to plot multiple figures in a grid
library(stringr) #to search in strings
library(scico) #color scales


# load data ---------------------------------------------------------------

#load data
#choose between 10 year and 2 year return levels
#10 year return levels
#PATH: TO EDIT IF NEEDED
#load("C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/output_bel_merge_with_altitude_ARF.RData")

#2 year return levels:
#PATH: TO EDIT IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/output_bel_2yr_elevation.RData")
#no ARFs applied, but observations are not used from this file, only elevation and models

data<-output


#create dataframe to save correlations for the EUROCORDEX models ------------------------------------

#select the variable names that contain all of the search_strings in a vector/list
#each varname corresponds to a model run
#also works with just one string
select_names<-function(search_strings){
  names=colnames(data) #save the names of all variables in the file
  for(s in search_strings){ #for each of the given strings
    names=names[str_detect(names, s)] #select the names that contain this string
  }
  return(names) #return the final selection
}

#select the variable names containing return levels
varnames<-select_names('returnlevels')

#create dataframe with as many columns as varnames
cors<-data.frame(matrix( nrow=length(varnames),ncol=5))
colnames(cors)<-c('gcm','rcm','run','dur','correlation') #set column names

# calculate correlations for EUROCORDEX models--------------------------------------------------

for(i in 1:length(varnames)){
  varname=varnames[i] #get the variable name
  parts=str_split(varname, pattern='#') #split it up
  cors$gcm[i]=parts[[1]][2] #get the gcm and save it
  cors$rcm[i]=parts[[1]][3] #get the rcm and save it
  cors$run[i]=parts[[1]][4] #get the run and save it
  cors$dur[i]=sub('returnlevels','',parts[[1]][1]) #extract the duration and save it
  cors$correlation[i]=cor(data[,varname],data$H ) #calculate the correlation with the elevation
}


#load observational correlations with CI ----------------------------------

#choose which observational data to use:

#produced with Corr_bootstrap_SETHY.R
#2 year return level SETHY data: only for 1h, needs next one as well for 24h
#PATH: EDIT IF NEEDED
cors_obs_SETHY<-get(load('C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/cors_obs_SETHY_2year.RData'))

#produced with Corr_bootstrap.R
#2 year return levels
#PATH: EDIT IF NEEDED
cors_obs<-get(load('C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/cors_obs_2year.RData'))

#produced with Corr_bootstrap.R
#10 year return levels
#load('C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/cors_obs_10year.RData')


#convert columns to numeric
cors_obs$correlation<-as.numeric(cors_obs$correlation)
cors_obs$lower<-as.numeric(cors_obs$lower)
cors_obs$upper<-as.numeric(cors_obs$upper)

#convert more columns to numeric
cors_obs_SETHY$correlation<-as.numeric(cors_obs_SETHY$correlation)
cors_obs_SETHY$lower<-as.numeric(cors_obs_SETHY$lower)
cors_obs_SETHY$upper<-as.numeric(cors_obs_SETHY$upper)


# plotting all durations ----------------------------------------------------------------
#using data from climatological and hydrological network


#some tweaking to be able to get legends in plot
cors_obs$shape<-'star' 
cors$fill<-'star'

#plotting
g<-ggplot()+
  geom_boxplot(cors,mapping=aes(y=correlation,fill=fill,
                                x=factor(dur, levels=c('1h','6h','12h','24h','48h','72h'))), 
               color='gray45', staplewidth = 0)+ #staplewidth: recent version ggplot needed
  geom_point(cors_obs, mapping=aes(y=correlation, shape=shape,
                                  x=factor(dur, levels=c('1h','6h','12h','24h','48h','72h'))), size=2)+
  geom_errorbar(cors_obs,mapping=aes(ymin = lower, ymax = upper,y=correlation,
                                     x=factor(dur, levels=c('1h','6h','12h','24h','48h','72h'))),width=0.15)+
  #position= position_nudge(x=-.05) #moves slightly to left
  scale_shape_manual('', labels=c('Observations'), values=c(15))+ #to get legend for obs
  scale_fill_scico_d('',labels=c('EURO-CORDEX\nmodels'), palette = 'nuuk')+ #set color palette
  ylab('Correlation with elevation')+
  xlab('Duration (h)')+
  ylim(c(-0.6,1))+ #set limits 
  scale_x_discrete(labels=c('1','6','12','24','48','72'))+ #adjust x axis ticks
  theme(text=element_text(size=8), #adjust text size and so on
              axis.text = element_text(size=8), 
              axis.title = element_text(size=9),
              legend.text = element_text(size = 10),
              legend.key.size = unit(0.7, 'cm')) 
  
g

#save the plot
#PATH: EDIT IF NEEDED
ggsave("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/elevation_correlation/presaved figures/2y_correlationboxplots.pdf",
       g, width=8, height = 3, units = 'in', dpi = 300)  


# plotting only 1h and 24h with SETHY -------------------------------------

#prepare data frame
cors_obs_1h_24h<-cors_obs #copy observational correlations
cors_obs_1h_24h[1,]<-cors_obs_SETHY #insert results from SETHY for duration 1h
cors_obs_1h_24h<-subset(cors_obs_1h_24h, dur=='1h'|dur=='24h') #select durations 1h and 24h

cors_1h_24h<-subset(cors, dur=='1h'|dur=='24h')

#some tweaking to be able to get legends in plot
cors_obs_1h_24h$shape<-'star' 
cors$fill<-'star'

#plotting
g<-ggplot()+
  #boxplots for the models:
  geom_boxplot(cors_1h_24h,mapping=aes(y=correlation,fill=fill,x=factor(dur, levels=c('1h','6h','12h','24h','48h','72h'))), 
               color='gray45')+
  #points for the observations
  geom_point(cors_obs_1h_24h, mapping=aes(y=correlation, shape=shape,
                                   x=factor(dur, levels=c('1h','6h','12h','24h','48h','72h'))), size=2)+
  #errorbars for the observations
  geom_errorbar(cors_obs_1h_24h,mapping=aes(ymin = lower, ymax = upper,y=correlation,x=factor(dur, levels=c('1h','6h','12h','24h','48h','72h'))),width=0.15)+
  scale_shape_manual('', labels=c('Obser-\nvations'), values=c(15))+ #to get legend for obs
  scale_fill_manual('',labels=c('EURO-\nCORDEX\nmodels'), values = 'lightseagreen')+ #set color palette
  #scale_fill_scico_d('',labels=c('EURO-CORDEX\nmodels'), palette = 'nuuk')+ #set color palette
  ylab('Correlation with elevation')+
  xlab('Duration (h)')+
  ylim(c(-0.6,1))+ #set limits 
  scale_x_discrete(labels=c('1','24'))+ #adjust x axis ticks
  theme(text=element_text(size=12), #adjust text size and so on
        legend.position = "bottom", #put legend on the bottom
        legend.key.size = unit(0.7, 'cm')) 

g

#save the plot
#PATH: EDIT IF NEEDED
ggsave("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/elevation_correlation/presaved figures/2y_correlationboxplots_1h_24h.pdf",
       g, width=5, height = 3, units = 'in', dpi = 300)  

#save ggplot object of the figure
#to combine with diurnal and seasonal plot
elevation_plot<-g
save(elevation_plot, 
     file="C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/elevation_correlation/elevation_plot.RData")


