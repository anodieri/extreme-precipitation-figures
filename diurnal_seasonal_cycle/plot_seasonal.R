#############################################################################################
## Plot the seasonal cycle for Belgium
## for EURO-CORDEX models
#############################################################################################
## Use: adapt PATHs where needed
## for plot with seasonal and diurnal: use in combination with plot_seasonal.R
#############################################################################################


# Load libraries ----------------------------------------------------------
library(ggplot2)
library(stringr)
library(reshape2)
library(cowplot)

# Observations ------------------------------------------------------------

#read in observations
#PATH: EDIT IF NEEDED
Seasonal.obs <- readRDS("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/diurnal_seasonal_cycle/data/OBS_seasonal.Rda")

#create frequency table for observations
freq_table_obs_month=data.frame(1:12) #dataframe with seasons
colnames(freq_table_obs_month)<-c('Month') 

#loop over stations to fill in frequency table
# and count number of observations in the meantime
n_obs=0 
for(i in 1:length(Seasonal.obs)){
  freq_var=as.data.frame(table(factor(Seasonal.obs[[i]]$month1h, levels = 1:12))) #construct table with counts
  colnames(freq_var)<-c('Month',Seasonal.obs[[i]]$station) #set name of the variable as the location of the table
  freq_table_obs_month=merge(freq_table_obs_month, freq_var, by='Month') #merge into existing frequency table
  n_obs=n_obs+sum(!is.na(Seasonal.obs[[i]]$month1h)) #dont count na's
  #length(Seasonal.obs[[i]]$month1h) # alternative: do count na's
}

#create columns with overall sum absolute frequencies over all stations, divide by total number of obs and times 100 to get a percentage
freq_table_obs_month$Observations=rowSums(x=freq_table_obs_month[,2:20], na.rm = TRUE)/n_obs*100

freq_table_obs=data.frame(c("Winter","Spring","Summer","Autumn")) #dataframe with seasons
colnames(freq_table_obs)<-c('Season')
#convert months to seasons
freq_winter=freq_table_obs_month$Observations[12]+freq_table_obs_month$Observations[1]+freq_table_obs_month$Observations[2]
freq_spring=freq_table_obs_month$Observations[3]+freq_table_obs_month$Observations[4]+freq_table_obs_month$Observations[5]
freq_summer=freq_table_obs_month$Observations[6]+freq_table_obs_month$Observations[7]+freq_table_obs_month$Observations[8]
freq_autumn=freq_table_obs_month$Observations[9]+freq_table_obs_month$Observations[10]+freq_table_obs_month$Observations[11]

#set frequencies
freq_table_obs$Frequency=c(freq_winter, freq_spring,freq_summer, freq_autumn)

#label the observations as observations
freq_table_obs$Model="Observations"

# Load data EUROCORDEX and convert to months ------------------------------------------

#load output of get_times_am.R
#PATH: TO EDIT IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/diurnal_seasonal_cycle/data/times_bel_am.RData")
times=times[, colSums(is.na(times)) != nrow(times)]#remove col with only NA

times_dates=as.data.frame(lapply(times, as.Date, origin="1949-12-01")) #convert to dates, starting from specified date
times_months=format(times_dates,"%m") #get months
colnames(times_months)<-colnames(times) #column names also got altered by above transformations, so should be set back to original

# Convert to seasons ------------------------------------------------------

#meteorological seasons : winter =dec, jan, feb, spring= mar, apr, may, summer= jun, jul, aug, autumn= sep, okt, nov
seasons=times_months
seasons[seasons=="01"|seasons =="02"|seasons=="12"]='Winter'
seasons[seasons=="03"|seasons =="04"|seasons=="05"]='Spring'
seasons[seasons=="06"|seasons =="07"|seasons=="08"]='Summer'
seasons[seasons=="09"|seasons =="10"|seasons=="11"]='Autumn'


# exception HadGEM --------------------------------------------------------

#the runs with GCM HadGEM use a 360 day calendar instead of a normal one
#so after some time, seasons deviate and the above procedure to get the seasons does not work
hadgems=c("MOHC-HadGEM2-ES#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r1i1p1", "MOHC-HadGEM2-ES#SMHI-RCA4_v1#r1i1p1")

times_hadgems=times[, hadgems] #select the times for these models
times_hadgems=times_hadgems-30 #subtract 30 days to start in january instead of december (start date "1949-12-01" )

times_days_hadgems=times_hadgems%%360 #remove all 'full' 360 day years

seasons_hadgems=times_days_hadgems #make a copy

#define hadgem seasons
seasons_hadgems[times_days_hadgems<=60|times_days_hadgems >330]='Winter'
seasons_hadgems[times_days_hadgems>60 &times_days_hadgems <=150]='Spring'
seasons_hadgems[times_days_hadgems>150 & times_days_hadgems <=240]='Summer'
seasons_hadgems[times_days_hadgems>240 & times_days_hadgems<=330]='Autumn'

seasons[, hadgems]=seasons_hadgems #put theses seasons into the general table with seasons

# Frequency table ---------------------------------------------------------

freq_table=data.frame(c("Winter","Spring","Summer","Autumn")) #dataframe with seasons
colnames(freq_table)<-c('Season') 

#get number of years times gridpoints for each model (when the model does not have an annual max for a year, the table contains NA)
n_years=as.data.frame(colSums(!is.na(times_months)))

#loop over variables 
for(varname in colnames(seasons)){
  freq_var=as.data.frame(table(seasons[,varname])) #construct table with counts
  colnames(freq_var)<-c('Season',varname) #set name of the variable as colname
  freq_var[,varname]=freq_var[,varname]/n_years[varname,]*100 #divide by the number of annual maxima for this model and multiply with 100 to get percentages
  freq_table=merge(freq_table, freq_var, by='Season') #merge into existing frequency table
}

# Merge different runs ----------------------------------------------------
#some RCM-GCM combinations are run multiple times, these are merged
# the counts are added and divided by the number of runs (for correct comparison with RCM-GCM combos with only 1 run)
# otherwise this would not be percentages later on when dividing by the number of rows in times_hour

freq_table$"ICHEC-EC-EARTH#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r"<-(freq_table$"ICHEC-EC-EARTH#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r12i1p1"+      
                                                                  freq_table$"ICHEC-EC-EARTH#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r1i1p1" +      
                                                                  freq_table$"ICHEC-EC-EARTH#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r3i1p1")/3
freq_table[,c("ICHEC-EC-EARTH#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r1i1p1","ICHEC-EC-EARTH#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r12i1p1", "ICHEC-EC-EARTH#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r3i1p1")]<-NULL

freq_table$"MPI-M-MPI-ESM-LR#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r"<-(freq_table$"MPI-M-MPI-ESM-LR#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r1i1p1" +   
                                                                    freq_table$"MPI-M-MPI-ESM-LR#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r2i1p1" +    
                                                                    freq_table$"MPI-M-MPI-ESM-LR#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r3i1p1")/3
freq_table[,c("MPI-M-MPI-ESM-LR#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r2i1p1","MPI-M-MPI-ESM-LR#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r3i1p1", "MPI-M-MPI-ESM-LR#CLMcom-ETH-COSMO-crCLIM-v1-1_v1#r1i1p1")]<-NULL

freq_table$"ICHEC-EC-EARTH#SMHI-RCA4_v1#r"<-(freq_table$"ICHEC-EC-EARTH#SMHI-RCA4_v1#r12i1p1"+                         
                                               freq_table$"ICHEC-EC-EARTH#SMHI-RCA4_v1#r1i1p1"+                          
                                               freq_table$"ICHEC-EC-EARTH#SMHI-RCA4_v1#r3i1p1")/3
freq_table[,c("ICHEC-EC-EARTH#SMHI-RCA4_v1#r12i1p1","ICHEC-EC-EARTH#SMHI-RCA4_v1#r1i1p1","ICHEC-EC-EARTH#SMHI-RCA4_v1#r3i1p1")]<-NULL

freq_table$"MPI-M-MPI-ESM-LR#SMHI-RCA4_v1#r"<-(freq_table$"MPI-M-MPI-ESM-LR#SMHI-RCA4_v1#r1i1p1"+                        
                                                 freq_table$"MPI-M-MPI-ESM-LR#SMHI-RCA4_v1#r2i1p1")/2
freq_table[,c("MPI-M-MPI-ESM-LR#SMHI-RCA4_v1#r1i1p1", "MPI-M-MPI-ESM-LR#SMHI-RCA4_v1#r2i1p1")]<-NULL


# Convert to long frequency table -----------------------------------------


freq_table=melt(freq_table, id.vars = 'Season') #convert to long format
colnames(freq_table)<-c('Season','Model','Frequency') #set colnames



# plotting ----------------------------------------------------------------

#define colors and alphas
rcmnames=c("Observations","SMHI-RCA4_v1","CLMcom-ETH-COSMO-crCLIM-v1-1_v1")
rcmcols=c('black','lightseagreen','darkblue') #hcl.colors(6, palette = "Viridis")  #either choose to be same as other plots or same as seasonal cycle (viridis)
names(rcmcols)=rcmnames

rcmalphas=c(rep(1,3))
names(rcmalphas)=rcmnames


#plot both rcms at once
plot_both_rcms<-function(save=FALSE){
  
  #select data
  data=freq_table
  data$rcm<-sapply(str_split(data$Model, pattern='#'), function(x) (x[2])) #shorten names: only GCM needed in legend
  #split by rcm
  data_rca=subset(data, rcm=="SMHI-RCA4_v1")
  data_cosmo=subset(data, rcm=="CLMcom-ETH-COSMO-crCLIM-v1-1_v1")
  #calculate means by rcm
  mean_rca<-aggregate(Frequency~Season, data_rca, FUN=mean)
  mean_cosmo<-aggregate(Frequency~Season, data_cosmo, FUN=mean)
  #add column with Model again
  mean_rca$Model<-"SMHI-RCA4_v1"
  mean_cosmo$Model<-"CLMcom-ETH-COSMO-crCLIM-v1-1_v1"
  #merge the two datasets again, with observations as well
  data<-rbind(mean_rca, mean_cosmo, freq_table_obs)
  
  #actual plotting
  g<-ggplot(data)+
    geom_col(aes(x=factor(Season, levels=c("Winter","Spring","Summer", "Autumn")),
                 y=Frequency, fill=factor(Model, levels=rcmnames), 
                 group=factor(Model,levels=rcmnames), alpha= Model), 
             position = "dodge2")+ #plot bars
    ylab("Frequency (%)")+
    xlab("Season")+
    scale_fill_manual("RCM",values = rcmcols, breaks = rcmnames[-1],labels=c('RCA4','COSMO'))+
    scale_alpha_manual("",values = rcmalphas, breaks = "Observations",labels=c('Obser-\nvations'))+ #trick to get observations separately in legend)+
    theme(text=element_text(size=12), #set text size
          panel.grid.major.x = element_blank(),#remove vertical lines
          legend.position = "bottom")+ #put legend on bottom
    guides(fill=guide_legend(nrow=2,byrow=TRUE, order = 2),
           alpha=guide_legend(order = 1, override.aes = list( alpha=1, fill='black'))) #put legend on two rows
  
  #save if required
  if(save){
    ggsave(paste("Seasonal_Belgium.pdf", sep=''),g, width=12, height = 7, units = 'in')
  }
  
  return(g)
}

plot_both_rcms()

#save the seasonal plot
seasonal_plot<-plot_both_rcms(save=FALSE)

#load the diurnal plot, produced with plot_diurnal.R
#PATH: TO EDIT IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/diurnal_seasonal_cycle/diurnal_plot.RData")

#load the elevation correlation plot, produced with elevation_correlation_plot.R
#PATH: TO EDIT IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/elevation_correlation/elevation_plot.RData")


#plot seasonal, diurnal and elevation correlation plots next to each other in a grid
g<-plot_grid(diurnal_plot, seasonal_plot, elevation_plot, rel_widths = c(1,0.85,0.8),nrow=1)
g

#save this joined plot
#PATH: EDIT IF NEEDED
setwd("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/diurnal_seasonal_cycle")
ggsave(filename = 'diurnal_seasonal_elevation.pdf', g, width=11.2, height=4, units='in')
