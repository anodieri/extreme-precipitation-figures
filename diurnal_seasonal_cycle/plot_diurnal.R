#############################################################################################
## Plot the diurnal cycle for Belgium
## for EURO-CORDEX models
#############################################################################################
## Use: adapt PATHs where needed
## for plot with seasonal and diurnal: use in combination with plot_seasonal.R
#############################################################################################

# Load libraries ----------------------------------------------------------
library(ggplot2)
library(stringr)
library(reshape2)

# Observations ------------------------------------------------------------

#read in observations
#PATH: EDIT IF NEEDED
Diurnal.obs <- readRDS("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/diurnal_seasonal_cycle/data/OBS_diurnal.Rda")

#create frequency table for observations
freq_table_obs=data.frame(matrix(ncol=1,nrow=24)) #empty dataframe to create frequency table
colnames(freq_table_obs)<-c('Hour') 
freq_table_obs$Hour<-seq(0,23) #hours: moments of measurement

#loop over stations to fill in frequency table
# and count number of observations in the meantime
n_obs=0 
for(i in 1:length(Diurnal.obs)){
  freq_var=as.data.frame(table(factor(Diurnal.obs[[i]]$when.amax, levels = 0:24))) #construct table with counts
  colnames(freq_var)<-c('Hour',Diurnal.obs[[i]]$station) #set name of the variable as the location of the table
  freq_table_obs=merge(freq_table_obs, freq_var, by='Hour') #merge into existing frequency table
  n_obs=n_obs+sum(!is.na(Diurnal.obs[[i]]$when.amax)) #number of recorded observations (without na's)
  #length(Diurnal.obs[[i]]$when.amax) #alternative to use total with na s to normalize
}

#obs: total 777, not na 666 so 111 na's

#create columns with overall sum absolute frequencies over all stations, divide by total number of obs and times 100 to get a percentage
freq_table_obs$Observations=rowSums(x=freq_table_obs[,2:20], na.rm = TRUE)/n_obs*100



# Load data EUROCORDEX and convert to hours ------------------------------------------

#load output of get_times_am.R
#PATH: EDIT IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/diurnal_seasonal_cycle/data/times_bel_am.RData")
times=times[, colSums(is.na(times)) != nrow(times)]#remove col with only NA

times_dec=times%%1 #get decimal part, only this matters for hour of the day as time is expressed in days  since 1949...
times_hour=round(times_dec*24, digits = 1) #convert decimal part to hours, round to avoid numerical difficulties


# Frequency table ---------------------------------------------------------

freq_table=data.frame(matrix(ncol=1,nrow=24)) #empty dataframe
colnames(freq_table)<-c('Hour') 
freq_table$Hour<-seq(0.5,23.5) #insert half hours: moments of measurement

#get number of years for each model (when the model does not have an annual max for a year, the table contains NA)
n_years=as.data.frame(colSums(!is.na(times_hour)))

#loop over variables 
for(varname in colnames(times_hour)){
  freq_var=as.data.frame(table(times_hour[,varname])) #construct table with counts
  colnames(freq_var)<-c('Hour',varname) #set name of the variable as colname
  freq_var[,varname]=freq_var[,varname]/n_years[varname,]*100 #divide by the number of annual maxima for this model and multiply with 100 to get percentages
  freq_table=merge(freq_table, freq_var, by='Hour') #merge into existing frequency table
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


freq_table=melt(freq_table, id.vars = 'Hour') #convert to long format
colnames(freq_table)<-c('Hour','Model','Frequency') #set colnames


# Plotting ----------------------------------------------------------------

#PATH TO SAVE FIGURES
setwd("C:/Users/Anouk/Documents/cursussen/MP/europa/figs/diurnal and seasonal/")


#define colors for the plot below
rcmcols=c('lightseagreen','darkblue') #hcl.colors(6, palette = "Viridis")  #either choose to be same as other plots or same as seasonal cycle (viridis)
rcmnames=c("SMHI-RCA4_v1","CLMcom-ETH-COSMO-crCLIM-v1-1_v1")
names(rcmcols)=rcmnames

#plot both rcms at once
plot_both_rcms<-function(save=FALSE){
  
  #select data
  data=freq_table
  data$rcm<-sapply(str_split(data$Model, pattern='#'), function(x) (x[2])) #shorten names: only GCM needed in legend
  #split by rcm
  data_rca=subset(data, rcm=="SMHI-RCA4_v1")
  data_cosmo=subset(data, rcm=="CLMcom-ETH-COSMO-crCLIM-v1-1_v1")
  #calculate means by rcm
  mean_rca<-aggregate(Frequency~Hour, data_rca, FUN=mean)
  mean_cosmo<-aggregate(Frequency~Hour, data_cosmo, FUN=mean)
  #add column with rcm again
  mean_rca$rcm<-"SMHI-RCA4_v1"
  mean_cosmo$rcm<-"CLMcom-ETH-COSMO-crCLIM-v1-1_v1"
  #merge the two datasets again
  means<-rbind(mean_rca, mean_cosmo)
  
  #actual plotting
  g<-ggplot(data)+
    geom_line(aes(x=Hour, y=Frequency, colour=rcm, group=Model), alpha=0.2)+ #plot lines individual models lightly in background
    geom_point(data = freq_table_obs, aes(x=Hour, y=Observations, size="Observations"))+ #plot obs points
    geom_line(data = freq_table_obs,aes(x=Hour, y=Observations, linewidth ="Observations"))+ #plot obs lines 
    geom_point(data=means,aes(x=Hour, y=Frequency, color=rcm,shape=rcm), size = 2)+ #plot means of by rcm
    geom_line(data=means,aes(x=Hour, y=Frequency, color=rcm), linewidth=0.7)+ #plot means by rcm
    scale_linewidth_manual(name="", values = 0.7)+ #make sure legend obs appears
    scale_size_manual(name="", values = 2)+ #make sure legend obs appears with circle
    ylab("Frequency (%)")+
    xlab("Time (UTC)")+
    guides(colour = guide_legend(override.aes = list(alpha = 1)))+
    ylim(c(0,10))+ #momenteel  allebei op zelfde schaal om te vgln
    scale_color_manual('RCM', values=rcmcols, breaks=rcmnames,labels=c('RCA4','COSMO'))+
    scale_shape_manual('RCM', values=c(15,17), breaks=rcmnames, labels=c('RCA4','COSMO'))+
    scale_x_continuous(breaks=c(0,6,12,18,24),
                       labels=c("00:00","06:00","12:00","18:00","24:00"))+
    theme(text=element_text(size=12),
          legend.position = "bottom")+
    guides(color=guide_legend(nrow=2,byrow=TRUE)) #put legend on two rows
  
  #save if required
  if(save){
    ggsave(paste("Diurnal_Belgium.pdf", sep=''),g, width=12, height = 7, units = 'in')
  }
  return(g)
  
}

plot_both_rcms(save=FALSE)

diurnal_plot<-plot_both_rcms(save=FALSE)
#save diurnal plot to put in raster with seasonal plot
#PATH: TO EDIT
save(diurnal_plot, 
     file="C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/diurnal_seasonal_cycle/diurnal_plot.RData")

plot_both_rcms(save=TRUE)

