####################################################################
## Calculating statistics from Vautard (2021)
## Decoupling bias from RCM and GCM using
## mean within RCM/GCM normalised variance
## for both historical bias and climate signal: change in intensity
####################################################################


# loading packages -----------------------------------------------

library(ggplot2) # for plotting
library(scico) #for color scales


# HISTORICAL BIAS ---------------------------------------------------------

# load data  --------------------------------------------------------------

#collecting data from all countries
#ned
#EDIT PATH IF NEEDED
load("data/output_ned_merged.RData")
data_ned=output
data_ned$country<-"ned"

#fin
#EDIT PATH IF NEEDED
load("data/output_fin_merged.RData")
data_fin=output
data_fin$country<-"fin"

#den
#EDIT PATH IF NEEDED
load("data/output_den_merged.RData")
data_den=output
data_den$country<-"den"

#uk, belgium, germany (these are summaries, created with make_summary.R)
# the summaries contain the mean return levels for all durations, for Netherlands, Finland
# and Denmark this is already the case in the output file
#EDIT PATHS IF NEEDED
load("data/output_bel_merge_summary_ARF.RData")
load("data/output_uk_merge_summary_ARF.RData")
load("data/output_dui_merge_summary_ARF.RData")
data_dui<-summary

#putting these together in one large dataframe, ignoring columns rmse and bias from bel, germany, uk
data<-rbind(data_ned, data_fin, data_den,data_bel[,colnames(data_ned)], data_uk[,colnames(data_ned)], data_dui[,colnames(data_ned)])

# calculating WGNV and WRNV -----------------------------------------------
##calculating statistics using formulas 4.1-4.5 (thesis)

#general numbers
N_countries<-length(unique(data$country)) #number of countries
N_gcm<-length(unique(data$gcm)) #number of gcms
N_rcm<-length(unique(data$rcm)) #number of rcms


#total variance by country (den, ned, fin, uk, bel or dui)
#variance is computed by country
V_k<-function(dur, country_k){
  varname=paste("returnlevel",dur,sep = "")
  #create a subset of the data for this country
  data_country=subset(data,country==country_k)
  #calculate mean bias of the return level for this duration for this country
  B_k=mean(data_country[,varname])
  #calculate variance per country
  l<-nrow(data_country) #number of rows
  V_k=1/l*sum((data_country[varname]-B_k)^2) #mean variance 
  return(V_k)
}

V_k(dur='1h', country_k='uk')


#calculate WGNV_i for GCM n
WGNV_GCM_i<-function(dur, gcm_n){
  varname<-paste("returnlevel",dur,sep = "") #construct the variable name
  data_gcm=subset(data,gcm==gcm_n) #create a subset of the data containing only gcm of interest
  B_ik=aggregate(get(varname)~country, data=data_gcm, mean) #calculate B_ik for all countries
  for(country_k in unique(data_gcm$country)){ #loop over countries
    V_k=V_k(dur, country_k=country_k) #calculate variance per country
    B_ik_country=B_ik[B_ik$country==country_k,]$"get(varname)" #select B_ik for country k
    #normalisation is already applied since this should be done separately for each country
    #normalised by sqrt(V_k) since in the next step the square is taken
    data_gcm$B_ik[data_gcm$country==country_k]<-B_ik_country/sqrt(V_k) #add extra column to data_gcm with B_ik for each country
    data_gcm[,varname][data_gcm$country==country_k]<-data_gcm[,varname][data_gcm$country==country_k]/sqrt(V_k) #normalise
  }
  WGNV_i<-1/(N_countries*N_rcm)*sum((data_gcm[varname]-data_gcm$B_ik)^2)
  return(WGNV_i)
}

WGNV_GCM_i(dur='1h', gcm_n = 'CNRM-CERFACS-CNRM-CM5')

#WGNV= weighted mean of WGNV_i's
WGNV<-function(dur){
  WGNV=0 #initialise at zero
  for(gcm_i in unique(data$gcm)){ # for each gcm
    N_gcm_i=sum(data$gcm==gcm_i)/N_countries #get number of runs with this gcm
    WGNV=WGNV+WGNV_GCM_i(dur=dur,gcm_n = gcm_i)/N_gcm_i #and add WGNV_i divided by this number to total
  }
  return(WGNV)
}


WGNV(dur='24h')

#similar calculation for WGRV_RCM_j for RCM j
WRNV_RCM_j<-function(dur, rcm_j){
  varname<-paste("returnlevel",dur,sep = "")
  data_rcm=subset(data, rcm==rcm_j)
  B_jk=aggregate(get(varname)~country, data=data_rcm, mean)
  for(country_k in unique(data_rcm$country)){
    V_k=V_k(dur, country_k=country_k) #calculate variance per country
    B_jk_country=B_jk[B_jk$country==country_k,]$"get(varname)"
    #below, normalisation is already applied since this should be done by country
    data_rcm$B_jk[data_rcm$country==country_k]<-B_jk_country/sqrt(V_k) #add extra column to data_gcm with B_ik for each country
    data_rcm[,varname][data_rcm$country==country_k]<-data_rcm[,varname][data_rcm$country==country_k]/sqrt(V_k) #normalise
  }
  WGRV_j<-1/(N_countries*N_gcm)*sum((data_rcm[varname]-data_rcm$B_jk)^2)
  return(WGRV_j)
}

WRNV_RCM_j(dur='1h', rcm = 'CNRM-ALADIN63')

#WRNV= weighted mean of WRNV_i's
WRNV<-function(dur){
  WRNV=0
  for(rcm_j in unique(data$rcm)){
    N_rcm_j=sum(data$rcm==rcm_j)/N_countries
    WRNV=WRNV+WRNV_RCM_j(rcm = rcm_j, dur=dur)/N_rcm_j
  }
  return(WRNV)
}


WRNV(dur = '24h')


# plotting historical bias----------------------------------------------------------------

#calculate all values
Durations<-c('1h','6h','12h','24h','48h','72h')
WRNVs<-c(WRNV(dur = '1h'),WRNV(dur = '6h'),WRNV(dur = '12h'),WRNV(dur = '24h'), WRNV(dur = '48h'),WRNV(dur = '72h'))
WGNVs<-c(WGNV(dur = '1h'),WGNV(dur = '6h'),WGNV(dur = '12h'),WGNV(dur = '24h'), WGNV(dur = '48h'),WGNV(dur = '72h'))
type<-rep('historical',length(Durations))

#construct data frame containing these values
df_hist<-data.frame(Durations,WRNVs,WGNVs, type)

#plot
ggplot()+geom_point(df_hist, mapping=aes(x = WGNVs,y=WRNVs, color=factor(Durations, levels=c('1h','6h','12h','24h','48h','72h'))), size=3)+
  geom_abline(slope=1, intercept = 0)+
  xlim(c(0,1))+
  ylim(c(0,1))+
  xlab("Within GCM normalised variance")+
  ylab("Within RCM normalised variance")+
  ggtitle('Variance of biases EURO-CORDEX')+
  scale_color_discrete('Durations')+
  theme(text=element_text(size=16))


#  CHANGE IN INTENSITY BETWEEN GWL +1.5°C AND +3°C ----------------------------

# load data ---------------------------------------------------------------

# first relative difference, then area mean
#EDIT PATH IF NEEDED
#load(file='data/relative_difference_all_regions_extended_durations.RData')
#data<-data_all_regions

#relative difference computed after taking area mean
#EDIT PATH IF NEEDED
load(file='data/relative_difference_after_mean_all_regions_extended_durations.RData')
data<-data_all_regions_rel_after_mean


# calculating WGNV and WRNV -----------------------------------------------
##calculating statistics using formulas 4.1-4.5 (MP)

#general numbers 
N_regions<-length(unique(data$region)) #number of PRUDENCE regions 
N_gcm<-length(unique(data$gcm)) #number of gcms
N_rcm<-length(unique(data$rcm)) #number of rcms

#total variance by region (FR, ME, MD, BI, IP, EA, SC, AL)
#problem with the normalisation using the total variance as above
#variance is computed by region
V_k<-function(dur, region_k){
  #create a subset of the data for this region
  data_region=subset(data,region==region_k)
  #calculate mean bias of the return level for this duration for this region
  B_k=mean(data_region[,dur])
  #calculate variance per region
  #V_k=1/(N_gcm*N_rcm)*sum((data_region[dur]-B_k)^2)
  l=nrow(data_region)
  V_k=1/l*sum((data_region[dur]-B_k)^2)
  return(V_k)
}

V_k(dur = '1h',region_k = 'BI')

#calculate WGNV_i for GCM i
WGNV_GCM_i<-function(dur, gcm_i){
  varname<-dur #the variable name of interest is the duration
  data_gcm=subset(data, gcm==gcm_i) #get subset of data for this gcm
  B_ik=aggregate(get(varname)~region, data=data_gcm, mean) #calculate B_ik
  #loop over the regions
  for(region_k in unique(data_gcm$region)){
    V_k<-V_k(dur, region_k = region_k) #calculate normalisation
    B_ik_region=B_ik[B_ik$region==region_k,]$"get(varname)" # select the B_ik for the region
    data_gcm$B_ik[data_gcm$region==region_k]<-B_ik_region/sqrt(V_k) #add column for easier calculation
    #normalisation uses sqrt(V_k) since later on this is squared, so sqrt and ² cancel each other
    #normalise the B_ijk also:
    data_gcm[,varname][data_gcm$region==region_k]<-data_gcm[,varname][data_gcm$region==region_k]/sqrt(V_k)
  }
  WGNV_i<-1/(N_regions*N_rcm)*sum((data_gcm[varname]-data_gcm$B_ik)^2) #final calculation
  l=nrow(data_gcm)
  print(l)
  #WGNV_i<-1/l*sum((data_gcm[varname]-data_gcm$B_ik)^2) #final calculation
  return(WGNV_i)
}

WGNV_GCM_i('1h',"ICHEC-EC-EARTH")

#WGNV= weighted mean of WGNV_i's
WGNV<-function(dur){
  WGNV=0 
  for(gcm_i in unique(data$gcm)){
    N_gcm_i=sum(data$gcm==gcm_i)/N_regions
    WGNV=WGNV+WGNV_GCM_i(gcm_i = gcm_i, dur=dur)/N_gcm_i
  }
  return(WGNV)
}


WGNV(dur='12h')

#analogon WGRV_RCM_j for RCM j
WRNV_RCM_j<-function(dur, rcm_j){
  varname<-dur
  data_rcm=subset(data, rcm==rcm_j)
  B_jk=aggregate(get(varname)~region, data=data_rcm, mean)
  for(region_k in unique(data_rcm$region)){
    V_k=V_k(dur=dur, region_k = region_k)
    B_jk_region=B_jk[B_jk$region==region_k,]$"get(varname)"
    data_rcm$B_jk[data_rcm$region==region_k]<-B_jk_region/sqrt(V_k)
    data_rcm[,varname][data_rcm$region==region_k]<-data_rcm[,varname][data_rcm$region==region_k]/sqrt(V_k)
  }
  WGRV_j<-1/(N_regions*N_gcm)*sum((data_rcm[varname]-data_rcm$B_jk)^2)
  l=nrow(data_rcm)
  print(l)
  #WGRV_j<-1/l*sum((data_rcm[varname]-data_rcm$B_jk)^2)
  return(WGRV_j)
}

WRNV_RCM_j(dur='1h', rcm = 'ALADIN63')

#WRNV= weighted mean of WRNV_i's
WRNV<-function(dur){
  WRNV=0
  for(rcm_j in unique(data$rcm)){
    N_rcm_j=sum(data$rcm==rcm_j)/N_regions
    WRNV=WRNV+WRNV_RCM_j(rcm = rcm_j, dur=dur)/N_rcm_j
  }
  return(WRNV)
}


WRNV(dur = '12h')

# plotting future ----------------------------------------------------------------

#calculate all values and put in vectors
Durations<-c('1h','6h','12h','24h','48h','72h')
WRNVs<-c(WRNV(dur = '1h'),WRNV(dur = '6h'),WRNV(dur = '12h'), WRNV(dur = '24h'),WRNV(dur = '48h'),WRNV(dur = '72h'))
WGNVs<-c(WGNV(dur = '1h'),WGNV(dur = '6h'),WGNV(dur = '12h'), WGNV(dur = '24h'),WGNV(dur = '48h'),WGNV(dur = '72h'))
type<-rep('future',length(Durations))
#construct data frame
df_future<-data.frame(Durations,WRNVs,WGNVs, type)

#plot
ggplot()+geom_point(df_future, mapping=aes(x = WGNVs,y=WRNVs, color=factor(Durations, levels=c('1h','12h','24h'))), size=3)+
  geom_abline(slope=1, intercept = 0)+
  xlim(c(0,1.5))+
  ylim(c(0,1.5))+
  xlab("Within GCM normalised variance")+
  ylab("Within RCM normalised variance")+
  ggtitle('Variance of biases EURO-CORDEX')+
  scale_color_discrete('Durations')+
  theme(text=element_text(size=16))


# plotting historical and future ------------------------------------------

df<-rbind(df_future, df_hist) #merge historical and future dataframe
#add space in between duration and h 
df$Durations<-sub('h',' h',df$Durations)

#actual plotting
g<-ggplot()+geom_point(df, mapping=aes(x = WGNVs,y=WRNVs,
          shape=type, color=factor(Durations, levels=c('1 h','6 h','12 h','24 h','48 h','72 h'))), size=4)+
  geom_abline(slope=1, intercept = 0, linetype='dashed')+ #reference line
  scale_shape_discrete('', labels=c('Relative change \nbetween GWLs\n+1.5°C and +3°C', 'Historical bias'))+
  xlim(c(0,1))+ #limits x axis
  ylim(c(0,1))+ #limits y axis
  xlab("Within GCM normalised variance")+
  ylab("Within RCM normalised variance")+
  #scale_color_manual('Durations', values = c('orange','darkred','darkblue'))+
  scale_color_scico_d('Durations', palette='managua')+ #color scale
  theme(text=element_text(size=14)) #text size
g

#save plot
#EDIT PATH IF NEEDED
ggsave("variance_of_biases.pdf",
       g, width=7, height = 4.3, units = 'in', dpi = 300)  
