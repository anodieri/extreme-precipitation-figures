################################################
## Calculate correlation between 2 year return 
## levels and elevation, with confidence intervals
## for Belgian station observations
## from climatological and hydrological network
################################################
## Load libraries
################################################


library(fExtremes, lib.loc='../lib')
library(ismev, lib.loc='../lib')
library(evd, lib.loc='../lib')

################################################################
### Function for return level computation with maxium likelihood
################################################################
#return_level <- function(x) {
# mle <- gev.fit(x[!is.na(x)], show=F)$mle 
# rl <- qgev(p=1-1/2, loc=mle[1], scale=mle[2], shape=mle[3])
 
# return(rl)
#}


################################################################
### Function for return level computation with probability-weigthed method
### Much faster than MLE!
################################################################
return_level <- function(x,T=2) {
 fit <- fExtremes::gevFit(x, type="pwm")
 par <- fit@fit$par.ests
 par <- par[c(2,3,1)]
 
 rl <- evd::qgev(p=1-1/T, loc=par[1], scale=par[2], shape=par[3])
 
 return(rl)
}



##########################################################
### Load data (AM & elevation) of climatological network
###########################################################
#load("../../Operational/Voor_Anouk/AM_climate.RData")

#from climatological network for durations longer than or equal to 24h

#PATH: EDIT IF NEEDED
load('./stations/AM_climate.RData')

##############################################################
### Load data (AM & elevation) of hydrometeorological network
##############################################################
#load("../../Operational/Voor_Anouk/AM_hydroclim.RData")

#from hydrological network for durations shorter than 24h

#PATH: EDIT IF NEEDED
load('./stations/AM_hydroclim.RData')


#########################################################
## Construct dataframe to save cors and CI
#########################################################

durations=c('1h','6h','12h','24h','48h', '72h') #list all durations

cors_obs<-data.frame(matrix( nrow=length(durations),ncol=4)) #construct dataframe to save correlations
colnames(cors_obs)<-c('dur','correlation','lower','upper') #set column names

##########################################################
#### Bootstrap on the years with replacement
##########################################################

#function to calculate correlation with elevation and confidence interval
#using n.boot bootstraps for specified duration
#duration should be 1h, 6h, 12h, 24h, 48h or 72h
bootstrap<-function(n.boot=10000, dur){
  
  
  #select the annual maxima for the selected duration
  if(dur=='1h'){AM <-AM_hydroclim$AM_60m}
  else if(dur=='6h'){AM<-AM_hydroclim$AM_360m}
  else if(dur=='12h'){AM<-AM_hydroclim$AM_720m}
  else if(dur=='24h'){AM<-AM_climat$AM_1d}
  else if(dur=='48h'){AM<-AM_climat$AM_2d}
  else if(dur=='72h'){AM<-AM_climat$AM_3d}
  
  n.years <- nrow(AM)  ### number of years
  
  #select the elevation of the stations depending on which network is used
  if(dur=='1h'|dur=='6h'|dur=='12h'){ #hydrological network
    ELEV <- AM_hydroclim$covar$elevation
  }else{ #climatological network
    ELEV <- AM_climat$covar$elevation
  }
  
  #calculate return level
  RL <- apply(AM, 2, return_level)
  
  #calculate correlation
  Corr <- cor(RL,ELEV)
  
  #initialise empty vector
  Corr_boot <- rep(NA, n.boot)
  
  #perform bootstrap
  for (boot in 1:n.boot) {
    
    AM_sampled <- AM[ sample(1:n.years, replace=TRUE), ] #sample AM
    RL_sampled <- apply(AM_sampled, 2, return_level) #calculate return level from sampled AM
    Corr_boot[boot] <- cor(RL_sampled, ELEV) #calculate correlation
    
    if (boot%%100==0)print(boot/100) #print progress
  }
  
  #calculate confidence interval
  CI <- quantile(Corr_boot, probs=c(0.025,0.975))
  
  # return correlation and confidence interval
  return(c(Corr, CI))
}



#calculate correlation and confidence interval for each duration
#and save in dataframe
for(i in 1:length(durations)){
  dur=durations[i]
  print(dur)
  cor<-bootstrap(dur=dur)
  cors_obs[i,]<-c(dur, cor)
}

#save the results in a dataframe
#PATH: EDIT IF NEEDED
save(cors_obs,file='cors_obs_2year.RData')
#save(cors_obs,file='C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/cors_obs_2year.RData')

