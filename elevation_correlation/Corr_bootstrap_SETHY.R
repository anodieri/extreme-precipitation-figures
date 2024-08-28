################################################
## Calculate correlation between 2 year return 
## levels and elevation, with confidence intervals
## for hourly observations from the SETHY network
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



###########################################################
### Load data (AM & elevation) of SETHY network
#############################################################
#PATH: EDIT IF NEEDED
load('./stations/AM_SETHY.RData')


#########################################################
## Construct dataframe to save cors and CI
#########################################################

durations=c('1h') #list all durations: only 1h maxima for SETHY
#durations=c('1h','6h','12h','24h','48h', '72h') #extension

#construct dataframe to save correlations
cors_obs<-data.frame(matrix( nrow=length(durations),ncol=4)) 
colnames(cors_obs)<-c('dur','correlation','lower','upper') #set column names

##########################################################
#### Bootstrap on the years with replacement
##########################################################

#function to calculate correlation with elevation and confidence interval
#using n.boot bootstraps for specified duration
bootstrap<-function(n.boot=10000, dur){
  

  #select the annual maxima for the selected duration
  if(dur=='1h'){AM <-AM_SETHY$AM1h}
  #extendable to more durations
  
  n.years <- nrow(AM)  ### number of years
  
  #select the elevation of the stations
  ELEV <- AM_SETHY$ELEVATION
  
  #calculate return level
  RL <- apply(AM, 2, return_level) # apply return level function along columns
  
  #calculate correlation
  Corr <- cor(RL,ELEV)
  
  #initialise vector
  Corr_boot <- rep(NA, n.boot)
  
  #perform bootstrapping
  for (boot in 1:n.boot) {
    
    AM_sampled <- AM[ sample(1:n.years, replace=TRUE), ]  #sample
    RL_sampled <- apply(AM_sampled, 2, return_level) #calculate sampled RL
    Corr_boot[boot] <- cor(RL_sampled, ELEV) #calculate correlation
    
    if (boot%%100==0)print(boot/100)#print progress
  }
  
  #calculate confidence interval
  CI <- quantile(Corr_boot, probs=c(0.025,0.975))
  
  #return correlation and confidence interval
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
save(cors_obs,file='cors_obs_SETHY_2year.RData')
#save(cors_obs,file='C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/cors_obs_2year.RData')

