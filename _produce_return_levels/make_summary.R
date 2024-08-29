########################################################################
## Make summarising dataframe 
## for EUROCORDEX
## to use in Vautard.R, 
## also contains bias, rmse, and bias (%) for plot_table.R
#######################################################################

###################################################################
#load required libraries
##################################################################
library(stringr) #to replace in strings
library(dplyr) #to use select function

###################################################################
#define functions to calculate bias, bias(%), and rmse
##################################################################

#compute root mean squared error function with NA removed
rmse <- function(x, y){
  res <- sqrt(mean((x-y)^2, na.rm = TRUE))
  return(res)
}

#compute mean bias
bias <- function(obs, predicted){
  res <- mean(predicted-obs, na.rm = TRUE)
  return(res)
}
#so mean bias = -error
#neg bias=underestimation, pos bias= overestimation

#compute the mean bias as a percentage 
bias_percentage <- function(obs, predicted){
  res <- mean((predicted-obs)/obs, na.rm = TRUE)*100
  return(res)
}

########################################################
# create a summary dataframes for EUROCORDEX
########################################################

#make summary dataframe for output saved as rdata, i.e. for UK and Belgium
# output= the dataframe containing the return levels
# country= 'bel' or 'dui'
make_summary<-function(output, country){
  
  #create summary dataframe 
  nmodels=27 #number of eurocordex models #(ncol(output)-5)/6 #number of columns minus 5 (lon, lat , obs 3 dur, /6 (3 dur returnlevels and errors))
  summary=data.frame(matrix(ncol = 6,nrow= nmodels))
  colnames(summary)=c("model","rcm","gcm","run","npoints","country")
  
  #fill in basics
  summary$country<-country
  summary$npoints<-nrow(output)
  summary$model<-1:nmodels #lines up with order netherlands etc
  
  #select all return level columns and calculate means
  colnames<-colnames(output)[grepl("returnlevels",colnames(output))]
  means=lapply(output[colnames], mean, na.rm = TRUE)
  
  #select all 1h return level column names
  colnames<-colnames(output)[grepl("returnlevels1h",colnames(output))]
  colnames<-colnames[grep("UKCP18", colnames, invert=TRUE)] #ignores cpms in case UK
  
  #iterate over these columns to get model specs and mean return levels
  for(i in 1:length(colnames)){
    colname=colnames[i]
    #get model specs
    split=strsplit(colname,'#')[[1]]
    summary$gcm[i]=split[2]
    summary$rcm[i]=split[3]
    summary$run[i]=split[4]
    
    #select means
    summary$returnlevel1h[i]=means[colname][[1]]
    summary$returnlevel6h[i]=means[str_replace(colname,"1h","6h")][[1]]
    summary$returnlevel12h[i]=means[str_replace(colname,"1h","12h")][[1]]
    summary$returnlevel24h[i]=means[str_replace(colname,"1h","24h")][[1]]
    summary$returnlevel48h[i]=means[str_replace(colname,"1h","48h")][[1]]
    summary$returnlevel72h[i]=means[str_replace(colname,"1h","72h")][[1]]

    
    #compute bias, bias(%) and rmse for each duration and save in dataframe
    #1h
    obs=select(output, contains(paste('obs_1h', sep = ''))&!contains('xi'))[,1]
    summary$rmse1h[i]=rmse(obs,as.vector(output[colname][,1]))
    summary$bias1h[i]=bias(obs,as.vector(output[colname][,1]))
    summary$bias_percentage1h[i]=bias_percentage(obs,as.vector(output[colname][,1]))
    
    #6h
    obs=select(output, contains(paste('obs_6h', sep = ''))&!contains('xi'))[,1]
    summary$rmse6h[i]=rmse(obs,as.vector(output[str_replace(colname,"1h","6h")][,1]))
    summary$bias6h[i]=bias(obs,as.vector(output[str_replace(colname,"1h","6h")][,1]))
    summary$bias_percentage6h[i]=bias_percentage(obs,as.vector(output[str_replace(colname,"1h","6h")][,1]))
    
    #12h
    obs=select(output, contains(paste('obs_12h', sep = ''))&!contains('xi'))[,1]
    summary$rmse12h[i]=rmse(obs,as.vector(output[str_replace(colname,"1h","12h")][,1]))
    summary$bias12h[i]=bias(obs,as.vector(output[str_replace(colname,"1h","12h")][,1]))
    summary$bias_percentage12h[i]=bias_percentage(obs,as.vector(output[str_replace(colname,"1h","12h")][,1]))
    
    #24h
    obs=select(output, contains(paste('obs_24h', sep = ''))&!contains('xi'))[,1]
    summary$rmse24h[i]=rmse(obs,as.vector(output[str_replace(colname,"1h","24h")][,1]))
    summary$bias24h[i]=bias(obs,as.vector(output[str_replace(colname,"1h","24h")][,1]))
    summary$bias_percentage24h[i]=bias_percentage(obs,as.vector(output[str_replace(colname,"1h","24h")][,1]))
    
    #48h
    obs=select(output, contains(paste('obs_48h', sep = ''))&!contains('xi'))[,1]
    summary$rmse48h[i]=rmse(obs,as.vector(output[str_replace(colname,"1h","48h")][,1]))
    summary$bias48h[i]=bias(obs,as.vector(output[str_replace(colname,"1h","48h")][,1]))
    summary$bias_percentage48h[i]=bias_percentage(obs,as.vector(output[str_replace(colname,"1h","48h")][,1]))
    
    #72h
    obs=select(output, contains(paste('obs_72h', sep = ''))&!contains('xi'))[,1]
    summary$rmse72h[i]=rmse(obs,as.vector(output[str_replace(colname,"1h","72h")][,1]))
    summary$bias72h[i]=bias(obs,as.vector(output[str_replace(colname,"1h","72h")][,1]))
    summary$bias_percentage72h[i]=bias_percentage(obs,as.vector(output[str_replace(colname,"1h","72h")][,1]))
    
  }
  
  return(summary)
}

#EDIT PATHS IF NEEDED
#create summary for Belgium and save it
load("C:/Users/Anouk/Documents/cursussen/MP/europa/belgium/output_bel_merge_ARF.RData")
data_bel<-make_summary(output,"bel")
setwd("C:/Users/Anouk/Documents/cursussen/MP/europa/")
save(data_bel,file="belgium/output_bel_merge_summary_ARF.RData")

#create summary for UK and save it
load("C:/Users/Anouk/Documents/cursussen/MP/europa/UK/output_UK_merge_ARF.RData")
data_uk<-make_summary(output,"uk")
save(data_uk,file="UK/output_uk_merge_summary_ARF.RData")


#make a summary dataframe for countries saved as netcdf files, i.e. Germany

#load libraries (commented: for remote server)
library(ncdf4)#, lib.loc="../lib") #to open netcdf files
library(stringr)#, lib.loc="../lib") #to replace in strings
library(dplyr)#, lib.loc = '../lib') #to use select 
library(stringr)#, lib.loc = '../lib') #to replace in strings

#specify name of the netcdf file where the return levels are saved and the country name
make_summary_netcdf<-function(filename, country){
  #create summary_dataframe dataframe
  nmodels=27 #number of eurocordex models #(ncol(output)-5)/6 #number of columns minus 5 (lon, lat , obs 3 dur, /6 (3 dur returnlevels and errors))
  summary=data.frame(matrix(ncol = 9,nrow= nmodels))
  colnames(summary)=c("model","rcm","gcm","run","returnlevel1h","returnlevel12h","returnlevel24h","npoints","country")
  
  #fill in basics
  summary$country<-country
  summary$model<-1:nmodels #lines up with order netherlands etc
  
  #open netcdf file
  nc<-nc_open(filename)
  colnames<-names(nc$var)

  #select all observations
  obsname1h<-colnames[grepl('obs_1h', colnames)] #xi was not saved in these netcdf files, so no need to exclude it
  obs1h=as.vector(ncvar_get(nc = nc, obsname1h)) #as vector since these are matrices
  obs12h=as.vector(ncvar_get(nc = nc, str_replace(obsname1h,"1h","12h") ))
  obs24h=as.vector(ncvar_get(nc = nc, str_replace(obsname1h,"1h","24h")))
  obs6h=as.vector(ncvar_get(nc = nc, str_replace(obsname1h,"1h","6h") ))
  obs48h=as.vector(ncvar_get(nc = nc, str_replace(obsname1h,"1h","48h")))
  obs72h=as.vector(ncvar_get(nc = nc, str_replace(obsname1h,"1h","72h")))
  
  
  #select all return level columns
  colnames<-colnames[grepl("returnlevels",colnames)]
  
  #calculate means of all these columns
  vars=sapply(colnames, ncvar_get,nc=nc) #get all variables as a matrix
  means<-colMeans(vars, na.rm=TRUE)
  
  #convert to dataframe for selecting in loop 
  vars<-as.data.frame(vars)
  
  #close netcdf file
  nc_close(nc)
  
  #add number of points
  summary$npoints<-nrow(vars)
  
  #select all 1h return level column names
  colnames<-colnames[grepl("returnlevels1h",colnames)]
  colnames<-colnames[grep("UKCP18", colnames, invert=TRUE)] #ignores cpms in case UK
  
  #iterate over these columns to get model specs and mean return levels
  for(i in 1:length(colnames)){
    colname=colnames[i]
    #get model specs and save in dataframe
    split=strsplit(colname,'#')[[1]]
    summary$gcm[i]=split[2]
    summary$rcm[i]=split[3]
    summary$run[i]=split[4]
    #select means
    summary$returnlevel1h[i]=means[colname][[1]]
    summary$returnlevel6h[i]=means[str_replace(colname,"1h","6h")][[1]]
    summary$returnlevel12h[i]=means[str_replace(colname,"1h","12h")][[1]]
    summary$returnlevel24h[i]=means[str_replace(colname,"1h","24h")][[1]]
    summary$returnlevel48h[i]=means[str_replace(colname,"1h","48h")][[1]]
    summary$returnlevel72h[i]=means[str_replace(colname,"1h","72h")][[1]]

    
    #compute bias, bias(%) and rmse for each duration and save in dataframe
    #1h
    summary$rmse1h[i]=rmse(obs1h,as.vector(vars[colname][,1]))
    summary$bias1h[i]=bias(obs1h,as.vector(vars[colname][,1]))
    summary$bias_percentage1h[i]=bias_percentage(obs1h,as.vector(vars[colname][,1]))
    
    #6h
    summary$rmse6h[i]=rmse(obs6h,as.vector(vars[str_replace(colname,"1h","6h")][,1]))
    summary$bias6h[i]=bias(obs6h,as.vector(vars[str_replace(colname,"1h","6h")][,1]))
    summary$bias_percentage6h[i]=bias_percentage(obs6h,as.vector(vars[str_replace(colname,"1h","6h")][,1]))
    
    #12h
    summary$rmse12h[i]=rmse(obs12h,as.vector(vars[str_replace(colname,"1h","12h")][,1]))
    summary$bias12h[i]=bias(obs12h,as.vector(vars[str_replace(colname,"1h","12h")][,1]))
    summary$bias_percentage12h[i]=bias_percentage(obs12h,as.vector(vars[str_replace(colname,"1h","12h")][,1]))
    
    #24h
    summary$rmse24h[i]=rmse(obs24h,as.vector(vars[str_replace(colname,"1h","24h")][,1]))
    summary$bias24h[i]=bias(obs24h,as.vector(vars[str_replace(colname,"1h","24h")][,1]))
    summary$bias_percentage24h[i]=bias_percentage(obs24h,as.vector(vars[str_replace(colname,"1h","24h")][,1]))
  
    #48h
    summary$rmse48h[i]=rmse(obs48h,as.vector(vars[str_replace(colname,"1h","48h")][,1]))
    summary$bias48h[i]=bias(obs48h,as.vector(vars[str_replace(colname,"1h","48h")][,1]))
    summary$bias_percentage48h[i]=bias_percentage(obs48h,as.vector(vars[str_replace(colname,"1h","48h")][,1]))
    
    #72h
    summary$rmse72h[i]=rmse(obs72h,as.vector(vars[str_replace(colname,"1h","72h")][,1]))
    summary$bias72h[i]=bias(obs72h,as.vector(vars[str_replace(colname,"1h","72h")][,1]))
    summary$bias_percentage72h[i]=bias_percentage(obs72h,as.vector(vars[str_replace(colname,"1h","72h")][,1]))
    
    }
  
  #save the final dataframe
  #edit name depending on ARF or no ARF
  #EDIT PATH IF NEEDED
  save(summary,file="output_dui_merge_summary_ARF.RData")
  
}

#make a summary for Germany
#EDIT PATHS IF NEEDED
make_summary_netcdf("output_Dui_merge_1-12-24.nc","dui")
make_summary_netcdf("C:/Users/Anouk/Documents/cursussen/MP/europa/Germany/output_Dui_merge_ARF.nc","dui")


# Create summaries for Denmark, the Netherlands and Finland

#make summary dataframe for output saved as rdata for countries where only mean return level is saved
#namely Netherlands, Denmark and Finland
#output= the dataframe with the mean return levels
# country=  'ned', 'den' or 'fin'
make_summary_country_one_mean<-function(output, country){
  
  #create summary dataframe by copying output
  summary=output
  nmodels=27 #number of eurocordex models 

  #load observations with ARF
  load("C:/Users/Anouk/Documents/cursussen/MP/europa/countries1mean/obs_ARF.RData")

  #compute bias, bias(%) and rmse for each duration
  #1h
  obs1h=obs[country,'1h']
  summary$bias1h=output$returnlevel1h-obs1h
  summary$bias_percentage1h=summary$bias1h/obs1h*100
  summary$rmse1h=sqrt(summary$bias1h^2)
  
  #6h
  obs6h=obs[country,'6h']
  summary$bias6h=output$returnlevel6h-obs6h
  summary$bias_percentage6h=summary$bias6h/obs6h*100
  summary$rmse6h=sqrt(summary$bias6h^2)
  
  #12h
  obs12h=obs[country,'12h']
  summary$bias12h=output$returnlevel12h-obs12h
  summary$bias_percentage12h=summary$bias12h/obs12h*100
  summary$rmse12h=sqrt(summary$bias12h^2)
  
  #24h
  obs24h=obs[country,'24h']
  summary$bias24h=output$returnlevel24h-obs24h
  summary$bias_percentage24h=summary$bias24h/obs24h*100
  summary$rmse24h=sqrt(summary$bias24h^2)
  
  return(summary)
}


#create summary for ned, den and fin and save it by first loading return levels and
# then applying summary function
#EDIT PATHS IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/europa/countries1mean/output_den_merged.RData")
data_den<-make_summary_country_one_mean(output,"den")
setwd("C:/Users/Anouk/Documents/cursussen/MP/europa/countries1mean/")
save(data_den,file="output_den_merge_summary_ARF.RData")

load("C:/Users/Anouk/Documents/cursussen/MP/europa/countries1mean/output_fin_merged.RData")
data_fin<-make_summary_country_one_mean(output,"fin")
save(data_fin,file="output_fin_merge_summary_ARF.RData")

load("C:/Users/Anouk/Documents/cursussen/MP/europa/countries1mean/output_ned_merged.RData")
data_ned<-make_summary_country_one_mean(output,"ned")
save(data_ned,file="output_ned_merge_summary_ARF.RData")