rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")

#load data
alt_layers_obs=readRDS('altitude_layers_obs.rds')
width_layers_obs=readRDS('width_layers_obs.rds')
dates=readRDS('dates.rds')

#initialize plot and hold
plot.new()
par(new=T)

#set numeric (0) to NA
idx=!(sapply(width_layers_obs,length))
width_layers_obs[idx]=NA

#initialize matrix 
dates_vector=rep(0,length(unlist(width_layers_obs)))
class(dates_vector)="Date"

#loop through days
for (tick_day in 1:length(dates)){
  
  #number of layers in that date
  no_layers=length(width_layers_obs[[tick_day]])
  
  #put date in first 0 entry in dates_vector
  dates_vector[which(dates_vector==0)[1]]=dates[[tick_day]]
  
  #conditional for days with 1+ layers
  if (no_layers>1){
    for (tick_layers in 2:no_layers){
      
      #adds on the date into next zero place
      dates_vector[which(dates_vector=='1970-01-01')[1]]=dates[[tick_day]]
    }  
  }
  
}

#unlist the width layers obs to plot it with the dates_vector
width_layers_obs_vector=matrix(unlist(width_layers_obs),ncol=length(unlist(width_layers_obs)),nrow=1)

#plotting the width layers obs wrt dates
plot(dates_vector,width_layers_obs_vector,type='p')
