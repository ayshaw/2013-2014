rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")

#load data
alt_layers_obs=readRDS('altitude_layers_obs.rds')
width_layers_obs=readRDS('width_layers_obs.rds')
dates=readRDS('dates.rds')

#initialize plot and hold
plot.new()
par(new=T)

#initialize matrix
layer=matrix(NA,nrow=length(dates),ncol=2)

#loop through days
for (tick_day in 1:length(dates)){
  
  #save first item in matrix first row
  layer[tick_day,1]=width_layers_obs[[tick_day]][1]
  
  #save second item in second row
  if(length(width_layers_obs[[tick_day]])>1){
  layer[tick_day,2]=width_layers_obs[[tick_day]][2]}
  
  # #plot first layer
  # plot(dates[[tick_day]],width_layers_obs[[tick_day]][1],ylim=c(0,4))
  # par(new=T)
  # 
  # #number of layers per date
  # no_layers_today=length(width_layers_obs[[tick_day]])
  # 
  # #conditional if statement for multiple layers
  # if (length(width_layers_obs[[tick_day]])>1){
  #   
  #   #loop through additional layers
  #   for (tick_layers in 2:no_layers_today){
  #   plot(dates[[tick_day]],width_layers_obs[[tick_day]][tick_layers],ylim=c(0,4))}
  #   par(new=T)
  #   
  # }
  
}
plot(dates,layer[,1])
