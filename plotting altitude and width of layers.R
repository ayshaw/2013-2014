rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")

alt_layers_obs=readRDS('altitude_layers_obs.rds')
width_layers_obs=readRDS('width_layers_obs.rds')
dates=readRDS('dates.rds')
plot(dates,alt_layers_obs)
plot(dates,width_layers_obs)
