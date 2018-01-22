rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")

#load layers
obs=readRDS('nolayers\\num_layers_obs.rds')
bin=readRDS('nolayers\\num_layers_bin.rds')
mod=readRDS('nolayers\\num_layers_mod.rds')

# #load filenames
# load("file_name.Rdata")
# 
# #some funky data on this date
# file_name=file_name[-which(file_name=="th968_2014_06_05_fleout")]

#load dates
dates=readRDS('dates.rds')

#plot comparisons of mod, bin, obs
plot(dates,obs,col='black')
par(new=T)
plot(dates,bin,col='blue')
par(new=T)
plot(dates,mod,col='red')

yy_log=logical(length(unlist(obs)))
yn_log=logical()
nn_log=logical()
ny_log=logical()

#Total events
N=sum(yy_log,yn_log,nn_log,ny_log)
yy=sum(yy_log)
yn=sum(yn_log)
nn=sum(nn_log)
ny=sum(ny_log)

# Fraction of Correct Prediction
PC=(yy+nn)/N

#Probability of Detection
POD=yy/(yy+ny)

#False Alarm Ratio
FAR=yn/(yy+yn)