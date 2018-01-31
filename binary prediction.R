rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")

#load layers
obs=readRDS('nolayers\\num_layers_obs.rds')
bin=readRDS('nolayers\\num_layers_bin.rds')
mod=readRDS('nolayers\\num_layers_mod.rds')

#load dates
dates=readRDS('dates.rds')

#plot comparisons of mod, bin, obs
png('comparisons\\number of layers.png')
plot.new()
par(new=T,mar=c(5,5,5,6))
plot(dates,obs,col='black',type='l',main='2013-2014',ylab='no. layers',xlab='')
lines(dates,bin,col='blue')
lines(dates,mod,col='red')
mtext('obs',side=4,col='black',las=1,at=1,line=2)
mtext('model',side=4,col='red',las=1,at=1.1,line=2)
mtext('bin',side=4,col='blue',las=1,at=1.2,line=2)
dev.off()

#initializing binary prediction vectors
yy_log=matrix(0,1,length(unlist(obs)))
yn_log=matrix(0,1,length(unlist(obs)))
nn_log=matrix(0,1,length(unlist(obs)))
ny_log=matrix(0,1,length(unlist(obs)))

#predicting bin

#loop through days and scenarios
for (tick_day in 1:length(obs)){
  if(obs[[tick_day]]==2 &bin[[tick_day]]==1){yy_log[[tick_day]]=0.5;ny_log[[tick_day]]=0.5}
  else if(obs[[tick_day]]==1 &bin[[tick_day]]==1){yy_log[[tick_day]]=1}
  else if(obs[[tick_day]]==0 &bin[[tick_day]]==0){nn_log[[tick_day]]=1}
  else if(obs[[tick_day]]==1 &bin[[tick_day]]==0){ny_log[[tick_day]]=1}
  else if(obs[[tick_day]]==0 &bin[[tick_day]]==1){yn_log[[tick_day]]=1}
  else if(obs[[tick_day]]==2 &bin[[tick_day]]==0){ny_log[[tick_day]]=1}
  else if(obs[[tick_day]]==0 &bin[[tick_day]]==2){yn_log[[tick_day]]=1}
  }


#Total events
N=sum(yy_log,yn_log,nn_log,ny_log)
yy=sum(yy_log)
yn=sum(yn_log)
nn=sum(nn_log)
ny=sum(ny_log)

# Fraction of Correct Prediction
CP_b=(yy+nn)/N

#Probability of Detection
POD_b=yy/(yy+ny)

#False Alarm Ratio
FAR_b=yn/(yy+yn)

#predicting model

#loop through days and scenarios
for (tick_day in 1:length(obs)){
  if(obs[[tick_day]]==2 &mod[[tick_day]]==1){yy_log[[tick_day]]=0.5;ny_log[[tick_day]]=0.5}
  else if(obs[[tick_day]]==1 &mod[[tick_day]]==1){yy_log[[tick_day]]=1}
  else if(obs[[tick_day]]==0 &mod[[tick_day]]==0){nn_log[[tick_day]]=1}
  else if(obs[[tick_day]]==1 &mod[[tick_day]]==0){ny_log[[tick_day]]=1}
  else if(obs[[tick_day]]==0 &mod[[tick_day]]==1){yn_log[[tick_day]]=1}
  else if(obs[[tick_day]]==2 &mod[[tick_day]]==0){ny_log[[tick_day]]=1}
  else if(obs[[tick_day]]==0 &mod[[tick_day]]==2){yn_log[[tick_day]]=1}
}


#Total events
N=sum(yy_log,yn_log,nn_log,ny_log)
yy=sum(yy_log)
yn=sum(yn_log)
nn=sum(nn_log)
ny=sum(ny_log)

# Fraction of Correct Prediction
CP_m=(yy+nn)/N

#Probability of Detection
POD_m=yy/(yy+ny)

#False Alarm Ratio
FAR_m=yn/(yy+yn)

#make table of values
table_val=matrix(c(CP_m,POD_m,FAR_m,CP_b,POD_b,FAR_b),ncol=3,byrow=TRUE)
colnames(table_val)=c('CP','POD','FAR')
rownames(table_val)=c('model','bin')
table_val=as.table(table_val)

