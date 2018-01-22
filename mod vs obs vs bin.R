rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")

#opening up the model data file
b=read.csv('data_model.csv')
c=matrix(unlist(b), ncol = 33, byrow = TRUE)
d=c[2:73,]*10^6

#the altitudes and levels
lvl=seq(1,72,by=1)
e=read.csv('levelalt.csv')
f=matrix(unlist(e))
g=seq(2,144,by=2)[1:72]
alt=rev(f[g])

#openning up the altitude boundaries for binning the observation data
h=read.csv('boundaries.csv')
bounds=h[seq(143,1,by=-2)[1:72],1]

#load filenames and required packages
load("file_name.Rdata")
require('signal');require('prospectr');require('pspline')

#the altitudes needed for the graphing of free tropospheric layers
binnedobs_alt=alt[14:29]
# write.csv(binnedobs_alt,file='binnedobs_alt.csv',row.names=F)

#making lists to store variables outside the loop
difference=list()
diff_mean=list()
num_layers=list()
date=list()
binnedobs_o3=matrix(0,length(binnedobs_alt),length(file_name))

for (tick_s in 1:length(file_name)){
  
  #making list to store raw data in the loop
  b=list()
  
  #loading raw data
  load(paste('data_fixed\\data_fixed',file_name[tick_s],'.Rdata',sep=''))
 
  #getting rid of NA
  red=cbind(b$o3_mr,b$alt,b$temperature)
  blue=red[complete.cases(red), ]
  
  #defining the free troposphere from 2 to 10 km 
  
  #observation
  tpause=10
  in_tpause=which(blue[,2]>tpause)[1]
  in_ft=which(blue[,2]>2)[1]
  purple=blue[in_ft:in_tpause,]
  
  #model
  in_ft_mod=which(alt>2)[1]
  in_tpause=which(alt>tpause)[1]
  
  #binning the observations to model resolution
  binnedobs_o3[,tick_s]=tapply(purple[,1],.bincode(purple[,2],breaks=bounds,right=T,include.lowest=T),mean,na.rm=T)
  
  #date of each observation
  date[[tick_s]]=paste(substr(file_name[tick_s],7,10),substr(file_name[tick_s],12,13),substr(file_name[tick_s],15,16),sep='/')
  
  #plotting model vs binned vs observation
  png(paste('comparisons\\mod vs obs vs bin',file_name[tick_s],'.png',sep=''))
  plot(purple[,1],purple[,2],ylim=c(2,10),col=rgb(0.5,0.5,0.5,0.4),
       main=paste('Ozone', date[[tick_s]],sep=', '),xlab='ppmv',ylab='km',xlim=c(0.03,0.12))
  lines(d[,tick_s][in_ft_mod:in_tpause],alt[in_ft_mod:in_tpause],lty=2)
  lines(binnedobs_o3[,tick_s],binnedobs_alt,lty=2,col='blue')
  legend('bottomright',legend = c('obs','model','binned obs'),lty=c(NA,2,2),
         pch=c(1,NA,NA),col=c(rgb(0.5,0.5,0.5,0.4),'black','blue'))

 
  dev.off()
}

#saving binned observation o3
# write.csv(binnedobs_o3,file='binnedobs_o3.csv',row.names=F)


