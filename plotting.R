setwd('c:/Users/ashaw/Documents/R/EPS 236/EPS 236 final project/2013-2014')
rm(list=ls())
load('file_name.Rdata')
b=read.csv('data_model.csv')
c=matrix(unlist(b), ncol = 33, byrow = TRUE)
d=c[2:73,1:33]*10^6
lvl=seq(1,72,by=1)
e=read.csv('levelalt.csv')
f=matrix(unlist(e))
g=seq(1,145,by=2)[1:72]
alt=rev(f[g])
date=list()
for (tick_f in 1:length(file_name)){
  #load file
  b=list()
  b=read.csv(paste(file_name[tick_f],'.csv',sep=''))
  # get file name
  file_n <- unlist(strsplit(file_name[tick_f], split='.csv', fixed=TRUE))[1]
  # cleaning outliers  
  outliers_wv=which(b[,'wv_mr']>mean(b[,'wv_mr'])*10)
  outliers_pp_o3=which(b[,'pp_o3']>mean(b[,'pp_o3'])*10)
  outliers_alt=which(b[,'alt']>mean(b[,'alt'])*1000)
  outliers_o3_mr=which(b[,'o3_mr']>15)
  outliers_pressure=which(b[,'pressure']>1300)
  outliers_temp=which(b[,'temperature']>45&b[,'temperature']<(-60))
  b[,'wv_mr'][outliers_wv]=NA
  b[,'pp_o3'][outliers_pp_o3]=NA
  b[,'alt'][outliers_alt]=NA
  b[,'o3_mr'][outliers_o3_mr]=NA
  b[,'pressure'][outliers_pressure]=NA
  b[,'temperature'][outliers_temp]=NA
  save(b,file=paste('data_fixed\\data_fixed',file_n,'.Rdata',sep=''))
  red=cbind(b$wv_mr,b$pp_o3,b$alt)
  blue=red[complete.cases(red),]
  index_max_alt=which.max(red[,3])
  # purple=blue[1:index_max_alt,]
  # plot the alt vs o3
  date[[tick_f]]=paste(substr(file_name[tick_f],7,10),substr(file_name[tick_f],12,13),substr(file_name[tick_f],15,16),sep='/')
  
  png(paste('Alt_vs_o3\\Alt vs o3',file_name[tick_f],'.png'),height=600, width = 1000)
  par(mar=c(6,6,6,6))
  plot(b[,'o3_mr'][1:index_max_alt],b[,'alt'][1:index_max_alt],type='l',xlab='Ozone [ppmv]',ylab='Altitude [km]',cex=2,col='blue',cex.lab=1.5,cex.axis=1.5,xlim=range(b[,'o3_mr'],na.rm=T),main=date[[tick_f]])
  lines(d[,tick_f],alt,lty=2)
  dev.off()
  #   plot the alt vs wv_mr
  if (length(which(is.na(b[,'wv_mr'])))<.10*length(b[,'wv_mr'])){
    png(paste('Alt_vs_wv\\Alt vs wv',file_name[tick_f],'.png'), height = 1000, width = 1000)
    plot(b[,'wv_mr'][1:index_max_alt],b[,'alt'][1:index_max_alt],type='l',xlab='Water Vapor Mixing Ratio [ppmv]',ylab='Altitude [km]',cex=2,col='red',cex.lab=1.5,cex.axis=1.5,xlim=range(b[,'wv_mr'],na.rm=T))
        dev.off()
  }
  
}
