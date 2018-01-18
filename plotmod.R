rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")
b=read.csv('data_model.csv')
c=matrix(unlist(b), ncol = 33, byrow = TRUE)
d=c[2:73,]*10^6
lvl=seq(1,72,by=1)
e=read.csv('levelalt.csv')
f=matrix(unlist(e))
g=seq(2,144,by=2)[1:72]
alt=rev(f[g])
h=read.csv('boundaries.csv')
bounds=h[seq(143,1,by=-2)[1:72],1]
load("file_name.Rdata")
require('signal');require('prospectr');require('pspline')
difference=list()
diff_mean=list()
num_layers=list()
date=list()
binnedobs_alt=alt[14:29]
write.csv(binnedobs_alt,file='binnedobs_alt.csv',row.names=F)
binnedobs_o3=matrix(0,length(binnedobs_alt),length(file_name))
for (tick_s in 1:length(file_name)){
  b=list()
  load(paste('data_fixed\\data_fixed',file_name[tick_s],'.Rdata',sep=''))
  #getting rid of missing data
  red=cbind(b$o3_mr,b$alt,b$temperature)
  blue=red[complete.cases(red), ]
  index_tropp=which(blue[,2]>12)[1]
  ####defining tropopause
  tprange_bot=which(blue[,2]>6)[1]
  tprange_top=which(blue[,2]>16)[1]
  if(is.na(tprange_top)==T){tprange_top=max(blue[,2])}
  tprange=blue[tprange_bot:tprange_top,]
  tprange_fit=sm.spline(x=tprange[,2],y=tprange[,3],spar=8E-1)
  lapse_rate=c(matrix(0,c(1,1)),diff(tprange_fit$ysmth))/c(matrix(0,c(1,1)),diff(tprange_fit$x))
  tpause=10
  in_tpause=which(blue[,2]>tpause)[1]
  #### range of values (purple)
  in_ft=which(blue[,2]>2)[1]
  purple=blue[in_ft:in_tpause,]
  binnedobs_o3[,tick_s]=tapply(purple[,1],.bincode(purple[,2],breaks=bounds,right=T,include.lowest=T),mean,na.rm=T)
  in_ft_mod=which(alt>2)[1]
  in_tpause=which(alt>tpause)[1]
  date[[tick_s]]=paste(substr(file_name[tick_s],7,10),substr(file_name[tick_s],12,13),substr(file_name[tick_s],15,16),sep='/')
  png(paste('mod vs obs\\mod vs obs',file_name[tick_s],'.png',sep=''))
  plot(purple[,1],purple[,2],ylim=c(2,10),col=rgb(0.5,0.5,0.5,0.4),
       main=paste('Ozone', date[[tick_s]],sep=', '),xlab='ppmv',ylab='km',xlim=c(0.03,0.12))
  lines(binnedobs_o3[,tick_s],binnedobs_alt,lty=2,col='blue')
  lines(d[14:29,tick_s],binnedobs_alt,lty=2,col='black')
  legend('bottomright',legend = c('obs','model','mod. res. obs.'),lty=c(NA,2,2),
         pch=c(1,NA,NA),col=c(rgb(0.5,0.5,0.5,0.4),'black','blue'))

  dev.off()
}
write.csv(binnedobs_o3,file='binnedobs_o3.csv',row.names=F)
