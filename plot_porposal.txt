  rm(list=ls())
  setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")
  b=read.csv('data_model.csv')
  c=matrix(unlist(b), ncol = 33, byrow = TRUE)
  d=c[2:73,]*10^9
  lvl=seq(1,72,by=1)
  e=read.csv('levelalt.csv')
  f=matrix(unlist(e))
  g=seq(2,144,by=2)[1:72]
  alt=rev(f[g])
  h=read.csv('boundaries.csv')
  bounds=h[seq(143,1,by=-2)[1:72],1]
  load("file_name.Rdata")
  require('signal');require('prospectr');require('pspline');library(RColorBrewer)
  difference=list()
  diff_mean=list()
  num_layers=list()
  date=list()
  binnedobs_alt=alt[14:29]
  # cols <- c('black')
  write.csv(binnedobs_alt,file='binnedobs_alt.csv',row.names=F)
  binnedobs_o3=matrix(0,length(binnedobs_alt),length(file_name))
  png(paste('mod vs obs proposal\\proposal','.png',sep=''),height=700,width=1500)
  plot.new()
  par(mfrow=c(1,4),mar=c(6,2,3,1),oma=c(4,5,6,1))
  vecfilename=c(2,7,30)
  for (tick_s in 1:length(vecfilename)){
    b=list()
    load(paste('data_fixed\\data_fixed',file_name[vecfilename[tick_s]],'.Rdata',sep=''))
    #getting rid of missing data
    red=cbind(b$o3_mr*10^3,b$alt,b$temperature)
    blue=red[complete.cases(red), ]
    tpause=10
    in_tpause=which(blue[,2]>tpause)[1]
    #### range of values (purple)
    in_ft=which(blue[,2]>2)[1]
    purple=blue[in_ft:in_tpause,]
    binnedobs_o3[,tick_s]=tapply(purple[,1],.bincode(purple[,2],breaks=bounds,right=T,include.lowest=T),mean,na.rm=T)
    in_ft_mod=which(alt>2)[1]
    in_tpause=which(alt>tpause)[1]
    date[[tick_s]]=paste(substr(file_name[vecfilename[tick_s]],7,10),substr(file_name[vecfilename[tick_s]],12,13),substr(file_name[vecfilename[tick_s]],15,16),sep='/')
    
    plot(purple[,1],purple[,2],ylim=c(2,10),col='black',
         main=paste( date[[tick_s]]),xlab='',ylab='',xlim=c(0,120),axes=F,cex.main=3)
    lines(d[14:29,vecfilename[tick_s]],binnedobs_alt,lty=3,col='red',lwd=2)
    lines(binnedobs_o3[,tick_s],binnedobs_alt,lty=5,col='black',lwd=2)
    axis(side=1,xlim=c(0,120),cex.axis=2,lwd=2)
    axis(side=2,ylim=c(2,10),cex.axis=2,lwd=2)
    mtext(side=1,text='ozone [ppbv]',line=4,cex=2)
    if (tick_s==1){mtext(side=2,text='altitude [km]',line=3,adj=.5,cex=2,outer=T)
      legend('topleft',legend=c('observations','model'),cex=2.7,bty = "n",text.col=c('black','red'))}
    if (tick_s==2){mtext(side=1,line=8,text='(a)',cex=2)}
    # legend('bottomright',legend=c('observations','model'),
    #        col='black',lty=c(1,3),cex=2.3,bty='n',lwd=2)
  }
  rm(list=ls())
  setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")
  load("file_name.Rdata")
  require('signal');require('prospectr');require('pspline')
  difference=list()
  diff_mean=list()
  num_layers=list()
  date=list()
  
  
  # alt=list()
  b=read.csv('binnedobs_o3.csv')
  c=matrix(unlist(b), ncol = 33, byrow = TRUE)*10^3
  
  e=read.csv('binnedobs_alt.csv')
  f=matrix(unlist(e))
  g=read.csv('data_model.csv')
  h=matrix(unlist(g),ncol=33,byrow=TRUE)*10^9
  for (tick_s in 1:length(file_name)){
    date[[tick_s]]=as.numeric(paste(substr(file_name[tick_s],7,10),substr(file_name[tick_s],12,13),substr(file_name[tick_s],15,16),sep=''))
  }
  o3mat=c
  seasons=c(20130600,20130900,20131200,20140300,20140700)
  season=.bincode(unlist(date),breaks=seasons,right=F,include.lowest=T)
  
  season_name=c('Summer 2013','Fall 2013','Winter 2014','Spring 2014')
tick_se=2

  {avg=apply(o3mat[,which(season==tick_se)],MARGIN = 1,mean,na.rm=T)
  avg_mod=apply(h[,which(season==tick_se)],MARGIN = 1,mean,na.rm=T)
  std=apply(o3mat[,which(season==tick_se)],MARGIN = 1,sd,na.rm=T)
  std_mod=apply(h[,which(season==tick_se)],MARGIN = 1,sd,na.rm=T)
  
  plot(avg,e$x,type='l',xlab='',ylab='',main=season_name[tick_se],xlim=c(0,120),cex.main=3,col='black',axes=F,lwd=2)
  axis(side=1,xlim=c(0,120),cex.axis=2,lwd=2)
  axis(side=2,ylim=c(2,10),cex.axis=2,lwd=2)
  arrows(avg-std, e$x, avg+std, e$x, code=3, angle=90, length=0.1,col='black',lwd=2)
  arrows(avg_mod[14:29]-std_mod[14:29], e$x, avg_mod[14:29]+std_mod[14:29], e$x, code=3, angle=90, length=0.1,col='red',lwd=1.5)
  mtext(side=1,text='ozone [ppbv]',line=4,cex=2)
  # mtext(side=2,text='altitude [km]',line=5,adj=.5,cex=1.5)
  lines(avg_mod[14:29],e$x,lty=3,lwd=2,col='red')
  mtext(side=1,text='(b)',line=8,cex=2)
  }

  mtext(side=3,text='Ozone Profiles over Trinidad Head, California',outer=T,cex=3,line=2)
  dev.off()
  # write.csv(binnedobs_o3,file='binnedobs_o3.csv',row.names=F)
