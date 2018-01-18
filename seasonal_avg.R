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
c=matrix(unlist(b), ncol = 33, byrow = TRUE)

e=read.csv('binnedobs_alt.csv')
f=matrix(unlist(e))
g=read.csv('data_model.csv')
h=matrix(unlist(g),ncol=33,byrow=TRUE)*10^6
for (tick_s in 1:length(file_name)){
  date[[tick_s]]=as.numeric(paste(substr(file_name[tick_s],7,10),substr(file_name[tick_s],12,13),substr(file_name[tick_s],15,16),sep=''))
}
o3mat=c
seasons=c(20130600,20130900,20131200,20140300,20140700)
season=.bincode(unlist(date),breaks=seasons,right=F,include.lowest=T)
png('seasonal_avg.png',height=700,width=1500)
plot.new()
par(mfrow=c(1,4))
par(mar=c(6,5,3,3))
season_name=c('Summer 2013','Fall 2013','Winter 2014','Spring 2014')
for (tick_se in 1:4)
{avg=apply(o3mat[,which(season==tick_se)],MARGIN = 1,mean,na.rm=T)
avg_mod=apply(h[,which(season==tick_se)],MARGIN = 1,mean,na.rm=T)
std=apply(o3mat[,which(season==tick_se)],MARGIN = 1,sd,na.rm=T)
std_mod=apply(h[,which(season==tick_se)],MARGIN = 1,sd,na.rm=T)

plot(avg,e$x,type='l',xlab='',ylab='',main=season_name[tick_se],xlim=c(0,.2),cex.main=3,col=rgb(1,0,0,1),axes=F,lwd=2)
axis(side=1,xlim=c(0,.15),cex.axis=2,lwd=2)
axis(side=2,ylim=c(2,10),cex.axis=2,lwd=2)
arrows(avg-std, e$x, avg+std, e$x, code=3, angle=90, length=0.1,col=rgb(1,0,0,0.5),lwd=1.5)
arrows(avg_mod[14:29]-std_mod[14:29], e$x, avg_mod[14:29]+std_mod[14:29], e$x, code=3, angle=90, length=0.1,col=rgb(0,0,0,0.4),lwd=1.5)
mtext(side=1,text='ozone [ppmv]',line=4,cex=1.5)
mtext(side=2,text='altitude [km]',line=5,adj=.5,cex=1.5)
lines(avg_mod[14:29],e$x,lty=2,lwd=2)
}
legend('bottomright',legend=c('observations','model'),cex=2.3,bty = "n",text.col=c('red','black'))

dev.off()
