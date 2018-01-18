rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")
load("file_name.Rdata")
require('signal');require('prospectr');require('pspline')
difference=list()
diff_mean=list()
num_layers_obs=list()
date=list()
#some funky data on this date:
file_name=file_name[-which(file_name=="th968_2014_06_05_fleout")]

width_layers=list()
altitude_layers=list()
for (tick_s in 1:length(file_name)){
  b=list()
  load(paste('data_fixed\\data_fixed',file_name[tick_s],'.Rdata',sep=''))
  #getting rid of missing data
  red=cbind(b$o3_mr,b$alt,b$temperature,b$wv_mr)
  blue=red[complete.cases(red), ]
  index_tropp=which(blue[,2]>12)[1]
  ####defining tropopause
  tprange_bot=which(blue[,2]>8)[1]
  tprange_top=which(blue[,2]>16)[1]
  plot_top=which(blue[,2]>12)[1]
  plot_bot=which(blue[,2]>2)[1]
  orange=blue[plot_bot:plot_top,]
  if(is.na(tprange_top)==T){tprange_top=blue[which(blue[,2]-11<.3)[1],2]}
  tprange=blue[tprange_bot:tprange_top,]
  tprange_fit=sm.spline(x=tprange[,2],y=tprange[,3],spar=9E-1)
  lapse_rate=c(matrix(0,c(1,1)),diff(tprange_fit$ysmth))/c(matrix(0,c(1,1)),diff(tprange_fit$x))
  tpause=tprange_fit$x[which(abs(lapse_rate)<2)[1]]
  in_tpause=which(blue[,2]>tpause-1.5)[1]
  in_ft=which(blue[,2]>2)[1]
  purple=blue[in_ft:in_tpause,]
  ### fit spline
  z0=sm.spline(x=purple[,2],y=purple[,1],spar=1E-1)
  
  
  source("finite_differences.R")
  fdx=finite.differences(z0$x,z0$ysmth)
  y=sm.spline(x=z0$x,y=fdx,spar=1E-1)
  y_reshape=approx(x=y$x,y=y$ysmth,xout=z0$x,n=length(fdx))
  fdx2=finite.differences(z0$x,y_reshape$y)
  yy=sm.spline(x=z0$x,y=fdx2,spar=1E-2)
  yy_reshape=approx(x=yy$x,y=yy$ysmth,xout=z0$x,n=length(fdx2))
  
  
  index0_diff1=which(diff(sign(y_reshape$y))!=0)
  index0_diff2=which(diff(sign(yy_reshape$y))!=0)
  layers=logical(length(index0_diff1))
  purple_reshape=approx(x=purple[,2],y=purple[,1],xout=z0$x,n=length(fdx))
  width=rep(0,length(index0_diff1))
  maxima=rep(0,length(index0_diff1))
  max_pt=rep(0,length(index0_diff1))
  altitude_layer=rep(0,length(index0_diff1))

  if(length(index0_diff1)>0){  
  for (tick_alt in 1:length(index0_diff1)){
     if(index0_diff1[tick_alt]<max(index0_diff2)& index0_diff1[tick_alt]>min(index0_diff2)){
    upper_in=index0_diff2[which(index0_diff2>index0_diff1[tick_alt])[1]] 
    upperbound=z0$x[upper_in] 
    lower_in=index0_diff2[rev(which(index0_diff2<index0_diff1[tick_alt]))[1]]
    lowerbound=z0$x[lower_in]
       width[tick_alt]=upperbound-lowerbound
       if(purple_reshape$y[upper_in]-purple_reshape$y[lower_in]>.015)
       {width[tick_alt]=.1}
       
      maxima[tick_alt]=max(purple_reshape$y[lower_in:upper_in])-.5*(purple_reshape$y[lower_in]+purple_reshape$y[upper_in])    
      max_pt[tick_alt]=max(purple_reshape$y[lower_in:upper_in])
      altitude_layer[tick_alt]=purple_reshape$x[which.max(purple_reshape$y[lower_in:upper_in])]
      }
    
  }

  #### look for places that meet the laminae criteria
  maxima_10_in=which(maxima>.01)
  width_met_in=which(width>.3&width<3.5)
  ppb_80_in=which(max_pt>.08&max_pt<.2)
  width_layers[[tick_s]]=width[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]
  altitude_layers[[tick_s]]=altitude_layer[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]
  
  date[[tick_s]]=paste(substr(file_name[tick_s],7,10),substr(file_name[tick_s],12,13),substr(file_name[tick_s],15,16),sep='/')
  
  num_layers_obs[[tick_s]]=length(intersect(intersect(maxima_10_in,width_met_in),ppb_80_in))
  png(paste('identifyinglayers\\identifyinglayers_adamethod',file_name[tick_s],'.png'))
  plot(orange[,1],orange[,2],col=rgb(0.5,0.5,0.5,0.4),main=date[[tick_s]],xlab='ppmv',ylab='km',xlim=c(.03,.12),ylim=c(2,10))
  lines(z0$ysmth,z0$x)
  obs_max_o3=max_pt[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]
  obs_max_alt=purple_reshape$x[index0_diff1[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]]
  points(obs_max_o3,obs_max_alt,col='red')
  points(z0$ysmth[index0_diff2],z0$x[index0_diff2],col='blue')
  mtext(paste('no.layers=',num_layers_obs[[tick_s]],sep=''), side = 4)
  abline(v=.08, lty=2,col='red')
  dev.off()
  write.csv(cbind(obs_max_o3,obs_max_alt),file = paste('obs_max\\obs_max',file_name[tick_s],'.csv'),row.names = F)
  png(paste('correlating\\correlating_wv_adamethod',file_name[tick_s],'.png'))
  par(mar=c(9,4,3,3))
  plot(orange[,1],orange[,2],col=rgb(0.5,0.5,0.5,0.4),main=date[[tick_s]],xlab='',ylab='',xlim=c(0,.15),ylim=c(2,12),axes=FALSE)
  
  par(new=T)
  plot(z0$ysmth,z0$x,xlim=c(0,.15),ylim=c(2,12),type='l',axes=F,xlab='',ylab='')
  
  points(max_pt[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)],purple_reshape$x[index0_diff1[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]],col='red')
  points(z0$ysmth[index0_diff2],z0$x[index0_diff2],col='blue')
  axis(2,ylim=c(2,12),las=1)
  axis(1,xlim=c(0,.12),las=1,col='black')
  mtext('ozone ppmv',1,line=2,at=0.075,col='black')
  mtext(paste('no.layers=',num_layers_obs[[tick_s]],sep=''), side = 4)
  abline(v=.08, lty=2,col='red')
  par(new=T)
  plot(orange[,4],orange[,2],col=rgb(1,165/255,0,0.4),ylab='',xlab='',ylim=c(2,12),xlim=range(orange[,4]),axes=F)
  axis(1,xlim=range(orange[,4]), line=4,col=rgb(1,165/255,0,1),col.ticks=rgb(1,165/255,0,1),
          col.axis=rgb(1,165/255,0,1))
  mtext('H20 ppmv',1,line=6,at=0.5*(range(orange[,4])[2]-range(orange[,4])[1]),col=rgb(1,165/255,0,1))
  
   par(new=F)
  dev.off()
  }

}
png('hist\\histogram_widths.png')
hist(unlist(width_layers),breaks=10,xlab='Layer Width [km]',main='Distribution of Layer Widths',col='red')
dev.off()
png('hist\\histogram_altitude.png')
hist(unlist(altitude_layers),breaks=10,xlab='Layer altitude [km]',main="Distribution of Layer altitudes",col='blue')
dev.off()
png('nolayers\\nolayers_adamethod.png')
dateplot=as.Date(unlist(date),'%Y/%m/%d')
plot(dateplot,num_layers_obs,type='l',col='black',xlab='month',ylab='no. layers',ylim=c(0,2))
save(num_layers_obs,file = "num_layers_obs.Rdata")
dev.off()

