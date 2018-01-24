rm(list=ls())
setwd("C:\\Users\\ashaw\\Documents\\R\\EPS 236\\EPS 236 final project\\2013-2014")

#packages and data required
load("file_name.Rdata")
require('signal');require('prospectr');require('pspline')

#create list to store data
difference=list()
diff_mean=list()
num_layers=list()
date=list()
width_layers=list()
altitude_layers=list()

#some funky data on this date:
file_name=file_name[-which(file_name=="th968_2014_06_05_fleout")]

for (tick_s in 1:length(file_name)){
  #list of raw data
  b=list()
  
  #loading the saved raw data
  load(paste('data_fixed\\data_fixed',file_name[tick_s],'.Rdata',sep=''))
  
  #getting rid of missing data
  red=cbind(b$o3_mr,b$alt,b$temperature,b$wv_mr)
  blue=red[complete.cases(red), ]
  
  #defining altitude range for tropopause
  tprange_bot=which(blue[,2]>8)[1]
  tprange_top=which(blue[,2]>16)[1]
  plot_top=which(blue[,2]>12)[1]
  plot_bot=which(blue[,2]>2)[1]
  orange=blue[plot_bot:plot_top,]
  
  #if the top is an NA, we assign 11km as the tropopause
  if(is.na(tprange_top)==T){tprange_top=blue[which(blue[,2]-11<.3)[1],2]}
  
  #range of values to search for lapse rate change
  tprange=blue[tprange_bot:tprange_top,]
  tprange_fit=sm.spline(x=tprange[,2],y=tprange[,3],spar=9E-1)
  
  #using lapse rate to define tropopause
  lapse_rate=c(matrix(0,c(1,1)),diff(tprange_fit$ysmth))/c(matrix(0,c(1,1)),diff(tprange_fit$x))
  tpause=tprange_fit$x[which(abs(lapse_rate)<2)[1]]
  in_tpause=which(blue[,2]>tpause-1.5)[1]
  in_ft=which(blue[,2]>2)[1]
  purple=blue[in_ft:in_tpause,]
  
  #fit spline to ozone data 
  z0=sm.spline(x=purple[,2],y=purple[,1],spar=1E-1)
  
  #find the max and concavity. I fitted a spline to the data to clean up stragglers
  source("finite_differences.R")
  fdx=finite.differences(z0$x,z0$ysmth)
  y=sm.spline(x=z0$x,y=fdx,spar=1E-1)
  y_reshape=approx(x=y$x,y=y$ysmth,xout=z0$x,n=length(fdx))
  fdx2=finite.differences(z0$x,y_reshape$y)
  yy=sm.spline(x=z0$x,y=fdx2,spar=1E-2)
  yy_reshape=approx(x=yy$x,y=yy$ysmth,xout=z0$x,n=length(fdx2))
  index0_diff1=which(diff(sign(y_reshape$y))!=0)
  index0_diff2=which(diff(sign(yy_reshape$y))!=0)
  purple_reshape=approx(x=purple[,2],y=purple[,1],xout=z0$x,n=length(fdx))
 
  #list of variables stored in loop
  width=rep(0,length(index0_diff1))
  maxima=rep(0,length(index0_diff1))
  max_pt=rep(0,length(index0_diff1))
  altitude_layer=rep(0,length(index0_diff1))

  if(length(index0_diff1)>0){  #make sure that you don't have a flat profile
  for (tick_alt in 1:length(index0_diff1)){ #looping through all max/min
    
    #making sure that the max is between a concavity change
     if(index0_diff1[tick_alt]<max(index0_diff2)& index0_diff1[tick_alt]>min(index0_diff2)){
    
        #finding the lower and upper bounds of the layer
        upper_in=index0_diff2[which(index0_diff2>index0_diff1[tick_alt])[1]] 
        upperbound=z0$x[upper_in] 
        lower_in=index0_diff2[rev(which(index0_diff2<index0_diff1[tick_alt]))[1]]
        lowerbound=z0$x[lower_in]
        
        #finding width of layer 
        width[tick_alt]=upperbound-lowerbound
        
        #making sure that the ozone values in the upper and lower constraints of width aren't too different
         if(purple_reshape$y[upper_in]-purple_reshape$y[lower_in]>.015)
         {width[tick_alt]=.1}
       
        #value of maxima is computed from difference of ozone values at concavity changes
        maxima[tick_alt]=max(purple_reshape$y[lower_in:upper_in])-.5*(purple_reshape$y[lower_in]+purple_reshape$y[upper_in])    
        
        #real maxima value is computed from the max point      
        max_pt[tick_alt]=max(purple_reshape$y[lower_in:upper_in])
      
        #altitude of layer is stored as the max_pt altitude
        altitude_layer[tick_alt]=purple_reshape$x[which.max(purple_reshape$y[lower_in:upper_in])]
      }
    
  }

  #look for places that meet the laminae criteria
  maxima_10_in=which(maxima>.01)
  width_met_in=which(width>.3&width<3.5)
  ppb_80_in=which(max_pt>.08&max_pt<.2)
  
  #record the width and altitude of the layers that met the criteria
  width_layers[[tick_s]]=width[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]
  altitude_layers[[tick_s]]=altitude_layer[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]
 
  
  #store date of the maxima and number of qualified laminae observed
  date[[tick_s]]=paste(substr(file_name[tick_s],7,10),substr(file_name[tick_s],12,13),substr(file_name[tick_s],15,16),sep='/')
  num_layers[[tick_s]]=length(intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)) 
  
  #exception for layer is split in two 8.29.13
  if(tick_s==7)
    {width_layers[[tick_s]]=sum(width_layers[[tick_s]])
    altitude_layers[[tick_s]]=mean(altitude_layers[[tick_s]])
    num_layers[[tick_s]]=1
    }
  
  #store maximum values and altitudes and saving it in obs_max files
  obs_max_o3=max_pt[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]
  obs_max_alt=purple_reshape$x[index0_diff1[intersect(intersect(maxima_10_in,width_met_in),ppb_80_in)]]
  write.csv(cbind(obs_max_o3,obs_max_alt),file = paste('max\\obs_max',file_name[tick_s],'.csv'),row.names = F)
  
  #plotting layers identified
  png(paste('identifyinglayers\\identifyinglayers_observation',file_name[tick_s],'.png'))
  plot(orange[,1],orange[,2],col=rgb(0.5,0.5,0.5,0.4),main=date[[tick_s]],xlab='ppmv',ylab='km',xlim=c(.03,.12),ylim=c(2,12))
  lines(z0$ysmth,z0$x)
  points(obs_max_o3,obs_max_alt,col='red')
  points(z0$ysmth[index0_diff2],z0$x[index0_diff2],col='blue')
  mtext(paste('no.layers=',num_layers[[tick_s]],sep=''), side = 4)
  abline(v=.08, lty=2,col='red')
  dev.off()
  
  #plot correlating water vapor and maxima to identify the stratospheric intrusions from pollution
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
  mtext(paste('no.layers=',num_layers[[tick_s]],sep=''), side = 4)
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

#histograms
png('hist\\histogram_widths.png')
hist(unlist(width_layers),breaks=10,xlab='Layer Width [km]',main='Distribution of Layer Widths-obs',col='red',xlim=c(0,1.5))
dev.off()
png('hist\\histogram_altitude.png')
hist(unlist(altitude_layers),breaks=10,xlab='Layer altitude [km]',main="Distribution of Layer altitudes-obs",col='blue',xlim=c(0,4),ylim=c(0,8))
dev.off()
png('nolayers\\nolayers_obs.png')
dateplot=as.Date(unlist(date),'%Y/%m/%d')
plot(dateplot,num_layers,type='l',col='black',xlab='month',ylab='no. layers',ylim=c(0,2))
dev.off()

#save number of layers and altitude of layers per date
saveRDS(num_layers,file = "nolayers\\num_layers_obs.rds")
saveRDS(altitude_layers,file='altitude_layers_obs.rds')


