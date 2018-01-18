rm(list=ls())
setwd('c:/Users/ashaw/Documents/R/EPS 236/EPS 236 final project/2013-2014')
load("urls.Rdata")

load('file_name.Rdata')
for (tick_urls in 1:length(urls)){
  d=get(load(file=paste('raw data list',file_name[tick_urls],'.Rdata')))
  wv_mr <- as.numeric(matrix(unlist(d$RS.H2O.Mr)))
  pp_o3 <- as.numeric(matrix(unlist(d$O3.P)))
  alt <- as.numeric(matrix(unlist(d$GPS.alt)))
  o3_mr <- as.numeric(matrix(unlist(d$O3.Mr)))
  pressure<-as.numeric(matrix(unlist(d$Press)))
  temperature<-as.numeric(matrix(unlist(d$Temp)))
  
  save_name <- paste(file_name[tick_urls],'.csv',sep='')
  b=cbind(wv_mr,pp_o3,alt,o3_mr,pressure,temperature)
  write.csv(b,file=save_name,row.names=F)
}





