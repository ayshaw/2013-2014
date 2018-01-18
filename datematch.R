rm(list=ls())
source('blankDates.R')
setwd("c:/Users/ashaw/Documents/R/EPS 236/EPS 236 final project/2013-2014")
y2013=blankDates(7,12,2013)
y2014=blankDates(1,8,2014)
y_com=(rbind(y2013,y2014))
y_com_mat=cbind(y_com$Year,y_com$Month,y_com$Day)
y_com_vec=rep(0,length(y_com_mat[,1]))
for (tick_d in 1:length(y_com_mat[,1])){
  if (nchar(y_com_mat[tick_d,2])<2&nchar(y_com_mat[tick_d,3])==2){
    y_com_vec[tick_d]= as.numeric(paste(y_com_mat[tick_d,1],0,y_com_mat[tick_d,2],y_com_mat[tick_d,3], sep = ""))
  }
  
  if (nchar(y_com_mat[tick_d,3])<2&nchar(y_com_mat[tick_d,2])==2){
    y_com_vec[tick_d]= as.numeric(paste(y_com_mat[tick_d,1],y_com_mat[tick_d,2],0,y_com_mat[tick_d,3], sep = ""))
  }
  if (nchar(y_com_mat[tick_d,3])<2&nchar(y_com_mat[tick_d,2])<2){
    y_com_vec[tick_d]= as.numeric(paste(y_com_mat[tick_d,1],0,y_com_mat[tick_d,2],0,y_com_mat[tick_d,3], sep = ""))
    
  }
  if (nchar(y_com_mat[tick_d,3])==2&nchar(y_com_mat[tick_d,2])==2){
    y_com_vec[tick_d]= as.numeric(paste(y_com_mat[tick_d,1],y_com_mat[tick_d,2],y_com_mat[tick_d,3], sep = ""))
  }

}
beg=which(y_com_vec==20130702)
end=which(y_com_vec==20140701)
y_mod=y_com_vec[beg:end]
load('file_name.Rdata')
year=substr(file_name,7,10)
month=substr(file_name,12,13)
day=substr(file_name,15,16)
y_obs=as.numeric(paste(year,month,day,sep=''))
T18_in=rep(0,length(y_obs))
for (tick_t in 1:length(y_obs)){
T18_in[tick_t]=which(y_mod==y_obs[tick_t])-1
}
write.table(T18_in,'indices18.csv',sep=',')

