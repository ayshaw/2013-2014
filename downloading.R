rm(list=ls())
setwd('c:/Users/ashaw/Documents/R/EPS 236/EPS 236 final project/2013-2014')
load("urls.Rdata")
file_name=rep('',length(urls))
for (tick_urls in 1:length(urls)){
  a <- unlist(strsplit(urls[tick_urls], split='ftp://aftp.cmdl.noaa.gov/data/ozwv/Ozonesonde/Trinidad%20Head,%20California/Native%20Resolution/', fixed=TRUE))[2]
  file_name[tick_urls] <- unlist(strsplit(a, split='.dat', fixed=TRUE))[1]
  #downloading data
  data_raw <- read.delim(urls[tick_urls], header = T, sep = ",", skip = 46)
  d <- data_raw[-1,]
  save(d,file=paste('raw_data\\raw data list',file_name[tick_urls],'.Rdata'))
}
file_name=file_name[-34]
save(file_name,file='file_name.Rdata')
