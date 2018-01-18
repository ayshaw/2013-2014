rm(list=ls())
setwd('c:/Users/ashaw/Documents/R/EPS 236/EPS 236 final project/2013-2014')
urls_raw <- read.table('urls for ozonesonde.txt')
a <- as.character(urls_raw$V1)
names <- rep('',length(a))
urls <- rep('',length(a))
starter <- 'ftp://aftp.cmdl.noaa.gov/data/ozwv/Ozonesonde/Trinidad%20Head,%20California/Native%20Resolution/'
for (tick_url in 1:length(a)){
  names[tick_url] <- substr(a[tick_url],17,43)
  urls[tick_url] <- paste(starter,names[tick_url],sep='') 
}
save(urls,file='urls.Rdata')
