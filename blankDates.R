#open packages
library("fields");library("plyr");library("R.matlab");library("pracma")
library("leaps");library("usdm");library("RColorBrewer");library("rgdal")
library("rgeos")

blankDates <- function(sMonth,eMonth,inYears) {
  daysN <- c(31,28,31,30,31,30,31,31,30,31,30,31,0)
  daysL <- c(31,29,31,30,31,30,31,31,30,31,30,31,0)
  
  dayLall <- list()
  for (iYear in 1:length(inYears)) {
    if (inYears[iYear] %% 4 != 0) {
      inDays <- daysN
    } else (inDays <- daysL)
    TDays <- sum(inDays)
    
    dayL <- list()
    for (iMonth in 1:12) {
      dayL[[iMonth]] <- cbind(rep(inYears[iYear],inDays[iMonth]),rep(iMonth,inDays[iMonth]),1:inDays[iMonth])
    }
    dayLall[[iYear]] <- cbind(do.call(rbind,dayL),1:TDays)
  } 
  
  dayLall <- data.frame(do.call(rbind,dayLall))
  colnames(dayLall) <- c("Year","Month","Day","Julian")
  
  sRow <- which(dayLall$Year == inYears[1] & dayLall$Month == sMonth)[1]
  eRow <- sort(which(dayLall$Year == inYears[length(inYears)] & dayLall$Month == eMonth),decreasing=T)[1]
  
  dayLall <- dayLall[sRow:eRow,]
  
  return(dayLall)
}