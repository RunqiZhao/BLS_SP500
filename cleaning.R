
library(stringr)
library(plyr)
library(reshape2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)

sp500 <- read.csv("sp.csv")
sp500$Date <- mdy(sp500$Date)
sp500$wday <- wday(sp500$Date)

sp500$year <- year(sp500$Date)
sp500$month <- month(sp500$Date)
sp500$ym <- str_c(sp500$year,sp500$month,sep= "-")


Nub <- 0
sp500$pub <- 0
for (i in 2:dim(sp500)[1]){
  ym <- sp500$ym[(i-1)]
  if(sp500$ym[i] == ym){
    if(sp500$wday[i] == 6){
      Nub <- Nub+1
      sp500$pub[i] <- Nub
    }
  }
  else{
    ym <- sp500$ym[i]
    Nub <- 0
    if(sp500$ym[i] == ym){
      if(sp500$wday[i] == 6){
        Nub <- Nub+1
        sp500$pub[i] <- Nub
      }
    }
  }
}

for (i in 1:dim(sp500)[1]){
  if(sp500$pub[i] != 1){
    sp500$pub[i] <-0
  }
}
