
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

sp500$pct <- NA
for (i in 2:dim(sp500)[1]){
  sp500$pct[i] <- (sp500$Adj.Close[i] - sp500$Adj.Close[(i-1)])/sp500$Adj.Close[(i-1)]
}

sp500 <- sp500 %>% select("Date","Close","Adj.Close","ym","pub","pct")

sp500$BA <- NA
sp500$group <- NA
for (i in 5:dim(sp500)[1]){
  if(sp500$pub[i] == 1){
    sp500$BA[(i-5):(i-1)] <- "Before"
    sp500$BA[i:(i+4)] <- "After"
    sp500$group[(i-5):(i+4)] <- sp500$Date[i]
  }
}

dt <- na.omit(sp500)

before <- dt %>% filter(BA == "Before")
after <- dt %>% filter(BA == "After")

# dt2 <- before %>% group_by(group) %>% mean(pct)
dt2 <- ddply(before,.(group),function(sub){data.frame(pct.mean = mean(sub$pct))})
bb <- dt2$pct.mean
hist(bb)

dt3 <- ddply(after,.(group),function(sub){data.frame(pct.mean = mean(sub$pct))})
aa <- dt3$pct.mean
hist(aa)
