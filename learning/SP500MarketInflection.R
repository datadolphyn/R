# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library(XML)
library(plyr)
library(quantmod)
#library(data.table)
#library("zoo")

# get the list of symbols from Wiki
sp500 <- readHTMLTable('http://en.wikipedia.org/wiki/List_of_S%26P_500_companies', 
                       which=2, header=FALSE, skip.rows=1)
sp500.tickers <- as.vector(sp500$V1)
#Remove LOW
sp500.tickers <- sp500.tickers[-which(sp500.tickers=="LOW")]
sp500.tickers <- sp500.tickers[-which(sp500.tickers=="PSX")]
sp500.tickers <- sp500.tickers[-which(sp500.tickers=="XYL")]
sp500.tickers <- sp500.tickers[-which(sp500.tickers=="TRIP")]
sp500.tickers <- sp500.tickers[-which(sp500.tickers=="WPX")]

sp500.tickers[which(sp500.tickers=="BFB")]="BF-B"

isFiftyTwoWeekLow <- function(sym) {
  #to <- "2012/6/01"
  #to <- "2012/4/02"
  to <- "2011/10/04"
  from <- last(seq(as.Date(to), by="-1 week", length.out=52))
  #last(seq(Sys.Date(), by="-1 week", length.out=52))
  print(sym)
  y <- getSymbols(sym, auto.assign=FALSE, from=from, to=to)
  low <- ifelse(Cl(last(y))[[1]] < Cl(seriesLo(last(y, '52 weeks')))[[1]], 1, 0)
  return (low)
}
#Percent of stocks trading at 52 weeks low
sp500.low <- ldply(sp500.tickers, isFiftyTwoWeekLow, .progress="text")
per.stocks.trading.low.Jun1 <- (sum(sp500.low$V1)/length(sp500.low$V1))*100

#Get SP500 Index data
getSymbols("^gspc")
Cl(last(GSPC))[[1]]  #Get Last Close
Cl(seriesLo(last(GSPC, '52 weeks')))[[1]] #Get Low from last 52weeks

#April 2nd - SP500 closed at 1419, multi-year high - What Percent of stocks trading at 52 weeks low
sp500.low.Apr2 <- ldply(sp500.tickers, isFiftyTwoWeekLow, .progress="text")
per.stocks.trading.low.Apr2 <- (sum(sp500.low.Apr2$V1)/length(sp500.low.Apr2$V1))*100


#October 4th - SP500 closed at 1123.95 - What Percent of stocks were trading at 52 weeks low
sp500.low.Oct4 <- ldply(sp500.tickers, isFiftyTwoWeekLow, .progress="text")
per.stocks.trading.low.Oct4 <- (sum(sp500.low.Oct4$V1)/length(sp500.low.Oct4$V1))*100

