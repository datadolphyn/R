# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library(XML)
library(plyr)
library(quantmod)
library(data.table)
library("zoo")

folder.path = "./raw-data/fin-markets/"
russell <- new.env()
# get the list of symbols from the csv file
ru2000 <- read.csv(paste(folder.path, "russell-2000-components.csv", sep=""), stringsAsFactors=F)
colnames(ru2000) <- c("name", "ticker")
#ru2000$name  <- gsub(ru2000$ticker, "", ru2000$name) 

getDailyReturns <- function(sym) {
  y <- tryCatch(getSymbols(sym, auto.assign=FALSE, from="2007-01-01"), error = identity)
  
  if(inherits(y, "error"))
    cat("Symbol '", sym, "' not downloadable!\n", sep = "")
  else {
    y <- tryCatch(cbind(y, OpCl(y)*100), error = identity)
    if(inherits(y, "error"))
      cat("Symbol '", sym, "' not downloadable!\n", sep = "")
    else {
      write.csv(data.frame( date=index(y), coredata(y) ),row.names=FALSE, file=paste(sym, ".csv", sep=""))
      cat("Sucessfully saved the stock data to %s",sym)
    }
  }
}

ru2000.DR <- llply(ru2000$Ticker[1109:nrow(ru2000)], getDailyReturns, .progress="text")


#transpose d to have tickers as columns and date as rows
ru2000.DR.T <- t(ru2000.DR)
colnames(ru2000.DR.T)<- head(ru2000$Ticker)
AACC <- data.frame[seq(as.Date("2007-01-01"), Sys.Date(), by="day"), ru2000.DR.T$AACC]
#SP100.MR <- data.table(SP100.MR)
plot(SP100.MR$AAP, ylab="Apple", main="Montly Returns Stock"
     read.zoo(SP100.MR)
     #d <- unlist(llply(l, getMonthlyReturns, .progress="text"))
     # bounds at -10% and +10% for visual clarity
     d[d < -10] <- -10
     d[d > 10] <- 10
     
     heatmap_2(t(matrix(d, ncol=100)), col=brewer.pal(9, 'PuBu'), 
               Rowv=NA, Colv=NA, do.dendro=c(FALSE,FALSE), scale='none', legend=2, 
               main="S&P 100 since 2007 (monthly returns)")