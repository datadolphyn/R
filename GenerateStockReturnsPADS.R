# Download historical stock performance for S&P 100, S&P 500, Nasdaq and Russel2000
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

require("XML")
require("quantmod")
require("zoo")
require("chron")
source("CreatePADS.R")

#
# startup
#
startup <- function() {
  #initialize system
  initializeSystem()
  
  assign("yahoo.folder.path", "./pads/raw-data/fin-markets/", envir=.GlobalEnv)  
  assign("dataset", "Yahoo Finance", envir=.GlobalEnv)
  
  #prepare pad meta data
  series <- list()
  series["country"] <- "USA"
  series["source"] <- "Yahoo Finance"
  series["category"] <- "Financial Sector"
  series["subcategory"] <- "Capital markets"
  series["category_id"]<- 14
  series["subcategory_id"]<- 158
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, sep=","))
  assign("series", series, envir=.GlobalEnv)
  #assign("series.desc", series.desc, envir=.GlobalEnv)
  
  #load data
  loadData()
}

#
# cleanup IPL
#
cleanup <- function(){
  cleaupSystem()
}

#
# loadData
#
loadData <- function(){
  # get the list of symbols for SP100 from Wikipedia
  sp100.list <- try(readHTMLTable('http://en.wikipedia.org/wiki/S%26P_100')[[2]], silent=T)
  
  if(class(sp100.list) %in% c("try-error")) {
    stop("Can't continue. SP100 index components can't be get from Wikipedia")
  } else {
    assign("sp100.list", sp100.list,  envir=.GlobalEnv)
  }
  
  # get the list of symbols for SP500 from Wikipedia
  sp500.list <- try(readHTMLTable('http://en.wikipedia.org/wiki/List_of_S%26P_500_companies')[[2]], silent=T)
  if(class(sp500.list) %in% c("try-error")) {
    stop("Can't continue. sp500 index components can't be get from Wikipedia")
  } else {
    sp500.list <- sp500.list[c(1,2)]
    colnames(sp500.list) <- c("Symbol", "Name")
    assign("sp500.list", sp500.list,  envir=.GlobalEnv)
  }
  
  # get the list of symbols from the csv file
  ru2000.tickers <- read.csv(paste(yahoo.folder.path, "russell-2000-components.csv", sep=""), stringsAsFactors=F)
  ru2000.tickers <- ru2000.tickers[c(2,1)]
  colnames(ru2000.tickers) <- c("Symbol", "Name")
  assign("ru2000.tickers",ru2000.tickers,  envir=.GlobalEnv)
  
  # get the list of symbols from the csv file
  nasdaq.tickers <- read.csv(paste(yahoo.folder.path, "nasdaq-components.csv", sep=""), stringsAsFactors=F)
  nasdaq.tickers <- nasdaq.tickers[c(1,2)]
  colnames(nasdaq.tickers) <- c("Symbol", "Name")
  assign("nasdaq.tickers",nasdaq.tickers,  envir=.GlobalEnv)
}

#
# get returns for a given period
#
getReturns <- function(period, stock.prices, stock.ticker){
  switch(period, 
         Daily = { 
           pr <- dailyReturn(stock.prices)
           col.name <- paste(stock.ticker, "_daily_returns", sep="")
         },
         Weekly = {
           pr <- weeklyReturn(stock.prices)
           col.name <- paste(stock.ticker, "_weekly_returns", sep="")
         },
         Monthly = {
           pr <- monthlyReturn(stock.prices)
           col.name <- paste(stock.ticker, "_montlhy_returns", sep="")
         },
         Quarterly = {
           pr <- quarterlyReturn(stock.prices)
           col.name <- paste(stock.ticker, "_quarterly_returns", sep="")
         }, 
         Annual = {
           pr <- annualReturn(stock.prices)
           col.name <- paste(stock.ticker, "_annual_returns", sep="")
         }
  )
  #returns in percent 
  pr <- round(pr[,1]*100, 1)
  
  #generate a data.frame
  pr <- data.frame(index(pr), coredata(pr))
  colnames(pr) <- c("date", col.name)
  return(pr)
}
#l <- getReturns("m", OEX, "S&P100")

#
# get Combined returns for a given period
# 
getCombinedReturns <- function(period, stock.prices, stock.ticker, index.prices, index.ticker){ 
  switch(period, 
         Daily = { 
           sr <- dailyReturn(stock.prices)
           ir <- dailyReturn(index.prices)
           col.names <- c(paste(stock.ticker, "_daily_returns", sep=""), paste(index.ticker, "_daily_returns", sep=""))
         },
         Weekly = {
           sr <- weeklyReturn(stock.prices)
           ir <- weeklyReturn(index.prices)           
           col.names <- c(paste(stock.ticker, "_weekly_returns", sep=""), paste(index.ticker, "_weekly_returns", sep=""))
         },
         Monthly = {
           sr <- monthlyReturn(stock.prices)
           ir <- monthlyReturn(index.prices)
           col.names <- c(paste(stock.ticker, "_montlhy_returns", sep=""), paste(index.ticker, "_montlhy_returns", sep=""))
         },
         Quarterly = {
           sr <- quarterlyReturn(stock.prices)
           ir <- quarterlyReturn(index.prices)
           col.names <- c(paste(stock.ticker, "_quarterly_returns", sep=""), paste(index.ticker, "_quarterly_returns", sep=""))
         }, 
         Annual = {
           sr <- annualReturn(stock.prices)
           ir <- annualReturn(index.prices)
           col.names <- c(paste(stock.ticker, "_annual_returns", sep=""), paste(index.ticker, "_annual_returns", sep=""))
         }
  )
  #combined returns
  combined.returns <- merge(sr, ir, join='left')
  
  #combined returns in percent 
  combined.returns[,1:2] = round(combined.returns[,1:2]*100, 1)
  
  #get start year
  #start.year <- years(start(combined.annual.returns))
  
  #get end year
  #end.year <- years(end(combined.annual.returns))
  
  pr <- data.frame(index(combined.returns), coredata(combined.returns))
  colnames(pr) <- c("date", col.names)
  return(pr)
}
#l <- getCombinedReturns("a",y, "AAPL", OEX, "S&P100")

#
# padify company returns
#
padifyCompanyReturns <- function(period, stock.prices, company.name, company.symbol, series){
  series.data <- getReturns(period, stock.prices, company.symbol)
  
  start.year <- years(series.data$date[1])
  end.year <- years(series.data$date[nrow(series.data)])
  
  series["title"] <- paste(period, " Returns of ", company.name, " (", company.symbol, ")",  " (", start.year , "-", end.year, ")", sep="") 
  series["desc"] <- paste(series["title"], 
                          "(Units: %, Frequency: ", period, ")", sep=" ")
  #generate pads
  padify(series, series.data)
}

#
# padify Combined Returns
#
padifyCombinedReturns <- function(period, stock.prices, company.name, company.symbol, series,  index.prices, index.name, index.desc){
  series.data <- getCombinedReturns(period, stock.prices, company.symbol, index.prices, index.name)
  
  start.year <- years(series.data$date[1])
  end.year <- years(series.data$date[nrow(series.data)])
  
  series["title"] <- paste("Comparing ", period, " Returns of ", company.name, " (", company.symbol, ") and ", index.name, " (", start.year , "-", end.year, ")", sep="") 
  series["desc"] <- paste(series["title"], 
                          "(Units: %, Frequency: ", period, ")", index.desc, sep=" ")
  #generate pads
  padify(series, series.data)
}

#
# padify  Returns
#
padifyReturns <- function(company.name, company.symbol, index.prices, index.name, index.desc, period.from="1980-01-01", company.returns=T) {
  #company.symbol <- "AAPL"
  #company.name <- "Apple Inc."
  print(paste("Starting",  company.name, company.symbol, sep=" "))
  
  y <- try(getSymbols(company.symbol, auto.assign=FALSE, from=period.from), silent=T)
  #check to see if the padification went through
  if(class(y) %in% c("try-error")) {
    print("Failed to get stock for this company...")
    #stop("Can't continue.")
  } else {
    if(index.name != "S&P100") #padify stock price first
      padifyStockPrices(y, company.name, company.symbol, period.from)
    
    periods <- c("Daily", "Weekly", "Monthly", "Quarterly", "Annual")
    if(company.returns) {
      for(i in periods){
        if(verbose) print(paste("Getting", i, "returns for the company:", company.symbol, sep=" "))
        padifyCompanyReturns(i, y, company.name, company.symbol, series)
      }
    }
    
    #
    # getting combined returns
    #
    for(i in periods){
      if(verbose) print(paste("Getting", i, "combined returns for the company:", company.symbol, " and ", index.name, sep=" "))
      padifyCombinedReturns(i, y, company.name, company.symbol, series, index.prices, index.name, index.desc)
    }
  }
}

#
# padify  stock price
#
padifyStockPrices <- function(y, company.name, company.symbol, period.from="1950-01-01") {
  #Get just the close prices
  y.close <- Cl(y)
  # get a data.frame
  series.data <- data.frame(index(y.close), coredata(y.close))
  colnames(series.data) <- c("date", paste(company.symbol, "_close_price", sep=""))
  
  start.year <- years(series.data$date[1])
  end.year <- years(series.data$date[nrow(series.data)])
  
  series["title"] <- paste("Stock price of ", company.name, " (", company.symbol, ")", 
                           " (", start.year , "-", end.year, ")", sep="") 
  series["desc"] <- paste(series["title"], 
                          "(Units: level, Frequency: Daily, Data as of ", Sys.Date(), ")", sep=" ")
  #generate pads
  padify(series, series.data)
}


#
# get sp100 returns
#
getSP100Components <- function(){
  sp100 <- getSymbols("^OEX", auto.assign=FALSE, from="1980-01-01")
  index.desc <- "The S&P 100 Index is a stock market index of United States stocks maintained by Standard & Poors."
  index.name <- "S&P100"
  period.from <- "1980-01-01"
  for(i in 1:nrow(sp100.list)) {
    r <- sp100.list[i,]
    if(verbose) print(paste(i, r$Name, sep="::"))
    padifyReturns(gsub("\\'", "\\\\'", as.character(r$Name)), as.character(r$Symbol), sp100, index.name, index.desc,period.from, F)
    # do not generate company returns in this case - S&P 500 will do it.
  }
}

#
# get sp500 returns
#
getSP500Components <- function(){
  # get SP500 returns
  sp500 <- getSymbols("^GSPC", auto.assign=FALSE, from="1950-01-01")
  index.desc <- "The S&P 500 stock market index, maintained by Standard & Poors, comprises 500 large-cap American companies covering about 75 percent of the American equity market by capitalization. The index is weighted by market capitalization, so large companies account for relatively more of the index. The index constituents and the constituent weights are updated regularly using rules published by Standard & Poors. The index constituents listed below were current as of the start of the trading day of February 15, 2013."
  index.desc <- gsub("\\'", "\\\\'", index.desc)
  index.name <- "S&P500"
  period.from <- "1950-01-01"
  for(i in 1:nrow(sp500.list)) {
    r <- sp500.list[i,]
    if(verbose) print(paste(i, r$Name, sep="::"))
    padifyReturns(gsub("\\'", "\\\\'", as.character(r$Name)), as.character(r$Symbol), sp500, index.name, index.desc, period.from, T)
  }
}

#
# Russell 2000
#
getRu2000Components <- function(){
  # Russell 2000 returns
  ru2000 <- getSymbols("^RUT", auto.assign=FALSE, from="1987-01-01")
  index.desc <- "The Russell 2000 Index is a small-cap stock market index of the bottom 2,000 stocks in the Russell 3000 Index. The Russell 2000 is by far the most common benchmark for mutual funds that identify themselves as small-cap, while the S&P 500 index is used primarily for large capitalization stocks. It is the most widely quoted measure of the overall performance of the small-cap to mid-cap company shares."
  index.name <- "Russell 2000"
  period.from <- "1987-01-01"
  #1327
  for(i in 1:nrow(ru2000.tickers)) {
    r <- ru2000.tickers[i,]
    symbol <- as.character(r$Symbol)
    company <-  gsub(" $", "", gsub("\\'", "\\\\'", gsub(symbol, "", as.character(r$Name))))
    if(verbose) print(paste(i, company, symbol, sep="::"))
    padifyReturns(company, symbol, ru2000, index.name, index.desc, period.from, T)
    # do not generate company returns in this case - S&P 500 will do it.
  }
}

#
# NASDAQ Composite 
#
getNasdaqComponents <- function(){
  # NASDAQ Composite returns
  nasdaq <- getSymbols("^IXIC", auto.assign=FALSE, from="1970-01-01")
  index.desc <- "The Nasdaq Composite is a stock market index of the common stocks and similar securities (e.g. ADRs, tracking stocks, limited partnership interests) listed on the NASDAQ stock market, meaning that it has over 3,000 components. It is highly followed in the U.S. as an indicator of the performance of stocks of technology companies and growth companies. Since both U.S. and non-U.S. companies are listed on the NASDAQ stock market, the index is not exclusively a U.S. index."
  index.name <- "Nasdaq Composite"
  period.from <- "1970-01-01"
  
  for(i in 1:nrow(nasdaq.tickers)) {
    r <- nasdaq.tickers[i,]
    symbol <- as.character(r$Symbol)
    company <-  gsub(" $", "", gsub("\\'", "\\\\'", as.character(r$Name)))
    if(verbose) print(paste(i, company, symbol, sep="::"))
    padifyReturns(company, symbol, nasdaq, index.name, index.desc, period.from, T)
    # do not generate company returns in this case - S&P 500 will do it.
    #Stock levels
    #padifyStockPrices(company, symbol, period.from)
  }
}

#
# run this
#
runYahoo <- function(){
  startup()
  getSP100Components()
  getSP500Components()
  getRu2000Components()
  getNasdaqComponents()
  cleanup()
  updateCatPadCount()
}