# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 2012-9-9
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# Description: Financial Markets and President Elect: Do Financial Markets Favor A Republican Over A Democrat?
# Packages Used: RCurl, RJSONIO
# Blog Reference: http://allthingsbusinessanalytics.blogspot.com/2012/11/financial-markets-and-president-elect.html
# All rights reserved.

require(XML)
require(plyr)
require(quantmod)

setwd("C:/mydir")

#######################################################################
# Get SPY data for US election dates
#######################################################################

#Presedential elections are first tuesday after first monday every 4 years
election.years <- seq(1952,2012, by=4)
election.months <- seq(as.Date("1952/11/1"), by="4 year", length.out=16)
election.dates <- election.months + ((9-as.POSIXlt(election.months)$wday) %% 7)
election.dates.1dl <- election.dates + 1

getSymbols("^GSPC",src="yahoo", from="1950-1-1")
colnames(GSPC) <- c("Open", "High", "LOW", "Close", "Volume", "Adjusted.Close")
write.zoo(GSPC, file="pads/data/financials/SP500_historical_OHLC_data.csv", sep=",")

#Generate Monthly Returns for 2008 and save them as a file
write.zoo(monthlyReturn(GSPC["2008"]), file="pads/data/financials/SP500_2008_monthly_returns.csv", sep=",")


# only concerned about close
SPY.close <- Ad(GSPC)

#system("cat pads/data/financials/SP500_historical_OHLC_data.csv")
spy.daily.returns <- dailyReturn(SPY.close)
#one.day.return <- subset(spy.daily.returns, index(spy.daily.returns) %in% election.dates.1daylater)
#one.day.return$year <- format(index(one.day.return), "%Y")

getQuote <- function(year, SPY)
{
  #Determine election date
  election.month <- as.Date(paste(year, "11/1", sep="/"))
  election.date <- election.month + ((9-as.POSIXlt(election.month)$wday) %% 7)
  #election.date <- election.dates[which(format(election.dates, "%Y")==year)]

  #Get dates 1-day, 1-week, 4-weeks, 12-weeks and 52-weeks afte the election day
  quote.dates <- c(election.date, election.date+1, election.date+7, 
                   election.date+(4*7), election.date+(12*7), election.date+(52*7), election.date+(50*7*4))
  if(year == 2008) 
    quote.dates[length(quote.dates)] <- Sys.Date()
      
  # Check to see if the dates exist in SPY index, if not advance or lag by 1 day
  quote.dates <- as.Date(sapply(quote.dates, function(x) 
    if(length(SPY.close[x])){return(as.Date(x))}else{ return(as.Date(x)-1)}))
  
  #Get the quotes for all these dates
  quotes <- SPY[quote.dates]
  returns <- sapply(quote.dates, 
                    function(x) (as.numeric(quotes[x])/as.numeric(quotes[quote.dates[1]]) - 1)*100)
  r <- list()
  return(r[[year]]<-returns)
}

#Remove 2012 from this vector
election.years <- election.years[!election.years==2012] 
returns <- do.call(cbind, llply(election.years, function(x) getQuote(x, SPY.close)))
colnames(returns) <- election.years
#Remove first row
returns <- returns[-1,]
t.returns <- t(returns)
old.rownames <- rownames(t.returns)
obama.1d.return <- as.numeric(spy.daily.returns["2012-11-07", 1]) * 100
t.returns <- rbind(t.returns, c(obama.1d.return, NA, NA, NA, NA, NA))
rownames(t.returns) <- c(old.rownames, 2012)
colnames(t.returns) <- c("1_day_return", "1_week_return", "4_weeks_return", "12_weeks_return", 
                         "52_weeks_return", "term_return")
write.csv(t.returns, "pads/data/history/SP500 returns after US president elections_1.csv", row.names = TRUE)

#######################################################################
# Get Presidential Names
#######################################################################

wiki <- "http://en.wikipedia.org/wiki/United_States_presidential_election"
tables <- readHTMLTable(wiki, stringsAsFactors=FALSE)
#print(names(tables))
wiki.table <- tables[[4]]
#Create a new column by removing all the special chars from the winner column
wiki.table$sub <- gsub("\\*", "", wiki.table$Winner)
#Replace all meta chars
#data$sub <- gsub("[^A-Za-z0-9 ]+", ",", data$sub)  
wiki.table$sub <- gsub("\\(|\\)", ",", wiki.table$sub)
#Now split and create a new dataframe
us.prez.elect <- data.frame(do.call("rbind", strsplit(wiki.table$sub, ",", fixed=TRUE)))
drops <- c("X3","X4")
us.prez.elect <- us.prez.elect[,!(names(us.prez.elect) %in% drops)]
election_years <- gsub("[^A-Za-z0-9]+", "", wiki.table$"Election year")
election_years[1] <- 1788
election_years[20] <- 1864
colnames(us.prez.elect) <- c("president", "party")
rownames(us.prez.elect) <- election_years
election.months <- seq(as.Date("1788/11/1"), by="4 year", length.out=(2016-1788)/4)
election.dates <- election.months + ((9-as.POSIXlt(election.months)$wday) %% 7)
us.prez.elect$election_date <- as.Date(election.dates)
write.csv(us.prez.elect, "pads/data/history/US president elects.csv", row.names = TRUE)
us.prez.elect.since.1950 <- subset(us.prez.elect, rownames(us.prez.elect) %in% election.years)

#
# Merge the two data frames
#
new.col.names <- c('election_year', colnames(us.prez.elect.since.1950), colnames(t.returns))
prez.data <- merge(us.prez.elect.since.1950, t.returns, by=0)
colnames(prez.data) <- new.col.names
write.csv(prez.data, "pads/data/history/US president elects and SP500 returns.csv", row.names = FALSE)


#all.dates <- seq(as.Date("1952-1-1"), Sys.Date(), by="day")
#election.year.dates <- subset(all.dates, years(all.dates) %in% election.years)
#election.months <- subset(election.year.dates, months(election.year.dates)=="November")
#elections.months.days <- subset(election.months, weekdays(election.months) %in% c("Monday", "Tuesday"))
#df <- data.frame(date=elections.months.days, day=weekdays(elections.months.days))
#election.years <- seq(as.Date("1952/1/1"), by="4 year", length.out=16)
#http://statistics.berkeley.edu/classes/s133/dates.html
#sapply(quote.dates, function(x) if(length(SPY.close[x])){cat("True\n")}else{ cat("False\n")}) 
#x <- SPY.close[paste(election.date, election.date+(52*7), sep="/")]
#d <- dailyReturn(x)
#w <- weeklyReturn(x)
#keeps <- c("y","a")
#DF[keeps]
