# Generate airlines / flight PADS
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


source("CreatePADS.R")

#
# startup
#
startup <- function() {
  #initialize system
  initializeSystem()
  
  assign("flights.folder.path", "./pads/raw-data/flights/stats/", envir=.GlobalEnv)  
  assign("dataset", "US-Flights", envir=.GlobalEnv)
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "Bureau of Transportation Statistics"
  series["category"] <- "Transportation"
  series["subcategory"] <- "Flights Records USA"
  series["category_id"]<- 23
  series["subcategory_id"]<- 210
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "Flights Airlines, Airports, USA", sep=","))
  series.desc <- "US Flights Data. The data are collected from BTS, USA and includes only passenger flights as reports to BTS by airlines."
  assign("series", series, envir=.GlobalEnv)
  assign("series.desc", series.desc, envir=.GlobalEnv)
  
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
  n <- 1:6
  filenames <- paste(flights.folder.path, n, ".csv", sep="")
  assign("filenames", filenames, envir=.GlobalEnv)
  
  #load carriers data
  carriers <- data.table(read.csv(paste(flights.folder.path, "carriers.csv", sep=""),stringsAsFactors=F))
  setnames(carriers,colnames(carriers),tolower(colnames(carriers)))
  carriers <- carriers[, description:=NULL]
  carriers$airlines <- removeMetaChars(carriers$airlines)
  setkey(carriers, uniquecarrier)
  assign("carriers", carriers, envir=.GlobalEnv)
  
  #load airports data
  airport.list <- data.table(read.csv(paste(flights.folder.path, "airports.csv", sep=""),stringsAsFactors=F))
  airport.list <- airport.list[country=="USA"][, city:=NULL][, state:=NULL][, country:=NULL][, lat:=NULL][, long:=NULL]
  airport.list$airport <- removeMetaChars(airport.list$airport)
  setkey(airport.list, iata)
  assign("airport.list", airport.list, envir=.GlobalEnv)
  
  #lmonths
  lmonths <- data.table(val=1:12, 
                        month=c("January","February","March", "April","May","June",
                                "July","August","September", "October","November","December"))
  setkey(lmonths, val)
  assign("lmonths", lmonths, envir=.GlobalEnv)  
}

#
# overall summary stats
#
overallSummaryStats <- function(flights.data, for.period){
  #for all years
  series.data <- flights.data[, list(flights=sum(flights), 
                                     flights_delayed=sum(flights_departed_late), 
                                     flights_cancelled=sum(flights_cancelled), 
                                     flights_diverted=sum(flights_diverted),
                                     total_delay_in_mins=sum(total_dep_delay_in_mins))]
  series["title"] <- paste("Key Statistics - US Flights Data ", for.period, sep="")
  series["desc"] <- series.desc
  series.data <- as.data.frame(t(series.data))
  series.data$measure <- rownames(series.data)
  series.data <- series.data[,c(2,1)]
  colnames(series.data) <- c("measure", "aggregate_value")
  padify(series, series.data[order(-series.data$aggregate_value),])
  
  ## airlines by year
  series.data <- flights.data[, list(year, airlines)]
  series["title"] <- paste("Total Number of Passenger Airlines", for.period, sep=" ")
  padify(series, series.data)
  
  ## airports by year
  series.data <- flights.data[, list(year, airports=dep_airports)]
  series["title"] <- paste("Total Number of Passenger Airports", for.period, sep=" ")
  padify(series, series.data)
  
  ## Flights  by year
  series.data <- flights.data[, list(year, flights)]
  series["title"] <- paste("Total Number of Passenger Flights", for.period, sep=" ")
  padify(series, series.data)
  # get growth rates
  series.data$growth <- c("NA", diff(series.data$flights))
  series.data <- series.data[-c(1:2),][,flights:=NULL]
  series.data$growth <- as.integer(series.data$growth)
  series["title"] <- paste("Growth (Decline) In Passenger Flights", for.period, sep=" ")
  padify(series, series.data) 
}

#
# summary stats by year or by month
#
summaryStatsByPeriod <- function(flights.data, freq="year", for.period=NULL, monthly.compare=F) {
  #convert to a data.table
  flights.data <- data.table(flights.data)
  if(is.null(for.period))
    for.period <- paste("(", flights.data$year[1],")", sep="")
  if(monthly.compare)
    for.period <- paste("For", lmonths[flights.data$month[1]]$month,for.period, sep=" ")  
  cat("\n", freq, " - ", for.period)  
  series["desc"] <- series.desc
  
  ## Flights
  series.data <- flights.data[, list(period=get(freq), flights)]
  series["title"] <- paste( "Total Number of Passenger Flights", for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## Flights departed late
  series.data <- flights.data[, list(period=get(freq), late_flights=flights_departed_late)]
  series["title"] <- paste( "Total Number of Passenger Flights Departed Late", for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## Flights vs late departed flights
  series.data <- flights.data[, list(period=get(freq), flights, late_flights=flights_departed_late)]
  series["title"] <- paste( "Total Number of Passenger Flights and Flights Delayed", for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## % of flights departed late by year
  series.data <- flights.data[, list(period=get(freq), percent_late_flights=round(flights_departed_late/flights, 2)*100)]
  series["title"] <- paste( "Percent of Passenger Flights Departed Late", for.period, sep=" ")
  series["desc"] <- paste(series.desc, "Unit: in percent(%).", sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  #reset
  series["desc"] <- series.desc
  
  ## Flights Canceled
  series.data <- flights.data[, list(period=get(freq), canceled_flights=flights_cancelled)]
  series["title"] <- paste( "Total Number of Canceled Passenger Flights", for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## % of flights Canceled
  series.data <- flights.data[, list(period=get(freq), percent_canceled_flights=round(flights_cancelled/flights, 2)*100)]
  series["title"] <- paste( "Percent of Canceled Passenger Flights", for.period, sep=" ")
  series["desc"] <- paste(series.desc, "Unit: in percent(%).", sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  #reset
  series["desc"] <- series.desc
  
  ## Flights diverted  by year
  series.data <- flights.data[, list(period=get(freq), diverted_flights=flights_diverted)]
  series["title"] <- paste( "Total Number of Diverted Passenger Flights ", for.period, sep="")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  #   ## % of flights Canceled by year
  #   series.data <- flights.data[, list(period=get(freq), percent_diverted_flights=round(flights_diverted/flights, 2)*100)]
  #   series["title"] <- paste( "Percent of Diverted Passenger Flights ", for.period, sep="")
  #   series["desc"] <- paste(series.desc, "Unit: in percent(%).", sep=" ")
  #   if(freq=="month"){
  #     setkey(series.data, period)
  #     series.data <- series.data[lmonths][,period:=NULL]
  #     series$title <- paste("Monthly", series$title)
  #   }
  #   padify(series, series.data)
  #   #reset
  #   series["desc"] <- series.desc
  
  ## total delay
  series.data <- flights.data[, list(period=get(freq), total_dep_delay_in_mins)]
  series["title"] <- paste( "Total Departure Delay (in mins) ", for.period, sep="")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## average dep delay
  series.data <- flights.data[, list(period=get(freq), avg_dep_delay_in_mins)]
  series["title"] <- paste( "Average Departure Delay (in mins)", for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## median dep delay
  series.data <- flights.data[, list(period=get(freq), median_dep_delay_in_mins)]
  series["title"] <- paste( "Median Departure Delay (in mins)", for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  return(NULL)
}

#
#build over all summary stats without breaking it by airlines or by airports
#
buildSummaryStats <- function(){
  flights <- data.table(read.csv(filenames[1], stringsAsFactors=F))
  period.min <- min(flights$year)
  period.max <- max(flights$year)
  years <- paste("(", period.min, "-", period.max, ")", sep="")
  
  # get overall summary stats
  overallSummaryStats(flights, years)
  
  #get summary stats
  summaryStatsByPeriod(flights, "year", years)
  
  ## read the second file
  flights <- data.table(read.csv(filenames[2], stringsAsFactors=F))
  setkeyv(flights, c("month", "year"))
  
  #load flights stats file by month for every year
  try(ddply(flights, .(year), summaryStatsByPeriod, "month"), silent=F)
  
  #compare across same months for every year
  try(ddply(flights, .(month), summaryStatsByPeriod, "year", years, T), silent=F)
}

#
#summaryStatsfor AllAirlines 
#
summaryStatsAllAirlines <- function(flights.data, for.period=NULL){
  if(is.null(for.period))
    for.period <- flights.data$year[1]
  series["desc"] <- series.desc
  print(for.period)
  
  ## Flights by airlines by year
  f.d <- flights.data[, list(flights=sum(flights)), by=uniquecarrier][order(-flights)]
  series["title"] <- paste("Top 15 Airlines by Flights", for.period, sep=" ")
  setkey(f.d, uniquecarrier)
  series.data <- carriers[f.d][,uniquecarrier:=NULL][,description:=NULL][order(-flights)][1:15]
  padify(series, series.data)
  
  ## Delayed Flights by airlines by year
  d.d <- flights.data[, list(late_flights=sum(flights_departed_late)), by=uniquecarrier][order(-late_flights)]
  series["title"] <- paste("Worst 15 Airlines by Number of Late Flights", for.period, sep=" ")
  setkey(d.d, uniquecarrier)
  series.data <- carriers[d.d][,uniquecarrier:=NULL][,description:=NULL][order(-late_flights)][1:15]
  padify(series, series.data)
  
  # merge the two - flights and delayed flights
  series.data <- f.d[d.d]
  series["title"] <- paste("Top 15 Airlines by Number of Flights and Late Flights", for.period, sep=" ")
  setkey(series.data, uniquecarrier)
  series.data <- carriers[series.data][,uniquecarrier:=NULL][,description:=NULL][order(-flights)][1:10]
  padify(series, series.data)
  
  
  ## Flights by total delay
  t.d <- flights.data[, list(dep_delay=sum(total_dep_delay_in_mins)), by=uniquecarrier][order(-dep_delay)]
  series["title"] <- paste("Top 15 Worst Airlines by Total Departure Delay", for.period, sep=" ")
  setkey(t.d, uniquecarrier)
  series.data <- carriers[t.d][,uniquecarrier:=NULL][,description:=NULL][order(-dep_delay)][1:15]
  padify(series, series.data)
  
  # merge the two - average dep. delay
  series.data <- d.d[t.d]
  series.data <- series.data[, list(uniquecarrier, avg_delay_in_mins=round(dep_delay/late_flights, 2))]
  series["title"] <- paste("Top 15 Worst Airlines by Average Departure Delay", for.period, sep=" ")
  setkey(series.data, uniquecarrier)
  series.data <- carriers[series.data][,uniquecarrier:=NULL][,description:=NULL][order(-avg_delay_in_mins)][1:15]
  padify(series, series.data)
  
  # for large airlines
  series.data <- d.d[t.d]
  series.data <- series.data[, list(uniquecarrier, late_flights, avg_delay_in_mins=round(dep_delay/late_flights, 2))]
  series["title"] <- paste("Top 15 Worst Large Airlines by Average Departure Delay", for.period, sep=" ")
  setkey(series.data, uniquecarrier)
  series.data <- carriers[series.data][,uniquecarrier:=NULL][,description:=NULL][order(-late_flights)][,late_flights:=NULL][1:15][order(-avg_delay_in_mins)]
  padify(series, series.data)  
  
  ## Canceled Flights by airlines by year
  c.d <- flights.data[, list(canceled_flights=sum(flights_cancelled)), by=uniquecarrier][order(-canceled_flights)]
  series["title"] <- paste("Worst 15 Airlines by Number of Canceled Flights", for.period, sep=" ")
  setkey(c.d, uniquecarrier)
  series.data <- carriers[c.d][,uniquecarrier:=NULL][,description:=NULL][order(-canceled_flights)][1:15]
  padify(series, series.data)
  
  # merge the two - flights and delayed flights
  series.data <- f.d[c.d]
  series.data <- series.data[, list(uniquecarrier,percent_canceled_flights=round(canceled_flights/flights, 4))]
  series["title"] <- paste("Worst 15 Airlines by Percent of Canceled Flights", for.period, sep=" ")
  setkey(series.data, uniquecarrier)
  series.data.w <- carriers[series.data][,uniquecarrier:=NULL][,description:=NULL][order(-percent_canceled_flights)][1:15]
  padify(series, series.data.w)
  
  #best
  series["title"] <- paste("Best 15 Airlines by Percent of Canceled Flights", for.period, sep=" ")
  series.data.b <- carriers[series.data][,uniquecarrier:=NULL][,description:=NULL][order(percent_canceled_flights)][1:15]
  padify(series, series.data.b)
  
  ## diverted Flights by airlines by year
  di.d <- flights.data[, list(flights_diverted=sum(flights_diverted)), by=uniquecarrier][order(-flights_diverted)]
  series["title"] <- paste("Worst 15 Airlines by Number of Diverted Flights", for.period, sep=" ")
  setkey(di.d, uniquecarrier)
  series.data <- carriers[di.d][,uniquecarrier:=NULL][,description:=NULL][order(-flights_diverted)][1:15]
  padify(series, series.data)
  return(NULL)
}

#Summary Stats by airlines
summaryStatsByAirlines <- function(flights.data, freq="year", for.period=NULL) {
  series["desc"] <- series.desc
  flights.data <- data.table(flights.data)
  
  #get airline name
  airline <- carriers[flights.data$uniquecarrier[1]]$airlines
  
  if(is.null(for.period))
    for.period <- paste("(", flights.data$year[1],")", sep="")
  cat("\n", airline, " - ", freq, " - ", for.period)  
  
  ## airports by year
  series.data <- flights.data[, list(period=get(freq), airports=dep_airports)]
  series["title"] <- paste("Number of Passenger Airports for", airline, ",", for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## Flights  by year
  series.data <- flights.data[, list(period=get(freq), flights)]
  series["title"] <- paste("Number of Passenger Flights by", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## Flights & airports by year
  series.data <- flights.data[, list(period=get(freq), flights, airports=dep_airports)]
  series["title"] <- paste("Number of Passenger Flights and Airports for", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## Flights departed late by year
  series.data <- flights.data[, list(period=get(freq), flights_departed_late)]
  series["title"] <- paste("Total Number of Passenger Flights Departed Late", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## Flights vs late departed flights by year
  series.data <- flights.data[, list(period=get(freq), flights, flights_departed_late)]
  series["title"] <- paste("Total Number of Passenger Flights and Flights Departed Late", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## % of flights departed late by year
  series.data <- flights.data[, list(period=get(freq), percent_delayed_flights=round(flights_departed_late/flights, 2)*100)]
  series["title"] <- paste("Percent of Passenger Flights Departed Late", airline, for.period, sep=" ")
  series["desc"] <- paste(series.desc, "Unit: in percent(%).", sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  #reset
  series["desc"] <- series.desc
  
  ## Flights Canceled late by year
  series.data <- flights.data[, list(period=get(freq), flights_cancelled)]
  series["title"] <- paste("Number of Canceled Passenger Flights", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## % of flights Canceled late by year
  series.data <- flights.data[, list(period=get(freq), percent_canceled_flights=round(flights_cancelled/flights, 2)*100)]
  series["title"] <- paste("Percent of Canceled Passenger Flights", airline, for.period, sep=" ")
  series["desc"] <- paste(series.desc, "Unit: in percent(%).", sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  #reset
  series["desc"] <- series.desc
  
  ## Flights diverted  by year
  series.data <- flights.data[, list(period=get(freq), flights_diverted)]
  series["title"] <- paste("Number of Diverted Passenger Flights", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## % of flights diverted  by year
  series.data <- flights.data[, list(period=get(freq), percent_diverted_flights=round(flights_diverted/flights, 2)*100)]
  series["title"] <- paste("Percent of Diverted Passenger Flights", airline, for.period, sep=" ")
  series["desc"] <- paste(series.desc, "Unit: in percent(%).", sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  #reset
  series["desc"] <- series.desc
  
  ## total delay
  series.data <- flights.data[, list(period=get(freq), total_dep_delay_in_mins)]
  series["title"] <- paste("Total Departure Delay (in mins)", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## average dep delay
  series.data <- flights.data[, list(period=get(freq), avg_dep_delay_in_mins)]
  series["title"] <- paste("Average Departure Delay (in mins)", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## median dep delay
  series.data <- flights.data[, list(period=get(freq), median_dep_delay_in_mins)]
  series["title"] <- paste("Median Departure Delay (in mins)", airline, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)    
  return(NULL)
}

#
# Summary Stats for airlines
#
buildSummaryStatsAirlines <- function(){
  flights <- data.table(read.csv(filenames[5], stringsAsFactors=F))
  setkeyv(flights, c("uniquecarrier", "year"))
  period.min <- min(flights$year)
  period.max <- max(flights$year)
  years <- paste("(", period.min, "-", period.max, ")", sep="")
  
  # overall airline summary for the entire period
  try(summaryStatsAllAirlines(flights, years), silent=T)
  
  #overall summary by every year
  try(ddply(flights, .(year), summaryStatsAllAirlines), silent=T)
  
  #summary be Airlines
  try(ddply(flights, .(uniquecarrier), summaryStatsByAirlines, "year", years), silent=F)
  
  #load the airlines stats file by month
  flights <- data.table(read.csv(filenames[6], stringsAsFactors=F))
  setkeyv(flights, c("uniquecarrier", "year"))  
  try(ddply(flights, .(uniquecarrier, year), summaryStatsByAirlines, "month"), silent=F)
}

#
# Summary Stats for all airports
#
summaryStatsAllAirports <- function(flights.data, for.period=NULL){
  series["desc"] <- series.desc
  
  if(is.null(for.period))
    for.period <- flights.data$year[1]
  print(for.period)
  
  ## Flights by airport by year
  f.d <- flights.data[, list(flights=sum(flights)), by=airport][order(-flights)]
  series["title"] <- paste("Top 15 Airports by Flights", for.period, sep=" ")
  padify(series, f.d[1:15])
  
  ## Delayed Flights by airlines by year
  d.d <- flights.data[, list(late_flights=sum(flights_departed_late)), by=airport][order(-late_flights)]
  series["title"] <- paste("Worst 15 Airports by Number of Delayed Flights", for.period, sep=" ")
  padify(series, d.d[1:15])
  
  # merge the two - flights and delayed flights
  setkey( f.d, "airport")
  setkey( d.d, "airport")
  series.data <- f.d[d.d][order(-flights)][1:15]
  series["title"] <- paste("Top 15 Airports by Number of Flights and Late Flights", for.period, sep=" ")
  padify(series, series.data)
  
  
  ## Flights by total delay
  t.d <- flights.data[, list(dep_delay=sum(total_dep_delay_in_mins)), by=airport][order(-dep_delay)]
  series["title"] <- paste("Top 15 Worst Airports by Total Departure Delay", for.period, sep=" ")
  padify(series, t.d[1:15])
  
  # merge the two - average dep. delay
  setkey( t.d, "airport")
  series.data <- d.d[t.d]
  series.data <- series.data[, list(airport, avg_delay_in_mins=round(dep_delay/late_flights, 2))][order(-avg_delay_in_mins)][1:15]
  series["title"] <- paste("Top 15 Worst Airports by Average Departure Delay", for.period, sep=" ")
  padify(series, series.data)
  
  # for large airlines
  series.data <- d.d[t.d]
  series.data <- series.data[, list(airport, late_flights, avg_delay_in_mins=round(dep_delay/late_flights, 2))][order(-late_flights)][1:15][,late_flights:=NULL]
  series["title"] <- paste("Top 15 Worst Large Airports by Average Departure Delay", for.period, sep=" ")
  padify(series, series.data)  
  
  ## Canceled Flights by
  c.d <- flights.data[, list(canceled_flights=sum(flights_cancelled)), by=airport][order(-canceled_flights)]
  series["title"] <- paste("Worst 15 Airports by Number of Canceled Flights", for.period, sep=" ")
  padify(series, c.d[1:15])
  
  # merge the two - flights and Canceled flights
  setkey( c.d, "airport")
  series.data <- f.d[c.d]
  series.data <- series.data[, list(airport,percent_canceled_flights=round(canceled_flights/flights, 4))]
  series["title"] <- paste("Worst 15 Airports by Percent of Canceled Flights", for.period, sep=" ")
  padify(series, series.data[order(-percent_canceled_flights)][1:15])
  
  # merge the two - flights and Canceled flights - for large airlines
  series.data <- f.d[c.d]
  series.data <- series.data[, list(airport, flights, percent_canceled_flights=round(canceled_flights/flights, 4))]
  series["title"] <- paste("Worst 15 Large Airports by Percent of Canceled Flights", for.period, sep=" ")
  padify(series, series.data[order(-flights)][,flights:=NULL][1:15])
  
  return(NULL)
}

#Summary Stats for individual airports across years and by months for every year
summaryStatsByAirport <- function(flights.data, freq="year", for.period=NULL) {
  flights.data <- data.table(flights.data)
  series["desc"] <- series.desc
  if(is.null(for.period))
    for.period <- paste("(", flights.data$year[1],")", sep="")
  
  #get full airport name
  ap <- paste(airport.list[flights.data$airport[1]]$airport, " (", flights.data$airport[1], ")", sep="")
  
  cat("\n", ap, " - ", freq, " - ", for.period)  
  
  ## Flights  by year
  series.data <- flights.data[, list(period=get(freq), flights)]
  series["title"] <- paste("Number of Passenger Flights by,", ap,for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## Flights departed late by year
  series.data <- flights.data[, list(period=get(freq), flights_departed_late)]
  series["title"] <- paste("Total Number of Passenger Flights Departed Late,", ap,for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## Flights vs late departed flights by year
  series.data <- flights.data[, list(period=get(freq), flights, flights_departed_late)]
  series["title"] <- paste("Total Number of Passenger Flights and Flights Departed Late,", ap, for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## % of flights departed late by year
  series.data <- flights.data[, list(period=get(freq), percent_delayed_flights=round(flights_departed_late/flights, 2)*100)]
  series["title"] <- paste("Percent of Passenger Flights Departed Late,", ap,for.period, sep=" ")
  series["desc"] <- paste(series.desc, "Unit: in percent(%).", sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  #reset
  series["desc"] <- series.desc
  
  ## Flights Canceled late by year
  series.data <- flights.data[, list(period=get(freq), flights_cancelled)]
  series["title"] <- paste("Number of Canceled Passenger Flights,", ap,for.period,sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## % of flights Canceled late by year
  series.data <- flights.data[, list(period=get(freq), percent_canceled_flights=round(flights_cancelled/flights, 2)*100)]
  series["title"] <- paste("Percent of Canceled Passenger Flights,", ap,for.period, sep=" ")
  series["desc"] <- paste(series.desc, "Unit: in percent(%).", sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  #reset
  series["desc"] <- series.desc
  
  ## total delay
  series.data <- flights.data[, list(period=get(freq), total_dep_delay_in_mins)]
  series["title"] <- paste("Total Departure Delay (in mins),", ap,for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## average dep delay
  series.data <- flights.data[, list(period=get(freq), avg_dep_delay_in_mins)]
  series["title"] <- paste("Average Departure Delay (in mins),", ap,for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)
  
  ## median dep delay
  series.data <- flights.data[, list(period=get(freq), median_dep_delay_in_mins)]
  series["title"] <- paste("Median Departure Delay (in mins),", ap,for.period, sep=" ")
  if(freq=="month"){
    setkey(series.data, period)
    series.data <- series.data[lmonths][,period:=NULL]
    series$title <- paste("Monthly", series$title)
  }
  padify(series, series.data)    
  return(NULL)
}

#
# Summary Stats 
#
buildSummaryStatsAirports <- function(){
  flights <- data.table(read.csv(filenames[3], stringsAsFactors=F))
  setkeyv(flights, c("airport", "year"))  
  period.min <- min(flights$year)
  period.max <- max(flights$year)
  years <- paste("(", period.min, "-", period.max, ")", sep="")
  
  # overall airline summary for the entire period
  try(summaryStatsAllAirports(flights, years), silent=F)
  
  #overall summary by every year
  try(ddply(flights, .(year), summaryStatsAllAirports), silent=F)
  
  #summary be airport
  try(ddply(flights, .(airport), summaryStatsByAirport, "year", years), silent=F)
  
  #load the airlines stats file by month
  flights <- data.table(read.csv(filenames[4], stringsAsFactors=F))
  setkeyv(flights, c("airport", "year"))  
  try(ddply(flights, .(airport, year), summaryStatsByAirport, "month"), silent=F)
}

runAirlines <- function(){
  startup()
  #355 pads
  buildSummaryStats()
  #4300 pads
  #buildSummaryStatsAirlines()
  #51,156
  #buildSummaryStatsAirports()
  cleanup()
  updateCatPadCount()
}

# http://www.linkedin.com/shareArticle?mini=true&source=datadolph.in&url=http://datadolph.in/dg/2013/05/number-of-passenger-flights-by-john-f-kennedy-intl-jfk-1987-2008/&title=Not%20very%20often%20you%20see%20that%20# of flights get reduced year over year - 2001 (911 attacks) and great recession in 2008 cut # of flights #bigdata&summary=Not very often you see that # of flights get reduced year over year - 2001 (911 attacks) and great recession in 2008 cut # of flights #bigdata
# http://www.linkedin.com/shareArticle?mini=true&source=datadolph.in&url=http://datadolph.in/dg/2013/05/number-of-passenger-flights-by-john-f-kennedy-intl-jfk-1987-2008/&title=Not%20very%20often%20you%20see%20that%20%23%20of%20flights%20get%20reduced%20year%20over%20year%20-%202001%20(911%20attacks)%20and%20great%20recession%20in%202008%20cut%20%23%20of%20flights%20%23bigdata&summary=Not%20very%20often%20you%20see%20that%20%23%20of%20flights%20get%20reduced%20year%20over%20year%20-%202001%20(911%20attacks)%20and%20great%20recession%20in%202008%20cut%20%23%20of%20flights%20%23bigdata
