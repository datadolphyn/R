# Authors: Rajani Aswani Co-Founder @ datadolph.in
# Date: 2013-15-5
# Description: The Airline data set consists of flight arrival and departure details for all commercial flights from 1987 to 2008. 
#     The approximately 120MM records (CSV format) occupy 12GB space.  Data can be downloaded from here: http://stat-computing.org/dataexpo/2009/
#     This R code simulates Map-Reduce functionality to analyze 22 years of historical data on flights. 
# Packages Used: data.table & plyr
# Blog Reference: http://blog.datadolph.in/2013/06/big-data-analysis-performance-story-of-chicago-ohare-airport/
# Blog Reference: http://allthingsr.blogspot.com/2013/06/simulating-map-reduce-in-r-for-big-data.html
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#
# initialize and read all 22 compressed CSV files
#
initForStats <- function(){
  assign("flights.folder.path", "./raw-data/flights/", envir=.GlobalEnv)
  assign("verbose", T, envir=.GlobalEnv)
  flights.files <- list.files(path=flights.folder.path, pattern="*.csv.gz")
  assign("flights.files", flights.files, envir=.GlobalEnv)
  assign("period", length(flights.files), envir=.GlobalEnv)
}

#
# get flights stats By airlines
# 
getFlightsStatusByAirlines <- function(flights, yr){   
  #
  # by Year
  #
  if(verbose) cat("Getting stats for airlines:", '\n')
  airlines.stats <- flights[, list(dep_airports=length(unique(origin)),
                                   flights=length(origin),
                                   flights_cancelled=sum(cancelled, na.rm=T),
                                   flights_diverted=sum(diverted, na.rm=T),
                                   flights_departed_late=length(which(depdelay > 0)),
                                   flights_arrived_late=length(which(arrdelay > 0)),
                                   total_dep_delay_in_mins=sum(depdelay[which(depdelay > 0)]),
                                   avg_dep_delay_in_mins=round(mean(depdelay[which(depdelay > 0)])),
                                   median_dep_delay_in_mins=round(median(depdelay[which(depdelay > 0)])),                 
                                   miles_traveled=sum(distance, na.rm=T)
  ),
                            by=uniquecarrier][, year:=yr]
  #change col order
  setcolorder(airlines.stats, c("year", colnames(airlines.stats)[-ncol(airlines.stats)]))
  #save this data
  saveData(airlines.stats, paste(flights.folder.path, "stats/5/airlines_stats_", yr, ".csv", sep=""))
  #clear up space
  rm(airlines.stats)  
  
  #
  # by month
  #
  if(verbose) cat("Getting stats for airlines by month:", '\n')
  airlines.stats <- flights[, list(dep_airports=length(unique(origin)),
                                   flights=length(origin),
                                   flights_cancelled=sum(cancelled, na.rm=T),
                                   flights_diverted=sum(diverted, na.rm=T),
                                   flights_departed_late=length(which(depdelay > 0)),
                                   flights_arrived_late=length(which(arrdelay > 0)),
                                   total_dep_delay_in_mins=sum(depdelay[which(depdelay > 0)]),
                                   avg_dep_delay_in_mins=round(mean(depdelay[which(depdelay > 0)])),
                                   median_dep_delay_in_mins=round(median(depdelay[which(depdelay > 0)])),
                                   miles_traveled=sum(distance, na.rm=T)
  ),
                            by=list(uniquecarrier, month)][, year:=yr]
  #change col order
  setcolorder(airlines.stats, c("year", colnames(airlines.stats)[-ncol(airlines.stats)]))
  #save this data
  saveData(airlines.stats, paste(flights.folder.path, "stats/6/airlines_stats_monthly_", yr, ".csv", sep=""))
  #clear up space
  rm(airlines.stats)
  
}

#
# get flights stats By airport
# 
getFlightsStatsByAirport <- function(flights, yr){
  #
  # by year
  #
  if(verbose) cat("Getting stats for airport:", '\n')
  airport.stats <- flights[, list(flights=length(uniquecarrier),
                                  flights_cancelled=sum(cancelled, na.rm=T),
                                  flights_departed_late=length(which(depdelay > 0)),
                                  total_dep_delay_in_mins=sum(depdelay[which(depdelay > 0)]),
                                  avg_dep_delay_in_mins=round(mean(depdelay[which(depdelay > 0)])),
                                  median_dep_delay_in_mins=round(median(depdelay[which(depdelay > 0)]))
  ),
                           by=origin][, year:=yr]
  #change col order
  setcolorder(airport.stats, c("year", colnames(airport.stats)[-ncol(airport.stats)]))
  #save this data
  saveData(airport.stats, paste(flights.folder.path, "stats/3/airport_stats_", yr, ".csv", sep=""))
  #clear up space
  rm(airport.stats)  
  
  #
  # by month
  #
  if(verbose) cat("Getting stats for airport by month:", '\n')
  airport.stats <- flights[, list(flights=length(uniquecarrier),
                                  flights_cancelled=sum(cancelled, na.rm=T),
                                  flights_departed_late=length(which(depdelay > 0)),
                                  total_dep_delay_in_mins=sum(depdelay[which(depdelay > 0)]),
                                  avg_dep_delay_in_mins=round(mean(depdelay[which(depdelay > 0)])),
                                  median_dep_delay_in_mins=round(median(depdelay[which(depdelay > 0)]))
  ), by=list(origin, month)][, year:=yr]
  #change col order
  setcolorder(airport.stats, c("year", colnames(airport.stats)[-ncol(airport.stats)]))
  #save this data
  saveData(airport.stats, paste(flights.folder.path, "stats/4/airport_stats_monthly_", yr, ".csv", sep=""))
  #clear up space
  rm(airport.stats)
}

#
# get flights stats
#  
getFlightStatsForYear <- function(flights, yr){
  #
  # for every year
  #  
  if(verbose) cat("Getting flight stats: ", '\n')
  flights.stats <- flights[, list(airlines = length(unique(uniquecarrier)),
                                  flights=length(uniquecarrier),
                                  flights_cancelled=sum(cancelled, na.rm=T),
                                  flights_diverted=sum(diverted, na.rm=T),
                                  flights_departed_late=length(which(depdelay > 0)),
                                  flights_arrived_late=length(which(arrdelay > 0)),
                                  total_dep_delay_in_mins=sum(depdelay[which(depdelay > 0)]),
                                  avg_dep_delay_in_mins=round(mean(depdelay[which(depdelay > 0)])),
                                  median_dep_delay_in_mins=round(median(depdelay[which(depdelay > 0)])),
                                  miles_traveled=sum(distance, na.rm=T),
                                  dep_airports=length(unique(origin)),
                                  arr_airports=length(unique(dest)),
                                  all_airports=length(union(unique(origin), unique(dest))),
                                  flights_delayed_reason_carrier=sum(!is.na(carrierdelay)), 
                                  flights_delayed_reason_weather=sum(!is.na(weatherdelay)),
                                  flights_delayed_reason_security=sum(!is.na(securitydelay))
  )][, year:=yr]
  #save this data
  saveData(flights.stats, paste(flights.folder.path, "stats/1/flights_stats_", yr, ".csv", sep=""))
  #clear up space
  rm(flights.stats)
  
  #
  # by month for every year
  # 
  if(verbose) cat("Getting flight stats by month: ", '\n')
  flights.stats.month <- flights[, list(airlines = length(unique(uniquecarrier)),
                                        flights=length(uniquecarrier),
                                        flights_cancelled=sum(cancelled, na.rm=T),
                                        flights_diverted=sum(diverted, na.rm=T),
                                        flights_departed_late=length(which(depdelay > 0)),
                                        flights_arrived_late=length(which(arrdelay > 0)),
                                        total_dep_delay_in_mins=sum(depdelay[which(depdelay > 0)]),
                                        avg_dep_delay_in_mins=round(mean(depdelay[which(depdelay > 0)])),
                                        median_dep_delay_in_mins=round(median(depdelay[which(depdelay > 0)])),
                                        miles_traveled=sum(distance, na.rm=T)
  ),
                                 by=month][, year:=yr]
  #change col order
  setcolorder(flights.stats.month, c("year", colnames(flights.stats.month)[-ncol(flights.stats.month)]))
  #save this data
  saveData(flights.stats.month, paste(flights.folder.path, "stats/2/flights_stats_by_month_", yr, ".csv", sep=""))
  #clear up space
  rm(flights.stats.month)
  
}

#
#map all calculations 
#
mapFlightStats <- function(){
  for(j in 1:period) {
    if( j > 2) {
      yr <- as.integer(gsub("[^0-9]", "", gsub("(.*)(\\.csv)", "\\1", flights.files[j])))
      flights.data.file <- paste(flights.folder.path, flights.files[j], sep="")
      if(verbose) cat(yr, ": Reading : ", flights.data.file, "\n")
      flights <- data.table(read.csv(flights.data.file, stringsAsFactors=F))
      col.names <- colnames(flights)
      setnames(flights, col.names, tolower(col.names))
      flights <- flights[, list(year, month, uniquecarrier, origin, 
                                dest, cancelled, diverted, depdelay, 
                                arrdelay, distance, carrierdelay,
                                weatherdelay,securitydelay)]
      setkeyv(flights, c("year", "uniquecarrier", "dest", "origin", "month")) 
      if(verbose) cat("Starting analysis on: ", yr, "\n") 
      getFlightStatsForYear(flights, yr)
      getFlightsStatusByAirlines(flights, yr)
      getFlightsStatsByAirport(flights, yr)
    }
  }
}  


#
#reduce all results
#
reduceFlightStats <- function(){
  n <- 1:6
  folder.path <- paste("./raw-data/flights/stats/", n, "/", sep="")
  print(folder.path)
  for(i in n){
    filenames <- paste(folder.path[i], list.files(path=folder.path[i], pattern="*.csv"), sep="") 
    dt <- do.call("rbind", lapply(filenames, read.csv, stringsAsFactors=F))
    print(nrow(dt))
    saveData(dt, paste("./raw-data/flights/stats/", i, ".csv", sep=""))
  }
}

#
# Run this job - initialize, generate stats for individual years and then aggregate them together 
# to get single file for flights, airports and airlines
#
runJob <- function(){
  initForStats()
  mapFlightStats()
  reduceFlightStats()
