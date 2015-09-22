# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#http://www.wisdenrecords.com/Records/India/Test/Bowling/Most_Wickets_in_Career.html
#http://www.wisdenrecords.com/Records/India/Test/Batting/Most_Career_Runs.html
#http://www.wisdenrecords.com/Records/Pakistan/Test/Batting/Most_Career_Runs.html
#http://www.wisdenrecords.com/Records/India/Test/Batting/Most_Career_Runs.html
require("RJSONIO")
require("plyr")
library("RCurl")

init <- function() {
  assign("folder.path", "./pads/raw-data/cricket/", envir=.GlobalEnv)
  assign("years", 2008:2013, envir=.GlobalEnv)
  assign("ipl.base.url", 'http://dynamic.pulselive.com/dynamic/data/core/cricket/2012/ipl', envir=.GlobalEnv)
  assign("ipl.end.url.runs", '/stats/player/full/mostRuns.js', envir=.GlobalEnv)
  assign("ipl.end.url.wickets", "/stats/player/full/mostWickets.js", envir=.GlobalEnv)
  assign("ipl.end.url.standings", "/groupStandings.js", envir=.GlobalEnv)  
}

# Save the JSON 
saveStatsData <- function(out.file, jsonSt){
  file.out <- file(paste(folder.path, out.file, ".JSON", sep=""), 'wt')
  cat(jsonSt, file=file.out, fill = TRUE)
  close(file.out)
}

#write a csv file 
saveStatsDataAsCSV <- function(data, out.file){
  file.location <- paste(folder.path, out.file, ".csv", sep="")
  write.csv(data, file.location, row.names=F)
}

#
# batting
#
battingStastByPlayer <- function(l){
  #return (data.frame(matrix(unlist(l), nrow=1, byrow=T)))
  return (data.frame(l$team$fullName, l$player$fullName, l$player$nationality, l$player$dateOfBirth,
                     matrix(unlist(l$battingStats), nrow=1, byrow=T), stringsAsFactors = F))
}

getBattingStats <- function(ipl.json, year){
  ipl.batting.stats <- ldply(ipl.json$mostRuns[[1]]$topPlayers, battingStastByPlayer)
  colnames(ipl.batting.stats) <- c("team", "player", "country", "dob", names(ipl.json$mostRuns[[1]]$topPlayers[[1]]$battingStats))
  ipl.batting.stats$hs <- sapply(ipl.batting.stats$hs, function(x) sub("\\*", "", x))
  ipl.batting.stats$year <- year
  # get age 
  ipl.batting.stats$age_in_months <- sapply(ipl.batting.stats$dob, function(x) getAge(x, paste(year, "/04/01", sep=""), "m"))
  return(ipl.batting.stats)
}

battingStats <- function(year){
  ipl.url <- paste(ipl.base.url, year, ipl.end.url.runs, sep="")
  print(ipl.url)
  content <- try(getURL(ipl.url), silent=F)
  if(class(content) %in% c("try-error")) stop("Can't continue...")
  content <- sub("\\);", "", sub(paste("onMostRuns", "\\(", sep=""), "", content))
  saveStatsData(paste("ipl", year, "batting-stats", sep="-"), content)
  ipl.json <- fromJSON(content)
  return(getBattingStats(ipl.json, year))
}

battingStatsFromFiles <- function(year){
  file.name <- paste(folder.path, paste("ipl", year, "batting-stats", sep="-"), ".json", sep="")
  file.out <- file(file.name)
  ipl.json <- fromJSON(paste(readLines(file.out), collapse = "\n"))  
  close(file.out)
  #assign("ipl", ipl.json,  envir=.GlobalEnv)
  return(getBattingStats(ipl.json, year))
}

#
# Bowling stats
#
bowlingStastByPlayer <- function(l){
  #return (data.frame(matrix(unlist(l), nrow=1, byrow=T)))
  return (data.frame(l$team$fullName, l$player$fullName, l$player$nationality, l$player$dateOfBirth,
                     matrix(unlist(l$bowlingStats), nrow=1, byrow=T), stringsAsFactors = F))
}

getBowlingStats <- function(ipl.json, year){
  ipl.bowling.stats <- ldply(ipl.json$mostWickets[[1]]$topPlayers, bowlingStastByPlayer)
  colnames(ipl.bowling.stats) <- c("team", "player", "country", "dob", names(ipl.json$mostWickets[[1]]$topPlayers[[1]]$bowlingStats))
  ipl.bowling.stats$year <- year
  # get age 
  ipl.bowling.stats$age_in_months <- sapply(ipl.bowling.stats$dob, function(x) getAge(x, paste(year, "/04/01", sep=""), "m"))

  return(ipl.bowling.stats)
}

bowlingStats <- function(year){
  ipl.url <- paste(ipl.base.url, year, ipl.end.url.wickets, sep="")
  print(ipl.url)
  content <- try(getURL(ipl.url), silent=T)
  if(class(content) %in% c("try-error")) stop("Can't continue...")
  content <- sub("\\);", "", sub(paste("onMostWickets", "\\(", sep=""), "", content))
  saveStatsData(paste("ipl", year, "bowling-stats", sep="-"), content)
  ipl.json <- fromJSON(content)
  return(getBowlingStats(ipl.json, year))
}

bowlingStatsFromFiles <- function(year){
  file.name <- paste(folder.path, paste("ipl", year, "bowling-stats", sep="-"), ".json", sep="")
  file.out <- file(file.name)
  ipl.json <- fromJSON(paste(readLines(file.out), collapse = "\n"))  
  close(file.out)
  return(getBowlingStats(ipl.json, year))
}

#
# standings
#
standingStatsBySeason <- function(l){
  return (data.frame(l$position, l$team$fullName, l$played, l$won, l$lost, l$tied, l$noResult, l$points, l$netRunRate))
}

getStandingStats <- function(ipl.json, year){
  ipl.standing.stats <- ldply(ipl.json$groups[[1]]$standings, standingStatsBySeason)
  colnames(ipl.standing.stats) <- c("position", "team", "played", "won", "lost", "tied", "noResult", "points", "netrunrate")
  ipl.standing.stats$year <- year
  return(ipl.standing.stats)
}

standingStats <- function(year){
  ipl.url <- paste(ipl.base.url, year, ipl.end.url.standings, sep="")
  print(ipl.url)
  content <- try(getURL(ipl.url), silent=F)
  if(class(content) %in% c("try-error")) stop("Can't continue...")
  content <- sub("\\);", "", sub(paste("onGroupStandings", "\\(", sep=""), "", content))
  saveStatsData(paste("ipl", year, "standings-stats", sep="-"), content)
  ipl.json <- fromJSON(content)
  #assign("ipl", ipl.json,  envir=.GlobalEnv)
  return(getStandingStats(ipl.json, year))
}

standingStatsFromFiles <- function(year){
  file.name <- paste(folder.path, paste("ipl", year, "standings-stats", sep="-"), ".json", sep="")
  file.out <- file(file.name)
  ipl.json <- fromJSON(paste(readLines(file.out), collapse = "\n"))
  close(file.out)
  #assign("ipl", ipl.json,  envir=.GlobalEnv)
  return(getStandingStats(ipl.json, year))
}


#
# get IPL stats
#
getStats <- function(){
  init()
  #
  #get batting stats
  #
  batting.stats <- adply(years, 1, battingStatsFromFiles)
  colnames(batting.stats) <- c("season", colnames(batting.stats)[2:9], "fours", "sixes", 
                               colnames(batting.stats)[12:13], "fiftys", "hundreds", 
                               colnames(batting.stats)[16:ncol(batting.stats)])
  
  saveStatsDataAsCSV(batting.stats, "IPL-T20-Batting-Stats-For-All-Seasons")

  #
  #get bowling stats
  #
  bowling.stats <- adply(years, 1, bowlingStatsFromFiles)
  colnames(bowling.stats) <- c("season", colnames(bowling.stats)[2:10], "fours", 
                               "sixes", colnames(bowling.stats)[13:21], "four_wickets", 
                               "five_wickets", "ten_wickets", 
                               colnames(bowling.stats)[25:ncol(bowling.stats)])
   
   saveStatsDataAsCSV(bowling.stats, "IPL-T20-Bowling-Stats-For-All-Seasons")
  
  #
  # get group stats
  #
  group.standings <- adply(years, 1, standingStatsFromFiles)
  colnames(group.standings) <- c("season", colnames(group.standings)[2:ncol(group.standings)])
  saveStatsDataAsCSV(group.standings, "IPL-T20-Standing-Stats-For-All-Seasons")
}

if(F){
  
  #http://dynamic.pulselive.com/dynamic/data/core/cricket/2012/ipl2013/groupStandings.js
  #http://dynamic.pulselive.com/dynamic/data/core/cricket/2012/ipl2012/groupStandings.js
  
  ipl.json <- fromJSON(paste(readLines(paste(folder.path, "ipl-2013-sample-bat.json", sep="")), collapse = "\n"))
  ipl <- read.csv(paste(folder.path, "IPLstats.csv", sep=""), stringsAsFactors=F)
  
  #http://dynamic.pulselive.com/dynamic/data/core/cricket/2012/ipl2012/stats/player/full/mostWickets.js
  for(y in years){
    aaply(ipl, 1, getStat, y)
  }
  
  ipl.url <- "http://dynamic.pulselive.com/dynamic/data/core/cricket/2012/ipl2012/stats/player/full/mostRuns.js"
  content <- getURL(ipl.url)
  content <- sub("\\);", "", sub("onMostRuns\\(", "", content),)
  
  theurl <- "http://en.wikipedia.org/wiki/Brazil_national_football_team"
  tables <- readHTMLTable(theurl)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  tables[[which.max(n.rows)]]
  colnames(ipl.batting.stats) <- sapply(names(unlist(ipl.json$mostRuns[[1]]$topPlayers[[1]])), function(x) {gsub("(.*)\\.", "",x)})
  gsub("(.*)\\.", "", "team.fullName")
}  
  
  
