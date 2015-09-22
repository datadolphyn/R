# Generate IPL PADS
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#
# Generate IPL PADS
#
source("CreatePADS.R")

#
# loadBattingStats
#
loadBattingStats <- function(){
  stats <- readFile(paste(ipl.folder.path, bat.file, sep=""))
  if(!is.na(stats)){
    cols <- c(1, 5:ncol(stats))
    stats[cols] <- as.numeric(as.matrix(stats[cols]))
    batting.stats <- data.table(stats)
    setkeyv(batting.stats, c("player", "year", "season", "country", "age_in_months"))
    assign("batting.stats", batting.stats, envir=.GlobalEnv)
  }
}

#
# loadBowlingStats
#
loadBowlingStats <- function(){
  stats <- readFile(paste(ipl.folder.path, bow.file, sep=""))
  if(!is.na(stats)){
    cols <- c(1, 5:ncol(stats))
    stats[cols] <- as.numeric(as.matrix(stats[cols]))
    bowling.stats <- data.table(stats)
    setkeyv(bowling.stats, c("player", "year", "season", "country"))
    assign("bowling.stats", bowling.stats, envir=.GlobalEnv)
  }
}

#
# loadStandingStats
#
loadStandingStats <- function(){
  stats <- readFile(paste(ipl.folder.path, standings.file, sep=""))
  if(!is.na(stats)){
    standing.stats <- data.table(stats)
    setkeyv(standing.stats, c("team", "year", "season"))
    assign("standing.stats", standing.stats, envir=.GlobalEnv)
  }
  
  stats <- readFile(paste(ipl.folder.path, stats.2013.file, sep=""))
  if(!is.na(stats)){
    match.stats <- data.table(stats)
    assign("match.stats", match.stats, envir=.GlobalEnv)
  }
}

#
# initialize
#
startup <- function() {
  #initialize system
  initializeSystem()
  
  assign("ipl.folder.path", "./pads/raw-data/cricket/", envir=.GlobalEnv)
  assign("years", 2008:2013, envir=.GlobalEnv)
  assign("bat.file", "IPL-T20-Batting-Stats-For-All-Seasons.csv", envir=.GlobalEnv)
  assign("bow.file", "IPL-T20-Bowling-Stats-For-All-Seasons.csv", envir=.GlobalEnv)
  assign("standings.file", "IPL-T20-Standing-Stats-For-All-Seasons.csv", envir=.GlobalEnv)
  assign("stats.2013.file", "IPL-all-matches-stats.csv", envir=.GlobalEnv)
  
  assign("dataset", "IPL-T20", envir=.GlobalEnv)
  loadBattingStats()
  loadBowlingStats()
  loadStandingStats()
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "IPLT20, ThatsCricket"
  series["category"] <- "Sports"
  series["subcategory"] <- "Cricket"
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "ipl, iplt20, cricket, India, Australia", sep=","))
  assign("series", series, envir=.GlobalEnv)
}

#
# cleanup IPL
#
cleanup <- function(){
  cleaupSystem()
}

#
# generate pads for batsman by seasons
#
generateBattingPADS <- function(){    
  #Players who have played all seasons
  series.data <- as.data.frame(batting.stats[, list(seasons=length(season)), by=player][seasons > 5][1:25])
  series["title"] <- "IPLT20 - Players Who Played All IPL Seasons"
  series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #Non-Indian Players Who Played 5 or More IPL Seasons
  series.data <- as.data.frame(unique(batting.stats[, list(seasons=length(season), country), 
                                                    by=player][seasons > 4][country!="Indian"]
                                      [order(-seasons)][,country:=NULL]))
  series["title"] <- "IPLT20 - Non-Indian Players Who Played 5 or More IPL Seasons"
  series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  ## youngest and oldest batsmen 
  series.data <- batting.stats[age_in_months > 120][, list(youngest_player=min(age_in_months)/12, oldest_player=max(age_in_months)/12), by=season]
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Age (in years) of Youngest and Oldest Batsmen by IPL Seasons"
  series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  x <- batting.stats[age_in_months > 120][,list(age_group=cut(age_in_months/12, breaks=5*(0:9), right=F, dig.lab=6), age_in_months), by=season][order(age_in_months)]
  series.data <- x[,list(players=length(age_in_months)), by=list(age_group, season)][order(season)]
  series.data$age_group <- sub(",", "-", sub("^[\\[]", "", sub("[\\)]$", "", as.character(series.data$age_group))))
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- paste("IPLT20 - Grouping of Batsmen by Age in All Seasons", sep="")
  series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  
  for (i in unique(batting.stats$season)) {
    series.data <- batting.stats[season==i][age_in_months > 120][, list(player, age=age_in_months)][order(age)][c(1,length(player))]
    series.data$age <- series.data$age/12
    series["title"] <- paste("IPLT20 - Youngest and Oldest Batsmen in Season",i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    #Now do the distribution
    x <- batting.stats[season==i][age_in_months > 120][,list(age_group=cut(age_in_months/12, breaks=5*(0:9), right=F, dig.lab=6), age_in_months)][order(age_in_months)]
    series.data <- x[,list(players=length(age_in_months)), by=age_group]
    series.data$age_group <- sub(",", "-",sub("^[\\[]", "", sub("[\\)]$", "", as.character(series.data$age_group))))
    series["title"] <- paste("IPLT20 - Grouping of Batsmen by Age in Season",i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
  }  

  # runs by seasons
  series.data <- as.data.frame(batting.stats[, list(runs=sum(r, na.rm = T)), by=season])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Runs Scored by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # fours by seasons
  series.data <- as.data.frame(batting.stats[, list(fours=sum(fours, na.rm = T)), by=season])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Fours Scored by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # sixes by seasons
  series.data <- as.data.frame(batting.stats[, list(sixes=sum(sixes, na.rm = T)), by=season])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Sixes Scored by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # runs, fours and sixes by seasons
  series.data <- as.data.frame(batting.stats[, list(runs=sum(r, na.rm = T), fours=sum(fours, na.rm = T), sixes=sum(sixes, na.rm = T)), by=season])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Runs, Fours & Sixes Scored by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # fours and sixes by seasons
  series.data <- as.data.frame(batting.stats[, list(fours=sum(fours, na.rm = T), sixes=sum(sixes, na.rm = T)), by=season])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Fours & Sixes Scored by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #do this for all seasons
  seasons <- c("All", years)
  for (i in seasons) {
    title.text <- "All IPL Seasons"
    if(i=="All")
      batting.stats.short <- batting.stats
    else {
      batting.stats.short <- batting.stats[year==i]
      title.text <- paste(i, " IPL Season", sep="")
    }
    cat("\n", i, "number of rows", nrow(batting.stats.short), sep="::")
    
    # Matches Played
    series.data <- as.data.frame(batting.stats.short[, list(matches=sum(m, na.rm = T)), by=player][order(-matches)][1:25])
    series["title"] <- paste("IPLT20 - Players Who Played Highest Number of Matches in ", title.text, sep="")
    series["desc"] <- paste(series["title"], 
                            "This PAD only shows top 25 players. (Data as of", Sys.Date(), ")", sep=" ")
    #generate pad
    padify(series, series.data)
    
    # Player distribution by countries
    series.data <- as.data.frame(batting.stats.short[, list(players=length(player)), by=list(country)][order(-players)])
    series.data$country[3] = "Aliens"
    series["title"] <- paste("IPLT20 - Players By Countries in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Players from many countries participate in IPL. Aliens are players who have not been correctly identified. (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    #Top runs scorer
    series.data <- as.data.frame(batting.stats.short[, list(runs=sum(r, na.rm = T)), by=player][order(-runs)][1:30])
    series["title"] <- paste("IPLT20 - Top Batsmen by Total Runs for ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Top fours scorer
    series.data <- as.data.frame(batting.stats.short[, list(fours=sum(fours, na.rm = T)), by=player][order(-fours)][1:30])
    series["title"] <- paste("IPLT20 - Top Batsmen by Total Fours for ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Top sixes scorer
    series.data <- as.data.frame(batting.stats.short[, list(sixes=sum(sixes, na.rm = T)), by=player][order(-sixes)][1:30])
    series["title"] <- paste("IPLT20 - Top Batsmen by Total Sixes for ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Top run scorer with 4s and 6s
    series.data <- as.data.frame(batting.stats.short[, list(runs=sum(r, na.rm = T), fours=sum(fours, na.rm = T), 
                                                            sixes=sum(sixes, na.rm = T)), by=player][order(-runs)][1:15])
    series["title"] <- paste("IPLT20 - Top Batsmen by Runs, Fours, & Sixes for  ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # 4s and 6s
    series.data <- as.data.frame(batting.stats.short[, list(fours=sum(fours, na.rm = T), 
                                                            sixes=sum(sixes, na.rm = T)), by=player][order(-fours)][1:15])
    series["title"] <- paste("IPLT20 - Top Batsmen by Fours, & Sixes for  ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Highest run scorer
    series.data <- as.data.frame(batting.stats.short[, list(highest_score=hs), by=player][order(-highest_score)][1:15])
    series["title"] <- paste("IPLT20 - Top 15 Highest Run Scored in an Inning in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most fifties by a batsman 
    series.data <- as.data.frame(batting.stats.short[, list(fifites=fiftys), by=player][fifites > 0][order(-fifites)][1:15])
    series["title"] <- paste("IPLT20 - Most Fifties By Batsmen in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Most hundreds by a batsman 
    series.data <- as.data.frame(batting.stats.short[, list(hundreds=hundreds), by=player][hundreds > 0][order(-hundreds)][1:15])
    series["title"] <- paste("IPLT20 - Most Hundreds By Batsmen in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Highest strike rates by a batsman > 300 runs
    series.data <- as.data.frame(batting.stats.short[, list(strike_rate=mean(sr), 
                                                            runs=sum(r, na.rm = T)), by=player][runs > 500][order(-strike_rate)][1:15][,runs:=NULL])
    series["title"] <-  paste("IPLT20 - Highest Strike Rates of Batsmen in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Batsmen who have scored more than 300 runs are considered here. Average strike rate is calculated. (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Highest strike rates by a batsman > 500 runs
    series.data <- as.data.frame(batting.stats.short[, list(strike_rate=mean(sr), 
                                                            runs=sum(r, na.rm = T)), by=player][runs > 500][order(-strike_rate)][1:15][,runs:=NULL])
    series["title"] <-  paste("IPLT20 - Highest Strike Rates of Batsmen in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Batsmen who have scored more than 500 runs are considered here. Average strike rate is calculated. (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Highest strike rates by a batsman > 1000 runs
    series.data <- as.data.frame(batting.stats.short[, list(strike_rate=mean(sr), 
                                                            runs=sum(r, na.rm = T)), by=player][runs > 1000][order(-strike_rate)][1:15][,runs:=NULL])
    series["title"] <-  paste("IPLT20 - Highest Strike Rates of Batsmen in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Batsmen who have scored more than 1000 runs are considered here. Average strike rate is calculated. (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Highest average strike rates by a batsman > 1500 runs
    series.data <- as.data.frame(batting.stats.short[, list(strike_rate=mean(sr), 
                                                            runs=sum(r, na.rm = T)), by=player][runs > 1500][order(-strike_rate)][1:15][,runs:=NULL])
    series["title"] <-  paste("IPLT20 - Highest Strike Rates of Batsmen in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Batsmen who have scored more than 1500 runs are considered here. Average strike rate is calculated. (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Highest strike rates by a batsman > 2000 runs
    series.data <- as.data.frame(batting.stats.short[, list(strike_rate=mean(sr), runs=sum(r, na.rm = T)), by=player][runs > 2000][order(-strike_rate)][1:15][,runs:=NULL])
    series["title"] <-  paste("IPLT20 - Highest Strike Rates of Batsmen in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Batsmen who have scored more than 2000 runs are considered here. Average strike rate is calculated. (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
  }
}

#
# generate pads for batsman by palyers by seasons
#
generateBattingPADSByPlayer <- function(){
    
  players <- unique(batting.stats$player)
  title.text <- paste("(", min(years), "-", max(years), ")", sep="")
  for(i in players){
    
    if(is.na(i) || is.null(i) || i== "?" || i=="")
      next
    
    batting.stats.short <- batting.stats[player==i]
    cat("\n", i, "number of rows", nrow(batting.stats.short), sep="::")
    series["tags"] <- tolower(paste(series$category, series$subcategory, series$source,i, "ipl", sep=","))
    if(nrow(batting.stats.short) == 0)
      next
    # Matches played by Seasons
    series.data <- as.data.frame(batting.stats.short[, list(season, matches=m)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Matches Played by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"], 
                            ". (Data as of", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    #Runs scroed by season
    series.data <- as.data.frame(batting.stats.short[, list(season, runs=r)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Total Runs Scored by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # Fours scrored 
    series.data <- as.data.frame(batting.stats.short[, list(season, fours)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Total Fours Scored by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # sixes scroed
    series.data <- as.data.frame(batting.stats.short[, list(season,sixes)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Total Sixes Scored by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # Top run scorer with 4s and 6s
    series.data <- as.data.frame(batting.stats.short[, list(season,runs=r, fours, sixes)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Runs, Fours, & Sixes by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # Highest run scorer
    series.data <- as.data.frame(batting.stats.short[, list(season, highest_score=hs)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Highest Run Scored in an Inning by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # fifties 
    series.data <- as.data.frame(batting.stats.short[, list(season,fifites=fiftys)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Fifties by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # hundreds 
    series.data <- as.data.frame(batting.stats.short[, list(season, hundreds)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Hundreds by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # Highest strike rates
    series.data <- as.data.frame(batting.stats.short[, list(season, strike_rate=sr)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <-  paste(i, ": Strike Rates by Season in IPL", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
  }  
}

#
# generate pads for bowlers by seasons
#
generateBowlingPADS <- function(){

  #age of bowlers by seasons
  series.data <- bowling.stats[age_in_months > 120][, list(youngest_player=min(age_in_months), oldest_player=max(age_in_months)), by=season]
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Age (in months) of Youngest and Oldest Bowlers by IPL Seasons"
  series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  x <- bowling.stats[age_in_months > 120][,list(age_group=cut(age_in_months/12, breaks=5*(0:9), right=F, dig.lab=6), age_in_months), by=season][order(age_in_months)]
  series.data <- x[,list(players_count=length(age_in_months)), by=list(age_group, season)][order(season)]
  series.data$age_group <- sub(",", "-", sub("^[\\[]", "", sub("[\\)]$", "", as.character(series.data$age_group))))
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- paste("IPLT20 - Grouping of Bowlers by Age in All Seasons", sep="")
  series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  for (i in unique(bowling.stats$season)) {
    series.data <- bowling.stats[season==i][age_in_months > 120][, list(player, age=age_in_months)][order(age)][c(1,length(player))]
    series.data$age <- series.data$age/12
    series["title"] <- paste("IPLT20 - Youngest and Oldest Bowlers in Season",i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    #Now do the distribution
    x <- bowling.stats[season==i][age_in_months > 120][,list(age_group=cut(age_in_months/12, breaks=5*(0:9), right=F, dig.lab=6), age_in_months)][order(age_in_months)]
    series.data <- x[,list(players=length(age_in_months)), by=age_group]
    series.data$age_group <- sub("^[\\[]", "", sub("[\\)]$", "", as.character(series.data$age_group)))
    series["title"] <- paste("IPLT20 - Grouping of Bowlers by Age in Season", i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
  }
  
  # overs by seasons
  series.data <- as.data.frame(bowling.stats[, list(overs=sum(ov, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Overs Bowled by Bowlers by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # wickets by seasons
  series.data <- as.data.frame(bowling.stats[, list(wickets=sum(w, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Players Out by Season"
  series["desc"] <- paste(series["title"],  "Wickets Fallen by Season, Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # dot balls by seasons
  series.data <- as.data.frame(bowling.stats[, list(dot_balls=sum(d, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Dot Balls Bowled by Bowlers by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # maiden overs by seasons
  series.data <- as.data.frame(bowling.stats[, list(maiden_overs=sum(maid, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Maiden Overs Bowled by Bowlers by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # nb by seasons
  series.data <- as.data.frame(bowling.stats[, list(no_balls=sum(nb, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - No Balls Bowled by Bowlers by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # wb by seasons
  series.data <- as.data.frame(bowling.stats[, list(wide_balls=sum(wb, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Wide Balls Bowled by Bowlers by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # runs conceded and balls bowled
  series.data <- as.data.frame(bowling.stats[, list(runs=sum(r, na.rm = T), balls=sum(b, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Runs Conceded and Balls Bowled by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # average economy by seasons
  series.data <- as.data.frame(bowling.stats[, list(average_economy_rate=mean(e, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Average Economy Rate by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # four wicket economy by seasons
  series.data <- as.data.frame(bowling.stats[, list(four_wickets=sum(four_wickets, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Four Wickets in an Inning by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # five wicket economy by seasons
  series.data <- as.data.frame(bowling.stats[, list(five_wickets=sum(five_wickets, na.rm = T)), by=season][order(season)])
  series.data$season <- paste("Season",  series.data$season, sep="-")
  series["title"] <- "IPLT20 - Five Wickets in an Inning by Season"
  series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #do this for all seasons
  seasons <- c("All", years)
  for (i in seasons) {
    title.text <- "All IPL Seasons"
    if(i=="All")
      bowling.stats.short <- bowling.stats
    else {
      bowling.stats.short <- bowling.stats[year==i]
      title.text <- paste(i, " IPL Season", sep="")
    }
    cat("\n", i, "number of rows", nrow(bowling.stats.short), sep="::")
    
    #Top wicket taker
    series.data <- as.data.frame(bowling.stats.short[, list(wickets=sum(w, na.rm = T)), by=player][order(-wickets)][1:20])
    series["title"] <- paste("IPLT20 - Top Bowler by Wickets Taken in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most economical bowler
    series.data <- as.data.frame(bowling.stats.short[, list(maiden_overs=maid), 
                                                     by=player][order(-maiden_overs)][1:20])
    series["title"] <- paste("IPLT20 - Top Economical Bowlers by Maiden Overs in", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most economical bowler
    series.data <- as.data.frame(bowling.stats.short[, list(dot_balls=d), 
                                                     by=player][order(-dot_balls)][1:20])
    series["title"] <- paste("IPLT20 - Top Economical Bowlers by Dot Balls in", title.text, sep="")
    series["desc"] <- paste(series["title"],  " Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most economical bowler
    series.data <- as.data.frame(bowling.stats.short[, list(economy_rate=e, ov), by=player][ov>5][,ov:=NULL][order(economy_rate)][1:20])
    series["title"] <- paste("IPLT20 - Top Economical Bowlers in", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  "Bowlers who bowled at least 5 overs were considered. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most economical bowler
    series.data <- as.data.frame(bowling.stats.short[, list(economy_rate=e, ov), by=player][ov>10][,ov:=NULL][order(economy_rate)][1:20])
    series["title"] <- paste("IPLT20 - Top Economical Bowlers in", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  "Bowlers who bowled at least 10 overs were considered. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most economical bowler
    series.data <- as.data.frame(bowling.stats.short[, list(economy_rate=e, ov), by=player][ov>15][,ov:=NULL][order(economy_rate)][1:20])
    series["title"] <- paste("IPLT20 - Top Economical Bowlers in ", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  "Bowlers who bowled at least 15 overs were considered. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most economical bowler and wicket taker
    series.data <- as.data.frame(bowling.stats.short[, list(wickets=sum(w, na.rm = T), 
                                                            economy_rate=e), by=player][order(-wickets)][1:15])
    series["title"] <- paste("IPLT20 - Top Wicket Takers With Economy Rates in ", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most errant 
    series.data <- as.data.frame(bowling.stats.short[, list(wide_balls=sum(wb, na.rm = T)), by=player][order(-wide_balls)][1:20])
    series["title"] <- paste("IPLT20 - Top Errant Bowlers in ", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  "Bowlers who bowled wide balls. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most errant by no balls
    series.data <- as.data.frame(bowling.stats.short[, list(no_balls=sum(nb, na.rm = T)), by=player][order(-no_balls)][1:20])
    series["title"] <- paste("IPLT20 - Top Errant Bowlers in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Bowlers who bowled no balls. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most errant by no and wide balls
    series.data <- as.data.frame(bowling.stats.short[, list(wide_balls=sum(wb, na.rm = T), no_balls=sum(nb, na.rm = T)), by=player][order(-wide_balls)][1:20])
    series["title"] <- paste("IPLT20 - Top Errant Bowlers in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Bowlers who bowled no balls. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Got hit for 4s
    series.data <- as.data.frame(bowling.stats.short[, list(fours=sum(fours, na.rm = T)), 
                                                     by=player][order(-fours)][1:20])
    series["title"] <- paste("IPLT20 - Bowlers Hit for Fours in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "In Ravi Shastri words - taken to cleaners. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Got hit for 6s
    series.data <- as.data.frame(bowling.stats.short[, list(sixes=sum(sixes, na.rm = T)), 
                                                     by=player][order(-sixes)][1:20])
    series["title"] <- paste("IPLT20 - Bowlers Hit for Sixes in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "In Ravi Shastri words - taken to cleaners. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # 4s and 6s
    series.data <- as.data.frame(bowling.stats.short[, list(fours=sum(fours, na.rm = T), sixes=sum(sixes, na.rm = T)), 
                                                     by=player][order(-sixes)][1:15])
    series["title"] <- paste("IPLT20 - Bowlers Hit for Fours and Sixes in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "In Ravi Shastri words - taken to cleaners. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    # Most errant by no and wide balls and wickets
    series.data <- as.data.frame(bowling.stats.short[, list(wide_balls=sum(wb, na.rm = T), 
                                                            no_balls=sum(nb, na.rm = T), 
                                                            wickets=sum(w, na.rm = T)), 
                                                     by=player][order(-wide_balls)][1:10])
    series["title"] <- paste("IPLT20 - Top Errant Bowlers in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Bowlers who bowled no balls. (Data as of ", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    
    # Highest runs conceded
    series.data <- as.data.frame(bowling.stats.short[, list(runs=sum(r, na.rm = T)), by=player][order(-runs)][1:20])
    series["title"] <-  paste("IPLT20 - Top Bowlers Conceding Most Runs in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Highest runs conceded and wickets taken
    series.data <- as.data.frame(bowling.stats.short[, list(runs=sum(r, na.rm = T),
                                                            wickets=sum(w, na.rm = T)), by=player][order(-runs)][1:20])
    series["title"] <-  paste("IPLT20 - Most Runs Conceded & Wickets Taken in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Lowest runs conceded
    series.data <- as.data.frame(bowling.stats.short[, list(runs=sum(r, na.rm = T), ov), by=player][ov > 10][,ov:=NULL][order(runs)][1:20])
    series["title"] <-  paste("IPLT20 - Bowlers Conceding Lowest Runs in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Bowler who bowled more than 10 overs are considered here. Average strike rate is calculated. (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Lowest runs conceded and Wickets taken
    series.data <- as.data.frame(bowling.stats.short[, list(runs=sum(r, na.rm = T), 
                                                            wickets=sum(w, na.rm = T),ov), by=player][ov > 10][,ov:=NULL][order(runs)][1:20])
    series["title"] <-  paste("IPLT20 - Bowlers Conceding Lowest Runs With Taking Wickets in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  "Bowler who bowled more than 10 overs are considered here. Average strike rate is calculated. (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Four Wickets taken
    series.data <- as.data.frame(bowling.stats.short[, list(four_wickets=sum(four_wickets, na.rm = T)), 
                                                     by=player][order(four_wickets)])
    series["title"] <-  paste("IPLT20 - Bowlers Taking Four Wickers in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)
    
    # Five Wickets taken
    series.data <- as.data.frame(bowling.stats.short[, list(five_wickets=sum(five_wickets, na.rm = T)), 
                                                     by=player][order(five_wickets)])
    series["title"] <-  paste("IPLT20 - Bowlers Taking Five Wickers in ", title.text, sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    #Remove rows that have NAs
    series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
    if(nrow(series.data) > 0)
      padify(series, series.data)                          
  }
}

#
# generate pads for bowlers by years seasons
#
generateBowlingPADSbyPlayer <- function(){
  
  bowlers <- unique(bowling.stats$player)
  title.text <- paste("(", min(years), "-", max(years), ")", sep="")
  for(i in bowlers){
    if(is.na(i) || is.null(i) || i== "?" || i=="")
      next
    bowling.stats.short <- bowling.stats[player==i]
    cat("\n", i, "number of rows", nrow(bowling.stats.short), sep="::")
    if(nrow(bowling.stats.short) == 0)
      next
    series["tags"] <- tolower(paste(series$category, series$subcategory, series$source,i, "ipl", sep=","))
    
    # Matches played by Seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, matches=m)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Matches Played in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],". (Data as of", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # overs by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, overs=ov)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Overs Bowled by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # wickets by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, wickets=w)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Wickets Taken by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # dot balls by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, dot_balls=d)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Dot Balls Bowled by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # maiden overs by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, maiden_overs=maid)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Maiden Overs Bowled by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # nb by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, no_balls=nb)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": No Balls Bowled by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # wb and nb by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, no_balls=nb, wide_balls=wb)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": No Balls and Wide Balls Bowled by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # wb by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, wide_balls=wb)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Wide Balls Bowled by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    
    # runs conceded and balls bowled
    series.data <- as.data.frame(bowling.stats.short[, list(season, 
                                                            runs=r, 
                                                            balls=b)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Runs Produced and Balls Bowled by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # average economy by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, 
                                                            economy_rate=e)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Economy Rate by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # average economy and wickets by seasons
    series.data <- as.data.frame(bowling.stats.short[, list(season, 
                                                            economy_rate=e, wickets=w)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Economy Rate and Wickets Taken by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  ". (Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # Got hit for 4s
    series.data <- as.data.frame(bowling.stats.short[, list(season, fours)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Fours Conceded by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  "Ravi Shastri - taken to cleaners - Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # Got hit for 6s
    series.data <- as.data.frame(bowling.stats.short[, list(season, sixes)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Sixes Conceded by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  "Ravi Shastri - taken to cleaners - Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    
    # 4s and 6s
    series.data <- as.data.frame(bowling.stats.short[, list(season, sixes, fours)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Sixes and Fours Conceded by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  "Ravi Shastri - taken to cleaners - Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # four_wickets
    series.data <- as.data.frame(bowling.stats.short[, list(season, four_wickets)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Fours Wickets by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  " The wicket taker! - Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
    padify(series, series.data)
    
    # five_wickets
    series.data <- as.data.frame(bowling.stats.short[, list(season, five_wickets)])
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, ": Five Wickets by Season in IPL", title.text, sep=" ")
    series["desc"] <- paste(series["title"],  " The wicket taker! - Data as of ", Sys.Date(), ")", sep=" ")
    # replace na with zeros
    series.data[is.na(series.data)] <- 0
  }
}

#
# generate standings pads
#
generateStandingPADS <- function(){  
  
  #Standing by Season
  for(i in unique(standing.stats$season)){
    series.data <- standing.stats[season==i][, list(team, wins=won, losses=lost, points)][order(-points)]
    series["title"] <- paste("IPLT20 - Team Standings For Season", i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    series.data <- standing.stats[season==i][, list(team, wins=won, losses=lost)][order(-wins)]
    series["title"] <- paste("IPLT20 - Team Wins & Losses For Season ", i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    series.data <- standing.stats[season==i][, list(team, points)][order(-points)]
    series["title"] <- paste("IPLT20 - Team Points For Season ", i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    series.data <- standing.stats[season==i][, list(team, netrunrate)][order(-netrunrate)]
    series["title"] <- paste("IPLT20 - Team Net Run Rate For Season ", i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    series.data <- standing.stats[season==i][, list(team, points, netrunrate)][order(-points)]
    series["title"] <- paste("IPLT20 - Team Points & Net Run Rate For Season ", i, sep="-")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
  }
  
  #Standing by Teams
  for(i in unique(standing.stats$team)){
    series.data <- standing.stats[team==i][, list(season, wins=won, losses=lost, points)]
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, " - Standings By Season", sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    series.data <- standing.stats[team==i][, list(season, wins=won, losses=lost)]
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, " - Wins & Losses By Season", sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    series.data <- standing.stats[team==i][, list(season, points)]
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i , " - Points By Season", sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    series.data <- standing.stats[team==i][, list(season, netrunrate)]
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, " - Net Run Rate By Season ", sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
    
    series.data <- standing.stats[team==i][, list(season, points, netrunrate)]
    series.data$season <- paste("Season",  series.data$season, sep="-")
    series["title"] <- paste(i, " - Points & Net Run Rate By Season ", sep="")
    series["desc"] <- paste(series["title"],  ". (Data as of", Sys.Date(), ")", sep=" ")
    padify(series, series.data)
  }
}


#
#grand finale for 2013
#

finaleIPL2013 <- function(){
  
  #overall team stats
  series.data <- standing.stats[year==2013][team %in% c("Mumbai Indians", "Chennai Super Kings")][,list(team, won, lost, netrunrate)]
  series["title"] <- paste("IPLT20 - Comparing Wins, Losses and NRR for Two Finalists - Mumbai Indians vs. Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. Wins, Losses and NRR for each team. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  
  #wins playing first vs chasing
  mi.csk <- rbind(match.stats[Team.1 %in% c("Mum Indians", "Super Kings")], match.stats[Team.2 %in% c("Mum Indians", "Super Kings")])
  #mi.csk[Winner %in% c("Mum Indians", "Super Kings")] [, nrow(.SD), by=list(won_when, Winner)]
  w.d <- mi.csk[Winner %in% c("Mum Indians", "Super Kings")][won_when=="defended"] [, list(won_defending=nrow(.SD)), by=list(Winner)]
  w.c <- mi.csk[Winner %in% c("Mum Indians", "Super Kings")][won_when=="chased"] [, list(won_chasing=nrow(.SD)), by=list(Winner)]
  setkey(w.d, Winner)
  setkey(w.c, Winner)
  series.data <- merge(w.d, w.c, all=T)
  series["title"] <- paste("IPLT20 - Wins When Chasing and Defending - Mumbai Indians vs. Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #wins playing first vs chasing
  #mi.csk[Winner %in% c("Mum Indians", "Super Kings")] [, nrow(.SD), by=list(won_when, Winner)]
  #   w.d <- mi.csk[Losser %in% c("Mum Indians", "Super Kings")][won_when=="defended"] [, list(lost_chasing=nrow(.SD)), by=Losser]
  #   w.c <- mi.csk[Losser %in% c("Mum Indians", "Super Kings")][won_when=="chased"] [, list(lost_defending=nrow(.SD)), by=list(Losser)]
  #   setkey(w.d, Losser)
  #   setkey(w.c, Losser)
  #   series.data <- merge(w.d, w.c, all=T)
  #   series["title"] <- paste("IPLT20 - Losses When Chasing and Defending - Mumbai Indians vs. Chennai Super Kings", sep="")
  #   series["desc"] <- paste(series["title"],  " For Season 6. (Data as of", Sys.Date(), ")", sep=" ")
  #   padify(series, series.data)
  
  # batting and bowling stats
  setkeyv(batting.stats, c("team","year"))
  setkeyv(bowling.stats, c("team","year"))         
  
  mi.bat <- batting.stats[year==2013][team=="Mumbai Indians"]  
  mi.bowl <- bowling.stats[year==2013][team=="Mumbai Indians"]
  
  csk.bat <- batting.stats[year==2013][team=="Chennai Super Kings"]  
  csk.bowl <- bowling.stats[year==2013][team=="Chennai Super Kings"]
  
  # no of batsman and runs they have scored
  mi <- mi.bat[,list(team="Mumbai Indians", runs=sum(r), fours=sum(fours), sixes=sum(sixes), players_count=length(player))]
  csk <- csk.bat[,list(team="Chennai Super Kings", runs=sum(r), fours=sum(fours), sixes=sum(sixes), players_count=length(player))]
  series.data <- rbind(mi, csk)
  series["title"] <- paste("IPLT20 - Comparing Runs, Fours, Sixes of Two Finalists - Mumbai Indians vs. Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. Runs, Fours, Sixes and Players for each team. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # players, fifties and hundreds
  mi <- mi.bat[,list(team="Mumbai Indians", players_count=length(player), fiftys=sum(fiftys), 
                     hundreds=sum(hundreds))]
  csk <- csk.bat[,list(team="Chennai Super Kings", players_count=length(player), fiftys=sum(fiftys), 
                       hundreds=sum(hundreds))]
  series.data <- rbind(mi, csk)
  series["title"] <- paste("IPLT20 - Comparing Players, Half-Centuries and Centuries of Two Finalists - Mumbai Indians vs. Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. Players, Half-Centuries and Centuries scored by each team. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #players - indian vs. non-indians
  mi <- mi.bat[, list(mumbai_indians=nrow(.SD)), by=country][order(country)]
  csk <- csk.bat[, list(chennai_super_kings=nrow(.SD)), by=country][order(country)]
  setkey(mi, country)
  setkey(csk, country)
  series.data <- merge(mi, csk, all=T)
  series.data[is.na(series.data)] <- 0 # replace NA with 0s
  #series.data <- merge(mi, csk, by="country")
  series["title"] <- paste("IPLT20 - Breakup of Players By Country for Mumbai Indians & Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. To compare origin of players in each team. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #players - indian vs. non-indians
  mi <- mi.bat[, list(mumbai_indians=hs)][order(-mumbai_indians)][1:5]
  csk <- csk.bat[, list(chennai_super_kings=hs)][order(-chennai_super_kings)][1:5]
  series.data <- cbind(mi, csk)
  series.data$high_scores <- c("first", "second", "third", "fourth", "fifth")
  series["title"] <- paste("IPLT20 - Comparing Top 5 Scores for Mumbai Indians & Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. Compare top scores from each team. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #players - young and old
  mi <- mi.bat[age_in_months > 200][, list(team="Mumbai Indians", youngest_player=round(min(age_in_months)/12,1), 
                                           oldest_player=round(max(age_in_months)/12,1))]
  csk <- csk.bat[age_in_months > 200][, list(team="Chennai Super Kings", youngest_player=round(min(age_in_months)/12,1), 
                                             oldest_player=round(max(age_in_months)/12,1))]
  series.data <- rbind(mi, csk)
  series["title"] <- paste("IPLT20 - Comparing Ages of Youngest & Oldest Batsmen for Mumbai Indians & Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #players - age distri
  mi <- mi.bat[age_in_months > 200][,list(age_group=cut(age_in_months/12, breaks=5*(0:9), right=F, dig.lab=6), age_in_months)][order(age_in_months)]
  mi <- mi[,list(mumbai_indians=length(age_in_months)), by=list(age_group)]
  mi$age_group <- sub(",", "-", sub("^[\\[]", "", sub("[\\)]$", "", as.character(mi$age_group))))
  setkey(mi, age_group)
  
  csk <- csk.bat[age_in_months > 200][,list(age_group=cut(age_in_months/12, breaks=5*(0:9), right=F, dig.lab=6), age_in_months)][order(age_in_months)]
  csk <- csk[,list(chennai_super_kings=length(age_in_months)), by=list(age_group)]
  csk$age_group <- sub(",", "-", sub("^[\\[]", "", sub("[\\)]$", "", as.character(csk$age_group))))
  setkey(csk, age_group)
  
  series.data <- merge(mi, csk, all=T)
  series["title"] <- paste("IPLT20 - Age Distribution of Batsmen for Mumbai Indians vs. Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  # players, fifties and hundreds
  mi <- mi.bowl[,list(team="Mumbai Indians", wickets=sum(w), fours=sum(fours), 
                      sixes=sum(sixes))]
  csk <- csk.bowl[,list(team="Chennai Super Kings", wickets=sum(w), fours=sum(fours), 
                        sixes=sum(sixes))]
  series.data <- rbind(mi, csk)
  series["title"] <- paste("IPLT20 - Comparing Bowling Performance of Two Finalists - Mumbai Indians vs. Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. Wickets taken & fours and sixes conceded by each team. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #players - young and oldest bowlers
  mi <- mi.bowl[age_in_months > 200][, list(team="Mumbai Indians", youngest_player=round(min(age_in_months)/12,1), 
                                            oldest_player=round(max(age_in_months)/12,1))]
  csk <- csk.bowl[age_in_months > 200][, list(team="Chennai Super Kings", youngest_player=round(min(age_in_months)/12,1), 
                                              oldest_player=round(max(age_in_months)/12,1))]
  series.data <- rbind(mi, csk)
  series["title"] <- paste("IPLT20 - Comparing Ages of Youngest & Oldest Bowlers for Mumbai Indians & Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
  #players - age distri
  mi <- mi.bowl[age_in_months > 200][,list(age_group=cut(age_in_months/12, breaks=5*(0:9), right=F, dig.lab=6), age_in_months)][order(age_in_months)]
  mi <- mi[,list(mumbai_indians=length(age_in_months)), by=list(age_group)]
  mi$age_group <- sub(",", "-", sub("^[\\[]", "", sub("[\\)]$", "", as.character(mi$age_group))))
  setkey(mi, age_group)
  
  csk <- csk.bowl[age_in_months > 200][,list(age_group=cut(age_in_months/12, breaks=5*(0:9), right=F, dig.lab=6), age_in_months)][order(age_in_months)]
  csk <- csk[,list(chennai_super_kings=length(age_in_months)), by=list(age_group)]
  csk$age_group <- sub(",", "-", sub("^[\\[]", "", sub("[\\)]$", "", as.character(csk$age_group))))
  setkey(csk, age_group)
  
  series.data <- merge(mi, csk, all=T)
  series["title"] <- paste("IPLT20 - Age Distribution of Bowlers for Mumbai Indians vs. Chennai Super Kings", sep="")
  series["desc"] <- paste(series["title"],  " For Season 6. (Data as of", Sys.Date(), ")", sep=" ")
  padify(series, series.data)
  
}


#
# IPL Finale 2 - PieCharts
#
iplFinale2 <- function(){
  # batting and bowling stats
  setkeyv(batting.stats, c("team","year"))
  setkeyv(bowling.stats, c("team","year"))         
  
  mi.bat <- batting.stats[year==2013][team=="Mumbai Indians"]  
  mi.bowl <- bowling.stats[year==2013][team=="Mumbai Indians"]
  
  csk.bat <- batting.stats[year==2013][team=="Chennai Super Kings"]  
  csk.bowl <- bowling.stats[year==2013][team=="Chennai Super Kings"]
  
  # breakup of runs they have scored
  series.data <- rbind(mi.bat[, list(runs=r), by=player][runs >= 50][order(-runs)], mi.bat[r < 50][, list(player="Others", runs=sum(r))])
  series["title"] <- paste("IPLT20 - Breakup of Runs Scored by Mumbai Indians Players in Season 6", sep="")
  series["desc"] <- paste(series["title"],  " (Data as of 2013-05-26)", sep="")
  padify(series, series.data)
  
  # breakup of wickets MI claimed
  series.data <- rbind(mi.bowl[, list(wickets=w), by=player][wickets >= 3][order(-wickets)], mi.bowl[w < 3][, list(player="Others", wickets=sum(w))])
  series["title"] <- paste("IPLT20 - Breakup of Wickets Claimed by Mumbai Indians Bowlers in Season 6", sep="")
  series["desc"] <- paste(series["title"],  " (Data as of 2013-05-26)", sep="")
  padify(series, series.data)
  
  # breakup of runs CSK have scored
  series.data <- rbind(csk.bat[, list(runs=r), by=player][runs >= 50][order(-runs)], csk.bat[r < 50][, list(player="Others", runs=sum(r))])
  series["title"] <- paste("IPLT20 - Breakup of Runs Scored by Chennai Super Kings Players in Season 6", sep="")
  series["desc"] <- paste(series["title"],  " (Data as of 2013-05-26)", sep="")
  padify(series, series.data)
  
  # breakup of wickets CSK claimed
  series.data <- rbind(csk.bowl[, list(wickets=w), by=player][wickets >= 3][order(-wickets)], csk.bowl[w < 3][, list(player="Others", wickets=sum(w))])
  series["title"] <- paste("IPLT20 - Breakup of Wickets Claimed by Chennai Super Kings Bowlers in Season 6", sep="")
  series["desc"] <- paste(series["title"],  " (Data as of 2013-05-26)", sep="")
  padify(series, series.data)
}


runIPL <- function(){ 
  # Start the clock!
  ptm <- proc.time()
  
  # initialize
  startup()
  
#   generateBattingPADS()
#   generateBattingPADSByPlayer()
#   generateBowlingPADS()  
#   generateBowlingPADSbyPlayer()
#   generateStandingPADS()
#  finaleIPL2013()
  
  # update pad count
  updateCatPadCount()
  
  #cleanup
  cleanup()
  
  # Stop the clock
  proc.time() - ptm
}

#delete few things - be careful - this will remove all pads from mongodb and remove the cache entirely
deleteFewThings <- function() {
  cleanCacheFiles()
  emptySystemPadsForCat(21)
  emptyCollection(mongo.db$system.pads)
  updateCatPadCount()
}

#run this
#runIPL()