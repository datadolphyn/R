# Generate PADS - Recessions, Social Networks, Olympic Medals, Failed Banks
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

# PADS Related to Recessions, Social Networks, Olympic Medals, Failed Banks
#
source("CreatePADS.R")

#
# load recession data
#
loadUSRecessionData <- function() {
  recessions.data = read.table(textConnection(
    "peak, trough
  1857-06-01, 1858-12-01
    1860-10-01, 1861-06-01
    1865-04-01, 1867-12-01
    1869-06-01, 1870-12-01
    1873-10-01, 1879-03-01
    1882-03-01, 1885-05-01
    1887-03-01, 1888-04-01
    1890-07-01, 1891-05-01
    1893-01-01, 1894-06-01
    1895-12-01, 1897-06-01
    1899-06-01, 1900-12-01
    1902-09-01, 1904-08-01
    1907-05-01, 1908-06-01
    1910-01-01, 1912-01-01
    1913-01-01, 1914-12-01
    1918-08-01, 1919-03-01
    1920-01-01, 1921-07-01
    1923-05-01, 1924-07-01
    1926-10-01, 1927-11-01
    1929-08-01, 1933-03-01
    1937-05-01, 1938-06-01
    1945-02-01, 1945-10-01
    1948-11-01, 1949-10-01
    1953-07-01, 1954-05-01
    1957-08-01, 1958-04-01
    1960-04-01, 1961-02-01
    1969-12-01, 1970-11-01
    1973-11-01, 1975-03-01
    1980-01-01, 1980-07-01
    1981-07-01, 1982-11-01
    1990-07-01, 1991-03-01
    2001-03-01, 2001-11-01
    2007-12-01, 2009-06-01"), 
                               sep=',',
                               colClasses=c('Date', 'Date'), header=TRUE)
  recessions.data$length <- recessions.data$trough - recessions.data$peak
  assign("recessions.data", recessions.data, envir=.GlobalEnv)
}

#
# generate
#
generate <- function() {
  #initialize system
  initializeSystem()
    
  #prepare pad meta data
  series <- list()
  series["source"] <- "LMC Automotive, CNN Money"
  series["category"] <- "Transportation"
  series["subcategory"] <- "Automotives"
  series["category_id"]<- 23
  series["subcategory_id"]<- 211
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "Tesla, Mercedes-Benz, BMW, Audi, Cars, Luxury", sep=","))
  series["desc"] <- "In the first quarter of 2013, more people bought a Tesla Model S than bought any of the similarly priced gasoline-powered cars from the top three German luxury brands, according to data from LMC Automotive. About 4,750 buyers bought a Model S while just over 3,000 people bought Mercedes top-level sedan."
  series["title"] <- "Tesla sales beating Mercedes, BMW and Audi, 1Q-2013"
  
  series.data <- data.frame(car_brand=c("Tesla", "Mercedes-Benz", "BMW", "Audi"), 
                               model=c("Model-S", "S Class", "7 Series", "A8"), 
                               sales=c(4750, 3077,2338,1462), stringsAsFactors=F)
  padify(series, series.data[1:2])    
  padify(series, series.data[1:2])    
  padify(series, series.data)    
  
  #clean up
  cleaupSystem()
  updateCatPadCount()
}


generateSN <- function() {
  #initialize system
  initializeSystem()
  assign("dataset", "Social-Network", envir=.GlobalEnv)
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "Browser Media, Socialnomics, MacWorld"
  series["category"] <- "Social"
  series["subcategory"] <- "People"
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "Facebook, Twitter, YouTube, Social Networks", sep=","))
  series["desc"] <- "Online social networks have emerged has the new way in which people connect socially. The leader currently being Facebook with over 1.2 billion members. Web-based social networking services make it possible to connect people who share interests and activities across political, economic, and geographic borders."
  series["title"] <- tocamel("Top ten most engaged countries for social networking")
  
  series.data <- data.frame(read.csv("./pads/raw-data/sn-by-country.csv", stringsAsFactors=F))
  padify(series, series.data)    
  
  #clean up
  cleaupSystem()
  updateCatPadCount()
}

generateIn <- function() {
  #initialize system
  initializeSystem()
  assign("verbose", T, envir=.GlobalEnv)
  
  assign("dataset", "India", envir=.GlobalEnv)
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "US Census Data, 2010"
  series["category"] <- "Social"
  series["subcategory"] <- "People"
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "India, Indian, Indian American, Population", sep=","))
  series["desc"] <- "Cities with large Indian American populations with a critical mass of at least 1% of the total urban population and at least 10% of the total suburban population. Information is based on the 2010 US Census. This article provides an incomplete list of places with large Indo-American populations."
  series["title"] <- tocamel("U.S. cities with large Indian-American populations")
  
  series.data <- data.frame(read.csv("./pads/raw-data/indian_population_in_major_cities.csv", stringsAsFactors=F))
  series.data <- trimData(series.data)
  series.data[3] <- as.numeric(gsub("%", "", series.data$percent_of_population, fixed=T))  
  padify(series, series.data)    
  
  #clean up
  cleaupSystem()
  updateCatPadCount()
}


generateOlympics <- function() {
  #initialize system
  initializeSystem()
  assign("verbose", T, envir=.GlobalEnv)
  
  assign("dataset", "Olympics", envir=.GlobalEnv)
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "Wikipedia Summer Olympics Medal Tables"
  series["category"] <- "Sports"
  series["subcategory"] <- "Olympics"
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "summer olpympics, medals, gold medals, silver medals, bronze medals", sep=","))
  series["desc"] <- "Summer Olympics Medal Tables, 1896-2012"
  
  # no of nations participating in olympics
  sum.oly <- data.table(read.csv("./pads/raw-data/olympics/summer-olympics-medals-1896-2012.csv", stringsAsFactors=F))
  sum.oly <- replaceNAWithZeros(sum.oly)
  setkeyv(sum.oly, c("year", "country", "country_code"))
  series["title"] <- "Number of Countries Participated in Summer Olympics"
  series.data <- sum.oly[, list(countries_count=nrow(.SD)), by=year]
  padify(series, series.data)    
  
  # no of olympics played by countries
  series["title"] <- "Participation in Summer Olympics by Country"
  series.data <- sum.oly[, list(times_participated_in_olympics=nrow(.SD)), by=country][order(-times_participated_in_olympics)][1:25]
  padify(series, series.data)
  
  #clean up
  cleaupSystem()
  updateCatPadCount()
}

generateFailedBanks <- function() {
  #recession data for bands
  loadUSRecessionData()
  
  #initialize system
  initializeSystem(0)
  #assign("verbose", T, envir=.GlobalEnv)
  assign("dataset", "FB", envir=.GlobalEnv)
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "FDIC"
  series["category"] <- "Financial Sector"
  series["subcategory"] <- "Banks"
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "Recession, US Banks, Failed Banks, Failed Institutions, Bank Failures", sep=","))
  series["desc"] <- "A bank failure is the closing of a bank by a federal or state banking regulatory agency. The FDIC is named as Receiver for a bank assets when its capital levels are too low, or it cannot meet obligations the next day. After a bank assets are placed into Receivership, the FDIC acts in two capacities—first, it pays insurance to the depositors, up to the deposit insurance limit, for assets not sold to another bank. Second, as the receiver of the failed bank, it assumes the task of selling and collecting the assets of the failed bank and settling its debts, including claims for deposits in excess of the insured limit."
  series["pagetag"] <- "failedbanks"
  #series["title"] <- tocamel("List of bank failures in the United States")
  
  #
  # read the bank names file
  #
  stats <- read.csv("./pads/raw-data/failed-banks/banklist_fdic.csv", stringsAsFactors=F)
  stats <- trimData(stats)
  colnames(stats) <- replaceMetaChars(tolower(colnames(stats)))
  
  # get col indexes that need to be converted to date 
  cols <- c(grep("closing_date", colnames(stats)), grep("updated_date", colnames(stats)))
  stats[cols] <- llply(stats[cols], as.Date, "%d-%b-%y") 
  
  # generate years, days, quarters and months
  stats$closing_year <- year(stats$closing_date)
  stats <- stats[complete.cases(stats),]
  stats$closing_day=as.character(factor(weekdays(stats$closing_date), levels=lDays))
  stats$closing_month=as.character(factor(months(stats$closing_date), levels=lMonths))
  stats$closing_quarter=as.character(quarters(stats$closing_date))
  
  #series.data[3] <- as.numeric(gsub("%", "", series.data$percent_of_population, fixed=T))  
  f.b <- data.table(stats)
  setkey(f.b, bank_name)
  
  # Number of failed banks by year
  s.d <- f.b[, list(number_of_failed_banks=nrow(.SD)), by=closing_year][order(closing_year)]
  series["title"] <- "Bank Failures in the United States Since 2000"
  padify(series, s.d)
  
  # Number of failed banks by state
  s.d <- f.b[, list(number_of_failed_banks=nrow(.SD)), by=st]
  setkey(s.d, st)
  setnames(states, colnames(states), c("state", "st"))
  s.d <- merge(s.d, states, by="st", all.x=T)[,st:=NULL]
  s.d$state <- tocamel(tolower(s.d$state))
  series["title"] <- "Bank Failures Across States in the United States Since 2000"
  padify(series, s.d[order(-number_of_failed_banks)])
  
  #by city
  s.d <- f.b[, list(number_of_failed_banks=nrow(.SD)), by=city]
  series["title"] <- "Bank Failures Across Cities in the United States Since 2000"
  padify(series, s.d[order(-number_of_failed_banks)][1:25,])
  
  #by day
  s.d <- f.b[, list(number_of_failed_banks=nrow(.SD)), by=closing_day]
  series["title"] <- "Bank Failures Across Weekdays in the United States Since 2000"
  s.d$closing_day <- factor(s.d$closing_day, levels= lDays)
  s.d <- s.d[order(closing_day)]
  s.d$closing_day <- as.character(s.d$closing_day)
  padify(series, s.d)
  
  #by month
  s.d <- f.b[, list(number_of_failed_banks=nrow(.SD)), by=closing_month]
  series["title"] <- "Bank Failures Across Months in the United States Since 2000"
  s.d$closing_month <- factor(s.d$closing_month, levels= lMonths)
  s.d <- s.d[order(closing_month)]
  s.d$closing_month <- as.character(s.d$closing_month)
  padify(series, s.d[order(-number_of_failed_banks)])
  
  #by quarter
  s.d <- f.b[, list(number_of_failed_banks=nrow(.SD)), by=closing_quarter]
  series["title"] <- "Bank Failures Across Quarters in the United States Since 2000"
  padify(series, s.d)
  
  #by acquiring_institution
  s.d <- f.b[, list(number_of_banks_acquired=nrow(.SD)), by=acquiring_institution][order(-number_of_banks_acquired)]
  series["title"] <- "Acquiring Institution for Failed Banks Since 2000"
  padify(series, s.d[order(-number_of_banks_acquired)][1:25,])
  
  #
  # Read the second file
  #
  #   stats <- read.csv("./pads/raw-data/failed-banks/failed_banks_assets_fdic.csv", stringsAsFactors=F)
  #   stats <- trimData(stats)
  #   stats$closing_date <- as.Date(stats$closing_date, "%Y-%m-%d") 
  #   # generate years, days, quarters and months
  #   stats$closing_year <- year(stats$closing_date)
  #   stats$assets_in_mil_usd <- as.numeric(stats$assets_in_mil_usd)
  #   stats <- stats[complete.cases(stats),]
  #   stats$closing_day=as.character(factor(weekdays(stats$closing_date), levels=lDays))
  #   stats$closing_month=as.character(factor(months(stats$closing_date), levels=lMonths))
  #   stats$closing_quarter=as.character(quarters(stats$closing_date))
  #   f.b.assets <- data.table(stats)
  #   setkey(f.b.assets, bank_name)
  
  # Assets of failed banks by year
  #s.d <- f.b.assets[, list(assets_in_usd = sum(assets_in_mil_usd)*1000000), by=closing_year][order(closing_year)]
  #series["title"] <- "Aggregated Assets (in USD) of Failed Banks in the US Since 2008"
  #padify(series, s.d)
  
  # Largest failed banks
  #s.d <- f.b.assets[f.b$bank_name, list(assets_in_usd = assets_in_mil_usd*1000000)][order(-assets_in_usd)]
  #series["title"] <- "Twenty Largest Bank (by Assets in USD) To Fail in the US Since 2000"
  #padify(series, s.d[1:20,])
  
  
  #
  # Read the third file
  #
  stats <- read.csv("./pads/raw-data/failed-banks/failed_bank_lists_1934_2013_fdic.csv", stringsAsFactors=F)
  stats <- trimData(stats)
  colnames(stats) <- replaceMetaChars(tolower(colnames(stats)))
  #stats$closing_date <- as.Date(stats$effective_date, "%m/%d/%y") 
  stats$closing_date <- getOlderDate(mdy(stats$effective_date))
  
  stats$city <- str_extract(stats$location, "^[A-Za-z ]+")
  stats$st <- str_extract(stats$location, "[A-Za-z ]+$")
  
  # generate years
  stats$closing_year <- year(stats$closing_date)
  stats$assets_in_mil_usd <- removeMetaFromNumeric(stats$total_assets)
  stats$deposits_in_mil_usd <- removeMetaFromNumeric(stats$total_deposits)
  stats$estimated_loss_in_mil_usd <- removeMetaFromNumeric(stats$estimated_loss)
  
  # generate table
  f.b.all <- data.table(stats)
  setkey(f.b.all, institution_name)
  
  #
  # Assisted banks
  #
  s.d <- f.b.all[, list(institution_name, failure_assistance, assets_in_usd = assets_in_mil_usd*1000, 
                        deposits_in_usd = deposits_in_mil_usd*1000)][order(-assets_in_usd)]
  series["title"] <- "Assets and Deposits of 10 Large Banks Who Received Assistance from FDIC"
  padify(series, s.d[failure_assistance=="ASSISTANCE"][,failure_assistance:=NULL][1:10,])
  
  #
  # Failed vs Assisted
  #
  s.d <- f.b.all[, list(no_of_banks=nrow(.SD)), by=failure_assistance]
  series["title"] <- "Ratio of Failed and Assisted Banks in the US Since 1934"
  padify(series, s.d)
  
  # Largest failed banks
  s.d <- f.b.all[closing_year >=2000][failure_assistance=="FAILURE"][, list(institution_name, assets_in_usd = assets_in_mil_usd*1000, 
                                                                            deposits_in_usd = deposits_in_mil_usd*1000)][order(-assets_in_usd)]
  series["title"] <- "Ten Largest Bank Failures (by Assets in USD) in the US Since 2000"
  padify(series, s.d[,c(1,2), with=F][1:10,])
  
  series["title"] <- "Assets and Deposits of Ten Largest Failed Banks in the US Since 2000"
  padify(series, s.d[1:10,])
  
  #
  # Failed banks
  #
  s.d <- f.b.all[failure_assistance=="FAILURE"][, list(number_of_failed_banks=nrow(.SD), assets_in_usd = sum(assets_in_mil_usd)*1000, 
                                                       deposits_in_usd = sum(deposits_in_mil_usd)*1000), by=closing_year][order(closing_year)]
  
  
  series["title"] <- "Number of Failed Banks in the US Between 1930 and 1980"
  padify(series, s.d[closing_year < 1980][, c(1,2), with=F])
  
  series["title"] <- "Number of Failed Banks in the US Between 1980 and 2013"
  padify(series, s.d[closing_year >= 1980][, c(1,2), with=F])
  
  s.d.second <- s.d[closing_year >= 1980][, c(1,2), with=F]
  series["title"] <- "Number of Failed Banks in the US Between 1980 and 2013 With Recession Bands"
  r.d <- subset(recessions.data, year(peak) >= min(s.d.second$closing_year ))
  x.plot.band <- as.matrix(r.d)
  dimnames(x.plot.band) <- NULL
  padify(series, s.d.second, x.plot.band)
  
  series["title"] <- "Assets and Deposits (in USD) of Failed Banks in the US Between 1930 and 1980"
  padify(series, s.d[closing_year < 1980][, c(1,3,4), with=F])
  
  series["title"] <- "Assets and Deposits (in USD) of Failed Banks in the US Between 1980 and 2013"
  padify(series, s.d[closing_year >= 1980][, c(1,3,4), with=F])
  
  s.d.second <- s.d[closing_year >= 1980][, c(1,3,4), with=F]
  series["title"] <- "Assets and Deposits (in USD) of Failed Banks in the US Between 1980 and 2013 With Recession Bands"
  r.d <- subset(recessions.data, year(peak) >= min(s.d.second$closing_year ))
  x.plot.band <- as.matrix(r.d)
  dimnames(x.plot.band) <- NULL
  padify(series, s.d.second, x.plot.band)
  
  #
  # loss to FDIC from failed banks
  #
  s.d <- f.b.all[failure_assistance=="FAILURE"][, list(estimated_losses_to_FDIC = sum(estimated_loss_in_mil_usd)*1000), 
                                                by=closing_year][order(closing_year)]  
  #remove NA
  s.d <- s.d[complete.cases(s.d),]
  series["title"] <- "Estimated Losses to FDIC from Failed Banks in the US Since 1989 With Recession Bands"
  # apply recession bands
  r.d <- subset(recessions.data, year(peak) >= min(s.d$closing_year ))
  x.plot.band <- as.matrix(r.d)
  dimnames(x.plot.band) <- NULL
  padify(series, s.d, x.plot.band)
  
  ##
  # add key stats
  #
  addPageStat(series$pagetag, prettyNum(nrow(f.b.all[failure_assistance=="FAILURE"]),big.mark = ","), "Banks Failed Since 1934")
  addPageStat(series$pagetag, paste("$", prettyNum(f.b.all[failure_assistance=="FAILURE"][, sum(estimated_loss_in_mil_usd, na.rm=T)*1000],big.mark = ","), sep=""), 
                                    "Esitimated Losses to FDIC")
  addPageStat(series$pagetag, prettyNum(f.b.all[failure_assistance=="FAILURE"][, list(n=nrow(.SD)), by=closing_year][which.max(n)]$n, big.mark = ","), 
              paste("Banks Closed in", f.b.all[failure_assistance=="FAILURE"][, list(n=nrow(.SD)), by=closing_year][which.max(n)]$closing_year, sep=" "))  
  addPageStat(series$pagetag, prettyNum(f.b.all[failure_assistance=="FAILURE"][, list(n=nrow(.SD)), by=st][which.max(n)]$n, big.mark = ","), 
              paste("Banks Closed in", f.b.all[failure_assistance=="FAILURE"][, list(n=nrow(.SD)), by=st][which.max(n)]$st, "State", sep=" "))  
  addPageStat(series$pagetag, f.b.all[failure_assistance=="FAILURE"][which.max(assets_in_mil_usd)]$institution_name, 
              "Largest Bank to Fail")  
  addPageStat(series$pagetag, f.b.all[which.max(assets_in_mil_usd)]$institution_name, 
              "Largest Bank to Receive FDIC Assistance")
  #clean up
  cleaupSystem()
  updateCatPadCount()
}


#delete few things - be careful - this will remove all pads from mongodb and remove the cache entirely
deleteFewThings <- function() {
  initializeSystem(0)
  cleanCacheFiles()
  deletePageStat("failedbanks")
  emptySystemPadsForCat(getOrInsertCategory("Financial Sector"))
  emptyCollection(mongo.db$system.pads)
  updateCatPadCount()
}
#generateIn()

