# Generate Crunchbase PADS
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#creating source file
source("CreatePADS.R")
library("reshape2")

#
# loadCompaniesStats
#
loadCompaniesStats <- function(){
  stats <- readFile(paste(folder.path, comps.file, sep=""))
  if(!is.na(stats)){
    
    # get col indexes that need to be converted to date 
    cols <- c(grep("founded_at", colnames(stats)), grep("first_funding_at", colnames(stats)), 
              grep("last_funding_at", colnames(stats)),
              grep("last_milestone_at", colnames(stats)))
    #stats[cols] <- as.Date(as.character(as.matrix(stats[cols])), format="%m/%d/%y")
    stats[cols] <- llply(stats[cols], as.Date, "%m/%d/%y") 
    #stats[cols] <- llply(stats[cols], as.Date, "%Y-%m-%d") 
    
    # get col indexes that need to be converted to numeric
    #cols <- c(grep("founded_year", colnames(stats)), grep("funded_year", colnames(stats)),
    #          grep("last_milestone_year", colnames(stats)))
    #stats[cols] <- as.numeric(as.matrix(stats[cols]))
    
    # generate years
    stats$founded_year <- year(stats$founded_at)
    stats$first_funding_year <- year(stats$first_funding_at)
    stats$last_funding_year <- year(stats$last_funding_at)
    stats$last_milestone_year <- year(stats$last_milestone_at)
    
    # numeric after removing commas
    stats$funding_total <- as.numeric(gsub(",", "", stats$funding_total, fixed=T))  
    stats$funding_rounds <- as.numeric(gsub(",", "", stats$funding_rounds, fixed=T))
    
    #remove rows that do not have funding info
    stats <- stats[!is.na(stats$funding_total),]
    
    #remove unwanted columns
    cols <- c(grep("permalink", colnames(stats)), grep("country_code", colnames(stats)))
    stats <- stats[-cols]
    
    # now get the data.table
    companies.stats <- data.table(stats)
    setkeyv(companies.stats, c("category_code", "status", "city", "region", "founded_year", "last_funding_year"))
    assign("companies.stats", companies.stats, envir=.GlobalEnv)
  }
}

#
# load companies rounds
#
loadCompanyRounds <- function(){  
  stats <- readFile(paste(folder.path, comps.rounds.file, sep=""))
  if(!is.na(stats)){
    # get col indexes that need to be converted to date 
    cols <- c(grep("funded_at", colnames(stats)))
    stats[cols] <- llply(stats[cols], as.Date, "%m/%d/%y") 
    
    # generate years
    stats$funded_year <- year(stats$funded_at)
    stats <- stats[complete.cases(stats),]
    
    stats$funded_day=as.character(factor(weekdays(stats$funded_at), levels=lDays))
    stats$funded_month=as.character(factor(months(stats$funded_at), levels=lMonths))
    stats$funded_quarter=as.character(quarters(stats$funded_at))
    
    # numeric after removing commas
    stats$funding_amount <- as.numeric(gsub(",", "", stats$total_usd, fixed=T))  
    
    #remove rows that do not have funding info
    stats <- stats[!is.na(stats$funding_amount),]
    
    #remove unwanted columns
    cols <- c(grep("permalink", colnames(stats)), grep("country_code", colnames(stats)), 
              grep("entity_type", colnames(stats)), grep("status", colnames(stats)), 
              grep("total_usd", colnames(stats)))
    stats <- stats[-cols]
    
    # now get the data.table
    comp.rounds.stats <- data.table(stats)
    setkeyv(comp.rounds.stats, c("name", "category_code", "type", "city", "region", "funded_year", "funded_at"))
    assign("comp.rounds.stats", comp.rounds.stats, envir=.GlobalEnv)
  }
}

#
# load companies rounds
#
loadInvestosStats <- function(){
  stats <- readFile(paste(folder.path, investors.file, sep=""))
  colnames(stats) <- tolower(colnames(stats))
  if(!is.na(stats)){
    # get col indexes that need to be converted to date 
    cols <- c(grep("funded_at", colnames(stats)))
    stats[cols] <- llply(stats[cols], as.Date, "%m/%d/%y") 
    
    # generate years
    stats$funded_year <- as.factor(year(stats$funded_at))
    
    # numeric after removing commas
    stats$funding_amount <- as.numeric(gsub(",", "", stats$received.usd, fixed=T))  
    
    #remove rows that do not have funding info
    stats <- stats[!is.na(stats$funding_amount),]
    
    #remove unwanted columns
    cols <- c(grep("permalink", colnames(stats)),  grep("investor_status", colnames(stats)),
              grep("company_country", colnames(stats)),                
              grep("received.usd", colnames(stats)))
    stats <- stats[-cols]
    
    # now get the data.table
    investor.stats <- data.table(stats)
    setkeyv(investor.stats, c("investor", "type", "funded_year", "funded_at"))
    assign("investor.stats", investor.stats, envir=.GlobalEnv)
  }
}

#
# initialize
#
startup <- function() {
  #initialize system
  initializeSystem(0)
  
  assign("folder.path", "./pads/raw-data/crunchbase/", envir=.GlobalEnv)
  assign("comps.file", "cb_companies_june_2013.csv", envir=.GlobalEnv)
  assign("comps.rounds.file", "cb_companies_rounds.csv", envir=.GlobalEnv)
  assign("investors.file", "cb_investors.csv", envir=.GlobalEnv)
  
  assign("dataset", "crunchbase", envir=.GlobalEnv)
  #assign("verbose", TRUE, envir=.GlobalEnv)

  loadCompaniesStats()
  loadCompanyRounds()
  loadInvestosStats()
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "CrunchBase"
  series["category"] <- "Financial Sector"
  series["subcategory"] <- "Investment"
  series["tags"] <- tolower(paste(series$source, "VC, venture capital, startups, US startups, investments, angel, series-a, series-b, series-c, funding, seed, biotech, ecommerce, enterprise, software, mobile, web", sep=","))
  series["pagetag"] <- "crunchbase"
  series["desc"] <- "Built using data from CrunchBase extracted on June 6, 2013." 
  assign("series", series, envir=.GlobalEnv)
}

#
# cleanup - command-option-0
#
cleanup <- function(){
  cleaupSystem()
}

generateStartupPADS <- function(){    
  companies.stats <- companies.stats[first_funding_year <= 2013]
  min.year <- min(companies.stats$first_funding_year, na.rm=T)
  max.year <- max(companies.stats$last_funding_year, na.rm=T)
  time.period <- paste(min.year, max.year, sep="-")
  
  #fundings by year
  series.data <- companies.stats[, list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_total, na.rm=T)), by=(funding_year=first_funding_year)][order(funding_year)]
  series["title"] <- paste("Number of Startups Funded Over", time.period, sep=" ")
  padify(series, series.data[,c(1,2),with=F])
  #2nd
  series["title"] <- paste("Funding (in USD) Received by Startups Over", time.period, sep=" ")
  padify(series, series.data[,c(1,3),with=F])
  
  #
  #fundings by state
  #
  series.data <- companies.stats[, list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_total)), by=list(state_code)]
  #remove empty
  series.data <- series.data[!series.data$state==""]  
  series.data <- merge(series.data, states, by="state_code", all.x=T)[,state_code:=NULL][order(-number_of_startups_funded)]
  series.data <- rbind(series.data[1:15,], series.data[16:nrow(series.data),
                                                       list(number_of_startups_funded=sum(number_of_startups_funded), funding_total=sum(funding_total), state_name="Others")])
  #first pad
  series["title"] <-  paste("Number of Startups Funded Across US States Over", time.period, sep=" ")
  padify(series, series.data[,c(1,3),with=F])
  #second pad
  series["title"] <-  paste("Funding (in USD) Received by Startups Across US States Over", time.period, sep=" ")
  padify(series, series.data[,c(2,3),with=F][order(-funding_total)])
  
  #third pad - percent of startups and funding by states
  series.data <- series.data[, list(state_name, 
                                    percent_of_startups=round((number_of_startups_funded/sum(number_of_startups_funded))*100,1), 
                                    percent_of_funding=round((funding_total/sum(funding_total))*100, 1))]
  series["title"] <-  paste("Distribution of Startups and Funding Across US States Over", time.period, sep=" ")
  padify(series, series.data)
  
  
  #
  #fundings by region
  #  
  series.data <- companies.stats[, list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_total)), by=list(region)][order(-number_of_startups_funded)]
  #remove empty
  series.data <- series.data[!series.data$region==""]  
  series.data <- rbind(series.data[1:15,], series.data[16:nrow(series.data),
                                                       list(region="Others", number_of_startups_funded=sum(number_of_startups_funded), funding_total=sum(funding_total))])
  #first pad
  series["title"] <-  paste("Number of Startups Funded Across Various US Regions Over", time.period, sep=" ")
  padify(series, series.data[,c(1,2),with=F])
  #second pad
  series["title"] <-  paste("Funding (in USD) Received by Startups Across Various US Regions Over", time.period, sep=" ")
  padify(series, series.data[,c(1,3),with=F][order(-funding_total)])
  
  #third pad - percent of startups and funding by states
  series.data <- series.data[, list(region, 
                                    percent_of_startups=round((number_of_startups_funded/sum(number_of_startups_funded))*100,1), 
                                    percent_of_funding=round((funding_total/sum(funding_total))*100, 1))]
  series["title"] <-  paste("Distribution of Startups and Funding Across Various US Regions Over", time.period, sep=" ")
  padify(series, series.data)
  
  
  #
  #fundings by category
  #  
  series.data <- companies.stats[category_code != "NULL"][, list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_total)), by=list(category=category_code)][order(-number_of_startups_funded)]
  #first pad
  series["title"] <-  paste("Number of Startups Funded Across Various Categories Over", time.period, sep=" ")
  padify(series, series.data[,c(1,2),with=F])
  #second pad
  series["title"] <-  paste("Funding (in USD) Received by Startups Across Various Categories Over", time.period, sep=" ")
  padify(series, series.data[,c(1,3),with=F][order(-funding_total)])
  
  #third pad - percent of startups and funding by categories
  series.data <- series.data[, list(category, 
                                    percent_of_startups=round((number_of_startups_funded/sum(number_of_startups_funded))*100,1), 
                                    percent_of_funding=round((funding_total/sum(funding_total))*100, 1))]
  series["title"] <-  paste("Distribution of Startups and Funding Across Various Categories Over", time.period, sep=" ")
  padify(series, series.data)
  
  #
  # Fundings by categories over the years
  #
  # Part A
  series.data <- companies.stats[category_code != "NULL"][,
                                                          list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_total)), 
                                                          by=list(category=category_code, funded_year=first_funding_year)]
  for(yr in unique(series.data$funded_year)){
    series.data.t = series.data[funded_year==yr]
    series["title"] <- paste("Number of Startups Funded Across Various Categories in", yr, sep=" ")
    padify(series, series.data.t[, c(1,3), with=F][order(-number_of_startups_funded)])
    
    series["title"] <-  paste("Funding (in USD) Received by Startups Across Various Categories in", yr, sep=" ")
    padify(series, series.data.t[, c(1,4), with=F][order(-funding_total)])
  }
  
  # Part B
  #Fundings in a categories over the time-period
  setkey(series.data, category)
  for(cat in unique(series.data$category)){
    series["title"] <- paste("Number of Startups Funded in", tocamel(cat), "Category Over", time.period, sep=" ")
    padify(series, series.data[category==cat][,c(2,3),with=F][order(-number_of_startups_funded)])
    
    series["title"] <- paste("Funding (in USD) Received by Startups in", tocamel(cat), "Category Over", time.period, sep=" ")
    padify(series, series.data[category==cat][,c(2,4),with=F][order(-funding_total)])
    
  }
  
  #all categories
  series.data.t <- dcast(series.data, funded_year~category, sum, value.var="total_no_of_fundings")
  series["title"] <- paste("Number of Startups Funded in All Categories Over", time.period, sep=" ")
  padify(series, series.data.t)
  
  #popular categories
  key.cat <- c("funded_year", "biotech", "ecommerce", "enterprise", "software", "mobile", "web")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Number of Startups Funded in Most Popular Categories Over", time.period, sep=" ")
  padify(series, series.data.t.s)
  
  #mobile and web categories
  key.cat <- c("funded_year", "biotech", "web")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Number of Startups Funded in BioTech and Web Categories Over", time.period, sep=" ")
  padify(series, series.data.t.s)
  
  #mobile and web categories
  key.cat <- c("funded_year", "web", "mobile")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Number of Startups Funded in Mobile and Web Categories Over", time.period, sep=" ")
  padify(series, series.data.t.s)
  
  #recast but with funding amount
  series.data.t <- dcast(series.data, funded_year~category, sum, value.var="funding_total")  
  
  #popular categories
  key.cat <- c("funded_year", "biotech", "ecommerce", "enterprise", "software", "mobile", "web")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Funding (in USD) Received by Startups in Most Popular Categories Over", time.period, sep=" ")
  padify(series, series.data.t.s)
  
  #mobile and web categories
  key.cat <- c("funded_year", "biotech", "web")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Funding (in USD) Received by Startups in BioTech and Web Categories Over", time.period, sep=" ")
  padify(series, series.data.t.s)
  
  #mobile and web categories
  key.cat <- c("funded_year", "web", "mobile")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Funding (in USD) Received by Startups in Mobile and Web Categories Over", time.period, sep=" ")
  padify(series, series.data.t.s)
  
  
  #
  # Fundings by categories across states
  #
  # Part A
  series.data <- companies.stats[category_code != "NULL"][state_code !=""][,
                                                                           list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_total)), 
                                                                           by=list(category=category_code, state=state_code)]
  for(st in unique(series.data$state)){
    series.data.t = series.data[state==st]
    series["title"] <- paste("Number of Startups Funded Across Various Categories in", st, "State, USA", sep=" ")
    padify(series, series.data.t[, c(1,3), with=F][order(-number_of_startups_funded)])
    
    series["title"] <-  paste("Funding (in USD) Received by Startups Across Various Categories in", st, "State, USA", sep=" ")
    padify(series, series.data.t[, c(1,4), with=F][order(-funding_total)])
  }  
  
  # Part B
  #Fundings in a categories across states
  for(cat in unique(series.data$category)){
    series["title"] <- paste("Number of Startups Funded in", tocamel(cat), "Category Across Top 20 States Over", time.period, sep=" ")
    padify(series, series.data[category==cat][,c(2,3),with=F][order(-number_of_startups_funded)][1:20,])
    
    series["title"] <- paste("Funding (in USD) Received by Startups in", tocamel(cat), "Category Across Top 20 States Over", time.period, sep=" ")
    padify(series, series.data[category==cat][,c(2,4),with=F][order(-funding_total)][1:20,])
    
  }
  
  #Fundings across categories in a state by year
  #series.data <- companies.stats[category_code != "NULL"][state_code !=""][,list(total_no_of_fundings=nrow(.SD)), by=list(category=category_code, state=state_code, funded_year=first_funding_year)]
  #for(st in unique(series.data$state)) {
  #  t.series.data <- series.data[state==st]
  #  for(cat in unique(t.series.data$category)) {
  #    trim.series.data = t.series.data[category==cat][,category:=NULL][order(-total_no_of_fundings)][1:25,]
  #    series["title"] <- paste("Number of Startups Funded in", st, "State in", tocamel(cat), "Category", sep=" ")
  #    padify(series, trim.series.data)
  # }
  #}
  
  # Most funded startups ever
  series.data <- companies.stats[,list(total_fundings_in_usd=funding_total), by=list(company=name)][order(-total_fundings_in_usd)][1:20,]
  series["title"] <- paste("Highest Funded Startups Over", time.period, sep=" ")
  padify(series, series.data)
  
  # Most funded startups 2009-2013
  series.data <- companies.stats[first_funding_year>2008][,list(total_fundings_in_usd=funding_total), by=list(company=name)][order(-total_fundings_in_usd)][1:20,]
  series["title"] <- paste("Highest Funded Startups Over 2009-2013", sep=" ")
  padify(series, series.data)
  
  
  #################
  #fundings by region in CA since 1999
  series.data <- companies.stats[state_code=="CA"][, list(number_of_startups_funded=nrow(.SD), 
                                                          funding_total=sum(funding_total, na.rm=T)), 
                                                   by=list(region=region)][order(-number_of_startups_funded)]
  series.data <- rbind(series.data[1:15,], series.data[16:nrow(series.data),
                                                       list(region="Others", number_of_startups_funded=sum(number_of_startups_funded),funding_total=sum(funding_total))])
  #first pad
  series["title"] <-  paste("Number of Startups Funded Across Various Regions in California Over", time.period, sep=" ")
  padify(series, series.data[,c(1,2),with=F])
  
  #second pad
  series["title"] <-  paste("Funding (in USD) Received by Startups Across Various Regions in California Over", time.period, sep=" ")
  padify(series, series.data[,c(1,3),with=F][order(-funding_total)])
  
  #third pad - percent of startups and funding by states
  series.data <- series.data[, list(region,
                                    percent_of_startups=round((number_of_startups_funded/sum(number_of_startups_funded))*100,1), 
                                    percent_of_funding=round((funding_total/sum(funding_total))*100, 1))]
  series["title"] <-  paste("Distribution of Startups and Funding Across Regions in California Over", time.period, sep=" ")
  padify(series, series.data)
  
  # Most funded startups by state
  series.data <- companies.stats[,list(company=name, total_fundings_in_usd=funding_total), by=list(state=state_code)]
  for(st in unique(series.data$state)) {
    t.series.data <- series.data[state==st][,state:=NULL][order(-total_fundings_in_usd)][1:20,]
    series["title"] <- paste("Highest Funded Startups in", st, "State Over", time.period, sep=" ")
    padify(series, t.series.data)
  }
  
  # Most funded startups by category
  series.data <- companies.stats[category_code!= "NULL"][,list(company=name, funding_total), by=list(category=category_code)]
  for(cat in unique(series.data$category)){
    t.series.data = series.data[category==cat][,category:=NULL][order(-funding_total)][1:20,]
    series["title"] <- paste("Highest Funded Startups in", cat, "Category Over", time.period, sep=" ")
    padify(series, t.series.data)
  }
  
  # Status of companies
  series.data <- companies.stats[category_code!= "NULL"][,list(number_of_startups=nrow(.SD)), by=list(status)]
  series["title"] <-  paste("Number of Startups by Status - IPO, Closed or Acquisition", time.period, sep=" ")
  padify(series, series.data)
  
  #recast but with funding amount
  series.data <- companies.stats[category_code!= "NULL"][,list(number_of_startups=nrow(.SD)), by=list(status, category=category_code)]
  series.data.t <- dcast(series.data, status~category, sum, value.var="number_of_startups")  
  n <- colnames(series.data.t)
  for(i in 2:ncol(series.data.t)){
    series["title"] <-  paste("Status of Startups - IPO, Closed or Acquisition - in ", tocamel(n[i]), "Category", time.period, sep=" ")
    padify(series, series.data.t[c(1,i)])
  }  
  
  #popular categories
  key.cat <- c("status", "biotech", "ecommerce", "enterprise", "software", "mobile", "web")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Status of Startups - IPO, Closed or Acquisition in Most Popular Categories", time.period, sep=" ")
  padify(series, series.data.t.s)
  
  #mobile and web categories
  key.cat <- c("status", "biotech", "web")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Status of Startups - IPO, Closed or Acquisition in BioTech and Web Categories", time.period, sep=" ")
  padify(series, series.data.t.s)
  
  #mobile and web categories
  key.cat <- c("status", "web", "mobile")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Status of Startups - IPO, Closed or Acquisition in Mobile and Web Categories", time.period, sep=" ")
  padify(series, series.data.t.s)
  
}

# angel funding across years for CA, NY and MA
# Where are angel funded companies?
# count of angel funded companies across states

generateRoundPADS <- function(){    
  comp.rounds.stats <- comp.rounds.stats[funded_year <= 2013]
  min.year <- min(comp.rounds.stats$funded_year, na.rm=T)
  max.year <- max(comp.rounds.stats$funded_year, na.rm=T)
  time.period <- paste(min.year, max.year, sep=" - ")
  
  #fundings by type
  series.data <- comp.rounds.stats[, list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_amount, na.rm=T)), by=(funding_type=type)][order(-number_of_startups_funded)]
  series["title"] <- paste("Total Number of Startups Funded Across Various Rounds Over", time.period, sep=" ")
  padify(series, series.data[,c(1,2),with=F])
  #2nd
  series["title"] <- paste("Total Funding (in USD) for Startups Across Various Rounds Over", time.period, sep=" ")
  padify(series, series.data[,c(1,3),with=F][order(-funding_total)])
  
  #series.data <- comp.rounds.stats[, list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_total, na.rm=T)), by=(funded_year=funded_year)][order(funded_year)]
  #
  # only work with angel-seriesc
  #
  stats <- comp.rounds.stats[type %in% c("series-c+", "series-b", "angel", "series-a")]
  series.data <- stats[, list(number_of_startups_funded=nrow(.SD), funding_total=sum(funding_amount, na.rm=T)), by=list(funded_year=funded_year, funding_type=type)]
  #reshape
  series.data.t <- dcast(series.data, funded_year~funding_type, sum, value.var="number_of_startups_funded")
  series["title"] <- paste("Trend for Startup Funding Across Various Rounds Over", time.period, sep=" ")
  padify(series, series.data.t)
  
  #angel fundings
  series["title"] <- paste("Trend for Startup Angel Funding Over", time.period, sep=" ")
  padify(series, series.data.t[,c(1,2)])
  
  #series-a fundings
  series["title"] <- paste("Trend for Startup Series-A Funding Over", time.period, sep=" ")
  padify(series, series.data.t[,c(1,3)])
  
  # Series-b fundings
  series["title"] <- paste("Trend for Startup Series-B Funding Over", time.period, sep=" ")
  padify(series, series.data.t[,c(1,4)])
  
  # Series-c fundings
  series["title"] <- paste("Trend for Startup Series-C Funding Over", time.period, sep=" ")
  padify(series, series.data.t[,c(1,5)])
  
  #reshape for a short time period
  series.data.t <- dcast(series.data[funded_year > 2004], funded_year~funding_type, sum, value.var="number_of_startups_funded")
  series["title"] <- paste("Trend for Startup Funding Across Various Rounds Since 2005", sep=" ")
  padify(series, series.data.t)
  
  # Fundings in USD
  series.data.t <- dcast(series.data, funded_year~funding_type, sum, value.var="funding_total")
  series["title"] <- paste("Startup Funding (in USD) Across Various Rounds Over", time.period, sep=" ")
  padify(series, series.data.t)
  
  #angel fundings
  series["title"] <- paste("Angel Funding (in USD) for Startups Over", time.period, sep=" ")
  padify(series, series.data.t[,c(1,2)])
  
  #series-a fundings
  series["title"] <- paste("Series-A Funding (in USD) for Startups Over", time.period, sep=" ")
  padify(series, series.data.t[,c(1,3)])
  
  # Series-b fundings
  series["title"] <- paste("Series-B Funding (in USD) for Startups Over", time.period, sep=" ")
  padify(series, series.data.t[,c(1,4)])
  
  # Series-c fundings
  series["title"] <- paste("Series-C Funding (in USD) for Startups Over", time.period, sep=" ")
  padify(series, series.data.t[,c(1,5)])
  
  #reshape for a short time period
  series.data.t <- dcast(series.data[funded_year > 2004], funded_year~funding_type, sum, value.var="funding_total")
  series["title"] <- paste("Startup Funding (in USD) Across Various Rounds Since 2005", sep=" ")
  padify(series, series.data.t)
}


otherPADS <- function(){
  #fundings by region in CA since 1999
  series["desc"] = paste("Data based on first funding date were used for analysis.", series["desc"], sep=" ")
  series.data <- companies.stats[first_funding_year > 2004][state_code=="CA"][, list(number_of_startups=nrow(.SD), 
                                                          funding_total=sum(funding_total, na.rm=T)), 
                                                   by=list(region=region, funding_year=first_funding_year)][order(funding_year)]
  series.data.t <- dcast(series.data, funding_year~region, sum, value.var="number_of_startups")  
  key.cat <- c("funding_year", "Los Angeles", "San Diego","SF Bay")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Trend of New Startups in Three California Regions 2005-2013", sep=" ")
  # Based on First Funding Date
  padify(series, series.data.t.s)
  
  series.data.t <- dcast(series.data, funding_year~region, sum, value.var="funding_total")  
  key.cat <- c("funding_year", "Los Angeles", "San Diego","SF Bay")
  series.data.t.s <- series.data.t[key.cat]
  series["title"] <- paste("Funding (in USD) in New Startups in Three California Regions 2005-2013", sep=" ")
  padify(series, series.data.t.s)
  
  
  #series.data <- companies.stats[last_funding_year > 2004][state_code=="CA"][, list(number_of_startups_funded=nrow(.SD), 
  #                                                                                   funding_total=sum(funding_total, na.rm=T)), 
  #                                                                            by=list(region=region, funding_year=last_funding_year)][order(funding_year)]
  
  
  series.data <- rbind(series.data[1:15,], series.data[16:nrow(series.data),
                                                       list(region="Others", number_of_startups_funded=sum(number_of_startups_funded),funding_total=sum(funding_total))])
  
}

#
# generate more rounds
#
generateMoreRoundPADS <- function(){
  types <- c("series-c+", "series-b", "angel", "series-a", "venture", "all")
  for(i in types){
    
    s <- comp.rounds.stats
    pre <- ""
    if(i != "all"){
      s <- comp.rounds.stats[type==i]
      pre <- tocamel(i)
    }
    
    #time period
    t.p <- getTimePeriod(s$funded_at)
    
    #by day
    s.d <- s[, list(number_of_startups_funded=nrow(.SD)), by=funded_day][order(-number_of_startups_funded)]  
    series["title"] <- paste("Distribution of Startups", pre, "Investments Over Weekdays", t.p, sep=" ")
    padify(series, s.d)
    
    s.d <- s[, list(number_of_startups_funded=nrow(.SD)), by=funded_month][order(-number_of_startups_funded)]  
    series["title"] <- paste("Distribution of Startups", pre, "Investments Over Months", t.p, sep=" ")
    padify(series, s.d)
    
    #by quarter
    s.d <- s[, list(number_of_startups_funded=nrow(.SD)), by=funded_quarter][order(-number_of_startups_funded)]  
    series["title"] <- paste("Distribution of Startups", pre, "Investments Over Quarters", t.p, sep=" ")
    padify(series, s.d)
  }
  
  types <- c("series-c+", "series-b", "angel", "series-a")    
  s <- comp.rounds.stats[type %in% types]
  
  #time period
  t.p <- getTimePeriod(s$funded_at)
  
  #by day
  s.d <- s[, list(number_of_startups_funded=nrow(.SD)), by=list(funded_day, type)]
  s.d.c <- data.table(dcast(s.d, funded_day ~ type, sum, value.var="number_of_startups_funded"))
  s.d.c$funded_day <- factor(s.d.c$funded_day, levels= lDays)
  s.d.c <- s.d.c[order(funded_day)]
  s.d.c$funded_day <- as.character(s.d.c$funded_day)
  series["title"] <- paste("Distribution of Startups Investments for Various Rounds Over Weekdays", t.p, sep=" ")
  padify(series, s.d.c)
  
  s.d <- s[, list(number_of_startups_funded=nrow(.SD)), by=list(funded_month, type)]
  s.d.c <- data.table(dcast(s.d, funded_month ~ type, sum, value.var="number_of_startups_funded"))
  s.d.c$funded_month <- factor(s.d.c$funded_month, levels= lMonths)
  s.d.c <- s.d.c[order(funded_month)]
  s.d.c$funded_month <- as.character(s.d.c$funded_month)
  series["title"] <- paste("Distribution of Startups Investments for Various Rounds Over Months", t.p, sep=" ")
  padify(series, s.d.c)
  
  s.d <- s[, list(number_of_startups_funded=nrow(.SD)), by=list(funded_quarter, type)]
  s.d.c <- data.table(dcast(s.d, funded_quarter ~ type, sum, value.var="number_of_startups_funded"))
  series["title"] <- paste("Distribution of Startups Investments for Various Rounds Over Quarters", t.p, sep=" ")
  padify(series, s.d.c)
  
  #
  #generate pads for companies
  #
  s.d <- comp.rounds.stats[!(name %in% c("#NAME?", "#waywire"))][, list(funded_at, funding_amount, 
                                                                        type, funded_day, funded_month, funded_quarter), by=name]  
  s.d <- rbind(s.d, list("GoodData", as.Date("06/12/2013", format="%m/%d/%Y"), "series-c+", 22000000, "Wednesday", "June", "Q2"))
  
  for(i in unique(s.d$name)){ 
    s.d.t <- s.d[name==i]
    company <- tocamel(removeMetaChars(i))
    print(i)
    
    if(nrow(s.d.t) == 1){
      series["title"] <- paste("Investment Round (in USD) for Startup", company, sep=" ")
      padify(series, s.d.t[, c(3,4), with=F])
    } else {
      
      series["title"] <- paste("Investment Rounds (in USD) for Startup", company, sep=" ")
      padify(series, s.d.t[, c(3,4), with=F])
      
      if(nrow(s.d.t) > 3){
        series["title"] <- paste("Investment Rounds (in USD) for Startup", company, "Over", getTimePeriod(s.d.t$funded_at), sep=" ")
        padify(series, s.d.t[, c(2,3), with=F][order(funded_at)])
        
        #by day
        s.d.t.1 <- s.d.t[, list(investment_rounds=nrow(.SD)), by=funded_day][order(-investment_rounds)]  
        series["title"] <- paste("Distribution of Investments Round for Startup", company, "Over Weekdays", sep=" ")
        padify(series, s.d.t.1)
        
        #by months
        s.d.t.1 <- s.d.t[, list(investment_rounds=nrow(.SD)), by=funded_month][order(-investment_rounds)]  
        series["title"] <- paste("Distribution of Investments Round for Startup", company, "Over Months", sep=" ")
        padify(series, s.d.t.1)
        
        #by quarter
        s.d.t.1 <- s.d.t[, list(investment_rounds=nrow(.SD)), by=funded_quarter][order(-investment_rounds)]  
        series["title"] <- paste("Distribution of Investments Round for Startup", company, "Over Quarter", sep=" ")
        padify(series, s.d.t.1)
      }
    }
  }
}

#
# add key stats
#
addKeyStats <- function(){
#   addPageStat("crunchbase", prettyNum(length(unique(investor.stats$investor)),big.mark = ","), "Number of Investors")
#   addPageStat("crunchbase", prettyNum(length(unique(companies.stats$name)),big.mark = ","),
#               "Number of Startups")
#   addPageStat("crunchbase", paste("$", 
#                                   prettyNum(sum(companies.stats$funding_total, rm.na=T),big.mark = ","), 
#                                   sep=""), "in Funding Raised")  
#   addPageStat("crunchbase", prettyNum(nrow(companies.stats[status=="acquired"]),big.mark = ","),
#               "Startups Got Acquired")
#   
#   addPageStat("crunchbase", prettyNum(nrow(companies.stats[status=="ipo"]),big.mark = ","),
#               "Startups Went IPO")
#   
#   addPageStat("crunchbase", prettyNum(nrow(companies.stats[status=="unknown"]),big.mark = ","),
#               "Startups in Unknown Status")
  addPageStat("crunchbase", paste("$", prettyNum(investor.stats[, list(funding_amount= sum(funding_amount, na.rm=T)), by=investor][which.max(funding_amount)]$funding_amount,big.mark = ",") 
                                  ,sep=""), paste("invested by", 
                                                  investor.stats[, list(funding_amount= sum(funding_amount, na.rm=T)), by=investor][which.max(funding_amount)]$investor, sep=" "))
  addPageStat("crunchbase", prettyNum(investor.stats[, list(fundings=nrow(.SD)), 
                                                  by=investor][which.max(fundings)]$fundings ,big.mark = ","), 
              paste("startups funded by", investor.stats[, list(fundings=nrow(.SD)), 
                    by=investor][which.max(fundings)]$investor, sep=" "))
  
  addPageStat("crunchbase", investor.stats[type=="angel"][, list(fundings= nrow(.SD)), by=investor][which.max(fundings)]$investor, 
              paste("made most angel investments:", investor.stats[type=="angel"][, list(fundings= nrow(.SD)), by=investor][which.max(fundings)]$fundings, sep=" "))
}

runCB <- function(){ 
  # Start the clock!
  ptm <- proc.time()
  
  # initialize
  startup()
  
  #generateStartupPADS()
  #generateRoundPADS()
  
  # update pad count
  updateCatPadCount()
  
  #cleanup
  cleanup()
  
  # Stop the clock
  proc.time() - ptm
}

#delete few things - be careful - this will remove all pads from mongodb and remove the cache entirely
deleteFewThings <- function() {
  initializeSystem(0)
  cleanCacheFiles()
  deletePageStat("crunchbase")
  
  #emptySystemPads()
  emptySystemPadsForCat(14)
  emptyCollection(mongo.db$system.pads)
  updateCatPadCount()
  
  #cleanup
  cleaupSystem()
}

#run this
#runCB()
