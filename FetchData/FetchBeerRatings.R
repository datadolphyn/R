#
# Get beer data
#
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.
require(data.table)
startup <- function( {
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

assign("folder.path", "./pads/raw-data/beer-data/", envir=.GlobalEnv)
assign("data.file", "beer_reviews.csv.gz", envir=.GlobalEnv)

beer.data <- data.table(read.csv(paste(folder.path, data.file, sep="")))
setkey(beer.data, beer_beerid)
#get beers with 500 or more reviews
b.d.500 <- beer.data[, list(no_of_reviews=nrow(.SD)), by=beer_beerid][no_of_reviews >= 500]
setkey(b.d.500, beer_beerid)
#merge to get all records ofbeers which have 500 or more reviews
b.d.500 <- merge(b.d.500, beer.data, all=F)
