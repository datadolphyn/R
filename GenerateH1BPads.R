# Generate H1B PADS
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

source("CreatePADS.R")

#
# h1b.datasets
#
loadH1BData <- function(){
  stats <-  read.csv(paste(h1b.folder.path, h1b.titles.file, sep=""), stringsAsFactors=F)
  if(!is.na(stats)){
    h1b.titles <- data.table(stats)
    h1b.titles$job_title <- gsub("  ", " ", str_trim(gsub("/", " ", tolower(h1b.titles$job_title))))
    setkeyv(h1b.titles, "year")
    assign("h1b.titles", h1b.titles, envir=.GlobalEnv)
  }
  #wages
  h1b.wages <-  data.table(read.csv(paste(h1b.folder.path, h1b.wages.file, sep=""), stringsAsFactors=F))
  setkeyv(h1b.wages, "year")
  assign("h1b.wages", h1b.wages, envir=.GlobalEnv)
  
  #cities
  h1b.cities <- data.table(read.csv(paste(h1b.folder.path, h1b.cities.file, sep=""), stringsAsFactors=F))
  h1b.cities$city <- tolower(h1b.cities$city)
  setkeyv(h1b.cities, "year", "city")
  assign("h1b.cities", h1b.cities, envir=.GlobalEnv)
  
  #states
  h1b.states <- data.table(read.csv(paste(h1b.folder.path, h1b.states.file, sep=""), stringsAsFactors=F))
  h1b.states$state <- tolower(h1b.states$state)
  setkeyv(h1b.states, "year", "state")
  assign("h1b.states", h1b.states, envir=.GlobalEnv)
  
  #companies
  h1b.companies <- data.table(read.csv(paste(h1b.folder.path, h1b.companies.file, sep=""), stringsAsFactors=F))
  h1b.companies$employer <- tocamel(cleanName(tolower(h1b.companies$employer)))
  setkeyv(h1b.companies, "year", "employer")
  assign("h1b.companies", h1b.companies, envir=.GlobalEnv)
}

#
# initialize
#
startup <- function() {
  #initialize system
  initializeSystem()
  
  assign("h1b.folder.path", "./pads/raw-data/immigration/", envir=.GlobalEnv)
  assign("h1b.titles.file", "US_H1B_top_titles_2002-2007.csv", envir=.GlobalEnv)
  assign("h1b.wages.file", "US_H1B_top_wages_2002-2007.csv", envir=.GlobalEnv)
  assign("h1b.cities.file", "US_H1B_top_cities_2002-2007.csv", envir=.GlobalEnv)
  assign("h1b.states.file", "US_H1B_top_states_2002-2007.csv", envir=.GlobalEnv)
  assign("h1b.companies.file", "US_H1B_top_companies_2002-2007.csv", envir=.GlobalEnv)
  
  assign("dataset", "US-H1B", envir=.GlobalEnv)
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "OFLC, Department of Labor"
  series["category"] <- "Immigration"
  series["subcategory"] <- "H1B Visas USA"
  series["category_id"]<- 23
  series["subcategory_id"]<- 211
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "H1B, Visas, regulation, USA, law", sep=","))
  assign("series", series, envir=.GlobalEnv)
  #load data
  loadH1BData()
}

#
# cleanup
#
cleanup <- function(){
  cleaupSystem()
}

#
# generate pads for titles
#
generateH1BPADS <- function(){  
  
  period.min <- min(h1b.titles$year)
  period.max <- max(h1b.titles$year)
  years <- paste("(", period.min, "-", period.max, ")", sep="")
  
  ## H1B Filings by year
  h1b.filings.year <- h1b.titles[, list(h1b_filings=sum(total, na.rm=T)), by=year]
  series["title"] <- paste("Number of H1B Filings ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.filings.year)
  
  ## titles count by year
  h1b.titles.count.year <- h1b.titles[, list(h1b_titles_count=length(unique(job_title))), by=year]
  series["title"] <- paste("Number of Unique H1B Titles Used in H1B Filings ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.titles.count.year)
  
  ## Join the two
  setkey(h1b.filings.year, year)
  setkey(h1b.titles.count.year, year)
  h1b.filings.titles <- h1b.filings.year[h1b.titles.count.year]
  series["title"] <- paste("Number of H1B Filings and Number of Titles Used in Filings", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.filings.titles)
  
  #### approved visas by year
  h1b.approvals.year <- h1b.titles[, list(h1b_approved=sum(approved)), by=year]
  series["title"] <- paste("Number of H1B Visas Approved ", years,  ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.approvals.year)
  
  ## approval ratio
  h1b.approval.ratio.year <- h1b.titles[, list(h1b_approval_rates_in_percent=round(sum(approved)/sum(total), 4)*100), by=year]
  series["title"] <- paste("H1B Approval Rates ", years,  ", USA", sep="")
  series["desc"] <- "Unit: in percent(%). The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.approval.ratio.year)
  
  ## Join the two
  setkey(h1b.approvals.year, year)
  setkey(h1b.approval.ratio.year, year)
  h1b.approvals <- h1b.approvals.year[h1b.approval.ratio.year]
  series["title"] <- paste("H1B Approvals and Approval Rate ", years,  ", USA", sep="")
  series["desc"] <- "Unit for Approval Rate: in percent(%).The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.approvals)
  
  #### Denied visas by year
  ##Sum
  h1b.denials.year <- h1b.titles[, list(h1b_denied=sum(denied)), by=year]
  series["title"] <- paste("Number of H1B Visas Denied ", years,  ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.denials.year)
  
  ## Ratio
  h1b.denial.ratio.year <- h1b.titles[, list(h1b_denial_rate_in_percent=round(sum(denied)/sum(total), 4)*100), 
                                      by=year]
  series["title"] <- paste("H1B Denial Rates ", years,  ", USA",sep="")
  series["desc"] <- "Unit: in percent(%). The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.denial.ratio.year)
  
  ## Join the two
  setkey(h1b.denials.year, year)
  setkey(h1b.denial.ratio.year, year)
  h1b.denials <- h1b.denials.year[h1b.denial.ratio.year]
  series["title"] <- paste("Number of Denied H1B Visas and Denial Rate ", years,  ", USA",sep="")
  series["desc"] <- "Unit: in percent(%). The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.denials)
  
  ## Join approval and denials
  series.data <- h1b.approvals.year[h1b.denials.year]
  series["title"] <- paste("Number of H1B Approved and Denied Visas ", years,  ", USA",sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  ## Join approval and denials
  series.data <- h1b.approval.ratio.year[h1b.denial.ratio.year]
  series["title"] <- paste("H1B Approval and Denial Rates ", years,  ", USA",sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  ## top 10 visa titles approved through out the period 
  h1b.approvals.title.year <- h1b.titles[, list(h1b_approved=sum(approved)), 
                                         by=list(job_title)][order(-h1b_approved)] [1:10]
  titles <- h1b.approvals.title.year$job_title
  series["title"] <- paste("Top 10 Visa Titles Approved ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.approvals.title.year)
  
  ## top 10 visa titles denied  through out the period
  h1b.denied.title.year <- h1b.titles[, list(h1b_deined=sum(denied)), 
                                      by=list(job_title)][order(-h1b_deined)][1:10]
  series["title"] <- paste("Top 10 Visa Titles Denied ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, h1b.denied.title.year)
  
  #by titles
  for(i in titles){
    series.data <- h1b.titles[job_title==i][, list(h1b_approved=sum(approved, na.rm=T)), 
                                            by=year]
    
    series["title"] <- paste("H1B Visa Approved for Title ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    ## denied by year
    series.data <- h1b.titles[job_title==i][, list(h1b_denied=sum(denied, na.rm=T)), 
                                            by=year]
    series["title"] <- paste("H1B Visa Denied for Title ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    # All three
    series.data <- h1b.titles[job_title==i][, list(h1b_approved=sum(approved, na.rm=T),
                                                   h1b_denied=sum(denied, na.rm=T), 
                                                   h1b_filings=sum(total, na.rm=T)), 
                                            by=year]
    
    series["title"] <- paste("H1B Visa Approved, Denied and Filings for Title ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
  
  for(i in period.min:period.max){
    ## top 10 visa titles approved by year
    series.data <- h1b.titles[year==i][, list(h1b_approved=sum(approved, na.rm=T)), 
                                       by=job_title][order(-h1b_approved)][1:10]
    
    series["title"] <- paste("Top 10 Visa Titles Approved in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    ## top 10 visa titles denied by year
    series.data <- h1b.titles[year==i][, list(h1b_deined=sum(denied, na.rm=T)), 
                                       by=job_title][order(-h1b_deined)][1:10]
    
    series["title"] <- paste("Top 10 Visa Titles Denied in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
  
  ### Pad ideas - column chart - approvals and denials for top titles every year
  
  ##### approval rates category by year
  ## highest 
  for(i in period.min:period.max){
    h1b.highest.approval.rates.year <- h1b.titles[year==i][, list(h1b_approval_rate=round(sum(approved)/sum(total),4)*100), 
                                                           by=list(job_title)][order(-h1b_approval_rate)][1:15]
    series["title"] <- paste("H1B Titles with Highest H1B Approval Rates in ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent(%). The data are collected from OFLC and includes only H1B e-filings."
    padify(series, h1b.highest.approval.rates.year)
    
    # 90 - 95%
    h1b.highest.approval.rates.year <- h1b.titles[year==i][, list(h1b_approval_rate=round(sum(approved)/sum(total),4)*100),  
                                                           by=list(job_title)][h1b_approval_rate < 95] [h1b_approval_rate > 90][order(-h1b_approval_rate)][1:15]
    series["title"] <- paste("H1B Titles with Approval Rates between 90% and 95% in ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent(%). The data are collected from OFLC and includes only H1B e-filings."
    padify(series, h1b.highest.approval.rates.year)
    
    # 80 - 90%
    h1b.highest.approval.rates.year <- h1b.titles[year==i][, list(h1b_approval_rate=round(sum(approved)/sum(total),4)*100),  
                                                           by=list(job_title)][h1b_approval_rate < 90] [h1b_approval_rate > 80][order(-h1b_approval_rate)][1:15]
    series["title"] <- paste("H1B Titles with Approval Rates between 80% and 90% in ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent(%). The data are collected from OFLC and includes only H1B e-filings."
    padify(series, h1b.highest.approval.rates.year)
    
    ##########denial rates category by year
    ## highest 
    h1b.highest.denial.rates.year <- h1b.titles[year==i][, list(h1b_denial_rate=round(sum(denied)/sum(total),4)*100, total=sum(total)), 
                                                         by=list(job_title)][total>50][,total:=NULL][order(-h1b_denial_rate)][1:15]
    series["title"] <- paste("H1B Titles with Highest Denial Rates in ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent(%). The data are collected from OFLC and includes only H1B e-filings."
    #padify(series, h1b.highest.approval.rates.year)
    
    ##lowest
    h1b.lowest.denial.rates.year <- h1b.titles[year==i][, 
                                                        list(h1b_denial_rate=round(sum(denied)/sum(total),4)*100, total=sum(total)), 
                                                        by=list(job_title)][total>50][,total:=NULL][order(h1b_denial_rate)][h1b_denial_rate>0][1:15]
    series["title"] <- paste("H1B Titles with Lowest Denial Rates in ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent(%). The data are collected from OFLC and includes only H1B e-filings."
    padify(series, h1b.highest.approval.rates.year)
  }
}

#
#H1b - wages
#
generateH1BWagesPADS <- function(){
  ###########H1b - top wages
  ### Pad ideas - wage offered and the title by year ()
  ### Pad ideas - wage offered and the company by year (bar chart )
  # IF one column is of type numeric and the second one is of type String - then a bar chart 
  
  period.min <- min(h1b.wages$year)
  period.max <- max(h1b.wages$year)
  period <- period.min:period.max
  years <- paste("(", period.min, "-", period.max, ")", sep="")
  
  #average wages offered
  series.data <- h1b.wages[, list(average_wage=mean(wage_offered)), by=year]
  series["title"] <- paste("Average Wage Offered on H1B Filings ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #median wages offered
  series.data <- h1b.wages[, list(median_wage=median(wage_offered)), by=year]
  series["title"] <- paste("Median Wage Offered on H1B Filings ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #wages offered below 50K
  series.data <- h1b.wages[, list(wages_below_50k=length(which(wage_offered < 50000)), filings=length(wage_offered)), by=year]
  series["title"] <- paste("Number of H1B Filings with Wages Offered Below $50,000 ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #Wages offered below 100K but above 50K
  series.data <- h1b.wages[, list(wages_between_50k_100k=length(which(wage_offered < 100000  & wage_offered >50000)), filings=length(wage_offered)), by=year]
  series["title"] <- paste("Number H1B of Filings with Wages Offered Between $50,000 and $100,000 ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #Wages offered over 100K
  series.data <- h1b.wages[, list(wages_over_100k=length(which(wage_offered >100000)), filings=length(wage_offered)), by=year]
  series["title"] <- paste("Number of H1B Filings with Wages Over $100,000", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #Wages offered over 100K
  series.data <- h1b.wages[, list(wages_over_150k=length(which(wage_offered >150000)), filings=length(wage_offered)), by=year]
  series["title"] <- paste("Number of H1B Filings with Wages Over $150,000", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #Wages offered above prevailing wages
  series.data <- h1b.wages[, list(filing_wages_offered_over_prevailing_wages=length(which(wage_offered > prevailing_wages)), filings=length(wage_offered)), by=year]
  series["title"] <- paste("Number of H1B Filings with Wages Over Prevailing Wages", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #Wages offered above prevailing wages
  series.data.a <- h1b.wages[,list(wages_over_prevailing_wages=(length(which(wage_offered > prevailing_wages))/length(wage_offered))*100), by=year]
  series["title"] <- paste("Percent of H1B Filings with Wages Over Prevailing Wages", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data.a)
  
  
  #Wages offered above prevailing wages
  series.data.b <- h1b.wages[,list(wages_below_prevailing_wages=(length(which(wage_offered < prevailing_wages))/length(wage_offered))*100), by=year]
  series["title"] <- paste("Percent of H1B Filings with Wages Below Prevailing Wages", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data.b)
  
  #Wages offered at prevailing wages
  series.data.c <- h1b.wages[,list(wages_at_prevailing_wages=(length(which(wage_offered == prevailing_wages))/length(wage_offered))*100), by=year]
  series["title"] <- paste("Percent of H1B Filings with Wages at Prevailing Wages", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data.c)
  
  #combine the two
  setkey(series.data.a, "year")
  setkey(series.data.b, "year")
  setkey(series.data.c, "year")
  series.data <- series.data.a[series.data.b[series.data.c]]
  series["title"] <- paste("Percent of H1B Filings with Wages Above, Below and at Prevailing Wages", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  for(i in period){
    x <- h1b.wages[year==2002][,list(wage_group=cut(wage_offered, breaks=25000*(0:8), right=F, dig.lab=6), wage_offered)][order(wage_offered)]
    series.data <- x[,list(h1b_filings=length(wage_offered)), by=wage_group]
    series.data$wage_group <- sub("^[\\[]", "", sub("[\\)]$", "", as.character(series.data$wage_group)))
    series["title"] <- paste("Distribution of Wages Offered in H1B Filings in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
}

#
#H1b - top cities
#
generateH1BCitiesPADS <- function(){
  ###########H1b - top wages
  ### Pad ideas - wage offered and the title by year ()
  ### Pad ideas - wage offered and the company by year (bar chart )
  # IF one column is of type numeric and the second one is of type String - then a bar chart 
  
  period.min <- min(h1b.cities$year)
  period.max <- max(h1b.cities$year)
  period <- period.min:period.max
  years <- paste("(", period.min, "-", period.max, ")", sep="")
  
  #h1b filings by cities 
  series.data <- h1b.cities[, list(h1b_filings=sum(total, na.rm=T)), by=city][order(-h1b_filings)][1:50, ]
  cities <- series.data$city
  series["title"] <- paste("Total H1B Filings by Cities ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data[1:20,])
  
  #h1b approvals by cities 
  series.data <- h1b.cities[, list(h1b_approvals=sum(approved, na.rm=T)), by=city][order(-h1b_approvals)][1:20, ]
  series["title"] <- paste("Total H1B Approvals by Cities ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #h1b approval rates by cities 
  series.data <- h1b.cities[, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                   total=sum(total, na.rm=T)), by=city][total > 10000][,total:=NULL][order(-h1b_approval_rate)][1:20, ]
  series["title"] <- paste("Cities With Highest H1B Approval Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). Only Cities from which more than 10,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #lowest h1b approval rates by cities 
  series.data <- h1b.cities[, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                   total=sum(total, na.rm=T)), by=city][total > 10000][,total:=NULL][order(h1b_approval_rate)][1:20, ]
  series["title"] <- paste("Cities With Lowest H1B Approval Rates ", years, ", USA",sep="")
  padify(series, series.data)
  
  #h1b denial by cities 
  series.data <- h1b.cities[, list(h1b_denials=sum(denied, na.rm=T)), by=city][order(-h1b_denials)][1:20, ]
  series["title"] <- paste("Total H1B Denials by Cities ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #h1b denial rates by cities 
  series.data <- h1b.cities[, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100, 
                                   total=sum(total, na.rm=T)), by=city][,total:=NULL][order(-h1b_denial_rate)][1:20, ]
  series["title"] <- paste("Cities With Highest H1B Denial Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). All cities are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  series.data <- h1b.cities[, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100, 
                                   total=sum(total, na.rm=T)), by=city][total > 100] [,total:=NULL][order(-h1b_denial_rate)][1:20, ]
  series["title"] <- paste("Cities With Highest H1B Denial Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). Only Cities from which more than 100 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #by cities by years
  for(i in cities){
    #h1b filings  
    series.data <- h1b.cities[city==i][, list(h1b_filings=sum(total, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Filings from ", tocamel(i), ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approvals 
    series.data <- h1b.cities[city==i][, list(h1b_approvals=sum(approved, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Approvals for ", tocamel(i), ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approval rates 
    series.data <- h1b.cities[city==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100), 
                                       by=year]
    series["title"] <- paste("H1B Approval Rate for ", tocamel(i), ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). Only Cities from which more than 10,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b denial  
    series.data <- h1b.cities[city==i][, list(h1b_denials=sum(denied, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Denied for ", tocamel(i), ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b denial rates by cities 
    series.data <- h1b.cities[city==i][, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100), 
                                       by=year]
    series["title"] <- paste("H1B Denial Rates for ", tocamel(i), ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
  
  #by years
  for(i in period){
    #h1b filings by cities 
    series.data <- h1b.cities[year==i][, list(h1b_filings=sum(total, na.rm=T)), by=city][order(-h1b_filings)][1:20, ]
    series["title"] <- paste("Total H1B Filings by Cities in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approvals by cities 
    series.data <- h1b.cities[year==i][, list(h1b_approvals=sum(approved, na.rm=T)), by=city][order(-h1b_approvals)][1:20, ]
    series["title"] <- paste("Total H1B Approvals by Cities in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approval rates by cities 
    series.data <- h1b.cities[year==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                              total=sum(total, na.rm=T)), by=city][total > 1000][,total:=NULL][order(-h1b_approval_rate)][1:20, ]
    series["title"] <- paste("Cities With Highest H1B Approval Rates in ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). Only Cities from which more than 1,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #lowest h1b approval rates by cities 
    series.data <- h1b.cities[year==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                              total=sum(total, na.rm=T)), by=city][total > 1000][,total:=NULL][order(h1b_approval_rate)][1:20, ]
    series["title"] <- paste("Cities With Lowest H1B Approval Rates in ", i, ", USA", sep="")
    padify(series, series.data)
    
    #h1b denial by cities 
    series.data <- h1b.cities[year==i][, list(h1b_denials=sum(denied, na.rm=T)), by=city][order(-h1b_denials)][1:20, ]
    series["title"] <- paste("Total H1B Denials by Cities in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
}

#
#H1b - top states
#

generateH1BStatesPADS <- function(){
  period.min <- min(h1b.states$year)
  period.max <- max(h1b.states$year)
  period <- period.min:period.max
  years <- paste("(", period.min, "-", period.max, ")", sep="")
  
  #h1b filings by states 
  series.data <- h1b.states[, list(h1b_filings=sum(total, na.rm=T)), by=state][order(-h1b_filings)][1:50, ]
  states <- series.data$state
  series.data$state <- toupper(series.data$state)
  series["title"] <- paste("Total H1B Filings by States ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data[1:20,])
  
  #h1b approvals by states 
  series.data <- h1b.states[, list(h1b_approvals=sum(approved, na.rm=T)), by=state][order(-h1b_approvals)][1:20, ]
  series.data$state <- toupper(series.data$state)
  series["title"] <- paste("Total H1B Approvals by States ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #h1b approval rates by states 
  series.data <- h1b.states[, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                   total=sum(total, na.rm=T)), by=state][total > 10000][,total:=NULL][order(-h1b_approval_rate)][1:20, ]
  series.data$state <- toupper(series.data$state)
  series["title"] <- paste("States With Highest H1B Approval Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). Only states from which more than 10,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #lowest h1b approval rates by states 
  series.data <- h1b.states[, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                   total=sum(total, na.rm=T)), by=state][total > 10000][,total:=NULL][order(h1b_approval_rate)][1:20, ]
  series.data$state <- toupper(series.data$state)
  series["title"] <- paste("States With Lowest H1B Approval Rates ", years, ", USA",sep="")
  padify(series, series.data)
  
  #h1b denial by states 
  series.data <- h1b.states[, list(h1b_denials=sum(denied, na.rm=T)), by=state][order(-h1b_denials)][1:20, ]
  series.data$state <- toupper(series.data$state)
  series["title"] <- paste("Total H1B Denials by States ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #h1b denial rates by states 
  series.data <- h1b.states[, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100, 
                                   total=sum(total, na.rm=T)), by=state][,total:=NULL][order(-h1b_denial_rate)][1:20, ]
  series.data$state <- toupper(series.data$state)
  series["title"] <- paste("States With Highest H1B Denial Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). All states are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  series.data <- h1b.states[, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100, 
                                   total=sum(total, na.rm=T)), by=state][total > 100] [,total:=NULL][order(-h1b_denial_rate)][1:20, ]
  series["title"] <- paste("States With Highest H1B Denial Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). Only states from which more than 100 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #by states by years
  for(i in states){
    #h1b filings  
    series.data <- h1b.states[state==i][, list(h1b_filings=sum(total, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Filings from ", toupper(i), ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approvals 
    series.data <- h1b.states[state==i][, list(h1b_approvals=sum(approved, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Approvals for ", toupper(i), ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approval rates 
    series.data <- h1b.states[state==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100), 
                                        by=year]
    series["title"] <- paste("H1B Approval Rate for ", toupper(i), ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). Only states from which more than 10,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b denial  
    series.data <- h1b.states[state==i][, list(h1b_denials=sum(denied, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Denied for ", toupper(i), ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b denial rates by states 
    series.data <- h1b.states[state==i][, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100), 
                                        by=year]
    series["title"] <- paste("H1B Denial Rates for ", toupper(i), ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
  
  #by years
  for(i in period){
    #h1b filings by states 
    series.data <- h1b.states[year==i][, list(h1b_filings=sum(total, na.rm=T)), by=state][order(-h1b_filings)][1:20, ]
    series["title"] <- paste("Total H1B Filings by States in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approvals by states 
    series.data <- h1b.states[year==i][, list(h1b_approvals=sum(approved, na.rm=T)), by=state][order(-h1b_approvals)][1:20, ]
    series["title"] <- paste("Total H1B Approvals by States in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approval rates by states 
    series.data <- h1b.states[year==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                              total=sum(total, na.rm=T)), by=state][total > 1000][,total:=NULL][order(-h1b_approval_rate)][1:20, ]
    series["title"] <- paste("States With Highest H1B Approval Rates in ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). Only states from which more than 1,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #lowest h1b approval rates by states 
    series.data <- h1b.states[year==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                              total=sum(total, na.rm=T)), by=state][total > 1000][,total:=NULL][order(h1b_approval_rate)][1:20, ]
    series["title"] <- paste("States With Lowest H1B Approval Rates in ", i, ", USA", sep="")
    padify(series, series.data)
    
    #h1b denial by states 
    series.data <- h1b.states[year==i][, list(h1b_denials=sum(denied, na.rm=T)), by=state][order(-h1b_denials)][1:20, ]
    series["title"] <- paste("Total H1B Denials by States in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
}

#
#H1b - top companies
#
generateH1BCompaniesPADS <- function(){
  period.min <- min(h1b.companies$year)
  period.max <- max(h1b.companies$year)
  period <- period.min:period.max
  years <- paste("(", period.min, "-", period.max, ")", sep="")
  
  #h1b filings by employer 
  series.data <- h1b.companies[, list(h1b_filings=sum(total, na.rm=T)), by=employer][order(-h1b_filings)][1:50, ]
  employers <- series.data$employer
  series["title"] <- paste("Total H1B Filings by Companies ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data[1:20,])
  
  #h1b approvals by employer 
  series.data <- h1b.companies[, list(h1b_approvals=sum(approved, na.rm=T)), by=employer][order(-h1b_approvals)][1:20, ]
  series["title"] <- paste("Total H1B Approvals by Companies ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #h1b approval rates by employer 
  series.data <- h1b.companies[, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                      total=sum(total, na.rm=T)), by=employer][total > 10000][,total:=NULL][order(-h1b_approval_rate)][1:20, ]
  series["title"] <- paste("Companies With Highest H1B Approval Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). Only employer from which more than 10,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #lowest h1b approval rates by employer 
  series.data <- h1b.companies[, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                      total=sum(total, na.rm=T)), by=employer][total > 10000][,total:=NULL][order(h1b_approval_rate)][1:20, ]
  series["title"] <- paste("Companies With Lowest H1B Approval Rates ", years, ", USA",sep="")
  padify(series, series.data)
  
  #h1b denial by employer 
  series.data <- h1b.companies[, list(h1b_denials=sum(denied, na.rm=T)), by=employer][order(-h1b_denials)][1:20, ]
  series["title"] <- paste("Total H1B Denials by Companies ", years, ", USA", sep="")
  series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #h1b denial rates by employer 
  series.data <- h1b.companies[, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100, 
                                      total=sum(total, na.rm=T)), by=employer][,total:=NULL][order(-h1b_denial_rate)][1:20, ]
  series["title"] <- paste("Companies With Highest H1B Denial Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). All employer are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  series.data <- h1b.companies[, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100, 
                                      total=sum(total, na.rm=T)), by=employer][total > 100] [,total:=NULL][order(-h1b_denial_rate)][1:20, ]
  series["title"] <- paste("Companies With Highest H1B Denial Rates ", years, ", USA", sep="")
  series["desc"] <- "Unit: in percent (%). Only employer from which more than 100 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
  padify(series, series.data)
  
  #by employer by years
  for(i in employers){
    #h1b filings  
    series.data <- h1b.companies[employer==i][, list(h1b_filings=sum(total, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Filings from ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approvals 
    series.data <- h1b.companies[employer==i][, list(h1b_approvals=sum(approved, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Approvals for ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approval rates 
    series.data <- h1b.companies[employer==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100), 
                                              by=year]
    series["title"] <- paste("H1B Approval Rate for ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). Only employer from which more than 10,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b denial  
    series.data <- h1b.companies[employer==i][, list(h1b_denials=sum(denied, na.rm=T)), by=year]
    series["title"] <- paste("Total H1B Denied for ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b denial rates by employer 
    series.data <- h1b.companies[employer==i][, list(h1b_denial_rate=(sum(denied, na.rm=T)/sum(total, na.rm=T))*100), 
                                              by=year]
    series["title"] <- paste("H1B Denial Rates for ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
  
  #by years
  for(i in period){
    #h1b filings by employer 
    series.data <- h1b.companies[year==i][, list(h1b_filings=sum(total, na.rm=T)), by=employer][order(-h1b_filings)][1:20, ]
    series["title"] <- paste("Total H1B Filings by Companies in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approvals by employer 
    series.data <- h1b.companies[year==i][, list(h1b_approvals=sum(approved, na.rm=T)), by=employer][order(-h1b_approvals)][1:20, ]
    series["title"] <- paste("Total H1B Approvals by Companies in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #h1b approval rates by employer 
    series.data <- h1b.companies[year==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                                 total=sum(total, na.rm=T)), by=employer][total > 1000][,total:=NULL][order(-h1b_approval_rate)][1:20, ]
    series["title"] <- paste("Companies With Highest H1B Approval Rates in ", i, ", USA", sep="")
    series["desc"] <- "Unit: in percent (%). Only employer from which more than 1,000 H1B visas were filed are included. The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
    
    #lowest h1b approval rates by employer 
    series.data <- h1b.companies[year==i][, list(h1b_approval_rate=(sum(approved, na.rm=T)/sum(total, na.rm=T))*100, 
                                                 total=sum(total, na.rm=T)), by=employer][total > 1000][,total:=NULL][order(h1b_approval_rate)][1:20, ]
    series["title"] <- paste("Companies With Lowest H1B Approval Rates in ", i, ", USA", sep="")
    padify(series, series.data)
    
    #h1b denial by employer 
    series.data <- h1b.companies[year==i][, list(h1b_denials=sum(denied, na.rm=T)), by=employer][order(-h1b_denials)][1:20, ]
    series["title"] <- paste("Total H1B Denials by Companies in ", i, ", USA", sep="")
    series["desc"] <- "The data are collected from OFLC and includes only H1B e-filings."
    padify(series, series.data)
  }
}


#Run this script
runH1B <- function(){ 
  # Start the clock!
  ptm <- proc.time()
  
  # initialize
  startup()
  
  generateH1BPADS()
  generateH1BWagesPADS()
  generateH1BCitiesPADS()  
  generateH1BStatesPADS()
  generateH1BCompaniesPADS()
  #cleanup
  cleanup()
  
  #update category pad count
  updateCatPadCount()
  
  # Stop the clock
  proc.time() - ptm
}

runH1B()