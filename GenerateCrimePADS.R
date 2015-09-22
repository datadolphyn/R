# Generate crime PADS
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

source("CreatePADS.R")
verbose <- T
#
# generate
#
generate <- function() {
  #initialize system
  initializeSystem()
  assign("crime.folder.path", "./pads/raw-data/crime", envir=.GlobalEnv)  
  assign("dataset", "US-Crime", envir=.GlobalEnv)
  
  #prepare pad meta data
  series <- list()
  series["source"] <- "  BJS, FBI, Uniform Crime Reports"
  series["category"] <- "Social"
  series["subcategory"] <- "US Crime Data"
  series["tags"] <- tolower(paste(series$category, series$subcategory, series$source, "usdoj, Supplementary Homicide Report, USA", sep=","))
  series["desc"] <- "Homicide Trends in United States: Data are based on annual estimates of homicide from previously published versions of Crime in the United States. Data for 1989 to 2008 reflect updated homicide estimates from Crime in the United States, 2008. Data for 2009 and 2010 reflect updated homicide estimates from Crime in the United States, 2010."
  series["pagetag"] <- "crunchbase"
  
  series.data <- read.csv(paste(crime.folder.path, "1.csv", sep="/"), stringsAsFactors=F)

  series["title"] <- "Homicide Rate per 100,000 Population (1950-2008)"
  padify(series, series.data[c(1,2)])    
  
  series["title"] <- "Homicide Incidents per 100,000 Population (1950-2008)"
  padify(series, series.data[c(1,3)])    
  
  series["title"] <- "Homicide Rate & Incidents per 100,000 Population (1950-2008)"
  padify(series, series.data)    
  
  series.data <- read.csv(paste(crime.folder.path, "2.csv", sep="/"), stringsAsFactors=F)
  series.data[2:4] <- data.frame(apply(series.data[2:4], 2, function(x) as.numeric(gsub("%", "", x, fixed=T))))  
  series["title"] <- "Victims and Offenders by Age in USA, Rate per 100,000 (1980-2008)"
  padify(series, series.data)    
  
  series.data <- read.csv(paste(crime.folder.path, "3.csv", sep="/"), stringsAsFactors=F)
  series.data[2:4] <- data.frame(apply(series.data[2:4], 2, function(x) as.numeric(gsub("%", "", x, fixed=T))))  
  series["title"] <- "Victims and Offenders by Gender in USA (1980-2008)"
  padify(series, series.data)    
  
  series.data <- read.csv(paste(crime.folder.path, "4.csv", sep="/"), stringsAsFactors=F)
  series.data[2:4] <- data.frame(apply(series.data[2:4], 2, function(x) as.numeric(gsub("%", "", x, fixed=T))))  
  series["title"] <- "Victims and Offenders by Race in USA (1980-2008)"
  padify(series, series.data)    
  
  series.data <- read.csv(paste(crime.folder.path, "2-1.csv", sep="/"), stringsAsFactors=F)
  series["title"] <- "Victims and Offenders by Age in USA, Rate per 100,000, (1980-2008)"
  padify(series, series.data)    
  
  series.data <- read.csv(paste(crime.folder.path, "3-1.csv", sep="/"), stringsAsFactors=F)
  series["title"] <- "Victims and Offenders by Gender in USA, Rate per 100,000 (1980-2008)"
  padify(series, series.data)    
  
  series.data <- read.csv(paste(crime.folder.path, "4-1.csv", sep="/"), stringsAsFactors=F)
  series["title"] <- "Victims and Offenders by Race in USA, Rate per 100,000 (1980-2008)"
  padify(series, series.data)    
  
  # Homicide victimization rates by age, (1980-2008)
  series.data <- read.csv(paste(crime.folder.path, "5.csv", sep="/"), stringsAsFactors=F)
  title <- "Homicide Victimization Rates By Age"
  period <- "(1980-2008)"
  colnames(series.data) <- gsub("X", "ages", colnames(series.data))
  col.names <- colnames(series.data) 
  for(i in 2:ncol(series.data))
  {
    series["title"] <- paste(title, ", ", col.names[i], period, sep="")
    print(i)
    padify(series, series.data[c(1,i)])    
  }
  series["title"] <- "Homicide Victimization Rates for All Ages (1980-2008)"
  padify(series, series.data)   
  
  
  # Homicide offending rates by age, 1980-2008
  series.data <- read.csv(paste(crime.folder.path, "5-1.csv", sep="/"), stringsAsFactors=F)
  title <- "Homicide Offending Rates By Age"
  period <- "(1980-2008)"
  colnames(series.data) <- gsub("X", "ages", colnames(series.data))
  col.names <- colnames(series.data) 
  for(i in 2:ncol(series.data))
  {
    series["title"] <- paste(title, ", ", col.names[i], period, sep="")
    print(i)
    padify(series, series.data[c(1,i)])    
  }
  series["title"] <- "Homicide Offending Rates for All Ages (1980-2008)"
  padify(series, series.data)
  
 #Average age of homicide victims and offenders, 1980-2008
  series.data <- read.csv(paste(crime.folder.path, "5-2.csv", sep="/"), stringsAsFactors=F)
  title <- "Average Age of Homicide"
  period <- "(1980-2008)"
  colnames(series.data) <- gsub("X", "ages", colnames(series.data))
  col.names <- colnames(series.data) 
  for(i in 2:ncol(series.data))
  {
    series["title"] <- paste(title, col.names[i], period, sep=" ")
    print(i)
    padify(series, series.data[c(1,i)])    
  }
  series["title"] <- "Average Age of Homicide Victims and Offenders (1980-2008)"
  padify(series, series.data)  
  
  #Figure 6. Percent of homicides in which offender was known to victim by age of victim, 1980-2008
  series.data <- read.csv(paste(crime.folder.path, "11.csv", sep="/"), stringsAsFactors=F)
  title <- "Percent of Homicides in Which Offender was Known to Victim (1980-2008)"
  padify(series, series.data)  
  
  #Figure 7. Number of homicides of children under age 5, by race of victim, 1980-2008
  #*Other race includes American Indians, Alaska Natives, Asians, Hawaiians, and other Pacific Islanders.
  series.data <- read.csv(paste(crime.folder.path, "12.csv", sep="/"), stringsAsFactors=F)
  title <- "Number of homicides of children under age 5, by race of victim"
  period <- "(1980-2008)"
  col.names <- colnames(series.data) 
  for(i in 2:ncol(series.data))
  {
    series["title"] <- paste(title, "-", col.names[i], period, sep=" ")
    padify(series, series.data[c(1,i)])    
  }
  series["title"] <- "Number of homicides of children under age 5, by race of victim (1980-2008)"  
  padify(series, series.data)  
  
  #Figure 8. Homicide victimization rates for children under age 5 by race of victim, 1980-2008  		
  #Rate per 100,000 population			Rate per 100,000 population
  series.data <- read.csv(paste(crime.folder.path, "12-1.csv", sep="/"), stringsAsFactors=F)
  title <- "Rate of homicides of children under age 5, by race of victim"
  period <- "(1980-2008)"
  col.names <- colnames(series.data) 
  for(i in 2:ncol(series.data))
  {
    series["title"] <- paste(title, "-", col.names[i], "per 100,000 population", period, sep=" ")
    padify(series, series.data[c(1,i)])    
  }
  series["title"] <- "Rate of homicides of children under age 5, by race of victim, per 100,000 population (1980-2008)"  
  padify(series, series.data)  

  #clean up
  cleaupSystem()
  updateCatPadCount()
}
generate()
