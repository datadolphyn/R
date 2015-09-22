# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

# Generate PADS - new way - read a csv file that has the list of pads
#
source("CreatePADS.R")

#
# generate Pads
#
generatePads <- function() {
  #initialize system
  initializeSystem()
  assign("folder.path", "./pads/raw-data/internet_stats/", envir=.GlobalEnv)
  assign("pads.file", "pads_meta.csv", envir=.GlobalEnv)
  assign("dataset", "Internet-Stats", envir=.GlobalEnv)
  pads <- readFile(paste(folder.path, pads.file, sep=""))
  for(i in 1:nrow(pads)){
    #prepare pad meta data
    series <- list()
    series["source"] <- pads$source[i]
    series["category"] <- pads$category[i]
    series["subcategory"] <- pads$subcategory[i]
    series["tags"] <- pads$tags[i]
    series["desc"] <- pads$desc[i]
    series["title"] <- pads$title[i]
    series["pagetag"] <- "internet"
    series.data <- trimData(readFile(paste(folder.path, pads$file[i], sep="")))
    series.data[2:ncol(series.data)] <- sapply(series.data[2:ncol(series.data)], removeMetaFromNumeric) 
    padify(series, series.data)
  }
  #clean up
  cleaupSystem()
  updateCatPadCount()
}

generatePads()
