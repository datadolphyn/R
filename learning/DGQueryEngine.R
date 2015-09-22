# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library(plyr)
library(RJSONIO)
#setwd("/Users/homemac/Toga/Alto")
#Set content type for output
setContentType("text/html")
#Query Engine - qds -  query this database, d=dimension, m=measure, f=funciton 
qds <- d <- m <- f <- g <- NULL
#d <- ""
#m <- ""
#f <- ""

#load pre-analyzed and saved dataframe
loadDS <- function(qds) {
  if(exists(qds)) {
      #lpads <- get("pads", envir=.GlobalEnv)
      #cat(exists(pads$sysID[pads$id==qds], envir=.GlobalEnv), "<br/>")
      #cat(qds)
      return(get(qds))    #get a local copy in this enviornment
    } else {
      cat(paste('{"response_code": "error", "error_message": "The pad ', qds, ' does not exist. Check to see if you start up R script in RApache ran correctly."}', sep=""))
    }
}

## Operate on a data.frame, split by "by", by could be a vector or a single filed
## aggreage by columns in vector on, on could be a single field also
getSum <- function(df, by, on) {
    #print(sum(df[,on],  na.rm=TRUE))
    #cat(paste(by, on, sep=", "))
    return(ddply(df, by, colwise(function(col) {sum(col, na.rm=TRUE)}, on)))
}

getMean <- function(df, by, on) {
  #print(sum(df[,on],  na.rm=TRUE))
  #cat(paste(by, on, sep=", "))
  return(ddply(df, by, colwise(function(col) {mean(col, na.rm=TRUE)}, on)))
}

#ddply(mbDS, "Opposition", function(tDF){data.frame(Lat=tDF$Opposition.Lat[1], Lng=tDF$Opposition.Lng[1], Test.Runs=sum(tDF$Test.Runs, na.rm=TRUE))})
## Operate on a data.frame, split by "by", by could be a vector or a single filed
## and aggreage by columns in vector on, on could be a single field also
#getCount <- function(df, by, on) {
#  return(ddply(df, by, colwise(function(col) {length(unique(col))}, on)))
#}

getCount<- function(df, by, on) {   
    lds <- ddply(df, by, "nrow")
    colnames(lds) <- c(by,on)
    return(lds)
}

#For counting column that has 0 or 1 value :: Very similar to getSum - the answer should be same
getBinaryCount <- function(df, by, on) {
    return(ddply(df, by, colwise(function(x) {length(which(x==1))}, on)))
    #length(which(mbDS[,"Test.Century"] ==1))
}

readRequest <- function() {
  #GetString <- str(GET)
  #cat(GetString)
  qds <<- GET$qds
  d <<- GET$d
  m <<- GET$m
  f <<- GET$f
  g <<- GET$g
  #cat(d, "," ,m, "," ,f, "," ,qds, sep="    ")
  #cat("<br/>")
}

processRequest <- function(fnType, df){  
    return(switch(fnType,
       s = getSum(df, d, m), 
       c = getCount(df, d, m),
       m = getMean(df,d,m),
       d = getValue(df,d,m),
       'default'
       )
    )   
}

writeResponse <- function(responseDF, df) {
  sJSON <- paste('{"response_code": "ok","', m, '":', toJSON(responseDF[,m]), sep="") #JSON String is not closed.
  if(!is.null(g) && g == "t") {
    latName <- paste(d, ".Lat", sep="")
    lngName <- paste(d, ".Lng", sep="")
    sJSON <- paste(sJSON, ', "mapsJSON": [', sep="")
    for(i in 1:nrow(responseDF)) {
      key <- responseDF[,d][i]
      #cat(paste ("<br/>", key , ", what?"))
      j <- match(key, df[,d])
      sJSON <- paste(sJSON, '{"d":"', key,
                               '", "lat":', 
                                df[latName][j,],
                              ', "lng":', df[lngName][j,], 
                               ', "data":', responseDF[i,2], 
                                '}', sep="")
      if(i!=nrow(responseDF))
        sJSON <- paste(sJSON, ",", sep="")
    }
    sJSON <- paste(sJSON, ']', sep="")
  }
    sJSON = paste(sJSON, "}", sep="")
  #Write response back
  cat(sJSON)
  #cat(ls())
  #rm(list=ls()) #Clean up everything - There has to be a better way than this.
  #cat(ls())
}

#Step 1 - Read Request, query string
readRequest()

#Step 2 - load environment
df <- loadDS(qds)

if(!is.null(df)) {
  #Step 3 - Process Request 
  responseDF <- processRequest(f, df)
  #Step 3.a - Apply Filter
  
  #Step 4 - Write Response
  writeResponse(responseDF, df)
}