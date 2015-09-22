# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#This version will use data.table in place of PLYR.
#library(plyr)
library(RJSONIO)
library(data.table)
#setwd("/Users/homemac/Toga/Alto")

#Set content type for output
setContentType("text/html")
#Query Engine - qds -  query this database, d=dimension, m=measure, g= geo, f=funciton 
qds <- d <- m <- f <- g <- NULL
#d <- ""
#m <- ""
#f <- ""

#load pre-analyzed and saved dataframe
loadDS <- function(qds) {
  if(exists("pads", envir=.GlobalEnv)) {
      #lpads <- get("pads", envir=.GlobalEnv)
      #cat(exists(pads$sysID[pads$id==qds], envir=.GlobalEnv), "<br/>")
      return(get(pads$sysID[pads$id==qds], envir=.GlobalEnv))    #get a local copy in this enviornment
    } else {
      print("PADS didn't exist. Check to see if you start up R script in RApache ran correctly.")
    }
}

## Operate on a data.frame, split by "by", by could be a vector or a single filed
## aggreage by columns in vector on, on could be a single field also

getResults <- function(dt, byVar, onVar) {
    e <- substitute(onVar)
    cat(e)
    return(dt[,eval(e),by=byVar])
}


## Operate on a data.frame, split by "by", by could be a vector or a single filed
## and aggreage by columns in vector on, on could be a single field also
#getCount <- function(df, by, on) {
#  return(ddply(df, by, colwise(function(col) {length(unique(col))}, on)))
#}

getCount<- function(dt, by, on) {   
    lds <- dt[], by, "nrow")
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
  cat(d, "," ,m, "," ,f, "," ,qds, sep="    ")
  #cat("<br/>")
}

processRequest <- function(fnType, dt){  
    return(switch(fnType,
       sum = dt[,eval(sum(m, na.rm=TRUE)), by=d]
       count = getCount(df, d, m),
       'default'
       )
    )   
}


library(data.table)

mbDT <- data.table(mbDS)

myfunction = function(dt, expr) {
  e = substitute(expr)
  dt[,eval(e),by=Species]
}

d <- quote(list(Opposition, Toss))

setkey(mbDT,Opposition)
#m <- quote(Test.Runs)
#mbDT[Opposition=="England" ,eval(m)]
#m <- quote(sum(Test.Runs))
#mbDT[Opposition=="England" ,eval(m)]

#m <- quote(list(T=sum(Test.Runs)))
#mbDT[Opposition=="England" ,eval(m)]

#m <- quote(list(Test.Runs=sum(Test.Runs)))
#mbDT[Opposition=="England" ,eval(m)]

#m <- quote(list(Test.Runs=sum(Test.Runs)))
#mbDT[,eval(m), by=d]


## Operate on a data.frame, split by "by", by could be a vector or a single filed
## aggreage by columns in vector on, on could be a single field also
getSum <- function(dt, onVar, byVar) {
    e <- substitute(onVar)
    j <- eval(e)
    print(j)
    dt[,eval(j),by=byVar]
}

m <- "Test.Runs"
d<- "Opposition"
#expr <- substitute(list(m=sum(m, na.rm=TRUE)))
expr <- paste("list(",m,"=sum(",m,",na.rm=TRUE))", sep="")
getSum(mbDT, paste("sum(",m,")", sep=""), d)


writeResponse <- function(responseDF, df) {
  sJSON <- paste('{"', m, '":', toJSON(responseDF[,m]), sep="") #JSON String is not closed.
  
  if(!is.null(g) && g == "t") {
    latName <- paste(m, ".Lat", sep="")
    lngName <- paste(m, ".Lng", sep="")
    sJSON <- paste(sJSON, ', "responseJSON": [', sep="")
    JSON <- with(head(df), paste('{"d":"',responseDF[m]
                               '", "center": new google.maps.LatLng(',
                                df[latName][df[m]==responeDF[m]][1], ',', df[lngName][df[m]==responeDF[m]][1], 
                               '), "data":', mbDS$Test.Runs, 
                                '},', sep=""))
    
  } else
    sJSON = paste(sJSON, "}", sep="")
  #Write response back
  cat(sJSON)
  #cat(ls())
  rm(list=ls()) #Clean up everything - There has to be a better way than this.
  #cat(ls())
}

writeError <- function() {
  sJSON <- paste('{ "Error":"An Error Occured :-( "}' , sep="") 
}

#Step 1 - Read Request, query string
readRequest()

#Step 2 - load environment
df <- loadDS(qds)

if(!is.null(df)) 
{
  dt <- data.table(df[order(df[,d]),])
  #Step 3 - Process Request 
  responseDF <- processRequest(dt)
  if(!is.null(responseDF)) {
    #Step 4 - Write Response
    writeResponse(responseDF)
  }
  else {
   writeError(); 
  }
} else {
  writeError();
}


 