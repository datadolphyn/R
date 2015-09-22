# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.
library(plyr)
library(RJSONIO)
library(data.table)
#Set content type for output
setContentType("text/html")
#Query Engine - qds -  query this database, d=dimension, m=measure, f=funciton 
qds <- d <- m <- f <- g <- fv <- NULL

#Convert to a ROW JSON
getRowWiseJson <- function (jsonDT) {
  row.json <- apply(jsonDT, 1, toJSON)
  json.st <- paste('[', paste(row.json, collapse=', '), ']')
  return (json.st)
}

#load pre-analyzed and saved dataframe
loadDS <- function(qds) {
  if(exists(qds, envir=.GlobalEnv)) {
      #lpads <- get("pads", envir=.GlobalEnv)
      #cat(exists(pads$sysID[pads$id==qds], envir=.GlobalEnv), "<br/>")
      return(get(qds, envir=.GlobalEnv))    #get a local copy in this enviornment
    } else {
      #print("PAD didn't exist. Check to see if you start up R script in RApache ran correctly.")
      cat(paste('{"response_code": "error", "error_message": "The pad ', qds, ' does not exist. Check to see if you start up R script in RApache ran correctly."}', sep=""))
    }
}

getData <- function(dat, expr, gby) {
  e <- substitute(expr)
  b <- substitute(gby)
  #cat("<br/>", gby)
  #cat(b)
  #cat(dat[,eval(e), by=gby])
  return(dat[,eval(e),by=b])
}
#getData(padDT, sum(eval(m), na.rm=TRUE), d)

readRequest <- function() {
  #GetString <- str(GET)
  #cat(GetString)
  qds <<- GET$qds
  d <<- GET$d
  m <<- GET$m
  f <<- GET$f
  g <<- GET$g
  fv <<- GET$fv
  #cat(d, "," ,m, "," ,f, "," ,qds, sep="    ")
  #cat("<br/>")
}
#Proecess Request
processRequest <- function(fnType, padDT, mea, dim, fvalue){  
  rs <- NULL
  if(is.null(fvalue)) { 
    switch(fnType,
           d={rs <- padDT[,list(m=get(mea)),by=dim]},       #default by accpets character function
           s= { rs <- padDT[, list(m=sum(get(mea), na.rm=TRUE)), by=dim]}, #sum
           c= {rs <- padDT[, list(m=length(na.omit(get(mea)))), by=dim]}, #count      
           m= {rs <- padDT[,list(m=mean(get(mea), na.rm=TRUE)),by=eval(dim)]},  #Average, eval for dim also works
           'default'
    )
  } else {
    switch(fnType,
           d={rs <- padDT[,list(m=get(mea)),by=dim][get(dim)==fvalue]},       #default by accpets character function
           s= { rs <- padDT[, list(m=sum(get(mea), na.rm=TRUE)), by=dim][get(dim)==fvalue]}, #sum
           c= {rs <- padDT[, list(m=length(na.omit(get(mea)))), by=dim][get(dim)==fvalue]}, #count      
           m= {rs <- padDT[,list(m=mean(get(mea), na.rm=TRUE)),by=eval(dim)][get(dim)==fvalue]},  #Average, eval for dim also works
           'default'
    )    
  }
   return (rs)
}
#test this out
#dt <- data.table(iris)
#m <- "Sepal.Length"
#d <- "Species"
#fv <- NULL
#processRequest("s", padDT=dt, mea=m, dim=d, fvalue=fv)

writeResponse <- function(responseDF, padDT) {
  if(is.null(d)) {
    responseDF$d = ""
  } else #Change the name to d
    setnames(responseDF, 1, "d")
  #sJSON <- paste('{"response_code": "ok","', m, '":', toJSON(responseDF[,m]), sep="") #JSON String is not closed.
  sJSON <- paste('{"response_code": "ok","chartData":', getRowWiseJson(responseDF), sep="") #JSON String is not closed.
  if(!is.null(g) && g == "t") {
    latName <- paste(d, ".Lat", sep="")
    lngName <- paste(d, ".Lng", sep="")
    sJSON <- paste(sJSON, ', "mapsJSON": [', sep="")
    for(i in 1:nrow(responseDF)) {
      key <- responseDF[,d][i]
      #cat(paste ("<br/>", key , ", what?"))
      j <- match(key, padDT[,d])
      sJSON <- paste(sJSON, '{"d":"', key,
                               '", "lat":', 
                                padDT[latName][j,],
                              ', "lng":', padDT[lngName][j,], 
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
  rm(list=ls()) #Clean up everything - There has to be a better way than this.
  #cat(ls())
}

#Step 1 - Read Request, query string
readRequest()

#Step 2 - load environment
padDT <- data.table(loadDS(qds))

#Step 3 - Process Request 
responseDF <- processRequest(f, padDT, mea=m, dim=d, fvalue=fv)

#Step 4 - Write Response
writeResponse(responseDF, padDT)



