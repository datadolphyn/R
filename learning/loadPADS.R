# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#setContentType("text/html")
setwd("/Users/homemac/Toga")
#read pre-analyzed data-sets file
#padsFile <- "pads/pads.json"
#getPADS <- function()
#{
#  library("RJSONIO")
#  con <- file(padsFile)
#  data.json <- fromJSON(paste(readLines(con), collapse=""))
#  close(con)
  #Convert the list to a matrix and then to a data frame
#  m <- matrix(unlist(data.json), ncol=10, byrow=TRUE)
#  pads <- data.frame(m, stringsAsFactors=FALSE)
  #colnames(pads) <- c("id", "location")
#  return (pads)
#}
#SYS <- new.env(hash = TRUE, size = NA)
#Read pads configuration
#pads <- getPADS()
pads <- data.frame(read.csv("Alto/pads/pads.csv", header=TRUE, sep=",",stringsAsFactors=FALSE ))
for(i in 1:nrow(pads)) {
  cat(pads$cache[i])
  cat("<br/>")
  #Load all the PADS
  #cat(pads$cache[i])
  #cat("<br/>")
  cat(try(load(pads$cache[i], envir=.GlobalEnv), FALSE))
  #try(load(pads$cache[i], envir=SYS), FALSE)
  #unlink(pads$cache[i])
  #cat("<br/>")
  
}
cat(ls(pattern="pad[0-9]", envir=.GlobalEnv))
# All Done - Al these variables get loaded in .GlobalEnv, run "search" to see.

