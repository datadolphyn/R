# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.
rm (list = ls("PADSEnv"))
padsEnv <- new.env()
padsFile <- "/Users/homemac/pads.json"

getPADS <- function()
{
  library("RJSONIO")
  con <- file(padsFile)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Convert the list to a matrix and then to a data frame
  m <- matrix(unlist(data.json), ncol=2, byrow=TRUE)
  pads <- data.frame(m, stringsAsFactors=FALSE)
  colnames(pads) <- c("id", "location")
  return (pads)
}

padsEnv$pads <- getPADS()
#Load all the PADS
sysID <- NULL
for(i in 1:length(padsEnv$pads)) {
   sysID[i] <- load(padsEnv$pads$location[i], padsEnv)
}
padsEnv$pads$sysID <- sysID
attach(padsEnv, name='PADSEnv')
# All Done .rAenv
rm(list=ls())
