# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library("RJSONIO")
 rm (list = ls())

# Read SachinTestRecords.csv

mb <- read.csv("datasets/SachinTestRecords.csv", as.is=TRUE, header=TRUE, stringsAsFactors=FALSE, strip.white=TRUE)
#mb <- read.csv("datasets/SachinTestRecords.csv", header=TRUE,strip.white=TRUE)
nRows <- nrow(mb)
nCols <- ncol(mb)
cNames <- colnames(mb)
cClass <- sapply(mb, class)
iCols <- list(name=cNames[1], class=class(mb[,1]), sample=head(mb[,1]))
for(i in 2:5) {
	iCols <- list(iCols, list(name=cNames[i], class=class(mb[,i]), sample=head(mb[,i])))
}
metaData <- list(nRows=nRows, nCols=nCols, iCols)
jsonString <- toJSON(metaData)