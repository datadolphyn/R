# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library(RJSONIO)
library(stringr)

#Set content type for output
setContentType("text/html")
#Query Engine - dfp datafilepath 
dfp <- NULL
udir <- NULL

#Convert to a ROW JSON
getRowWiseJson <- function (jsonDT) {
  row.json <- apply(jsonDT, 1, toJSON)
  json.st <- paste('[', paste(row.json, collapse=', '), ']')
  return (json.st)
}


# Read SachinTestRecords.csv
readDataSet <- function(file.name) {
  return(
    read.csv(file.name, na.strings="-", as.is=TRUE, header=TRUE, 
             stringsAsFactors=FALSE, strip.white=TRUE)
  )
}
readRequest <- function() {
  #GetString <- str(GET)
  #cat(GetString)
  dfp <<- GET$dfp
  udir <<- GET$udir
  #cat(d, "," ,m, "," ,f, "," ,qds, sep="    ")
  #cat("<br/>")
}
writeResponse <- function(df, datafile, dsID) {
  sJSON = paste('{"upad":"', datafile,'","dsID":"', dsID,'","colTypes":',toJSON(unlist(lapply(df, class), use.names=FALSE)),', "rows":', getRowWiseJson(head(df,50)), '}', sep="")
  #Write response back
  cat(sJSON)
}
#Step 1 - Read Request, query string
readRequest()

#Step 2 - load environment
df <- readDataSet(dfp)
#Remove white space in col names
colnames(df) <- str_replace(colnames(df), ' ', '')

#Get Unique ID
dsID <- paste("upad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
out.file.csv <- paste(udir,"analyzed/", dsID, ".csv", sep="")
#out.file.JSON <- paste("pads/meta/",dsID, ".JSON", sep="")
out.file.R.dataframes <- paste(udir, dsID, ".RData", sep="")
write.csv(df, out.file.csv)

# Write JSON
#jsonSt <- paste('{ 
#                "name":"', file.name, '",
#                "id":"', dsID, '",', sep="");
#dlist <- paste()
#                "dList": {
#                  "Player":', toJSON(levels(factor(ipl$Player))), '
#                },
#                "mList": {
#                  "Mat":"",
#                  "Sixes":""
#                },
#                "dListType": {
#                },
#                "source":"CricInfo"
#        }', sep="")
#file.out <- file(out.file.JSON, 'wt')
#cat(jsonSt, file=file.out, fill = TRUE)
#close(file.out)
#Save transformed dataset as data.frame for later reading
assign(dsID, df)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)

#Step 4 - Write Response
writeResponse(df, out.file.R.dataframes, dsID)
