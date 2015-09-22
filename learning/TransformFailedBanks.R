# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

rm (list = ls())
setwd("~/Toga/Alto")
library("chron")
library("plyr")
library("RJSONIO")
#Get Unique ID
dsID <- paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
out.file.csv <- paste("pads/data/",dsID, ".csv", sep="")
out.file.JSON <- paste("pads/meta/",dsID, ".JSON", sep="")
out.file.R.dataframes <- paste("pads/cache/", dsID, ".RData", sep="")
in.file.name <- "datasets/failedbanks/failedbanksdata.csv"

lMonths <- c("January","February","March", "April","May","June","July","August","September", "October","November","December")
lDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Read *.csv
readDataSet <- function(file.name) {
  return(
    read.csv(file.name, na.strings="-", as.is=TRUE, header=TRUE, 
             stringsAsFactors=FALSE, strip.white=TRUE)
  )
}

# Transform raw dataset
transformDataSet <- function(df) {
tdf <- mutate(df,
      Date=as.Date(Date, "%m/%d/%Y"),
      Day=factor(weekdays(Date), levels=lDays, ordered=TRUE), 
      Month=factor(months(Date), levels=lMonths, ordered=TRUE), 
      Year=years(Date),
      Bank.Name=Bank.Name, 
      City=as.factor(City), 
      State=as.factor(State),
      Assets=as.numeric(gsub('[\\$,]', '', Assets)),
      Deposits=as.numeric(gsub('[\\$,]', '', Deposits)),
      Branches=as.numeric(Branches),
      FDIC.Cost=as.numeric(gsub('[\\$,]', '', FDIC.Cost)),
      Lat=as.numeric(Lat),  
      Lng=as.numeric(Lng)
    )
return(with(tdf, data.frame(Date, Day, Month, Year,
              Bank.Name, City, State, Assets, Deposits, Branches, FDIC.Cost,
              Lat, Lng
        ))
       )
}

writeTransformedDS <- function(df, file.name) {
  write.csv(df, file.name)
}

writeJSON <- function(df, file.JSON, id) {
  
  jsonSt <- paste('{ 
                "datasetName": "FailedBanks",
                "dsID": id, 
                "dList": {
                  "Year":', toJSON(levels(df$Year)), ',
                  "Month":', toJSON(levels(df$Month)), ',
                  "Day":', toJSON(levels(df$Day)), ',
                  "Bank.Name":', toJSON(df$Bank.Name), ',
                  "City":', toJSON(levels(df$City)), ',
                  "State":', toJSON(levels(df$State)),
                '},
                "mList": {
                  "Assets":"",
                  "Deposits":"",
                  "Branches":"",
                  "FDIC.Cost":""
                }
        }')
  file.out <- file(file.JSON, 'wt')
  cat(jsonSt, file=file.out, fill = TRUE)
  close(file.out)
}

# Read Raw File and Transform it
fbDS <- transformDataSet(readDataSet(in.file.name))

#Write Transformed data set to a CSV file
writeTransformedDS(fbDS, out.file.csv)

#Generate JSON with m and d
writeJSON(fbDS, out.file.JSON, id)

#Save transformed dataset as data.frame for later reading
#save(fbDS, file=out.file.R.dataframes)
#Save transformed dataset as data.frame for later reading
assign(dsID, fbDS)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
pads <- rbind(pads, data.table(id=dsID, title="Failed Banks Analysis in US (2008-2011)", 
                               subtitle="This data set explores US Banks that perished during the 2008 downturn.", 
                   records=nrow(fbDS), analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                               cache=out.file.R.dataframes, source="Federal Reserve")
              )