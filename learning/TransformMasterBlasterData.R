# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

rm (list = ls())
setwd("~/Toga")
library("chron")
library("plyr")
library("RJSONIO")
#Get Unique ID
dsID <- paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
out.file.csv <- paste("Alto/pads/data/",dsID, ".csv", sep="")
out.file.JSON <- paste("Alto/pads/meta/",dsID, ".JSON", sep="")
out.file.R.dataframes <- paste("Alto/pads/cache/", dsID, ".RData", sep="")
in.file.name <- "Alto/datasets/masterblaster/SachinTestRecords.csv"


lMonths <- c("January","February","March", "April","May","June","July","August","September", "October","November","December")
lDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Use Google's Geocoding service and geo code the data
getGeoCode <- function(gcStr, cnPre)
{
  library("RJSONIO")
  gcStr <- gsub(' ','%20',gcStr)
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',gcStr, sep="")
  #print(connectStr)
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  data.json <- unlist(data.json)
  lat <- data.json["results.geometry.location.lat"]
  lng <- data.json["results.geometry.location.lng"]
  gcodes <- c(lat, lng)
  names(gcodes) <- c(paste(cnPre,".Lat",sep=""),paste(cnPre, ".Lng", sep=""))
  return (gcodes)
}

# Read SachinTestRecords.csv
readDataSet <- function(file.name) {
  return(
    read.csv(file.name, na.strings="-", as.is=TRUE, header=TRUE, 
             stringsAsFactors=FALSE, strip.white=TRUE)
  )
}

# Transform raw dataset
transformDataSet <- function(df) {
tdf <- mutate(df,
      StartDate=as.Date(StartDate, "%m/%d/%Y"),
      Day=factor(weekdays(StartDate), levels=lDays, ordered=TRUE), 
      Month=factor(months(StartDate), levels=lMonths, ordered=TRUE), 
      Year=years(StartDate),
      Ing1_Runs=as.numeric(gsub('\\*', '', Bat1)), 
      Ing1_NotOut=ifelse(is.na(Bat1), 0, ifelse(grepl("\\*", Bat1),1,0)), 
      Ing2_Runs=as.numeric(gsub('\\*', '', Bat2)),
      Ing2_NotOut=as.factor(ifelse(grepl("\\*", Bat2),1,0)),
      Test_Runs=as.numeric(Runs),
    	Opposition=as.factor(Opposition), 
      Ground=as.factor(Ground), 
      Result=as.factor(Result), 
      Toss=as.factor(Toss),
      Country=as.factor(Country),
    	Ing1_Century=ifelse(is.na(Ing1_Runs), 0, ifelse(Ing1_Runs>=100, 1, 0)), 
      Ing2_Century=ifelse(is.na(Ing2_Runs), 0, ifelse(Ing2_Runs>=100, 1, 0)), 
      Test_Century=ifelse((Ing1_Century==1)|(Ing2_Century==1), 1, 0),
      Test_DCentury=ifelse((!is.na(Ing1_Runs) & Ing1_Runs >=200)|(!is.na(Ing2_Runs) & Ing2_Runs>=200), 1, 0),
      Home_Away=ifelse(!is.na(Country) & Country=="India", "Home", "Away"),
      Ground_Country=as.factor(paste(Ground,Country, sep=","))
    )
head(tdf)
return(with(tdf, data.frame(TestNo, StartDate, Day, Month, 
              Year, Ing1_Runs, Ing1_NotOut, Ing2_Runs, Ing2_NotOut, Test_Runs,
              Opposition, Ground, Toss, Result, 
              Ing1_Century, Ing2_Century, 
              Test_Century, Test_DCentury, Home_Away, Country, 
              Ground_Country,
              laply(Opposition, function(x){getGeoCode(x, "Opposition")}),              
              laply(Ground_Country, function(x){getGeoCode(x, "Ground")}))
            ))
}

writeTransformedDS <- function(df, file.name) {
  write.csv(df, file.name)
}

writeJSON <- function(df, file.JSON, id) {
  
  jsonSt <- paste('{ 
                "datasetName": "SachinTestRecords",
                "id": "', id, '", 
                "dList": {
                  "Year":', toJSON(levels(df$Year)), ',
                  "Month":', toJSON(levels(df$Month)), ',
                  "Day":', toJSON(levels(df$Day)), ',
                  "Opposition":', toJSON(levels(df$Opposition)), ',
                  "Ground":', toJSON(levels(df$Ground)), ',
                  "Result":', toJSON(levels(df$Result)), ',
                  "Toss":', toJSON(levels(df$Toss)),',
                  "HomeOrAway":', toJSON(levels(df$Home_Away)),
                '},
                "mList": {
                  "Ing1_Runs":"",
                  "Ing2_Runs":"",
                  "Test_Runs":"",
                  "Ing1_Century":"",
                  "Ing2_Century":"",
                  "Test_Century":""
                },
                "dListType": {
                  "Opposition": "Geo",
                  "Ground": "Geo"
                },
                "source":"CricInfo"
        }')
  file.out <- file(file.JSON, 'wt')
  cat(jsonSt, file=file.out, fill = TRUE)
  close(file.out)
}

# Read Master Blaster Raw File and Transform it
mbDS <- transformDataSet(readDataSet(in.file.name))

#Write Transformed data set to a CSV file
writeTransformedDS(mbDS, out.file.csv)

#Generate JSON with m and d
writeJSON(mbDS, out.file.JSON, dsID)

#Save transformed dataset as data.frame for later reading
assign(dsID, mbDS)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
pads <- rbind(pads, data.frame(id=dsID, title="Master Blaster - Sachin Tendulkar(1940-2008)", 
                               subtitle="This data set explores Sachin Tendulkar's glorious test cricket record.", 
                   records=nrow(mbDS), analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                               cache=out.file.R.dataframes, source="CricInfo")
              )
#save(mbDS, file=out.file.R.dataframes)
