# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

rm (list = ls())
setwd("~/Toga")
library(XML)
library("chron")
library(plyr)
library(stringr)
library("RJSONIO")

#############################
# Olympic Tables
#library(XML)
#theurl <- "http://en.wikipedia.org/wiki/Brazil_national_football_team"
#tables <- readHTMLTable(theurl)
#n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
#the picked table is the longest one on the page
#braz <- tables[[which.max(n.rows)]]
#print(braz)
############################  
fixit <- function(x) { # Fix rowspan issue where there is single rank assing to multiple rows
  if(nchar(x[1]) > 3) { # other than rank
    for(i in length(x):2) { # Shift items by 1
      x[i] = x[i-1]
    }
    x[1] = -1 #Assign rank column to -1
  }
  return(x)
}
#tl <- rbind(n, ddply(medals, .(V1), temp))

readWikiOT <- function(d) { #Read Wiki olympic medal tables
  whichTable <- 1
  print(d$year) 
  if(!(d$year %in% c(1916, 1940, 1944))) { #Code breaks here due to wiki issues
    #whichTable <- 3
    if(d$year %in% c(1928, 1948, 1972, 1980,1996))
      whichTable <- 2
    if(d$year %in% c(1960, 2000, 2004, 2008, 2012))
      whichTable <- 3    
    print(whichTable)
    medals <- NULL
    if(d$year == 1992) { # Remove subscript and superscript text
      overview <- htmlParse(d$url,encoding="UTF-8")
      temp<-getNodeSet(overview, "/*//sup")
      removeNodes(temp)
      medals <- try(readHTMLTable(overview, header=FALSE, skip.rows=1, which=whichTable, stringsAsFactors = FALSE)) 
    } else
      medals <- try(readHTMLTable(d$url, header=FALSE, skip.rows=1, which=whichTable, stringsAsFactors = FALSE)) 
    
    medals <- medals[-nrow(medals),]
    if(d$year < 1992) 
      medals <- ddply(medals, .(V1), fixit)
    medals <- medals[, -c(1)]  
    #colnames(medals) <- c('Nation', 'Gold', 'Silver', 'Bronze', 'Total')
    medals <- tm <- with(medals, data.frame( 
                           #Nation=str_trim(sapply(strsplit(str_trim(medals$V2), " \\("), '[', 1)), 
                           #NationCode=sapply(strsplit(str_trim(sapply(strsplit(str_trim(medals$V2), " \\)"), '[', 2)), "\\)"), '[',1),
                           Country=substr(medals$V2, 1, nchar(medals$V2)-6),
                                             CountryCode=substr(medals$V2, nchar(medals$V2)-3, nchar(medals$V2)-1),
                           Gold=as.numeric(medals$V3), Silver=as.numeric(medals$V4), Bronze=as.numeric(medals$V5),
                           Total=as.numeric(medals$V6)))
    #medals$Rank <- rank(medals$V3)
    #medals <- medals[!c("Rank")]
    return (medals) 
  }
}

year <- seq(1896, 2012, by=4)
url <- paste("http://en.wikipedia.org/wiki/", year, "_Summer_Olympics_medal_table", sep="")
om <- data.frame(year=year, url=url, stringsAsFactors = FALSE) #Indian movies
omDS <- ddply(om, .(year), readWikiOT)
write.csv(omDS, "SummerOlympic-Medals-1896-2012.csv",row.names=FALSE)

### Read Bollywood.csv
in.file.name <- "datasets/BollywoodCinema-1940-2008.csv"
dt <- data.frame(read.csv(in.file.name, na.strings="-", as.is=TRUE, header=TRUE, 
                          stringsAsFactors=FALSE, strip.white=TRUE))
#Get Unique ID
dsID <- paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
out.file.csv <- paste("pads/data/",dsID, ".csv", sep="")
out.file.JSON <- paste("pads/meta/",dsID, ".JSON", sep="")
out.file.R.dataframes <- paste("pads/cache/", dsID, ".RData", sep="")
write.csv(dt, out.file.csv)
jsonSt <- paste('{ 
                "datasetName": "Bollywood Cinema (1940-2008)",
                "id":"', dsID, '",
                "dList": {
                "Year":', toJSON(levels(dt$Year)), ',
                "Director":', toJSON(levels(dt$Director)), '
                },
                "mList": {
                "Genre1":"", 
                "Genre2": ""
                },
                "dListType": {
                },
                "source":"Wikipedia"
                }', sep="")
file.out <- file(out.file.JSON, 'wt')
cat(jsonSt, file=file.out, fill = TRUE)
close(file.out)
#Save transformed dataset as data.frame for later reading
assign(dsID, dt)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
pads <- rbind(pads, data.table(id=dsID, title="Bollywood Cinema (1940-2008)", 
                               subtitle="Bollywood Movies from 1940 including director, cast and genre.", 
                               records=nrow(dt), analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                               cache=out.file.R.dataframes, source="Wiki")
)
#url <- paste("http://www.votesmart.org/candidate/evaluations/", 1:50 , sep = "")
#res <- llply(url, function(i) readHTMLtable(i))