# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library(XML)
library("chron")
library(plyr)
library(stringr)
library("RJSONIO")
#library("data.table")
#colClasses <- c("character", "character", "character", "integer", "integer", "integer", "character", "character", "character", "character")
#Convert to a ROW JSON
getRowWiseJson <- function (jsonDT) {
  row.json <- apply(jsonDT, 1, toJSON)
  json.st <- paste('[', paste(row.json, collapse=', '), ']')
  return (json.st)
}
#get unique ID for pads
getUID <- function(){
  return(paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep=""))
}

#get csv data file url
getDataFileURL <- function(id) {
  return(paste("Alto/pads/data/",id, ".csv", sep=""))
}
#get JSON data file url
getMetaDataFileURL <- function(id) {
  return(paste("Alto/pads/meta/",id, ".JSON", sep=""))
}
#get  R.data file url
getCacheDataFileURL <- function(id) {
  return(paste("Alto/pads/cache/",id, ".RData", sep=""))
}

#############################
#  IPL Batsman Table
############################
url <- "http://stats.espncricinfo.com/indian-premier-league-2012/engine/records/batting/most_runs_career.html?id=6680;type=tournament"
ipl <- try(readHTMLTable(url, which=62, 
                       colClasses=c("factor", "integer", "integer", "integer", "integer", "character", 
                                    "numeric", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer"),
                       stringsAsFactors = FALSE))
ipl<-ipl[-seq(2,nrow(ipl),2),]
colnames(ipl) <- str_replace(colnames(ipl), ' ', '')
colnames(ipl) <- c("Player", "Mat",    "Inns",   "NO",     "Runs",   "HS",     
                   "Ave",    "BallsFaced",     "StrikeRate", "Hunderds", 
                   "Fifties", "Ducks", "Fours", "Sixes")
#dsID <- unclass(Sys.time())
dsID <- getUID()
out.file.csv <- getDataFileURL(dsID)
out.file.JSON <- getMetaDataFileURL(dsID)
out.file.R.dataframes <- getCacheDataFileURL(dsID) 
write.csv(ipl, out.file.csv)
jsonSt <- paste('{ 
                "name": "IPL-2012-MostSuccessfullBatsman",
                "id":"', dsID, '",
                "dList": {
                  "Player":', toJSON(levels(factor(ipl$Player))), '
                },
                "mList": {
                  "Mat":"",
                  "Inns":"",
                  "NO":"",
                  "Runs":"",
                  "HS":"",
                  "Ave":"",
                  "BallsFaced":"",
                  "StrikeRate":"",
                  "Hundreds":"",
                  "Fifties":"",
                  "Ducks":"",
                  "Fours":"",
                  "Sixes":""
                },
                "dListType": {
                },
                "source":"CricInfo"
        }', sep="")
file.out <- file(out.file.JSON, 'wt')
cat(jsonSt, file=file.out, fill = TRUE)
close(file.out)

#Save transformed dataset as data.frame for later reading
assign(dsID, ipl)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
#rbind(pads, c(dsID, "IPL 2012 Successful Batsman", "This dataset has batting records of top batsman in IPL5.", 
#              nrow(ipl), 0, 0, out.file.csv, out.file.JSON, out.file.R.dataframes, "CricInfo"))
# For the very first time...
pads <- data.frame(dsID, "IPL 2012 Successful Batsman", "This dataset has batting records of top batsman in IPL5.", 
                   nrow(ipl), 0, 0, out.file.JSON, out.file.csv, out.file.R.dataframes, "CricInfo")
colnames(pads) <- c("id", "title","subtitle","records","analyzed","stories","json","data", "cache", "source")


#############################
#  IPL Results
############################
lMonths <- c("January","February","March", "April","May","June","July","August","September", "October","November","December")
lDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

url <- "http://stats.espncricinfo.com/indian-premier-league-2012/engine/records/team/match_results.html?id=6680;type=tournament"
iplr <- try(readHTMLTable(url, which=62, 
                       stringsAsFactors = FALSE))
#Remove white space in col names
colnames(iplr) <- str_replace(colnames(iplr), ' ', '') 
iplr$MatchDate <- as.Date(iplr$MatchDate, format="%b %d, %Y")
iplr$Day=factor(weekdays(iplr$MatchDate), levels=lDays, ordered=TRUE)
iplr$Month=factor(months(iplr$MatchDate), levels=lMonths, ordered=TRUE) 
iplr$Quarter=quarters(iplr$MatchDate)
iplr$Year=years(iplr$MatchDate)
iplr$Margin1 <- as.numeric(str_trim(sapply(strsplit(as.character(iplr$Margin),' '), "[", 1)))
iplr$Margin2 <- as.factor(str_trim(sapply(strsplit(as.character(iplr$Margin),' '), "[", 2)))

if(iplr$Margin2=="wickets") {
  iplr$WinnerBatted <- 0
  iplr$WinnerBowled <- 1 
} else {
  iplr$WinnerBatted <- 1
  iplr$WinnerBowled <- 0
  
}

iplr$WinnerBatted <- ifelse(iplr$Margin2=="wickets", 0, 1)
iplr$WinnerBowled <- ifelse(iplr$Margin2=="wickets", 1, 0)

iplr <- with(iplr, data.frame(MatchDate, Day, Month, Quarter, Year, Ground, Team1, Team2, Winner, WinnerBatted, Margin, Margin1, Margin2))
#Get Unique ID
dsID <- getUID()
out.file.csv <- getDataFileURL(dsID)
out.file.JSON <- getMetaDataFileURL(dsID)
out.file.R.dataframes <- getCacheDataFileURL(dsID)
write.csv(iplr, out.file.csv)
jsonSt <- paste('{ 
                "datasetName": "IPL-2012-Results",
                "id":"', dsID, '",
                "dList": {
                  "Month":', toJSON(levels(iplr$Month)), ',
                  "Day":', toJSON(levels(iplr$Day)), ',
                  "Ground":', toJSON(levels(iplr$Ground)), ',
                  "Winner":', toJSON(levels(iplr$Winner)), '
                },
                "mList": {
                  "WinnerBatted":"", 
                  "Margin1": "",
                  "Margin2": ""
                },
                "dListType": {
                },
                "source":"CricInfo"
        }', sep="")
file.out <- file(out.file.JSON, 'wt')
cat(jsonSt, file=file.out, fill = TRUE)
close(file.out)
#Save transformed dataset as data.frame for later reading
assign(dsID, iplr)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
pads <- rbind(pads, data.frame(id=dsID, title="IPL 2012 Results", 
                               subtitle="This dataset contains results of all the matches played in IPL5.", 
                   records=nrow(iplr), analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                               cache=out.file.R.dataframes, source="CricInfo")
              )

#############################
# Get Wiki Most Valuable Football Clubs
############################
mostValuableFootballClubs <- function(pads) {       
  getWikiFC <- function(d) {  
    l <- try(readHTMLTable("http://en.wikipedia.org/wiki/Forbes%27_list_of_the_most_valuable_football_clubs", 
              header=FALSE, skip.rows=1, which=d$tableNo, stringsAsFactors = FALSE))
    l$year = d$year
    ifelse(d$year > 2010, return(with(l, data.frame(year=as.factor(year), club=V2, country=V3, value=as.integer(gsub(",","",V4)), revenue=as.integer(gsub(",","",V6))))), 
           return(with(l, data.frame(year=as.factor(year), club=V2, country=V3, value=as.integer(gsub(",","",V4)), revenue=as.integer(gsub(",","",V7))))))
  }
  d <- data.frame(year=2012:2007, tableNo=2:7) 
  mostValuedSoccerClubs <- ddply(d, .(year), getWikiFC)
  #Remove white space in col names
  colnames(mostValuedSoccerClubs) <- str_replace(colnames(mostValuedSoccerClubs), ' ', '') 
  #mostValuedSoccerClubs$year <- factor(mostValuedSoccerClubs$year, levels=c(2007,2008,2009,200,2011,2012), ordered=TRUE)
  #Get Unique ID
  dsID <- paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
  out.file.csv <- getDataFileURL(dsID)
  out.file.JSON <- getMetaDataFileURL(dsID)
  out.file.R.dataframes <- getCacheDataFileURL(dsID) 
  write.csv(mostValuedSoccerClubs, out.file.csv)
  jsonSt <- paste('{ 
                  "datasetName": "Value of Soccer Clubs Over the Years",
                  "id":"', dsID, '",
                  "dList": {
                    "year":', toJSON(levels(mostValuedSoccerClubs$year)), ',
                    "club":', toJSON(levels(mostValuedSoccerClubs$club)), ',
                    "country":', toJSON(levels(mostValuedSoccerClubs$country)), '
                  },
                  "mList": {
                    "value":"", 
                    "revenue": ""
                  },
                "dListType": {
                },
                "source":"Wikipedia"
          }', sep="")
  file.out <- file(out.file.JSON, 'wt')
  cat(jsonSt, file=file.out, fill = TRUE)
  close(file.out)
  #Save transformed dataset as data.frame for later reading
  assign(dsID, mostValuedSoccerClubs)
  names <- c(eval(dsID))
  save(list=names, file=out.file.R.dataframes)
  pads <- rbind(pads, data.frame(id=dsID, title="Value of Soccer Clubs Over the Years (2007-2012)", 
                                 subtitle="This dataset contains valuation and revnues of top soccer clubs.", 
                     records=nrow(mostValuedSoccerClubs), analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                                 cache=out.file.R.dataframes, source="Wiki")
                )
  return(pads)
}

#############################
#   Indian Blockbuster Hits
############################      
readBOC<- function(d) { #Read Box offic chart
    #print(d$url)        
    l <- try(readHTMLTable(d$url, 
             colClasses=c("integer", "character", "character", "FormattedInteger", "FormattedInteger", "FormattedInteger", "character"),
             skip.rows=c(1:2), which=14, stringsAsFactors = FALSE))
    l$year <- d$year
    return (l)
}
#URLs
set1 <- paste("http://www.boxofficeindia.com/showProd.php?itemCat=", 196:212, sep="") #1990-2006
set2 <- paste("http://www.boxofficeindia.com/showProd.php?itemCat=", 214:215, sep="")#2007-2008
set3 <- paste("http://www.boxofficeindia.com/showProd.php?itemCat=", 288, sep="") #2009
set4 <- paste("http://www.boxofficeindia.com/showProd.php?itemCat=", 318, sep="") #2010
url <- c(set1, set2, set3, set4)
iboh <- data.frame(year=1990:2010, url=url, stringsAsFactors = FALSE) #Indian Box Office Hits
ibohDS <- ddply(iboh, .(year), readBOC)
ibohDS <- ibohDS[c("year", "V1", "V2", "V3", "V4", "V5", "V6", "V7")]
colnames(ibohDS) <- c("Year", "Rank", "Title", "OpentingNote", "Net Gross", "Gross", "DistributorsShare", "Verdict")
#Remove white space in col names
#colnames(ibohDS) <- str_replace(colnames(ibohDS), ' ', '') 
ibohDS$Year <- factor(ibohDS$Year, levels=c(1990:2010), ordered=TRUE)
ibohDS$Rank <- factor(ibohDS$Rank, levels=c(1:30), ordered=TRUE)
ibohDS$Verdict <- factor(ibohDS$Verdict)
ibohDS$OpentingNote <- factor(ibohDS$OpentingNote)
#Get Unique ID
dsID <- paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
out.file.csv <- getDataFileURL(dsID)
out.file.JSON <- getMetaDataFileURL(dsID)
out.file.R.dataframes <- getCacheDataFileURL(dsID) 
write.csv(ibohDS, out.file.csv)
jsonSt <- paste('{ 
                "datasetName": "Bollywood Box Office Hits (1990-2010)",
                "id":"', dsID, '",
                "dList": {
                  "Year":', toJSON(levels(ibohDS$Year)), ',
                  "Rank":', toJSON(levels(ibohDS$Rank)), ',
                  "Verdict":', toJSON(levels(ibohDS$Verdict)), ',
                  "OpentingNote":', toJSON(levels(ibohDS$OpeningNote)), '
                },
                "mList": {
                  "Net Gross":"", 
                  "Gross": ""
                },
                "dListType": {
                },
                "source":"Wikipedia"
        }', sep="")
file.out <- file(out.file.JSON, 'wt')
cat(jsonSt, file=file.out, fill = TRUE)
close(file.out)
#Save transformed dataset as data.frame for later reading
assign(dsID, ibohDS)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
pads <- rbind(pads, data.frame(id=dsID, title="Bollywood Box Office Hits (1990-2010)", 
                               subtitle="Box office hits since 1990 from Bollywood including Rank, Gross, Net Gross and more.", 
                   records=nrow(ibohDS), analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                               cache=out.file.R.dataframes, source="Wiki")
              )
#Save Pad
savePads(pads)
#############################
# Indian Movies
############################  
readWikiMovies <- function(d) { #Read Wiki movie chart
   whichTable <- 4
    print(d$year) 
   if(d$year %in% c(1980, 1982:1985)) #Code breaks here due to wiki issues
      whichTable <- 3
   if(d$year == 1988)
      whichTable <- 2
    print(whichTable)
    l <- try(readHTMLTable(d$url, skip.rows=1, which=whichTable, stringsAsFactors = FALSE))
    l$year <- d$year
    return (with(l, data.frame(year, Title, Director,Cast, Genre)))
}
year <- 1940:2008
url <- paste("http://en.wikipedia.org/wiki/Bollywood_films_of_", year, sep="") #1940-2008
im <- data.frame(year=year, url=url, stringsAsFactors = FALSE) #Indian movies
imDS <- ddply(im, .(year), readWikiMovies)
imDS$Genre1 <- str_trim(sapply(strsplit(as.character(imDS$Genre),','), "[", 1))
imDS$Genre2 <- str_trim(sapply(strsplit(as.character(imDS$Genre),','), "[", 2))
imDS$Genre3 <- str_trim(sapply(strsplit(as.character(imDS$Genre),','), "[", 3))
write.csv(imDS, "BollywoodCinema-1940-2008.csv")

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


#############################
#  Consumer Sentiment Index (Source: http://research.stlouisfed.org/fred2/data/UMCSENT.txt)
############################
lMonths <- c("January","February","March", "April","May","June","July","August","September", "October","November","December")
url <-"http://research.stlouisfed.org/fred2/data/UMCSENT.txt"
csiDF <- read.table(file=url(url), sep="", skip=15,  col.names=c("DATE", "VALUE"), strip.white=TRUE, stringsAsFactors=FALSE)
lapply(csiDF, class)
csiDF$Date=as.Date(csiDF$DATE, format="%Y-%m-%d")
csiDF <- with(csiDF, data.frame(Date, Month=factor(months(Date), levels=lMonths, ordered=TRUE), 
                                Quarter=factor(quarters(Date)),
                                Year=factor(years(Date)), 
                                ConsumerSentiment=VALUE
              ))
#Get Unique ID
dsID <- paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
out.file.csv <- getDataFileURL(dsID)
out.file.JSON <- getMetaDataFileURL(dsID)
out.file.R.dataframes <- getCacheDataFileURL(dsID) 
write.csv(csiDF, out.file.csv)
jsonSt <- paste('{ 
                "datasetName": "University of Michigan: Consumer Sentiment (1978-2011)",
                "id":"', dsID, '",
                "dList": {
                  "Date":', toJSON(as.character(csiDF$Date)), ',
                  "Year":', toJSON(levels(csiDF$Year)), ',
                  "Month":', toJSON(levels(csiDF$Month)), ',
                  "Quarter":', toJSON(levels(csiDF$Quarter)), '
                },
                "mList": {
                  "ConsumerSentiment":""
                },
                "dListType": {
                },
                "source":"Thomson Reuters/University of Michigan"
        }', sep="")
file.out <- file(out.file.JSON, 'wt')
cat(jsonSt, file=file.out, fill = TRUE)
close(file.out)
#Save transformed dataset as data.frame for later reading
assign(dsID, csiDF)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
pads <- rbind(pads, data.frame(id=dsID, title="University of Michigan: Consumer Sentiment (1978-2011)", 
                               subtitle="Surveys of Consumers.", 
                               records=nrow(csiDF), analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                               cache=out.file.R.dataframes, source="Thomson Reuters/University of Michigan")
)
#Save Pad
savePads(pads)


#############################
#  PMI Composite Index (NAPM) (Source: http://research.stlouisfed.org/fred2/series/NAPM/downloaddata?cid=32295)
############################
lMonths <- c("January","February","March", "April","May","June","July","August","September", "October","November","December")
pmiDF <- read.csv(file="Alto/HistoricalPMI.csv", na.strings="-", as.is=TRUE, header=TRUE, 
                  stringsAsFactors=FALSE, strip.white=TRUE)
lapply(pmiDF, class)
pmiDF$Date=as.Date(pmiDF$DATE, format="%Y-%m-%d")
pmiDF <- with(pmiDF, data.frame(Date, Month=factor(months(Date), levels=lMonths, ordered=TRUE), 
                                Quarter=factor(quarters(Date)),
                                Year=factor(years(Date)), 
                                ManifacturingIndex=VALUE
))
#Get Unique ID
dsID <- paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
out.file.csv <- getDataFileURL(dsID)
out.file.JSON <- getMetaDataFileURL(dsID)
out.file.R.dataframes <- getCacheDataFileURL(dsID) 
write.csv(csiDF, out.file.csv)
jsonSt <- paste('{ 
                "datasetName": "ISM Manufacturing: PMI Composite Index (NAPM) (1948-2012)",
                "id":"', dsID, '",
                "dList": {
                "Date":', toJSON(as.character(csiDF$Date)), ',
                "Year":', toJSON(levels(csiDF$Year)), ',
                "Month":', toJSON(levels(csiDF$Month)), ',
                "Quarter":', toJSON(levels(csiDF$Quarter)), '
                },
                "mList": {
                "ManifacturingIndex":""
                },
                "dListType": {
                },
                "source":"FRED & Institute for Supply Management"
                }', sep="")
file.out <- file(out.file.JSON, 'wt')
cat(jsonSt, file=file.out, fill = TRUE)
close(file.out)
#Save transformed dataset as data.frame for later reading
assign(dsID, pmiDF)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
pads <- rbind(pads, data.frame(id=dsID, title="ISM Manufacturing: PMI Composite Index (NAPM) (1948-2012)", 
                               subtitle="Manufacturing ISM Report on Business", 
                               records=nrow(pmiDF), analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                               cache=out.file.R.dataframes, source="FRED & Institute for Supply Management")
)
#Save Pad
savePads(pads)

#############################
#  World's dangerours roads (Source: http://research.stlouisfed.org/fred2/series/NAPM/downloaddata?cid=32295)
############################
df <- read.csv(file="Alto/WorldDangeorusRoads.csv", na.strings="-", as.is=TRUE, header=TRUE, 
               stringsAsFactors=FALSE, strip.white=TRUE)
lapply(df, class)
colnames(df) <- str_replace_all(colnames(df), '[.]+', '_') #[^A-Za-z0-9-_]

#Get Unique ID
dsID <- paste("pad", substr(as.character(unclass(Sys.time())), 12,16), sep="")
out.file.csv <- getDataFileURL(dsID)
out.file.JSON <- getMetaDataFileURL(dsID)
out.file.R.dataframes <- getCacheDataFileURL(dsID) 
write.csv(df, out.file.csv)
jsonSt <- paste('{ 
                "datasetName": "Worlds Dangerorus Roads (2007)",
                "id":"', dsID, '",
                "dList": {
                "Country":', toJSON(df$Country), '
                },
                "mList": {
                "Population":"",
                "GNI_per_capita_US_2007_":"",
                "Number_of_registered_vehicles":"",
                "Road_deaths_reported_":"",
                "Road_deaths_estimated":"",
                "Deaths_in_4_wheel_vehicles_percent_of_total_":"",
                "Deaths_in_2_wheel_vehicles_percent_of_total_":"",
                "Cyclists_percent_of_total_road_deaths_":"",
                "Pedestrians_percent_of_total_road_deaths_":""
                },
                "dListType": {
                },
                "source":"World Heatlh Orgnaization"
                }', sep="")
file.out <- file(out.file.JSON, 'wt')
cat(jsonSt, file=file.out, fill = TRUE)
close(file.out)
#Save transformed dataset as data.frame for later reading
assign(dsID, df)
names <- c(eval(dsID))
save(list=names, file=out.file.R.dataframes)
pads <- rbind(pads, data.frame(id=dsID, title="Worlds Dangerorus Roads (2007)", 
                               subtitle="Worlds Dangerorus Roads", 
                               records=nrow(df), category="World",
                               subcategory="Freakonomics", analyzed=0, stories=0, json=out.file.JSON, data=out.file.csv,
                               cache=out.file.R.dataframes, source="World Heatlh Orgnaization")
)
#Save Pad
savePads(pads)

#Save Pads
savePads <- function (pads) {
  write.csv(pads, "Alto/pads/pads.csv", row.names = FALSE)
  pads.json <- paste('{"pads":', getRowWiseJson(pads), '}', sep="")
  writeLines(pads.json, "Alto/pads/pads.json")                 
}

#Read Pads
readPads <- function() {
  pads <- read.csv("Alto/pads/pads.csv", header=TRUE, sep=",",stringsAsFactors=FALSE )
  #setkey(pads, id)
  return(pads)
}
#remove pad
removePad <- function(pads, padID) {
  out.file.csv <- paste("Alto/pads/data/",padID, ".csv", sep="")
  out.file.JSON <- paste("Alto/pads/meta/",padID, ".JSON", sep="")
  out.file.R.dataframes <- paste("Alto/pads/cache/", padID, ".RData", sep="")
  file.remove <- c( out.file.csv,  out.file.JSON,  out.file.R.dataframes)
  unlink(file.remove)
  pads <- pads[!pads$id==padID,]
}
pads <- readPads()
#pads <- removePad(pads, "pad51237")
pads <- mostValuableFootballClubs(pads)
savePads(pads)