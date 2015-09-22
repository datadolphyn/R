# Utility Funcitons
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.



#
#open a log file with today's date
#
openLogfile <- function(){
  file.log <- file( paste("logs/pads_status.log",dataset, Sys.Date(), sep="_"), 'w')
  #initialize log file
  assign("file.log", file.log, envir=.GlobalEnv)}

#
#close the log file
#
closeLogfile <- function(){
  close(file.log)  
}

#
# log a message in a vector
#
logMessage <- function(mesg){
  #tryCatch(cat(paste(Sys.Date(), mesg, sep=":"), file = file.log), 
  #         error=function(e){
  #           openLogfile()   
  #           cat(paste(Sys.Date(), mesg, sep=":"), file = file.log)
  #          }, 
  #         finally={
  #        }
  #)
  #cat("\n", file = file.log)           
  vec.log[length(vec.log)+1] <<- paste(Sys.Date(), mesg, sep=":")
}

#
# dump logs to the file system
#
dumpLogs <- function(){
  openLogfile()
  cat(paste(vec.log, collpase="\n"), file = file.log)
  closeLogfile()
}

#
# Clean Meta files folder
#
cleanMetaFiles <- function(){
  folder.meta <- "./pads/meta/"
  meta.files <- list.files(path=folder.meta, pattern="*.json")
  for(i in meta.files) {
    meta.file.name <- paste(folder.meta, i, sep="")
    #delete the file
    unlink(meta.file.name)
  }   
}

#
# Clean csv files folder
#
cleanDataFiles <- function(){
  folder.data <- "./pads/data/"
  data.files <- list.files(path=folder.data, pattern="*.csv")
  for(i in data.files) {
    data.file.name <- paste(folder.data, i, sep="")
    #delete the file
    unlink(data.file.name)
  }   
}

#
# Clean cache files folder
#
cleanCacheFiles <- function(){
  folder.cache <- "./pads/cache/"
  cache.files <- list.files(path=folder.cache, pattern="*.rdata")
  for(i in cache.files) {
    cache.file.name <- paste(folder.cache, i, sep="")
    #delete the file
    unlink(cache.file.name)
  }   
}

# 
#read a csv file 
#
readFile <- function(file.location) {
  out <- tryCatch(
      read.csv(file.location, as.is=TRUE, header=TRUE, stringsAsFactors=FALSE, strip.white=TRUE),
      error=function(e) {
        message(paste("Unable to read the file:", pad.location))
        message("Error message:")
        message(e)
        # Choose a return value in case of error
        return(NA)
      },
      finally={
        #message(paste("Processed URL:", url))
        #message("Some other message at the end")
    }
  )    
  return(out)
}

#
#write a csv file 
#
saveData <- function(data, file.location){
  write.csv(data, file.location, row.names=F)
}


#
# remove leading/trailing spaces from a data.frame
#
trimData <- function(data){
  return(data.frame(lapply(data, function(x) gsub("(^ +)|( +$)", "", x)), stringsAsFactors=F))
}

#
# replace NA in every column with zeros
#
replaceNAWithZeros <- function(data)  {
  data[is.na(data)] <- 0 # replace NA with 0s
  return(data)
}

#
# replace NULL in every cell with zeros
#
replaceNULLWithZeros <- function(data)  {
  data[is.null(data)] <- 0 # replace null with 0s
  return(data)
}

#
# replace NULL in every cell with NA
#
replaceNULLWithNA <- function(data)  {
  data[is.null(data)] <- NA # replace null with 0s
  return(data)
}

#
#get row wise json
#
getRowWiseJson <- function (jsonDT) {
  require("RJSONIO")
  row.json <- apply(jsonDT, 1, toJSON)
  json.st <- paste('[', paste(row.json, collapse=', '), ']')
  return (json.st)
}

#
#get unique ID for pads
#
getPadUID <- function(){
  systime <- as.character(unclass(Sys.time()))
  return(paste("pad", substr(systime, 1,10), substr(systime, 12,16), sep=""))
}

#
#get GUID 
#
getGUID <- function() {
  baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
  
  paste(
    substr(baseuuid,1,8),
    "-",
    substr(baseuuid,9,12),
    "-",
    "4",
    substr(baseuuid,13,15),
    "-",
    sample(c("8","9","a","b"),1),
    substr(baseuuid,16,18),
    "-",
    substr(baseuuid,19,30),
    sep="",
    collapse=""
  )
}

#
#get csv data file url
#
getDataFileURL <- function(id) {
  return(paste("./pads/data/",id, ".csv", sep=""))
}

#
#get JSON data file url
#
getMetaDataFileURL <- function(id) {
  return(paste("./pads/meta/",id, ".json", sep=""))
}

#
#get  R.data file url
#
getCacheDataFileURL <- function(id) {
  return(paste("./pads/cache/",id, ".rdata", sep=""))
}

#
# Save the JSON Meta data for a PAD
#
saveMetaData <- function(out.file, jsonSt){
  file.out <- file(out.file, 'wt')
  cat(jsonSt, file=file.out, fill = TRUE)
  close(file.out)
}

#
#replace meta chars from an array of col names
#
replaceMetaChars <- function(col.names) {
  return(gsub("[^A-Za-z0-9]+", "_", col.names))
}

#
#remove meta chars from an array of values
#
removeMetaChars <- function(values) {
  return(gsub("[^A-Za-z0-9 ]+", "", values))
}

#
#remove non-numeric chars from an array of values
#
removeNonNumericChars <- function(values) {
  return(gsub("[^0-9.]+", "", values, fixed=T))
}


#
#remove meta from numeric columns
#
removeMetaFromNumeric <- function(values) { 
  #as.numeric( gsub("(,+)|(%+)|(\\$+)","",x) ) 
  return(as.numeric(gsub("[^0-9.]","",values)))
}

#
#camel case
#
tocamel <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


#
# clean a name
#
cleanName <- function(name) {
  clean1 <-gsub("[,]*[ ]*[&]*[ ]+(the|inc|llc|llp|ltd|limited|lp|ll|affiliates|incorporated|incorporation|corp|co|corp|corporation|stores|international|n\\.a|u\\.s\\.a)*[\\.]*[,]*$", "", name,  ignore.case = T)
  clean2 <- gsub("[,]*[ ]*[&]*[ ]+(the|inc|llc|llp|ltd|limited|lp|affiliates|incorporated|incorporation|corp|co|corp|corporation|stores|international|n\\.a|u\\.s\\.a)*[\\.]*[,]*$", "", clean1,  ignore.case = T)
  clean3 <- gsub("[&]", "and", clean2)
  clean4 <- gsub("-", "", clean3)
  #clean5 <- gsub("[^a-zA-Z]", "_" , clean4)
  #clean5 <- gsub("(_)+", "_" , unique(clean4))
  return (clean4)
}

#
# Get Class function to return a single class for every object
#
getClass <- function(obj) {
  obj.class <- class(obj)
  if(length(obj.class) > 1)
    return(obj.class[2])
  else
    return(obj.class)
}

#
# Generate solr doc from pad meta
#
generateSolrDocFromPADMeta <- function(meta.data) {
  generateSolrDoc(meta.data$id, meta.data$title, meta.data$category, 
                       meta.data$subcategory,meta.data$tags)
}

#
# Generate solr doc and append it to a solr file for later processing
#
generateSolrDoc <- function(id, title, category, subcategory, tags) {
  file.out <- file("./pads/solr-docs.json", "r+")
  lines <- readLines(file.out)
  if(length(lines) != 0)
    solr.index.docs <- fromJSON(paste(lines, collapse=""))
  doc <- list("type"="pad", "id" = id, "title" = title, "category"=category, 
              "subcategory"=subcategory, "tags"=tags)
  solr.index.docs[[length(solr.index.docs)+1]] <- doc
  cat(toJSON(solr.index.docs), file=file.out, fill = TRUE)
  close(file.out)
}

#
# Save solr.index.doc to file system for later processing
#
persistSolrIndex <- function(solr.index.docs, source.name="") {
  file.out <- file(paste("./pads/solr-docs-", source.name, ".json", sep=""), "w")
  cat(toJSON(solr.index.docs), file=file.out, fill = TRUE)
  close(file.out)  
}

#
# Update multiple pads in a System PADS table
#
updateSystemPads <- function(file="./pads/QualifiedPADS.csv") {
  pads <- read.csv(file,  stringsAsFactors=FALSE)
  for(i in 1:nrow(pads)) {
    print(paste("#", i, "Updating...", pads$id[i], sep=" "))
    updateSystemPad(pads$id[i], pads$title[i], pads$desc[i], pads$category[i], 
                    pads$subcategory[i], pads$src[i], pads$tags[i])
    
    ###### Update solr document
    generateSolrDoc(pads$id[i], pads$title[i], pads$category[i], pads$subcategory[i],  
                    pads$src[i], pads$tags[i])
  }
}

#
# get age 
#
getAge <- function(dob, as.of.when, units){
  switch(units, 
         w = {# weeks
              return(difftime(strptime(as.of.when, format = "%Y/%m/%d"),
                strptime(dob, format = "%Y/%m/%d"),units="weeks"))
         },
         m = {
                # months
                return((as.yearmon(strptime(as.of.when, format = "%Y/%m/%d"))-
                  as.yearmon(strptime(dob, format = "%Y/%m/%d")))*12)
         },
         y = {
           return(year(strptime(as.of.when, format = "%Y/%m/%d"))-
             year(strptime(dob, format = "%Y/%m/%d")))
         },
  )
}
#
# get time period for a give vector of dates
#
getTimePeriod <- function (date.vector){
  yr <-  year(date.vector)
  min.year <- min(yr, na.rm=T)
  max.year <- max(yr, na.rm=T)
  time.period <- paste(min.year, max.year, sep="-")
  return(time.period)
}

#
# escpae meta chars
#
escapeMetaChars <- function(values){
    return(gsub("[^A-Za-z0-9 ]+", "", values))
}

#
# remove special chars
#
removeSpecialChars <- function (node) {
  #val = gsub("[[:space:]]", "", xmlValue(node))
  val = gsub("[\xc2\xa0]", "", xmlValue(node))
}

#
# set class num.with.dollar
#
setClass("num.with.dollar")
setAs("character", "num.with.dollar",
      function(from) as.numeric(gsub("$","",from, fixed=TRUE)))

#
# set class num.with.commas
#
setClass("num.with.commas")
setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from,  fixed=TRUE)))

#
# set class mix.dates
# an example - 02009-09-25 September 25, 2009
#
setClass("mixed.dates")
setAs("character", "mixed.dates", 
      function(from) as.Date(gsub("^0", "", str_extract(from, "^[0-9-]+")), "%Y-%m-%d"))

#
# revenrse string
# strReverse(c("abc", "Statistics"))
#
strReverse <- function(x)
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

#
# get older data
#
# x <- mdy(stats$effective_date)
getOlderDate <- function(x, year=1930){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}
