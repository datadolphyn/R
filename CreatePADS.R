# Read a CSV file
# Classify the column types
# Determine measures and dimensions
# Generate meta data and store JSON
# Query Solr and store the information in SOLR
# Save the data as cache

# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

require("RJSONIO")
require("plyr") 
require("data.table")
require("stringr")
require("lubridate")
require("zoo")

source("UtilPADS.R")
source("ClassifyData.R")
source("MySQLFunctions.R")
source("MongoFns.R")
source("DefaultChartForPAD.R")

# initializeSystem <- function(stack=0){
#   
#   # turn off scientific formatting - getOption("scipen")
#   options(scipen=999)
#   
#   #initialize solr docs
#   #assign("solr.index.docs", list(), envir=.GlobalEnv)
#   
#   #mongo 
#   mongo <- getDefaultMongoDBCon()
#   assign("mongo", mongo, envir=.GlobalEnv)
#   
#   #initialize stack
#   mysql.db <- list(user="ddfin_dev", pass="BfNdW87Ym9FmcYj7", name="ddfin_dev", host="localhost")
#   if(stack == 1) {
#     #prod 
#     mysql.db <- list(user="ddfin_prod", pass="8MQ5CRDzQufHKXTx", name="ddfin_prod", host="localhost")
#   }
#   assign("mysql.db", mysql.db, envir=.GlobalEnv)
#   
#   #mysql 
#   mysql <- getMSDBCon()
#   assign("mysql", mysql, envir=.GlobalEnv)
#   
#   #error counter
#   assign("error.count", 0, envir=.GlobalEnv)
#   
#   #initialize log file
#   assign("vec.log", vector(), envir=.GlobalEnv)
#   
#   #verbose mode
#   assign("verbose", F, envir=.GlobalEnv)
#   
#   #min log
#   assign("log.all", F, envir=.GlobalEnv)
# }

#
# loadStates
#
loadStates <- function(){
  states <- data.table(readFile("./pads/raw-data/states_names.csv"))
  setkey(states, state_code)
  assign("states", states, envir=.GlobalEnv)
}



#
# Initialize Padification Process
#
initializeSystem <- function(stack=0{
  
  # turn off scientific formatting - getOption("scipen")
  options(scipen=999)
  
  # day and months facors
  lMonths <- c("January","February","March", "April","May","June","July","August","September", "October","November","December")
  lDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  assign("lMonths", lMonths, envir=.GlobalEnv)
  assign("lDays", lDays, envir=.GlobalEnv)
  
  #loadStates
  loadStates(
  
  #initialize solr docs
  #assign("solr.index.docs", list(), envir=.GlobalEnv)
  
  #mongo 
  mongo <- getDefaultMongoDBCon()
  assign("mongo", mongo, envir=.GlobalEnv)
  
  #initialize stack
  mysql.db <- list(user="ddfin_dev", pass="b31nd1$$", name="ddfin_dev", host="localhost")  
  if(stack == 1) {
    #prod 
    mysql.db <- list(user="ddfin_prod", pass="g0f0r1t", name="ddfin_prod", host="localhost")
  }
  assign("mysql.db", mysql.db, envir=.GlobalEnv)
  
  #mysql 
  mysql <- getMSDBCon()
  assign("mysql", mysql, envir=.GlobalEnv)
  
  #error counter
  assign("error.count", 0, envir=.GlobalEnv)
  
  #initialize log file
  assign("vec.log", vector(), envir=.GlobalEnv)
  
  #verbose mode
  assign("verbose", F, envir=.GlobalEnv)

  #min log
  assign("log.all", F, envir=.GlobalEnv)
}

#
#cleanup
#
cleaupSystem <- function() {
  
  #disconnect Mongo
  disconnectMongoDB(mongo) 
  
  # disconnect msdb
  disconnectMSDB(mysql)
  
  #save the logs
  dumpLogs()
  
  # save the solr index to FS for later editing - this is now done through mysql
  #persistSolrIndex(solr.index.docs, dataset)
}

#
# create pad - the padification process starts here
#
createPAD <- function(pad, title, desc, category, subcategory, data.source, source.file, 
                      tags, pagetag) {
  
  if(ncol(pad) == 0)  stop('dolphy can not padify a pad that has zero columns')
    
  #only continue if there is one or more colum
  if(verbose) print(paste("Starting the padification process for source file", source.file, sep=":"))
  logMessage(paste("Starting the padification process for source file", source.file, sep=":"))
  
  #Very first task, classify pad
  pad.classified <- classifyData(pad)
  
  #if(verbose) print(pad.classified)
  
  # Generate extra date columns
  #if(length(which(col.classes == "Date")) > 0) {
  #  pad.classified <- generateExtraDateCols(pad.classified, which(col.classes == "Date"))
  #  col.classes <- laply(pad.classified, getClass)
  #}
  
  meta.data <- list(id="", "title"=title, "desc"=desc, 
                    "records"=as.integer(0), 
                    "columns"=as.integer(0), 
                    "category"=category, 
                    "subcategory"=subcategory,  
                    "src"=data.source, 
                    "src_file"= source.file, 
                    "cache_location"="", 
                    "dList"=list(), "mList"=list(), 
                    "tags"=tags,
                    "pagetag"=pagetag
  )  
  
  #Start filling up the meta data
  meta.data$records <- nrow(pad.classified)
  meta.data$columns <- ncol(pad.classified)
  
  #Replace all meta chars from column names including spaces
  colnames(pad.classified) <-  replaceMetaChars(colnames(pad.classified))
  
  col.classes <- laply(pad.classified, getClass)
  col.names <- colnames(pad.classified)
  
  #Create a dict (name=value pair)
  cols <- list()
  cols[col.names]=col.classes
  meta.data$dList <- cols[which(unlist(cols)!="numeric")]
  meta.data$mList <- cols[which(unlist(cols)=="numeric")]
  #meta.data$mList <- names(which(sapply(pad.classified, is.numeric)))
  #meta.data$dList <- setdiff(colnames(pad.classified), meta.data$mList)
  
  # Get unique id for pad
  padID <- getPadUID()
  #print(paste("Assing pad id of ", padID, sep="::"))
  logMessage(paste("Assigning pad id of ", padID, sep="::"))
  meta.data$id <- padID
  #meta.data$meta_location <- getMetaDataFileURL(padID)
  meta.data$cache_location <- getCacheDataFileURL(padID)
  #meta.data$data_location <- getDataFileURL(padID)
  
  #Save transformed dataset as data.frame for later reading
  assign(padID, pad.classified)
  names <- c(eval(padID))

  #get default chart info....
  meta.data <- getDefaultChartInfo(pad.classified, meta.data)
  
  #Save the cahce
  if(verbose)  print("saving cache")
  if(log.all) logMessage("Saving cache...")
  save(list=names, file=meta.data$cache_location)
 
  #Save meta data
  #jsonSt <- toJSON(meta.data)
  #saveMetaData(meta.data$meta_location, jsonSt)

  #Save underlying data into csv
  #saveData(pad.classified, meta.data$data_location)
  
  if(verbose) print("Padification Completed!")
  logMessage("Padification Completed!")
  return(meta.data)  
}

#
# padify
#
padify <- function(series, series.data, x.plot.band=NULL, y.plot.band=NULL){
  #Remove rows that have NAs
  series.data <- series.data[rowSums(is.na(series.data)) != ncol(series.data),]
  
  # Remove rows that have at least 1 NA
  series.data <- series.data[complete.cases(series.data),]
  
  if("data.table" %in% class(series.data)) 
    series.data <- as.data.frame(series.data, , stringsAsFactors=F)
  #create pad
  pmd <- try(createPAD(series.data, series$title, 
                       series$desc, series$category, series$subcategory, 
                       series$source, paste(series$name, series$country, sep="."), 
                       series$tags, series$pagetag), silent=T)
  
  #assign("pmd", pmd, envir=.GlobalEnv)
  #
  # Add this pad to system pads table in the database, to mongo db and save the cache
  #
  #check to see if the padification went through
  if(class(pmd) %in% c("try-error")) {
    if(verbose) print("couldn't padify process.")
    logMessage("couldn't padify the pad...")
    assign("error.count", error.count+1, envir=.GlobalEnv)
  } else {
    if(!is.null(x.plot.band))
      pmd$default$xaxis$plotbands <- x.plot.band
    
    if(!is.null(y.plot.band))
      pmd$default$yaxis$plotbands <- y.plot.band
    
    if(verbose) print("adding the pad to mysql")
    if(log.all) logMessage("Saving PAD meta data in the database...")
    insertPadToMySQL(pmd)
    
    # insert into mongo
    if(verbose)  print("adding the pad to mongo db")
    if(log.all)  print("adding the pad to mongo db")
    pad <- fromJSON(toJSON(pmd))
    
    insertPadToMongo(pmd$id, pad) 
    return (pmd$id)
#     #generate solr doc
#     if(verbose)  print("adding the pad to solr vector")
#     if(log.all)  print("adding the pad to mongo")
#     doc <- list("type"="pad", "id" = pmd$id, "title" = pmd$title, "desc"=pmd$desc, "category"=pmd$category, 
#                 "subcategory"=pmd$subcategory, "tags"=pmd$tags, "author"="system")
#     solr.index.docs[[length(solr.index.docs)+1]] <<- doc     
  }
}
