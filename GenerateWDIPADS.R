# Generate PADS from WDI data
# Get series list and the associated meta data from the database
#
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

setwd("/Users/homemac/R")
source("CreatePADS.R")
#
#initiate a connection to wdi database
#
getWDIDBCon <- function(multi.statements=FALSE) { 
  if(multi.statements)
    return(dbConnect(MySQL(), user="wdi", password="wdi", dbname="wdi", host="localhost", 
                     client.flag=CLIENT_MULTI_STATEMENTS))
  else 
    return(dbConnect(MySQL(), user="wdi", password="wdi", dbname="wdi", host="localhost"))
}

#
# get data for a given series and country
#

getSeriesData <- function(series.name, series.country){
  con <- getWDIDBCon()
  on.exit(disconnectDB(con)) # On function exit, close connection
  
  sql <- sprintf("select * from wdi.wdi_data where Indicator_Code = '%s' and Country_Name ='%s';", series.name, series.country)
  print(sql)
  res <- dbSendQuery(con, sql)
  series.data <- fetch(res, n = -1)
  dbClearResult(res)
  if(!is.null(series.data) && ncol(series.data) > 4)
    return(series.data[,5:ncol(series.data)])
  else 
    return(NULL)
}
#series.data <- getSeriesData('AG.AGR.TRAC.NO', 'C')

  
padifyWDI <- function(){
  pads <- getSystemPads()
  pads.cat <- getCategories()
  pads.subcat <- getSubcategories()
  solr.index.docs <- list()
  con <- getWDIDBCon()
  #read wdi data
  wdi.data <- dbReadTable(con, "wdi_data")
  #read sereis meta data - first four columns only  
  wdi.series <- dbReadTable(con, "wdi_series")[,1:4]
  #read country meta data - only two columns are needed for the time being
  wdi.cats <- dbReadTable(con, "wdi_categories")
  
  series <- list()
  series["source"] <- "World Bank"
  
  for(i in 1:nrow(wdi.series)){
    if(i > 39 && i < 46){
      series["name"] <- wdi.series$SeriesCode[i]
      series["title"] <- wdi.series$Indicator_Name[i] 
      series["longdef"] <- wdi.series$Long_definition[i]
      series["shortdef"] <- wdi.series$Short_definition[i]
      wdi.cats.matched <- wdi.cats[wdi.cats$SeriesCode==series$name,]
      series["category"] <- wdi.cats.matched$category_name
      series["subcategory"] <- wdi.cats.matched$subcategory_name
      series["category_id"]<- wdi.cats.matched$category_id
      series["subcategory_id"]<- wdi.cats.matched$subcategory_id
      #
      logMessage(paste("Starting loading series", i, series$name, sep=":"))
      print(paste("Starting loading series", i, series$name, sep=":"))
      
      #don't continue if number of na are greather than 1/5th of number of cols
      all.series.data <- wdi.data[wdi.data$Indicator_Code==series$name,]
      for(j in 1:nrow(all.series.data)){
          series.data <- all.series.data[j,]
          if(!is.null(series.data) && 
               ncol(series.data) > 4 && 
               sum(is.na(series.data)) < .9 * ncol(series.data)) {
            
            series["country"] <- series.data$Country_Name
          
            logMessage(paste("Starting country", series$country, sep=":"))
            print(paste("Starting country", series$country, sep=":"))
          
            
            series.data <- t(series.data[,5:ncol(series.data)])
            #generate data.frame
            series.data <- data.frame(gsub("X", "", rownames(series.data)),
                                      series.data[,1], 
                                      row.names=seq(1:nrow(series.data)), stringsAsFactors=FALSE)
            #change colnames
            colnames(series.data) <- c("date",  paste(series$name, series$country, sep="."))
            
            #Remove NA from series column
            series.data <- series.data[!is.na(series.data[2]),]
            #Create pad and get pad meta data
            pmd <- createPAD(series.data, paste(series$title, series$country, sep=", "), 
                      series$longdef, series$category, series$subcategory, 
                      series$source, paste(series$name, series$country, sep="."), 
                      tolower(paste(series$category, series$subcategory, series$source, series$country, sep=",")))
            
          
            pads <- rbind(pads, list(pmd$id, pmd$title, pmd$desc, pmd$records, pmd$columns, series$category_id, 
            series$subcategory_id, pmd$analyzed, pmd$stories, pmd$src, pmd$src_file, "", "", pmd$tags))
            #
            #generate solr doc
            #
            doc <- list("type"="pad", "id" = pmd$id, "title" = pmd$title, "desc"=pmd$desc, "category"=pmd$category, 
                        "subcategory"=pmd$subcategory, "tags"=pmd$tags)
            solr.index.docs[[length(solr.index.docs)+1]] <- doc            
            
          } else {
            logMessage(paste(i, series$name, "was empty for ", series$country, sep=":"))
            print("This series was empty.")
          }       
      }# inner for loop ends here
    }
  }# for loop for series ends here
  
  # now save solr doc
  generateSolrDocFromAllDocs(solr.index.docs)
  
  # Save pads to a csv file
  saveData(pads, "./pads/system_pads.csv")
  
  #now save pads to db
  scon <- getDBCon()
  dbRemoveTable(scon, "system_pads")
  dbWriteTable(scon, name="system_pads", value=pads, row.names = F, overwrite = T)  
  disconnectDB(scon)
  
  dumpLogs()
}

padifyWDI()


#use extreme caution when running this funciton - it will remove all meta, cahce, data and empty system_pads table
deleteEverything <- function(){
  unlink("./pads/meta/*.*")
  unlink("./pads/cache/*.*")
  unlink("./pads/data/*.*")
  emptySystemPads()
  unlink("./pads/solr-docs.json")
}
#read wdi data
#wdi.data <- dbReadTable(getWDIDBCon(), "wdi_data")[1:10,]
#wdi.data.i <- t(wdi.data)
