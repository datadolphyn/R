# Script to read wdi data and create tables in WDI database
# Make sure to convert file WDI_Description.csv to WDI_Description.txt - this file is not needed in the DB
# Take back up if necessary
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

setwd("/Users/homemac/R")

require("RMySQL")
require("plyr")

source("Util.R")
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
#disconnect a database
#
disconnectDB <- function(con) {
  #Close Connection
  dbDisconnect(con)
} 


#
#write a data.frame as a table in WDI database
# this funciton will overwrite the exisiting table 
#

writeTable <- function(table.name ,table.data) {
  #List tables and fields in a table:
  con <- getWDIDBCon()
  on.exit(disconnectDB(con)) # On function exit, close connection
  if(dbExistsTable(con, table.name)){
    dbRemoveTable(con, table.name)
    dbWriteTable(con, name=table.name, value=table.data, row.names = F, overwrite = T)  
  } else {
    dbWriteTable(con, name=table.name, value=table.data, row.names = F, overwrite = T)  
  }

}


saveAllWDIData <- function() {
  folder.path = "./raw-data/wdi"
  filenames <- list.files(path=folder.path, pattern="*.csv")
  for(i in filenames) {
    filePath <- file.path(folder.path,i)
    wdi.name <- unlist(strsplit(i, "\\."))[1] #Temp - use the file name as the title
    wdi.df <- readFile(filePath)
    #clean up
    wdi.df <- wdi.df[rowSums(is.na(wdi.df)) != ncol(wdi.df),] # remove empty rows
    wdi.df <- wdi.df[rowSums(is.na(wdi.df)) != ncol(wdi.df)-1,] # remove rows with just one cell filled out
    wdi.df <- wdi.df[,colSums(is.na(wdi.df)) != nrow(wdi.df)] # remove empty cols
    
    #some cells have new line variables, remove it
    #char.vars <- setdiff(char.vars, "W")  # remove a column
    char.vars <- names(wdi.df)[sapply(wdi.df, is.character)]  #select character variables
    for(i in 1:length(char.vars)) {
      wdi.df[char.vars[i]] = sapply(wdi.df[char.vars[i]], function(x) gsub("\\n", " ", x))
    }    
    
    #clean up column names    
    colnames(wdi.df) <- replaceMetaChars(colnames(wdi.df))
    writeTable(wdi.name, wdi.df)
  }
}

saveAllWDIData()


testFunctionDoNoRun <- function(){
  wdi.df <- read.csv("./raw-data/wdi/WDI_Series.csv", stringsAsFactors=F)
  wdi.df <- wdi.df[rowSums(is.na(wdi.df)) != ncol(wdi.df),] # remove empty rows
  wdi.df <- wdi.df[,colSums(is.na(wdi.df)) != nrow(wdi.df)] # remove empty cols
  
  char.vars <- names(wdi.df)[sapply(wdi.df, is.character)]  #select character variables
  for(i in 1:length(char.vars)) {
    wdi.df[char.vars[i]] = sapply(wdi.df[char.vars[i]], function(x) gsub("\\n", " ", x))
  }
  writeTable("wdi_series", wdi.df)
  

  df1 <- as.data.frame(cbind(sapply(df,function(x) { 
    if(is.character(x)) {
      gsub("\\n", " ", x)
    } 
    return(x)
    })))
}

generateCatsFromWDI <- function() {
  wdi.cats <- read.csv("./raw-data/wdi/WDI_DD_Categories.csv", stringsAsFactors=F)
  #colnames(wdi.cats)
  #unique(wdi.cats$Category)
  #unique(wdi.cats$Subcategory)
  wdi.cats$Category[wdi.cats$Category=="Economic Policy & Debt"] = "Economics"
  wdi.cats$Category[wdi.cats$Category=="Labor & Social Protection"] = "Social"
  wdi.cats$Category[wdi.cats$Category=="Public Sector"] = "Government"
  wdi.cats$Category[wdi.cats$Category=="Private Sector & Trade"] = "Private Sector"
  
  #write out the cat and subcats for entering into story pads data
  dd.cats <- unique(wdi.cats[,c("Category", "Subcategory")])
  colnames(dd.cats) <- tolower(colnames(dd.cats))
  write.csv(dd.cats, "./pads/PadsCategory.csv", row.names=F)
  #insertCatSubcat()
  con <- getDBCon()
  dd.cats <- dbReadTable(con, "pads_categories")
  dd.subcats <- dbReadTable(con, "pads_subcategories")
  disconnectDB(con)
  #Assign category id
  wdi.cats$category_id = 0
  for(i in 1:nrow(dd.cats)){
    wdi.cats$category_id[which(wdi.cats$Category==dd.cats$category_name[i])] = dd.cats$category_id[i]
  }
  #trim white spaces from front and end
  #wdi.cats$Subcategory <- gsub("^\\s|\\s$","", wdi.cats$Subcategory)
  #assign subcat_id
  wdi.cats$subcategory_id = 0
  for(i in 1:nrow(dd.subcats)){
    wdi.cats$subcategory_id[which(wdi.cats$Subcategory==dd.subcats$subcategory_name[i])] = dd.subcats$subcategory_id[i]
  }
  colnames(wdi.cats) = c("SeriesCode", "Topic", "category_name", "subcategory_name", "category_id", "subcategory_id")
  writeTable("wdi_categories", wdi.cats)
}
