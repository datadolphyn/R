# Author: Rajani Aswani Co-Founder @datadolph.in
# Author: Jitender Aswani
# Date: 4/15/2013
# Description: R & Solr Integration Using Solr's REST APIs
# Packages Used: RCurl, RJSONIO
# Blog Reference: http://www.r-bloggers.com/updated-sentiment-analysis-and-a-word-cloud-for-netflix-the-r-way/
# Download
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#
# Load Library
#
require(RCurl)
require(RJSONIO)

# define solr server address
solrServer <- "http://localhost:8080/solr-dev/"

#
# get row-wise JSON
#
getRowWiseJson <- function (jsonDT) {
  require("RJSONIO")
  row.json <- apply(jsonDT, 1, toJSON)
  json.st <- paste('[', paste(row.json, collapse=', '), ']')
  return (json.st)
}

getSolrServer <- function() {
  return(solrServer)
}

getUpdateURL <- function(){
  return(paste(getSolrServer(), "update/json?commit=true&wt=json", sep=""))
}

getCommitURL <- function() {
  return(paste(getSolrServer(),"update?commit=true&wt=json", sep=""))
}

getQueryURL <- function() {
  return(paste(getSolrServer(),"select?fl=id&wt=json&indent=true&fq=type:pad&q=", sep=""))
}

#
# query a field for the text and return docs
#
querySolr <- function(queryText, queryfield="all") {
  response <- fromJSON(getURL(paste(getQueryURL(), queryfield, ":", queryText, sep="")))
  if(!response$responseHeader$status) #if 0
    return(response$response$docs)
}

#
# delete all indexes from solr server
#
deleteAllIndexes <-function() {
  response <- postForm(getUpdateURL(),
                       .opts = list(postfields = '{"delete": {"query":"*:*"}}',
                                    httpheader = c('Content-Type' = 'application/json', 
                                                   Accept = 'application/json'),
                                    ssl.verifypeer=FALSE
                       )
  ) #end of PostForm
  return(fromJSON(response)$responseHeader[1])
}

#
# delete all indexes for a document type from solr server 
# in this example : type = sports
#
deleteSportsIndexes <-function() {
  response <- postForm(getUpdateURL(),
                       .opts = list(postfields = '{"delete": {"query":"type:sports"}}',
                                    httpheader = c('Content-Type' = 'application/json', 
                                                   Accept = 'application/json'),
                                    ssl.verifypeer=FALSE
                       )
  ) #end of PostForm
  return(fromJSON(response)$responseHeader[1])
}

#
# delete indexes for all baskeball category in sports type from solr server 
# in this example : type = sports and category: basketball
#
deleteSportsIndexesForCat <-function(category) {
  response <- postForm(getUpdateURL(),
                       .opts = list(postfields = 
                         paste('{"delete": {"query":"type:sports AND category:', category, '"}}', sep=""),
                                    httpheader = c('Content-Type' = 'application/json', 
                                                   Accept = 'application/json'),
                                    ssl.verifypeer=FALSE
                       )
  ) #end of PostForm
  return(fromJSON(response)$responseHeader[1])
}
#deletePadIndexesForCat("baskeball")


#
#Post a new document to Solr
#
postDoc <- function(doc) { 
  solr_update_url <- getUpdateURL()
  jsonst <- toJSON(list(doc))

  response <- postForm(solr_update_url,
                       .opts = list(postfields = jsonst,
                                    httpheader = c('Content-Type' = 'application/json', 
                                                   Accept = 'application/json'),
                                    ssl.verifypeer=FALSE
                       )) #end of PostForm
  return(fromJSON(response)$responseHeader[1])
  ########## Commit - only if it doesn't work the other way ###############
  #return(fromJSON(getURL(getCommitURL())))
}

#
#Post JSON document to Solr
#
postJSON <- function(jsonst) {  #pad.meta.data (i.e. pmd is sent)
  solr_update_url <- getUpdateURL()

  response <- try(postForm(solr_update_url,
                           .opts = list(postfields = jsonst,
                                        httpheader = c('Content-Type' = 'application/json', 
                                                       Accept = 'application/json'),
                                        ssl.verifypeer=FALSE
                           )), silent=T) #end of PostForm
  if(class(response) %in% c("try-error")){
    print(response)     
  } else {
    print(response)
    return(fromJSON(response)$responseHeader[1])
  }
  ########## Commit - only if it doesn't work the other way ###############
  #return(fromJSON(getURL(getCommitURL())))
}

#
#test it out 
#
TestAll <- function() {  
  status <- postJSON(toJSON(fromJSON(paste(readLines("./pads/solr-docs-IPL-T20.json"), collapse="\n"))))
  
  if(status) print("An error occurred creating indexes.")
  
  #delete test
  status <- deleteAllIndexes() # Status of 0 means that the operation was successfully completed.
  if(status) print("An error occurred while deleting all indexes.")
  
  #add doc test
  status <- postDoc(doc) # Status of 0 means that the operation was successfully completed.
  if(status) print("An error occurred while adding a document.")
  

  #Query SOlr
  docs <- querySolr("baseball", "tags") 
  print(docs)
  
  docs <- querySolr("baseball") #if no field is defined, default filed all_text will be searched
  print(docs)
}
