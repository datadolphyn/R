# Generate FRED PADS
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.
#

#Generate PADS from FRED data
setwd("/Users/homemac/R")
source("CreatePADS.R")

padifyFRED <- function(){
  folder.path = "./raw-data/FRED/"
  file.name = "FRED_series.csv"
  fred <- read.csv(paste(folder.path, file.name, sep=""), stringsAsFactors=F)
  #pads <- data.frame(pad_id=character(), title=character(), desc=character(), records=numeric(), columns=numeric(),
  #                   category_id=character(), subcategory_id=character(), analyzed=numeric(), stories_created=numeric(),
  #                   stories_published=numeric(), "source"=character(), source_data_file=character(), 
  #                   created_on=character(), last_updated=character(), tags=character())
  pads <- getSystemFREDPads()
  solr.index.docs <- list()
  series <- list()
  series["source"] <- "FRED"
  #for(i in 1:nrow(fred)){
  for(i in 1:2){
    #series["title"] <- ifelse(nchar(fred$Title[i]) > 58, 
    #                          paste(substr(fred$Title[i], 1, 58), "...", sep=""), fred$Title[i])
    series["title"] <- fred$Title[i]
    series["desc"] <- paste(fred$Title[i], " (Units:", fred$Units[i], 
                            ", Frequency:", fred$Frequency[i], ", Seasonal Adjustmet: ", 
                            fred$Seasonal.Adjustment[i], ")", sep="")
    series["category"] <- fred$category_name[i]
    series["subcategory"] <- fred$subcategory_name[i]
    series["category_id"]<- fred$category_id[i]
    series["subcategory_id"]<- fred$subcategory_id[i]
    
    series.data <- read.csv(paste(folder.path, "data/", gsub(" ", "", gsub("\\\\", "/", fred$File[i])), sep=""),stringsAsFactors=F)
    print(paste("Starting file #", i, " name: " , fred$File[i], sep=""))
    #print(series.data)
    #print(series)
    #Create pad and get pad meta data
    pmd <- createPAD(series.data, series$title, 
                     series$desc, series$category, series$subcategory, 
                     series$source, paste(series$name, series$country, sep="."), 
                     tolower(paste(series$category, series$subcategory, series$source, sep=",")))
    #l <- list(pmd$id, pmd$title, pmd$desc, pmd$records, pmd$columns, series["category_id"], 
    #          series["subcategory_id"], pmd$analyzed, pmd$stories_created, pmd$stories_published, 
    #          pmd$src, pmd$src_file, "", "", pmd$tags)
    
    pads <- rbind(pads, 
                  data.frame(pad_id=pmd$id[1], title=pmd$title[1], desc=pmd$desc[1], 
                             records=pmd$records[1], columns=pmd$columns[1], 
                             category_id=series$category_id, subcategory_id=series$subcategory_id, 
                             analyzed=pmd$analyzed[1], stories_created=pmd$stories_created[1], 
                             stories_published=pmd$stories_published[1], 
                             "source"=pmd$src[1], source_data_file=pmd$src_file[1], created_on="", last_updated="", 
                             tags=pmd$tags[1]))
    #
    #generate solr doc
    #
    doc <- list("type"="pad", "id" = pmd$id, "title" = pmd$title, "desc"=pmd$desc, "category"=pmd$category, 
                "subcategory"=pmd$subcategory, "tags"=pmd$tags, "author"="system")
    solr.index.docs[[length(solr.index.docs)+1]] <- doc            
  }
  # now save solr doc
  generateSolrDocFromAllDocs(solr.index.docs)
  
  # Save pads to a csv file
  saveData(pads, "./pads/system_pads.csv")
  
  #now save pads to db
  #scon <- getDBCon()
  #dbRemoveTable(scon, "system_pads")
  #dbWriteTable(scon, name="system_pads", value=pads, row.names = F, overwrite = T)  
  #disconnectDB(scon)
  
}

padifyFRED()