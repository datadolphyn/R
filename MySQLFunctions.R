# Interact with MySQL
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# Packages Used: RMySQL
# All rights reserved.

# Sys.setenv(MYSQL_HOME="C:/Program Files/MySQL/MySQL Server 5.5/")

require("RMySQL")

#
#initiate a connection
#
getMSDBCon <- function(multi.statements=FALSE) { 
  
  if(multi.statements)
    return(dbConnect(MySQL(), user=mysql.db$user, password=mysql.db$pass, dbname=mysql.db$name, host=mysql.db$host, 
                     client.flag=CLIENT_MULTI_STATEMENTS))
  else 
    return(dbConnect(MySQL(), user=mysql.db$user, password=mysql.db$pass, dbname=mysql.db$name, host=mysql.db$host))
}

#
#disconnect a database
#
disconnectMSDB <- function(mysql) {
  #Close Connection
  dbDisconnect(mysql)
} 

#
#list all tables in the database
#

listAllDBTables <- function() {
  #List tables and fields in a table:
  return(dbListTables(mysql))
}

#
# Get system pads as data.frame
#
getSystemPads <- function() {
  return(dbReadTable(mysql, "system_pads"))
}

#
#Import categories as a data.frame
#  mysql <- getMSDBCon()
# on.exit(disconnectMSDB(mysql)) # On function exit, close connection
#
getCategories <- function() {
  return(dbReadTable(mysql, "pads_categories")) 
}

#
# get a category id for a categoryname
#
getOrInsertCategory <- function(category.name){
  sql <- sprintf("select category_id from pads_categories where category_name='%s';", category.name)
  #print(sql)
  id <- dbGetQuery(mysql, sql)[1,1] 
  if(is.null(id)) {
    id<-addCategory(category.name)
  }
  return(id)
}

#
#Add a category and return categories data.frame
#
addCategory <- function(category.name) {
  
  sql <- sprintf("insert into pads_categories (category_name) values ('%s');", category.name)
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  dbClearResult(rs) # Make sure to clear the results 
  id <- dbGetQuery(mysql, "select last_insert_id();")[1,1] #get id of a newly inserted category
  return(id)  
  #return(getCategories(mysql))  
}

#
#Get subcategories as a data.frame
#
getSubcategories <- function() {
  return(dbReadTable(mysql, "pads_subcategories"))
}

#
# get a subcategory id for a subcategory name, if it doesn't exist, create one
#
getOrInsertSubCategory <- function(subcategory.name, category.name){
  
  sql <- sprintf("select subcategory_id from pads_subcategories where subcategory_name='%s';", subcategory.name)
  #print(sql)
  id <- dbGetQuery(mysql, sql)[1,1] 
  if(is.null(id)) {
    id<-addSubCategory(subcategory.name, category.name)
  }
  return(id)
}

#
#Add a subcategory and return categories data.frame
#
addSubCategory <- function(subcategory.name, category.name) {
  
  #INSERT INTO `pads_subcategories` (`category_id`, `subcategory_name`) SELECT category_id, 'Men' as subcategory_name FROM pads_categories WHERE category_name="Health"
  sql <- sprintf(
    "insert into pads_subcategories (category_id, subcategory_name) select category_id, ('%s') as subcategory_name FROM pads_categories WHERE category_name=('%s');", 
    subcategory.name, category.name)
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  dbClearResult(rs) # Make sure to clear the results  to get the 
  id <- dbGetQuery(mysql, "select last_insert_id();")[1,1] #get id of a newly inserted subcategory
  return(id)  
  #return(getSubcategories(mysql))  
}

#
# get a page_tag_id for a page_tag, if it doesn't exist, create one
#
getOrInsertPageTag <- function(page.tag){
  
  sql <- sprintf("select page_tag_id from page_tags where page_tag='%s';", page.tag)
  #print(sql)
  id <- dbGetQuery(mysql, sql)[1,1] 
  if(is.null(id)) {
    id<-addPageTag(page.tag)
  }
  return(id)
}

#
#Add a page tag and return categories data.frame
#
addPageTag <- function(page.tag, page.desc="", display.name="") {
  
  sql <- sprintf(
    "insert into page_tags (page_tag, page_desc, display_name) values ('%s', '%s', '%s');", 
    page.tag, page.desc,display.name)
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  dbClearResult(rs) # Make sure to clear the results  to get the 
  id <- dbGetQuery(mysql, "select last_insert_id();")[1,1] #get id of a newly inserted subcategory
  return(id)  
}

#
#Add stats for a page
#
addPageStat <- function(page.tag, stat.value, stat.name) {
  page.tag.id <- getOrInsertPageTag(page.tag)
  #INSERT INTO `page_key_stats` (`category_id`, `subcategory_name`) SELECT category_id, 'Men' as subcategory_name FROM pads_categories WHERE category_name="Health"
  sql <- sprintf(
    "insert into page_key_stats (page_tag_id, stat_name, stat_value) values (%i, '%s', '%s');", 
    page.tag.id, stat.name, stat.value)
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  dbClearResult(rs) # Make sure to clear the results  to get the 
  #return(getSubcategories(mysql))  
}

#
# empty stats for page tag
#
deletePageStat <- function(page.tag) {
  page.tag.id <- getOrInsertPageTag(page.tag)
  sql <- sprintf("delete from page_key_stats where page_tag_id = %i;", page.tag.id)
  rs <- dbSendQuery(mysql, sql)
  dbClearResult(rs) # Make sure to clear the results  to get the 
}

#
# Empty categories and subcategories table
#
emptyCatAndSubcats <- function() {  
  #subcategoreis first
  sql <- "delete from pads_subcategories"
  rs <- dbSendQuery(mysql, sql)
  #categoreis now  
  sql <- "delete from pads_categories"
  rs <- dbSendQuery(mysql, sql)
}

#
# update system pads in categories count
#
updateCatPadCount <- function() {
  mysql <- getMSDBCon()
  on.exit(disconnectMSDB(mysql)) # On function exit, close connection
  
  sql <- "update pads_categories set pads_count = (select count(system_pads.pad_id) from system_pads where system_pads.category_id=pads_categories.category_id and system_pads.active=1)"
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  dbClearResult(rs) # Make sure to clear the results  to get the   
  
  sql <- "select sum(pads_count) from pads_categories where category_id > 0"
  total <- dbGetQuery(mysql, sql)[1,1] 
  #print(total)
  
  sql <- sprintf("update pads_categories set pads_count = %i where category_id <= 0;", total)
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  dbClearResult(rs) # Make sure to clear the results  to get the   

}

#
# insert a pad
# connection object is storied in the 
#
insertPad <- function(id, title, desc, records, columns, cat.name, subcat.name, 
                      analyzed, stories_created, stories_published,
                      src, src.file, tags, page.tag) {
  cat.id <- as.integer(getOrInsertCategory(cat.name))  
  subcat.id <- as.integer(getOrInsertSubCategory(subcat.name, cat.name))
  page.tag.id <- as.integer(getOrInsertPageTag(page.tag))
  sql <- sprintf("INSERT INTO `system_pads`(`pad_id`, `title`, `desc`, `records`, `columns`, `category_id`, `subcategory_id`, `analyzed`, `stories_created`, `stories_published`, `source`, `source_data_file`, `created_on`, `last_updated`, `tags`, `page_tag_id`) VALUES (\"%s\", '%s', ' %s', %i, %i, %i, %i, %i, %i, %i, '%s', '%s',NOW(), NOW(), '%s', '%i');", 
                 id, title, desc, records, columns, cat.id, subcat.id, analyzed, stories_created, stories_published, src, src.file, tags, page.tag.id);
  #sql <- dbEscapeStrings(mysql, sql)
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  #dbClearResult(rs) # Make sure to clear the results  to get the 
}

#
# Add a pad
#
insertPadToMySQL <- function(pmd) { #pad.meta.data (i.e. pmd is sent)
  #print(pmd)
  insertPad(pmd$id[1], pmd$title[1], pmd$desc[1], pmd$records[1], pmd$columns[1], pmd$category[1], 
            pmd$subcategory[1], 0, 0, 0, pmd$src[1], pmd$src_file[1], pmd$tags[1], pmd$pagetag[1])
}

#
# Empty System PADS table
#
emptySystemPads <- function() {
  mysql <- getMSDBCon()
  on.exit(disconnectMSDB(mysql)) # On function exit, close connection
  
  sql <- "truncate table system_pads"
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  #print(rs)
}

#
# Empty System PADS table for a category
#
emptySystemPadsForCat <- function(cat.id=NULL, cat=NULL) {
  mysql <- getMSDBCon()
  assign("mysql", mysql, envir=.GlobalEnv)
  on.exit(disconnectMSDB(mysql)) # On function exit, close connection
  if(!is.null(cat.id))
    sql <- paste("DELETE FROM  system_pads WHERE category_id =", cat.id, sep="")
  else if(!is.null(cat)){
    cat.id <- getOrInsertCategory(cat)
    sql <- paste("DELETE FROM  system_pads WHERE category_id =", cat.id, sep="")
  }
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  #print(rs)
}

#
# Empty System PADS table for a subcategory
#
emptySystemPadsForSubCat <- function(subcat.id) {
  mysql <- getMSDBCon()
  on.exit(disconnectMSDB(mysql)) # On function exit, close connection
  
  sql <- paste("DELETE FROM  system_pads WHERE subcategory_id =", subcat.id, sep="")
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  #print(rs)
}

#
# Update a single PAD in System PADS table
#
updateSystemPad <- function(id, title, desc, category, subcategory, src, tags) {
  mysql <- getMSDBCon()
  on.exit(disconnectMSDB(mysql)) # On function exit, close connection
  
  cat.id <- as.integer(getOrInsertCategory(category))
  subcat.id <- as.integer(getOrInsertSubCategory(subcategory, category))
  sql <- sprintf("UPDATE system_pads SET title='%s', desc='%s', 
                  category_id=%i, subcategory_id=%i,source='%s',last_updated=NOW(), tags='%s' 
                 WHERE pad_id='%s';", title, desc, cat.id, subcat.id, src, tags ,id);
  #print(sql)
  rs <- dbSendQuery(mysql, sql)
  dbClearResult(rs) # Make sure to clear the results  to get the 
}


#
# From a csv file, get categories and sub-categories and insert them into categories and subcategories tables
#
insertCatSubcat <- function(file="./pads/PadsCategory.csv") {
  catsubcat <- read.csv(file,  header = TRUE, stringsAsFactors=FALSE)
  catsubcat$cat_id = sapply(catsubcat$category, getOrInsertCategory)
  catsubcat$subcat_id = sapply(catsubcat$subcategory, function(x) 
    getOrInsertSubCategory(x, catsubcat$category[catsubcat$subcategory==x]))
  return(catsubcat)
}


TestAll <- function() {
  categories <- getCategories(getMSDBCon())
  categories <- addCategory("Health")
  #dbListFields(mysql, "pads_categories")
  sub.categories <- addSubCategory("Baseball", "Sports")
  
  system.pads <- getSystemPads(getMSDBCon())
  
  cat.id <- getOrInsertCategory("Autos")
  if(!is.null(cat.id)) print(cat.id)
  subcat.id <- getOrInsertSubCategory("Basketball", "Sports")
  if(!is.null(subcat.id)) print(subcat.id)
}

TestNew <- function() {
  catsubcat <- insertCatSubcat()
  pads <- getSystemPads()
  emptySystemPads()
  mysql <- getMSDBCon()
  if(dbExistsTable(mysql, "system_pads")){
    #dbRemoveTable(mysql, "system_pads") ####### Do not call this method, it will remove the table and losse the structure
    dbWriteTable(mysql, "system_pads", pads, row.names=F, append=T)
  }
  write.csv(pads, "./pads/pads.csv", row.names=FALSE)
  pads <- read.csv("./pads/pads.csv", stringsAsFactors=FALSE)
  pads$category_id = sample(catsubcat$cat_id, nrow(pads), replace=T)
  pads$subcategory_id <- sapply(pads$category_id, function(x) catsubcat$subcat_id[catsubcat$cat_id==x][[1]])
  
  ######## Update a single PAD
  updateSystemPad("pad135577899735859",  "1 Month Treasury Rate",  "1 Month Treasury Rate",  
                  "Economics",  "Economic",  "FRED",	"Treasury, Econ, US Rates")

  sql <- sprintf("insert into pads_categories
                    (species_id, name, data_source, description, created_at)
                 values (%d, '%s', '%s', '%s', NOW());",
                 species.id, network.name, data.source, description)
  
  yourtable <- dbReadTable(mysql,"sometable")
  # write it back
  dbWriteTable(mysql,"yourTableinMySQL",yourtable,overwrite=T)
  
  dbWriteTable(mysql, "test2", "~/data/test2.csv") ## table from a file
  
  
  #
  #dbWriteTable(mysql, "WL2", a.data.frame)         ## table from a data.frame
  #dbWriteTable(mysql, "test2", "~/data/test2.csv") ## table from a file
  #Run an arbitrary SQL statement and extract all its output (returns a data.frame):
  #dbGetQuery(mysql, "select count(*) from a\_table")
  #Run an SQL statement and extract its output in pieces (returns a result set):
  # rs <- dbSendQuery(mysql, "select * from WL where width\_nm between 0.5 and 1")
  #   d1 <- fetch(rs, n = 10000)
  #   d2 <- fetch(rs, n = -1)
}
