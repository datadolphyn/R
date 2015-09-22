# Interact with MongoDB
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# Packages Used: rmongodb
# All rights reserved.

require("rmongodb")

mongo.db <- list(user="", pass="", name="ddfin_dev", system.pads="ddfin_dev.system_pads", host="localhost")

#
#initiate a connection to admin database
#
getDefaultMongoDBCon <- function() { 
  mongo <- mongo.create()
  if (!mongo.is.connected(mongo)) {
    print("Unable to connect.  Error code:")
    print(mongo.get.err(mongo))
  } else 
    return(mongo)
}

#
#initiate a connection to a db
# This will create a db if it doesn't exist
#
getMongoDBCon <- function(db.name) { 
  mongo <- mongo.create(db=db.name)
  if (!mongo.is.connected(mongo)) {
    cat("Unable to connect.  Error code:", "\n")
    cat(mongo.get.err(mongo), "\n")
  } else 
    return(mongo)
}

#
#disconnect
#
disconnectMongoDB <- function(mongo) {
  #Close Connection
  if (mongo.is.connected(mongo)) {
    mongo.destroy(mongo)
  }
} 

#
#list all databases in Mongo
#
listAllDBs <- function() {
  #List dbs
  mongo <- getDefaultMongoDBCon()
  on.exit(disconnectMongoDB(mongo)) # On function exit, close connection
  cat(mongo.get.databases(mongo), "\n")
}

#
# drop a db
#
dropDB <- function(db.name) {
  mongo <- getDefaultMongoDBCon()
  on.exit(disconnectMongoDB(mongo)) # On function exit, close connection
  cat(mongo.drop.database(mongo, db.name))  
}

#
#List collections for a given db
#
listAllCollections <- function(db.name) {
  mongo <- getDefaultMongoDBCon()
  on.exit(disconnectMongoDB(mongo)) # On function exit, close connection
  cat(mongo.get.database.collections(mongo, db.name), "\n")
}
#listAllCollections(mongo.db$name)

#
# drop a collection
#
dropCollection <- function(db.collection){
  mongo <- getDefaultMongoDBCon()
  on.exit(disconnectMongoDB(mongo))
  
  cat(mongo.drop(mongo, db.collection), "\n")
}
#dropCollection(mongo.db$system.pads)

#
# empty a collection
#
emptyCollection <- function(db.collection){
  mongo <- getDefaultMongoDBCon()
  on.exit(disconnectMongoDB(mongo))
  
  cat(mongo.remove(mongo, db.collection), "\n")
}
#emptyCollection(mongo.db$system.pads) 


#
# insert systemPads
#
insertPadDoc <- function(doc){
  
  if(!is.null(mongo)){
    status <- mongo.insert(mongo, mongo.db$system.pads, doc)
    if(status){
      if(verbose) cat("pad successfully inserted", "\n")
    } else {
      err <- mongo.get.last.err(mongo, mongo.db$name)
      cat(mongo.get.server.err(mongo), "\n")
      cat(mongo.get.server.err.string(mongo), "\n")
    }
  } else {
    cat("Connection is null", "\n")
  }
  mongo.bson.destroy(doc)
}

#
# update systemPads
#
updatePadDoc <- function(pad.id, doc){
  if(!is.null(mongo)){
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "_id", pad.id)
    criteria <- mongo.bson.from.buffer(buf)
    
    status <- mongo.update(mongo, mongo.db$system.pads, criteria, doc)
    if(status){
      if(verbose) cat("pad successfully updated", "\n")
    } else {
      err <- mongo.get.last.err(mongo, mongo.db$name)
      cat(mongo.get.server.err(mongo), "\n")
      cat(mongo.get.server.err.string(mongo), "\n")
    }
  } else {
    cat("Connection is null", "\n")
  }
  mongo.bson.destroy(doc)
}

#
#insert a pad into system_pads
# connection object is supplied
#
insertPadToMongo <- function(pad.id, pad){
  # Insert document to collection 'system_pads'
  doc <- mongo.bson.from.list(list(
    "_id"=pad.id, 
    pad=pad))
  
  if(verbose) cat("calling insert for pad ", pad.id, "\n")
  insertPadDoc(doc)
}

#
# get a single system pad
#
getSingleSystemPad <- function(padId) {
  
  mongo <- getDefaultMongoDBCon()
  on.exit(disconnectMongoDB(mongo))
  
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "_id", padId)
  query <- mongo.bson.from.buffer(buf)
  
  # Find the first 100 records in collection system_pads
  cursor <- mongo.find(mongo, mongo.db$system.pads, query)
  
  # Step though the matching records and display them
  while (mongo.cursor.next(cursor))
    l<- mongo.bson.to.list(mongo.cursor.value(cursor))
  #assign("l", l,  envir=.GlobalEnv)
  #l$pad$default$type = "timeseries"
  #updatePadDoc(l$'_id', mongo.bson.from.list(l))
  mongo.cursor.destroy(cursor)
}
#getSingleSystemPad("pad136971841842064")

#
#get pads from system_pads
#
getSystemPads <- function() {
  
  mongo <- getDefaultMongoDBCon()
  on.exit(disconnectMongoDB(mongo))
  
  # Find the first 100 records in collection system_pads
  cursor <- mongo.find(mongo, mongo.db$system.pads, limit=100L)
  
  # Step though the matching records and display them
  while (mongo.cursor.next(cursor))
    print(mongo.cursor.value(cursor))
  mongo.cursor.destroy(cursor)
}
#getSystemPads()

#
#get pads count from system_pads
#
getSystemPadsCount <- function(){
  mongo <- getDefaultMongoDBCon()
  on.exit(disconnectMongoDB(mongo))
  
  # Find the first 100 records
  #    in collection people of database test where age == 18
  pads.count <- mongo.count(mongo, mongo.db$system.pads)
  cat("total pads: ", pads.count, "\n")
}
#getSystemPadsCount()
