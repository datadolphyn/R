# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library("plyr")

mb.df <- "MB.RData"
load(mb.df)

getGeoCode <- function(str)
{
  print(str)
  library("RJSONIO")
  str <- gsub(' ','%20',str)
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',str, sep="")
  print(connectStr)
  #data.json <- fromJSON(paste(readLines(url("http://maps.google.com/maps/api/geocode/json?sensor=false&address=South%20Africa")), collapse=""))
  data.json <- fromJSON(paste(readLines(url(connectStr)), collapse=""))
  close(con)
  data.json <- unlist(data.json)
  #print(data.json["results.geometry.location.lat"])
    #return (lat = data.json["results.geometry.location.lat"])
    return (c(lat=data.json["results.geometry.location.lat"], lng=data.json["results.geometry.location.lng"]))
}
printVal <- function(val){
  print(val)
  return ("YO")
}
shortDS <- with(head(mbDS, 20), data.frame(Opposition, Ground, Toss))
shortDS <- mutate(shortDS, Oppostion, Ground, Toss, lat=printVal(Opposition))
                
cn <- levels(mbDS$Opposition)
llply(cn, function(x) {getGeoCode(x)})
ds <- transform(mbDS, lat=getGeoCode(Opposition))
d <- data.frame(Country=cn)
d$lng <- apply(d, 1, function(x) {getGeoCode(x)})


printVal <- function(val){
  print(val)
  return ("YO")
}
ddply(shortDS, "Opposition", function(x){ getGeoCode(x$Opposition[1])})



#************* Working Version
df <- NULL
#code <- with(head(mbDS, 4), getGeoCode(Opposition))
#m <- ddply(mbDS, "Opposition", function(x){c("lat", "lag")})
#t <- getGeoCode("Bangalore, India")
cn <- levels(mbDS$Opposition)
for(i in 1:length(cn)) {
  t <- getGeoCode(cn[i])
  r <- c(cn[i], t)
  df <- rbind(df, r) 
}
#************* Working Version

getGeoCode <- function(str)
{
  library("RJSONIO")
  str <- gsub(' ','%20',str)
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',str, sep="")
  #print(connectStr)
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  data.json <- unlist(data.json)
  #print(data.json["results.geometry.location.lat"])
    #return (lat = data.json["results.geometry.location.lat"])
    return (c(lat=data.json["results.geometry.location.lat"], lng=data.json["results.geometry.location.lng"]))
}

shortDS <- with(head(mbDS, 20), data.frame(Opposition, Ground.Country, Toss))
#v <- shortDS$Ground.Country
#g <- laply(v, function(x){getGeoCode(x)})
shortDS <- with(shortDS, data.frame(Opposition, Ground.Country, Toss,
                  laply(Ground.Country, function(val){getGeoCode(val)})))
closeAllConnections()

