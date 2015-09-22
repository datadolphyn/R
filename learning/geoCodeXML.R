# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

getDocNodeVal <- function(doc, path)
{
   sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}


getGeoCode <- function(str)
{
  library(XML)
  #str <- gsub(' ','%20',str)
  url <- paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
  response <- xmlTreeParse(url, useInternal=TRUE)
  
  lat <- getDocNodeVal(response, "/GeocodeResponse/result/geometry/location/lat")
  lng <- getDocNodeVal(response, "/GeocodeResponse/result/geometry/location/lng")
  return (c(lat,lng))
}

t <- getGeoCode("Bangalore, India")