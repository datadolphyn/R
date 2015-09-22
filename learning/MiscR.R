# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

responseDF <- switch(fnType,
       sum = getSum(df, d, m), 
       count = getCount(df, d, m),
       'default'
       )
    if(!is.null(g) && g == "t")
      responseDF <- geoCode(responseDF)
    return(responseDF)

{
        "name": "Australia",
        "center": new google.maps.LatLng(41.878113, -87.629798),
        "data": 2842518
      },

JSON <- with(head(mbDS), paste('{"d":"',mbDS$Country,
                               '", "center": new google.maps.LatLng(',
                                mbDS$Country.Lat, ',', mbDS$Country.Lng, 
                               '), "data":', mbDS$Test.Runs, 
                                '},', sep=""))

m <- "Country"
lat <- paste(m,".Lat", sep="")
sumDS <- ddply(mbDS, .(Country), summarise, Test.Runs=sum(Test.Runs, na.rm=TRUE))
JSON <- with(sumDS, paste('{"d":"',sumDS[m],
                               '", "center": new google.maps.LatLng(',
                                mbDS[lat][sumDS[m]==mbDS[m]], ',', mbDS$Country.Lng[mbDS$Country=="India"], 
                               '), "data":', sumDS$Test.Runs, 
                                '},', sep=""))


lookupLat <- function(m, i="India") 
{
  lat <- paste(m,".Lat",sep="")
  return(mbDS[lat][mbDS[m]==i][1])
}
lookupLat("Country", "India")
}
}
with(sumDS, data.frame(Country, Test.Runs, laply(Country, function(x){getGeoCode(x, x)})))

ddply(mbDS, "Opposition", function(x) c(Lat=unique(x$Opposition.Lat, Lng=unique(x$Opposition.Lng))))

with(sumDS, data.frame(Country, Test.Runs, laply(Country, function(x){lookupGeoCode(x)})))


with(responeDF, paste('{"d":"',responseDF[m]
                               '", "center": new google.maps.LatLng(',
                                df[latName][df[m]==responeDF[m]][1], ',', df[lngName][df[m]==responeDF[m]][1], 
                               '), "data":', mbDS$Test.Runs, 
                                '},', sep=""))


sumDS <- ddply(mbDS, .(Opposition), summarise, Test.Runs=sum(Test.Runs, na.rm=TRUE))

d <- "Opposition"
latName <- paste(d, ".Lat", sep="")
lngName <- paste(d, ".Lng", sep="")
m <- "Test.Runs"
responseDF <- sumDS
df <- mbDS
with(responseDF, paste('{"d":"',responseDF[d],
                               '", "center": new google.maps.LatLng(',
                                df[latName][df[d]==responseDF[d]][1], 
                                ',', df[lngName][df[d]==responseDF[d]][1], 
                               '), "data":', responseDF[m], 
                                '},', sep=""))
