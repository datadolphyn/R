# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

setwd("/Users/homemac/Toga/Alto")
library(RJSONIO)

### set parameters ###
api <- "YOUR_KEY" # API key goes here!!
q <- "health+care+reform" # Query string, use + instead of space
records <- 500 # total number of records to return, note limitations above
# calculate parameter for offset
os <- 0:(records/10-1)

# read first set of data in
uri <- paste ("http://api.nytimes.com/svc/search/v1/article?format=json&query=", q, "&offset=", os[1], "&fields=date&api-key=", api, sep="")
raw.data <- readLines(uri, warn="F") # get them
res <- fromJSON(raw.data) # tokenize

dat <- unlist(res$results) # convert the dates to a vector
# read in the rest via loop
for (i in 2:length(os)) {
# concatenate URL for each offset
uri <- paste ("http://api.nytimes.com/svc/search/v1/article?format=json&query=", q, "&offset=", os[i], "&fields=date&api-key=", api, sep="")
raw.data <- readLines(uri, warn="F")
res <- fromJSON(raw.data)
dat <- append(dat, unlist(res$results)) # append
}

# aggregate counts for dates and coerce into a data frame
cts <- as.data.frame(table(dat))
# establish date range
dat.conv <- strptime(dat, format="%Y%m%d") # need to convert dat into POSIX format for this
daterange <- c(min(dat.conv), max(dat.conv))
dat.all <- seq(daterange[1], daterange[2], by="day")
# all possible days
# compare dates from counts dataframe with the whole data range
# assign 0 where there is no count, otherwise take count
# (take out PSD at the end to make it comparable)
dat.all <- strptime(dat.all, format="%Y-%m-%d")

# cant' seem to be able to compare Posix objects with %in%, so coerce them to character for this:
freqs <- ifelse(as.character(dat.all) %in% as.character(strptime(cts$dat, format="%Y%m%d")), cts$Freq, 0)

plot (freqs, type="l", xaxt="n", main=paste("Search term(s):",q), ylab="# of articles", xlab="date")
axis(1, 1:length(freqs), dat.all)
lines(lowess(freqs, f=.2), col = 2)