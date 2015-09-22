# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

rm (list = ls())
setwd("~/Toga/Alto")
library("chron")
library("plyr")
library("RJSONIO")
library("data.table")
library("ggplot2")
in.file.name <- "datasets/BollywoodCinema-1940-2008.csv"
out.file.name <- "datasets/masterblaster/TransformedSachinTestRecords.csv"
out.file.JSON <- "datasets/masterblaster/SachinTestRecords.JSON"
out.file.R.dataframes <- "datasets/masterblaster/MB.RData" 
dt <- data.table(read.csv(in.file.name, na.strings="-", as.is=TRUE, header=TRUE, 
             stringsAsFactors=FALSE, strip.white=TRUE))

lMonths <- c("January","February","March", "April","May","June","July","August","September", "October","November","December")
lDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")


# Line plot
ggplot(totmidc, aes(variable, value)) + geom_line() + xlab("") + ylab("")

# Bar plot
# Note that the parameter stat="identity" passed to geom_bar()
ggplot(totmidc, aes(x=variable, y=value)) + geom_bar(stat="identity") + xlab("") + ylab("")


d <- dt[,list(Deols=sum(grepl("deol|Dharmendra",Cast, ignore.case=TRUE)), 
              Kapoors=sum(grepl("kapoor",Cast, ignore.case=TRUE)),
              Khans=sum(grepl("khan",Cast, ignore.case=TRUE)),
              Bachchans=sum(grepl("bachchan",Cast, ignore.case=TRUE))), 
        by=year]
d1 <- melt(d, id.var="year")
ggplot(d1)+geom_bar(aes(x=year,y=value,fill=variable),stat='identity')

ggplot(d, aes(year))+ geom_bar() +facet_wrap(~n)
ggplot(d, aes(x=factor(year), y=c(Deols, Kapoors))) + geom_bar(stat="identity") + xlab("") + ylab("Deols") 
n <- c("Deols", "Kapoors")
ggplot(d, aes(x=year, )) + geom_bar(stat="identity") + facet_wrap(n)



dat<-data.frame(num=1:3,usage=c(4,2,5),cap=c(10,20,10),diff=c(6,18,5)) 
dat.melt<-melt(dat,id.var=c('num','cap')) 
ggplot(dat.melt)+geom_bar(aes(x=num,y=value,fill=variable),stat='identity') 

#IPL DS
in.file.name <- "pads/data/pad56638.csv" pad89706
dt <- data.table(read.csv(in.file.name, na.strings="-", as.is=TRUE, header=TRUE, 
             stringsAsFactors=FALSE, strip.white=TRUE))
dt[,Sixes, by=Player][order(-Sixes)]

getCount <- function(dat, expr, gby) {
  e <- substitute(expr)
  b <- substitute(gby)
  print(dat[,eval(e),by=b])
}
getCount(dt, sum(Sixes), Player)

q <- quote(Sixes)
q1 <- quote(Player)
dt[,eval(q)), by=q1 ][order(-eval(q))]
dt[,q,by=q1]

#IPL DS
in.file.name <- "pads/data/pad89706.csv" 
mbdt <- data.table(read.csv(in.file.name, na.strings="-", as.is=TRUE, header=TRUE, 
             stringsAsFactors=FALSE, strip.white=TRUE))
mbdt[,list(sum(as.integer(Test_Runs), na.rm=TRUE)), by=Year][order(Year)]
