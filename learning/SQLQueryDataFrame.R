# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

rm (list = ls())
setwd("~/Toga/Alto")

#load pre-saved dataframe
out.file.R.dataframes <- "datasets/masterblaster/mb.RData" 
load(out.file.R.dataframes)

library(plyr)

## Operate on a data.frame, split by "by", by could be a vector or a single filed
## aggreage by columns in vector on, on could be a single field also
getSum <- function(df, by, on) {
    #print(sum(df[,on],  na.rm=TRUE))
    return(ddply(df, by, colwise(function(col) {sum(col, na.rm=TRUE)}, on)))
}
## Try it out
crpopy <- getSum(mbDS, .(Opposition, Year), .(Test.Century, Test.Runs))

## Operate on a data.frame, split by "by", by could be a vector or a single filed
## and aggreage by columns in vector on, on could be a single field also
getCount <- function(df, by, on) {
    return(ddply(df, by, colwise(function(col) {length(unique(col))}, on)))
}
## Try it out
sx <- getCount(mbDS, "Year", c("TestNo", "Ground"))

#For counting column that has 0 or 1 value :: Very similar to getSum - the answer should be same
getBinaryCount <- function(df, by, on) {
    return(ddply(df, by, colwise(function(x) {length(which(x==1))}, on)))
    #length(which(mbDS[,"Test.Century"] ==1))
}
## Try it out
cpo <- getBinaryCount(mbDS, "Opposition", c("Ing1.Century", "Ing2.Century", "Test.Century"))

###################### Samples ####################

#Runs by Opposition
#rpo <- ddply(mbDS, "Opposition", function(x) c(TotalRuns=sum(x$Test_Runs, na.rm=TRUE)))
#rpo <- ddply(mbDS, .(Opposition), summarise, TotalRuns=sum(Test_Runs, na.rm=TRUE))
rpo <- getSum(mbDS, "Opposition", "Test.Runs")

#Runs by Opposition by Inning
rpopi <- getSum(mbDS, "Opposition", c("Ing1.Runs", "Ing2.Runs", "Test.Runs"))

#Runs by Year
#rpy <- ddply(mbDS, .(Year), summarise, TotalRuns = sum(Test.Runs, na.rm=TRUE))
rpy <- getSum(mbDS, "Year", "Test_Runs")

#Tests Per Opposition
tpo <- getCount(mbDS, "Opposition", "TestNo")

#Tests Per Year
tpy <- getCount(mbDS, "Year", "TestNo")

#Runs by Opposition by Year - userful for clicking on an opposition and drilling down by year
#rpopy <- ddply(mbDS, .(Year, Opposition), summarise, TotalRuns = sum(Test_Runs, na.rm=TRUE))
rpopy <- getSum(mbDS, c("Opposition", "Year"), "Test.Runs")

#Tests Per Opposition By Year
tpopy <- getCount(mbDS, c("Opposition", "Year"), "TestNo")

#Runs and Tests Per Opposition
rtpo <- getSum1Count2(mbDS, "Opposition", c("Test.Runs", "TestNo")) ### Doesn't work

#Total Runs by Opposiotn by Inning
#rpopi <- ddply(mbDS, .(Opposition), summarise, 
#               1stIngRuns = sum(Ing1.Runs, na.rm=TRUE), 
#               2ndIngRuns = sum(Ing2.Runs, na.rm=TRUE),
#              TotalRuns = sum(Test_Runs, na.rm=TRUE))

#Total Runs by Year by Opposition - userful for clicking on a year to drill down
#rpypo <- ddply(mbDS, .(Year, Opposition), summarise, TotalRuns = sum(Test_Runs, na.rm=TRUE))


#Total Centuries by Opposition
#CPO <- ddply(mbDS, .(Opposition), summarise, TestCenturies = sum(Test.Century==1, na.rm=TRUE))
cpo <- getBinaryCount(mbDS, "Opposition", "Test.Century")
#cpo1 <- getSum(mbDS, "Opposition", "Test.Century")

cpo <- getBinaryCount(mbDS, "Opposition", c("Ing1.Century", "Ing2.Century", "Test.Century"))


#Centuries Per Year
cpy <- getBinaryCount(mbDS, "Year", "Test.Century")

#Centuries Per Opposition By Year
cpopy <- getBinaryCount(mbDS, c("Opposition", "Year"), "Test.Century")

#Mege T, R, C by Year
trcpy <- merge(x=merge(tpy,rpy, by.x="Year", by.y="Year"), cpy, by.x="Year", by.y="Year")
colnames(trcpy) <- c("Year", "Tests", "Runs", "Centuries")


# Passing multiple arguments ?colwise example
#sum <- function(x){sum(x, na.rm=TRUE)}
#new <- ddply(tips, c("sex", "smoker"), colwise(function(x) {sum(x, na.rm=TRUE)}, c("tip", "total_bill")))
#new <- ddply(tips, c("sex", "smoker"), colwise(sum, c("tip", "total_bill")))