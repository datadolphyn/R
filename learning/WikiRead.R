# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library(XML)
library(RColorBrewer)
library(plyr)
library(quantmod)
library(Heatplus)
 
# get the list of symbols
l <- readHTMLTable('http://en.wikipedia.org/wiki/List_of_S%26P_500_companies')[[2]]
l <- as.vector(l$Ticker)
l <- l[c(-59, -71, -80, -124, -141, -147, -275, -283, -292, -299, -309, -316, -360, -378, -381, -406, -439, -470, -471)]
 
getMonthlyReturns <- function(sym) {
  y <- to.monthly(getSymbols(sym, auto.assign=FALSE, from='2007-01-01'))
	as.vector(ClCl(y)*100)
}
 
d <- unlist(llply(l, getMonthlyReturns, .progress="text"))
# bounds at -10% and +10% for visual clarity
d[d < -10] <- -10
d[d > 10] <- 10
 
heatmap_2(t(matrix(d, ncol=481)), col=brewer.pal(9, 'PuBu'), Rowv=NA, Colv=NA, do.dendro=c(FALSE,FALSE), scale='none', legend=2, main="S&P 500 since 2007 (monthly returns)")



http://blog.datapunks.com/2011/10/sp-500-components-heatmap-in-r/
  
getWikiFC <- function(d) {  
  l <- try(readHTMLTable("http://en.wikipedia.org/wiki/Forbes%27_list_of_the_most_valuable_football_clubs", 
            header=FALSE, skip.rows=1, which=d$tableNo, stringsAsFactors = FALSE))
  l$year = d$year
  ifelse(d$year > 2010, return(with(l, data.frame(year, club=V2, country=V3, value=V4, revenue=V6))), 
         return(with(l, data.frame(year, club=V2, country=V3, value=V4, revenue=V7))))
}
d <- data.frame(year=2012:2007, tableNo=2:7) 
mostValuedSoccerClubs <- ddply(d, .(year), getWikiFC)




l <- readHTMLTable("http://en.wikipedia.org/wiki/Forbes%27_list_of_the_most_valuable_football_clubs")[[7]]