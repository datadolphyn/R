# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

getResult <- function(dt, expr, gby) {
  return(dt[,eval(expr), by=eval(gby)])
}
dt <- data.table(iris)
v1 <- "Sepal.Length"
v2 <- "Species"
e <- parse(text = paste("sum(", v1, ", na.rm = TRUE)"))
b <- parse(text = v2)
#rDT2 <- dt[, eval(e), by = eval(b)]
dtR <- getResult(dt = dt, expr = e, gby = b)


i <- which(colnames(padDT)==m)
getData(padDT, sum(i, na.rm=TRUE), d)
#getData(padDT, parse(text=paste("sum(", m, ",na.rm=TRUE)", sep="")), d) lapply(padDT, class)


getResult <- function(dt, expr, gby) {
  e <- substitute(expr)
  b <- substitute(gby)
  return(dt[,eval(e),by=b])
}
v1 <- "Sepal.Length"
v2 <- "Species"


getResult(dt, sum(get(v1), na.rm=TRUE), v2)
#e <- substitute(expr)
#b <- substitute(gby)
