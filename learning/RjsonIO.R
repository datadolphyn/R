# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

setwd("/Users/homemac/Toga/Alto")
library("RJSONIO")
l <- fromJSON('[{"winner":"68694999",  "votes":[ {"ts":"Thu Mar 25 03:13:01 UTC 2010", "user":{"name":"Lamur","user_id":"68694999"}}, {"ts":"Thu Mar 25 03:13:08 UTC 2010", "user":{"name":"Lamur","user_id":"68694999"}}], "lastVote":{"timestamp":1269486788526,"user":{"name":"Lamur","user_id":"68694999"}},"startPrice":0}]')
m <- lapply(l[[1]]$votes, function(x) c(x$user$name, x$user$user_id, x$ts))
ma <- matrix(unlist(m), ncol=3, byrow=TRUE)
p <- data.frame(ma)