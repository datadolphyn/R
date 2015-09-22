# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library("rjson")
x <- list( alpha = 1:5, beta = "Bravo", gamma = list(a=1:3, b=NULL), delta = c(TRUE, FALSE) ) 
json <- toJSON( x )
fromJSON( json )
#named vectors are treated as JSON objects (lists) 
toJSON(islands[1:4])
#data.frames must be converted into a list before converting into JSON 
plot(cars, pch=2) 
json_cars <- toJSON(as.list(cars)) 
points( data.frame( fromJSON( json_cars ) ), col="red", pch=3 )
#special R types are encoded as strings 
testString <- c(1,2,3,4,NA,NaN,Inf,8,9); 
toJSON(testString);

sample_json <- ' { "breakfast" : [ "milk", "fruit loops", "juice" ], "lunch" : [ "left over sushi" ] } '
parser <- newJSONParser()
parser$addData( sample_json ) 
food <- parser$getObject() 
print( food )

#This is equivalent to using FromJSON( sample_json ) 
#However, sample_json can be split into several parts:
### EXAMPLE 2:
part_1 <- '{ "breakfast" : [ "milk", "fruit loops", "juice" ], ' 
part_2 <- '"lunch" : [ "left over sushi" ]' 
part_3 <- '} [1,2,3,4,5]' #close off the first object, and create a 2nd JSON object, which i
parser <- newJSONParser() 
parser$addData( part_1 )
parser$getObject() #returns NULL - since part_1 isn't complete 
parser$addData( part_2 ) 
parser$getObject() #returns NULL - since part_2 still isn't complete 
parser$addData( part_3 )
parser$getObject() #returns the first food object 
parser$getObject() #returns the second array