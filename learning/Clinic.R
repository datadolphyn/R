# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

## generate data for medical example 
clinical.trial <-
    data.frame(patient = 1:100,
               age = rnorm(100, mean = 60, sd = 6),
			year.enroll = sample(paste("19", 85:99, sep = ""),
			                 100, replace = TRUE),
               treatment = gl(2, 50,
                 labels = c("Treatment", "Control")),
               center = sample(paste("Center", LETTERS[1:5]), 100, replace = TRUE)) 
## set some ages to NA (missing) 
is.na(clinical.trial$age) <- sample(1:100, 20)
summary(clinical.trial)

## a simple example of a table call 
table(clinical.trial$center)

## a logical vector is created and passed into table
table(clinical.trial$age < 60)
#FALSE  TRUE 
#   41    39  
 ## the useNA argument shows the missing values, too
table(clinical.trial$age < 60, useNA = "always")
#FALSE  TRUE  <NA> 
#  41    39    20

## the table of missing age by center 
table(clinical.trial$center, is.na(clinical.trial$age))
#           FALSE TRUE
#  Center A    16    6
#  Center B     8    2
#  Center C    23    5
#  Center D    20    3
#  Center E    13    4 
## centers with most missing ages listed in order 
## highest to lowest
sort(table(clinical.trial$center, is.na(clinical.trial$age))[, 2],      
       decreasing = TRUE)
#Center A Center C Center E Center D Center B 
#       6        5        4        3        2  


c1 <- cut(clinical.trial$age, breaks = seq(30, 80, by = 10))
table(c1)