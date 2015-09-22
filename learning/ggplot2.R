# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

pres <- read.csv("http://www.stanford.edu/~messing/primaryres.csv", as.is=T)

# sort data in order of percent of vote:
pres <- pres[order(pres$Percentage, decreasing=T), ]

# only show top 15 candidates:
pres <- pres[1:15,]

# create a precentage variable
pres$Percentage <- pres$Percentage*100

# reorder the Candidate factor by percentage for plotting purposes:
pres$Candidate <- reorder(pres$Candidate, pres$Percentage)

# To install ggplot2, run the following line after deleting the #
#install.packages("ggplot2")

library(ggplot2)
ggplot(pres, aes(x = Percentage, y = factor(Candidate) )) + 
  	geom_point() +
		theme_bw() + opts(axis.title.x = theme_text(size = 12, vjust = .25))+ 
		xlab("Percent of Vote") + ylab("Candidate") +
		opts(title = expression("New Hampshire Primary 2012"))