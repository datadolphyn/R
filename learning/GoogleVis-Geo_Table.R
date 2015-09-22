# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library("googleVis")
#Geo
G <- gvisGeoChart(Exports, "Country", "Profit",	options=list(width=200, height=100))
#Table
T <- gvisTable(Exports, options=list(width=200, height=270)) 
#Motion
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370)) 
#Merge Charts
GT <- gvisMerge(G,T, horizontal=FALSE) 
#Merge Horizontal
GTM <- gvisMerge(GT, M, horizontal=TRUE, tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
plot(GTM)