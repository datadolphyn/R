# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library("googleVis")

data(Fruits)
#typeof(Fruits)
M <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(M)

#Chart Outout
#print(M, 'chart')
#cat(M$html$header)
#cat(M$html$chart)
#cat(M$html$footer)