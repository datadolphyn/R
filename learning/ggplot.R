# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

library (ggplot2) 

v1  <- c(1,2,3,3,4) 
v2  <- c(4,3,1,1,9) 
v3  <- c(3,5,7,2,9) 
gender <- c("m","f","m","f","f") 

d.data  <- data.frame (v1, v2, v3, gender) 

molten <- melt(d.data) 

Average <- ddply(molten, c("gender", "variable"), function(z){ 
        c(Mean = mean(z$value))} 
) 

ggplot (data=Average, aes(x = variable, y = Mean, fill = gender)) + 
                geom_bar(position = position_dodge()) + 
                coord_flip() + 
                geom_text (position = position_dodge(0.5), 
aes(label=round(Mean, 1)), vjust=0.5, hjust=4,colour="white", size=7) 

ggplot (data=Average, aes(x = variable, y = Mean)) + 
                geom_bar() + 
                geom_text(aes(label=round(Mean, 1)), vjust=0.5, 
hjust=4,colour="white", size=7) + 
                facet_wrap(~gender) 