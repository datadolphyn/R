# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

e <- new.env()
e$foo <- 42
attach(e, name='datagram')
rm(list=ls()) # Remove all in global env
foo # Still there!
#...and to detach it:
#detach('datagram')