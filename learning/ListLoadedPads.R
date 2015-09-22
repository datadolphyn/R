# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

setContentType("text/html")
#Step 2 - load environment
pads.loaded <- ls(pattern='pad[0-9]', envir=.GlobalEnv)
#pads.loaded <- ls()
#cat(is.environment(.rAenv))
cat("This is a test script. It checks for pre-loaded pads. <br/> Following pads are pre-loaded in this environment: <br/>");
cat(paste("No. of Pads: ", length(pads.loaded), "<br/>", sep=""))
getDetails <- function(x)
{
  d <- get(x)
  return(paste("PadName:", x, ", Records:", nrow(d), ", Columns:", length(d), ", ColNames:", paste(colnames(d), collapse="; "), ", Col Classes:", paste(lapply(d, class), collapse="||"), sep=""))
}
v <- vapply(pads.loaded, getDetails, "Pad Was Not Found.")
cat(paste(v, collapse="<br/>"))