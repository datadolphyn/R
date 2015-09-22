# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

#Set content type for output
setContentType("text/html")
post <- str(POST)
print(post)
cat(post)
cat("<br/>")

#Files
FileStr <- str(FILES)
cat(FileStr)
cat("<br/>")

#Or you can use the paste function 
cat(paste(FILES,names(FILES)))
cat("<br/>")

destination <- file.path('/Users/homemac/Toga/tmp',FILES$FirstFile$name)
file.copy(FILES$FirstFile$tmp_name,destination,overwrite=TRUE)