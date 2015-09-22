#Check library path
pkg <- installed.packages() 
pkg[which(pkg[, 1] == "base"), 2]  