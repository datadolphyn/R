#
# Generate maplight
#

require(data.table)

assign("folder.path", "./pads/raw-data/maplight/", envir=.GlobalEnv)
#assign("data.file", "data.csv.gz", envir=.GlobalEnv)
#ml.data.f <- data.table(read.csv(paste(folder.path, data.file, sep="")))

assign("data.cache", "ml-data.Rdata", envir=.GlobalEnv)
ml.data <- data.table(read.csv(paste(folder.path, data.cache, sep="")))
