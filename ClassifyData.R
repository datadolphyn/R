# Classify Data
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.
# new addition


#
# Generate extra date cols
#
generateExtraDateCols < function(data, col.index) 
  lMonths <- c("January","February","March", "April,"May","June","July","August","September", "October","November","December")
  lDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  #data$Day_generated=factor(weekdays(data[,col.index]), levels=lDays, ordered=TRUE)
  data$Day_g=factor(weekdays(data[,col.index]), levels=lDays)
  #data$Month_generated=factor(months(data[,col.index]), levels=lMonths, ordered=TRUE) 
  data$Month_g=factor(months(data[,col.index]), levels=lMonths)
  data$Quarter_g=as.factor(quarters(data[,col.index]))
  data$Year_g=as.factor(years(data[,col.index]))
  #data$Year=as.numeric(format(data[,col.index], "%Y"))
  #as.numeric(format(date1, "%m"))
  #format(date1, "%b") ## Month, char, abbreviated
  return(data)
}
#
# Define all supported date formats
#
getAllowedDateFormats <- function() {
  #d = '([0 ]?[1-9]|[12][0-9]|3[01])'      # 1-31 with leading zero or space for single digits
  #m = '([0 ]?[1-9]|1[0-2])'               # 01-12 with leading zero or space for single digits
  d = '([0][1-9]|[12][0-9]|3[01])'      # 1-31 with leading zero for single digits
  m = '([0][1-9]|1[0-2])'               # 01-12 with leading zero for single digit  
  Y = '(17[0-9][0-9]|18[0-9][0-9]|19[0-9][0-9]|20[0-9][0-9])'   # 1700 - 2099
  #Y = '(19[0-9][0-9]|20[0-9][0-9])'   # 1900 - 2099
  y = '([0-9][0-9])'                  # 00-99
  b='([a-zA-Z]{3})'
  B='([a-zA-Z]+)'
  #q = '([qQ][1-4])'
  #patterns <- c("%Y%m%d", "%m%d%y", "%m%d%Y", "%d%m%y", "%d%m%Y", "%y%m%d")
  #patterns <- c("Ymd", "mdy", "mdY", "dmy", "dmY", "ymd", "y", "Y", "by", "bY", "By", "BY")
  #removed %y from the list to avoid getting numerical column identified, moved %Y at the front to avoid 4 digit year being classified as %d%m%y
  patterns <- c("Y","Ymd", "mdy", "mdY", "dmy", "dmY", "ymd","by", "bY", "By", "BY")
  #patterns <- c("Y")
  separators <- c("", " ", "/", "-", ".")
  #strptime {base}  /^[a-zA-Z]{3}\s+[0-9]{2}$/
  allowed.date.list <- list()
  
  for (i in 1:length(patterns)) {
    for (j in 1:length(separators)) {
      p <- patterns[i]
      s <- separators[j]
      pattern.length <- nchar(p)
      chars <- substring(p,seq(1, pattern.length), seq(1, pattern.length))
      m.chars <- paste("%", chars, sep="")
      format <- paste(m.chars,collapse=s)
      if(!s %in% c("", " ")){
        s <- paste("\\", s, "", sep="" )
      }
      regex <- ''
      if( pattern.length == 3) {
        regex <- paste('^', eval(get(chars[1])), s, eval(get(chars[2])), s, eval(get(chars[3])), '$', sep="")
      } else if ( pattern.length == 2) {
        regex <- paste('^', eval(get(chars[1])), s, eval(get(chars[2])),'$', sep="")
      } else if ( pattern.length == 1) {
        regex <- paste('^', eval(get(chars[1])),'$', sep="")
      }
      allowed.date.list[[regex]] = format
    }
  }
  return (allowed.date.list)
}

#
# get date format for sample of values
#
getDateFormat <- function(sample.dates) {
  allowed.date.list <- getAllowedDateFormats()
  #sample.dates <- sample.dates[!is.na(sample.dates)]
  for(i in 1:length(allowed.date.list)) {
    #Check how many items matced a date pattern
    matched <- sum(grepl(names(allowed.date.list)[i], sample.dates))
    if(matched == length(sample.dates)) {
      #print(names(allowed.date.list)[i])
      return(allowed.date.list[[i]])
    }
  }
  return(NULL)
}
  
#Test this function  
testDates <- function() {
  #dates <- c("March 2010","December 2011", "February 2012", "March 2014", "Jan 2013")
  dates <- c("1962", "1949", "1932", "1963", "1925")#, 1937, 1912, 1944, 1930, 1958, 1939, 1919, 1915)
  #dates <- seq(as.Date("2000/1/1"), by="month", length.out=1000)
  #Remove NA: dates <- dates[!is.na(dates)]
  date.format <- getDateFormat(dates)
  #d <- as.Date(dates, date.format)
  if(date.format %in% c("%y", "%Y")) {
    dates = as.Date(paste("1 1 ", dates, sep=""), format=paste("%d %m ", date.format, sep=""))
  } else if (date.format %in% c("%b%y","%b %y", "%b/%y", "%b-%y", "%b.%y", 
                                "%b%Y", "%b %Y", "%b/%Y", "%b-%Y", "%b.%Y", 
                                "%B%y", "%B %y", "%B/%y", "%B-%y", "%B.%y", 
                                "%B%Y", "%B %Y", "%B/%Y", "%B-%Y", "%B.%Y")) {
    sep <- gsub("%[a-zA-Z]{1}(.*)%[a-zA-Z]{1}", "\\1", date.format)
    dates = as.Date(paste("1", dates, sep=sep), format=paste("%d", date.format, sep=sep))
     
  } else {
    dates = as.Date(dates, format=date.format)
  }
}

#
# Test if date
#
isDate <- function(col, colname="date") {
  allowed.date.headers <- c("date", "time", "index")
  #if(tolower(colname) %in% allowed.date.headers || grepl("[date]")) {
  #  print("date found in the header")
  #}
  return (getDateFormat(col))
}

#
# Test if logical
#
isColLogical <- function(col) {
  if(length(levels(factor(col))) == 2) {
    bool.types <- c('true', 'false', 't', 'f', '1', '0', 'y', 'n', 'yes', 'no')
    countBool <- sum(sapply(tolower(col), function(x) x %in% bool.types), na.rm=TRUE)
    if (countBool == length(col))
      return(TRUE)
    else 
      return(FALSE)
  } else
      return (FALSE)
}

#
#test if number
#
isNumber <- function(col) {
  countBool <- sum(sapply(col, is.numeric), na.rm=TRUE)
  if (countBool == length(col))
    return(TRUE)
  else 
    return(FALSE)
}

#
#Main funciton which takes a column (or a vector) and attempts to classify it...
#
classifyData <- function(data) {
  #Default sample size
  sample.size <- 30
  if(nrow(data) <= 30)
    sample.size <- nrow(data)
  
  #Remove rows that have NA
  if(ncol(data) == 1) { # this is an intersting one - if pad has only one column, the above line returns a vector rather than a data.frame hence this loop
      col.names <- colnames(data)
      data <- as.data.frame(data[rowSums(is.na(data)) != ncol(data),])
  
      #Take a small sample
      data.sample <-  as.data.frame(data[sample(1:nrow(data)[1], size=sample.size, replace=FALSE),])
      colnames(data) <- col.names
  } else {
      data <- data[rowSums(is.na(data)) != ncol(data),]
      #Take a small sample
      data.sample <- data[sample(1:nrow(data)[1], size=sample.size, replace=FALSE),]
  }  
  cls <- sapply(data.sample, class)
  #print(cls)
  classified <- list()
  col.names <- colnames(data.sample)
  for (i in 1:ncol(data.sample)) {
    
    col <- data.sample[,i]
    col <- col[!is.na(col)] #remove NA

    #check for boolean
    if(isColLogical(col)) {
      #storage.mode(data[,i]) = "logical"
      data[,i] = as.factor(data[,i])
      #classes.classified[[i]] = "logical"
      next
    }
    
    #check for date names (in date, time)
    date.format <- isDate(col)
    #print(date.format)
    if(!is.null(date.format)) {
      if(date.format %in% c("%y", "%Y")) {
        data[,i] = as.Date(paste("1 1 ", data[,i], sep=""), format=paste("%d %m ", date.format, sep=""))
      } else if (date.format %in% c("%b%y","%b %y", "%b/%y", "%b-%y", "%b.%y", 
                                    "%b%Y", "%b %Y", "%b/%Y", "%b-%Y", "%b.%Y", 
                                    "%B%y", "%B %y", "%B/%y", "%B-%y", "%B.%y", 
                                    "%B%Y", "%B %Y", "%B/%Y", "%B-%Y", "%B.%Y")) {
        sep <- gsub("%[a-zA-Z]{1}(.*)%[a-zA-Z]{1}", "\\1", date.format)
        data[,i] = as.Date(paste("1", data[,i], sep=sep), format=paste("%d", date.format, sep=sep))
      } else {
        data[,i] = as.Date(data[,i], format=date.format)
        generateExtraDateCols(data, i)
      }
      next
    }
    
    #check for int
    if (isNumber(col)){
      data[,i] = as.numeric(data[,i])
      next
    }
    #check for factor  
      
  }
  if(verbose) print("Classification Done!")
  logMessage("Classification Done!")
  
  return(data)
}


testClassification <- function() {
  
  #l <- c(1,0,1,0,0,0,1,0)
  #z <- sample(c(TRUE,FALSE),1000000,rep=TRUE)
  #z <- tolower(z)
  #Geneates a matrix
  #date.patterns <- unlist(
  #  lapply(separtors, function(y)
  #    lapply(patterns, function(x) paste(substring(x,seq(1,nchar(x), 2), seq(2,nchar(x), 2)), 
  #                                       collapse=y)
  #    )))
  
  #sample.df <- data.frame(bigmove=sample(c("y","n"),1000,rep=TRUE), 
  #                        date=seq(as.Date("2000/1/1"), by="month", length.out=1000), 
  #                        value=rep(1:100,100), stringsAsFactors = FALSE)
  
  sample.df <- data.frame(dates=c("1-31-2012", "1-1-2011", "1-1-2010", "1-1-2009"), pass=c(1,0,1,0), name=c("a", "bsd", "adaads", "dafds"), stringsAsFactors = FALSE)
  print(sapply(sample.df, class))
  data <- classifyData(sample.df)
  print(sapply(data, class))
}

  if(FALSE) 
    {
    #Symbol  Meaning  Example
  #%d	day as a number (0-31)	01-31
  #%a
  #%A	abbreviated weekday 
  #unabbreviated weekday	Mon
  #Monday
  #%m	month (00-12)	00-12
  #%b
  #%B	abbreviated month
  #unabbreviated month	Jan
  #January
  #%y
  #%Y	2-digit year 
  #4-digit year	07
  #2007
   # metaChar = c("$","*","+",".","?","[","^","{","|","(","\\")
   # 
    #`~!@#$%^&*()_|+\-=?;:'",.<>\{\}\[\]\\\/]
    
  }
