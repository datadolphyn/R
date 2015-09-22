require(RCurl)
require(data.table)
require(plyr)
setwd("/Users/homemac/dolphy/R/pads/raw-data/immigration")


fetchData <- function(url,yr) {
  #url <- "http://www.flcdatacenter.com/download/H1B_efile_FY02_text.zip"
  #yr <- 2
  #temp <- tempfile()
  #tempdir()
  temp <- paste("temp", yr, ".zip", sep="_")
  download.file(url,temp)
  #data <- read.csv(unz(temp, read.file))
  #write.csv(data,csv.file, row.names=F)
  #l.f <- unzip("H1B_efile_FY03_text.zip") #unzips the file
  unzip(temp) #unzips the file
  unlink(temp) 
  #"EFILE_FY2006.txt"
  #H1B_efile_FY02.txt
}

fy <- 5:6
u <- paste("http://www.flcdatacenter.com/download/H1B_efile_FY0", fy, "_text.zip", sep="")
sapply(seq_along(u), function(i) fetchData(u[i], fy[i]))

readTextData <- function() {
  filenames <- list.files(path=".", pattern="*.txt")

    print(i)
    #yr <-   2000 + as.integer(gsub("[^0-9]", "", gsub("(\\./)(.*)_(.*)_(.*)(\\.txt)", "\\4", i)))
    yr <-   2000 + as.integer(gsub("[^0-9]", "", gsub("(.*)_(.*)_(.*)(\\.txt)", "\\3", i)))
    filename <- gsub("(.*)(\\.txt)", "\\1", i)
    print(yr)
    print(filename)
    pad <- data.table(read.csv(i, stringsAsFactors=F))
    col.names <- colnames(pad)
    setnames(pad, col.names, tolower(col.names))
    #colnames(pad) <- tolower(colnames(pad))
    write.csv(pad,paste(filename, ".csv", sep=""), row.names=F)
  }
}

readCSVData <- function() {
  filenames <- list.files(path=".", pattern="*.csv")
  
  for(i in filenames) {
    print(i)
    #yr <-   2000 + as.integer(gsub("[^0-9]", "", gsub("(\\./)(.*)_(.*)_(.*)(\\.txt)", "\\4", i)))
    yr <-   2000 + as.integer(gsub("[^0-9]", "", gsub("(.*)_(.*)_(.*)(\\.csv)", "\\3", i)))
    pad <- data.table(read.csv(i, stringsAsFactors=F))
    col.names <- colnames(pad)
    setnames(pad, col.names, tolower(col.names))
    generateStats(pad, yr)
  }
}

generateStats <- function(dt, yr) {
  #
  # by total
  #
  dt.total <- dt[,list(year=yr, total=length(approval_status), 
                       approved=length(which(approval_status=="Certified")),
                       denied=length(which(approval_status=="Denied")), 
                       hold=length(which(approval_status=="Hold")), 
                       pending=length(which(approval_status=="Pending")))]
  
  # save this to csv
  write.csv(dt.total, paste("US_H1B_breakdown_by_status_", yr, ".csv", sep=""), row.names=F)
  # bind this to csv
  H1B.total <<- rbind(H1B.total, dt.total)
  
  #
  # by company
  #
  dt.by.company <- dt[, list(year=yr, total=length(approval_status), 
                             approved=length(which(approval_status=="Certified")), 
                             denied=length(which(approval_status=="Denied")), 
                             hold=length(which(approval_status=="Hold")), 
                             pending=length(which(approval_status=="Pending"))), 
                      by=list(employer=name)][order(-total)]
  # save this to csv
  write.csv(dt.by.company[1:15,], paste("US_H1B_top_companies_", yr, ".csv", sep=""), row.names=F)
  # bind this to csv
  H1B.by.company <<- rbind(H1B.by.company, dt.by.company)
  
  #
  # by state
  #
  dt.by.state <- dt[,list(year=yr, total=length(approval_status), 
                          approved=length(which(approval_status=="Certified")),
                          denied=length(which(approval_status=="Denied")), 
                          hold=length(which(approval_status=="Hold")), 
                          pending=length(which(approval_status=="Pending"))), 
                    by=state][order(-total)]
  # save this to csv
  write.csv(dt.by.state[1:15,], paste("US_H1B_top_states_", yr, ".csv", sep=""), row.names=F)
  # bind this to csv
  H1B.by.state <<- rbind(H1B.by.state, dt.by.state)
  
  #
  # by city
  #
  
  dt.by.city <- dt[,list(year=yr, total=length(approval_status), 
                         approved=length(which(approval_status=="Certified")),
                         denied=length(which(approval_status=="Denied")), 
                         hold=length(which(approval_status=="Hold")), 
                         pending=length(which(approval_status=="Pending"))), 
                   by=city][order(-total)]
  # save this to csv
  write.csv(dt.by.city[1:15,], paste("US_H1B_top_cites_", yr, ".csv", sep=""), row.names=F)
  # bind this to csv
  H1B.by.city <<- rbind(H1B.by.city, dt.by.city)
  
  #
  # by title
  #
  dt.by.title <- dt[,list(year=yr, total=length(approval_status), 
                          approved=length(which(approval_status=="Certified")),
                          denied=length(which(approval_status=="Denied")), 
                          hold=length(which(approval_status=="Hold")), 
                          pending=length(which(approval_status=="Pending"))), 
                    by=job_title][order(-total)]
  # save this to csv
  write.csv(dt.by.title[1:15,], paste("US_H1B_top_titles_", yr, ".csv", sep=""), row.names=F)
  # bind this to csv
  H1B.by.title <<- rbind(H1B.by.title, dt.by.title)
  
  
  
  #contribs <- read.csv("path/to/file", colClasses=c(CTRIB_AMT="Currency"))
  
  #
  # by wage
  #
  dt$wage_offered <- as.numeric(gsub("[\\$,]","",dt$wage_rate_1))
  dt$prevailing_wages <- as.numeric(gsub("[\\$,]","",dt$prevailing_wage_1))
  dt.by.wage <- dt[,list(year=yr, wage_offered, 
                                 prevailing_wages, job_title, employer=name), 
                           by=list(rate_per_1=="Year", 
                                   approval_status=="Certified")][,rate_per_1:=NULL][,approval_status:=NULL][order(-wage_offered)][wage_offered < 200000]
  # save this to csv
  write.csv(dt.by.wage, paste("US_H1B_top_wages_", yr, ".csv", sep=""), row.names=F)
  # bind this to csv
  H1B.by.wages <<- rbind(H1B.by.wages, dt.by.wage)
}

H1B.total <- data.table()
H1B.by.company <- data.table()
H1B.by.state <- data.table()
H1B.by.city <- data.table()
H1B.by.title <- data.table()
H1B.by.wages <- data.table()

readCSVData()
write.csv(H1B.total, "US_H1B_breakdown_by_status_2002-2007.csv", row.names=F)
write.csv(H1B.by.company, "US_H1B_top_companies_2002-2007.csv", row.names=F)
write.csv(H1B.by.state, "US_H1B_top_states_2002-2007.csv", row.names=F)
write.csv(H1B.by.city, "US_H1B_top_cities_2002-2007.csv", row.names=F)
write.csv(H1B.by.title, "US_H1B_top_titles_2002-2007.csv", row.names=F)
write.csv(H1B.by.wages, "US_H1B_top_wages_2002-2007.csv", row.names=F)


#lt <- count(ldf2, "approval_status")

# 'x' %in% colnames(df)
lt2 <- as.data.frame.matrix(with(ldf2, table(name, approval_status)))
lt2$company <- rownames(lt2)
rownames(lt2) <- seq(1:nrow(lt2))



u <- "http://www.omegahat.org/RCurl/data.gz"




setClass("Currency")
setAs("character", "Currency",
      function(from) as.numeric(sub("$","",from, fixed=TRUE)))

setClass("CurrencyCommas")
setAs("character", "CurrencyCommas", 
      function(from) as.numeric(gsub(",", "", from) ) )
gsub("[\\$,]","","$1,075.20 ")
if(url.exists(u)) {
  content <- getBinaryURL(u)  #read in the https as a binary
dump.file <- file("temp.zip", open = "wb")
writeBin(content, dump.file)
close(dump.file)               #use this to close the connection
l <- unzip("temp.zip", list=TRUE) #unzips the file
#dir()                    #Look at files in the directory.  We want "SRC2010.mdb"
unlink("temp.zip")    #delete the zip file
#  shell.exec("SRC2010.mdb")
}
library(RODBC)
channel <- odbcConnectAccess("http://www.foreignlaborcert.doleta.gov/pdf/quarter_4_2011/H-1b_iCert_LCA_FY2011_Q4.mdb
")
demographics <- sqlFetch(channel, sqtable="Demographic Factors", colnames = FALSE, rownames = FALSE)
ave.class.sz <- sqlFetch(channel, sqtable="Average Class Size", colnames = FALSE, rownames = FALSE)
att.susp <- sqlFetch(channel, sqtable="Attendance and Suspensions", colnames = FALSE, rownames = FALSE)
drop.out <- sqlFetch(channel, sqtable="High School Completers", colnames = FALSE, rownames = FALSE)
staff <- sqlFetch(channel, sqtable="Staff", colnames = FALSE, rownames = FALSE)
close(channel)


## method 2 using curl
CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")

cookie = 'cookiefile.txt'
curlH = getCurlHandle(
  cookiefile = cookie,
  useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
  header = FALSE,
  verbose = TRUE,
  netrc = TRUE,
  maxredirs = as.integer(20),
  followlocation = TRUE,
  ssl.verifypeer = TRUE)


destfile = "log2.csv"
content = getBinaryURL(url, curl = curlH, cainfo = CAINFO)
## write to file
writeBin(content, destfile)
## read from binary object

## read from binary object
csv.data2 <- read.csv(textConnection(rawToChar(content)))
head(csv.data2)
csv.data2 == csv.data