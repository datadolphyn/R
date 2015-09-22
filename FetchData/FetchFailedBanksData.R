library(XML)
library("chron")
library(plyr)
library(stringr)

source("UtilPADS.R")

run <- function() {
  #col names
  col.names <- c("bank_name","city","st","closing_date","acquiring_institution","assets_in_mil_usd")
  #wiki url
  url <- "http://en.wikipedia.org/wiki/List_of_bank_failures_in_the_United_States_(2008%E2%80%93present)"
  # read wiki page
  page <- htmlParse(url, encoding="UTF-8")
  # read all tables
  all.tables <- try(readHTMLTable(page, colClasses=c('numeric', 'character', 'character', 'character', 'mixed.dates', 
                                            'character', 'num.with.commas'), stringsAsFactors = FALSE, 
                                              trim=TRUE, elFun = removeSpecialChars))
  
  #bind all the tables together
  s.d <- do.call(rbind, all.tables)
  # fix row names
  rownames(s.d) <- 1:nrow(s.d)
  # remove first column 
  s.d <- s.d[-c(1)]
  # assing column names
  colnames(s.d) <- col.names
  saveData(s.d, "./pads/raw-data/failed-banks/failed_banks_assets_fdic.csv")
}

run()