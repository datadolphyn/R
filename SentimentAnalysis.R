# Author: Rajani Aswani Co-Founder @datadolph.in
# Author: Jitender Aswani
# Date: 2012-30-1
# Description: Extracts tweets from twitter and run sentiment analysis on using list of sentiment words from Hu and Liu
# Packages Used: RCurl, XML, TwitteR, RJSONIO
# Blog Reference: http://www.r-bloggers.com/updated-sentiment-analysis-and-a-word-cloud-for-netflix-the-r-way/
# Download
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.
# Revised Sentiment Analyis using Hu & Liu's library of 6,800 negative and positive words

#Populate the list of sentiment words from Hu and Liu (http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html)
huliu.pwords <- scan(paste(SRC_DIR, 'opinion-lexicon/positive-words.txt', sep=""), what='character', comment.char=';')
huliu.nwords <- scan(paste(SRC_DIR,'opinion-lexicon/negative-words.txt', sep=""), what='character', comment.char=';')
# Add some extra words
huliu.nwords <- c(huliu.nwords,'wtf','wait','waiting','epicfail', 'crash', 'bug', 'bugy', 'bugs', 'slow', 'lie')
#Remove some words for example sap and Cloud
huliu.nwords <- huliu.nwords[!huliu.nwords=='sap']
huliu.nwords <- huliu.nwords[!huliu.nwords=='cloud']

#
# clean up a large character string
#
cleanText <- function(x) {
  # tolower
  x = tolower(x)
  # remove rt
  #x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove control characters
  x <-  gsub('[[:cntrl:]]', '', x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+",  "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", " ", x)
  # remove new lines
  x = gsub("[ |\n]{1,}", " ", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

#
# clean up tweets or any other doucment for corpus and sentiment  analysis
#
cleanContent <- function(content){
  # clean out non-ASCII characters, remove numbers, puncuations, stop words
  content <- sapply(content, function(x) iconv(x, "latin1", "ASCII", sub=""))
  content <- cleanText(content) # clean up
  # remove stop-words
  content <- removeWords(content,
                         c(stopwords("english"),  "twitter", "wikipedia"))
  return(content)
}
#
# build a generic tag cloud
#
buildTagCloud  <- function (content, word.threshold=2){
  #cleanup
  content <- cleanContent(content)
  # make corpus for text mining
  content.corpus <- Corpus(VectorSource(content))
 
  #build a term document
  #content.dtm <- TermDocumentMatrix(content.corpus, control = list(stopwords = TRUE, minWordLength = 5))
  content.dtm <- TermDocumentMatrix(content.corpus, control = list(minWordLength = 5))
  # get a matrix
  content.m = as.matrix(content.dtm)
  # get word counts in decreasing order
  content.words <- sort(rowSums(content.m), decreasing=TRUE)
  # create a data frame with words and their frequencies
  content.df = data.frame(text=names(content.words), size=content.words)
  #write.csv(content.words, "company-word-tag.csv", row.names=F)
  return( content.df[content.df$size > word.threshold,])
}

#
# get sentiment score for each tweet
#
getSentimentScore <- function(tweets) {
  scores <- laply(tweets, function(singleTweet) {
    tweetWords <- unlist(str_split(tolower(singleTweet), '\\s+'))
    # compare our words to the dictionaries of positive & negative terms
    # match() returns the position of the matched term or NA, apply is.na to convert to boolean
    pos.matches <- !is.na(match(tweetWords, huliu.pwords))
    neg.matches <- !is.na(match(tweetWords, huliu.nwords))
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  })
  return(data.frame(SentimentScore=scores, Tweet=tweets))
}

#
#perform sentiment analysis
#
performSentimentAnalysis <- function(tweets){
  #
  # perform twitter sentiment analysis for each tweet
  #
  tweets <- cleanContent(tweets)
  # call getSentiment score
  ss <- getSentimentScore(tweets)
 
  # Get rid of tweets that have zero score and seperate +ve from -ve tweets
  ss$posTweets <- as.numeric(ss$SentimentScore >=1)
  ss$negTweets <- as.numeric(ss$SentimentScore <=-1)
 
  # Let's summarize now
  summary <- list(TweetsFetched=length(ss$SentimentScore),
                  PositiveTweets=sum(ss$posTweets), NegativeTweets=sum(ss$negTweets),
                  AverageScore=round(mean(ss$SentimentScore),3))
 
  # some tweets have no score - positive offsets negative - so the next line is necessary
  summary$TweetsWithScore <- summary$PositiveTweets + summary$NegativeTweets
 
  #Get Sentiment Score
  summary$SentimentScore  <- round(summary$PositiveTweets/summary$TweetsWithScore, 2)
  return(summary)
}

#
#search twitter using hash tags
#
searchTwitterHashtag <- function(tw.hashtag, certificate.path, how.many=300, what.lang="en") {
  tweets <- try(searchTwitter(tw.hashtag, lang=what.lang, n=how.many, cainfo=certificate.path), silent=T)
  if("try-error" %in% class(tweets))
    return(data.frame(error="Oops an error occurred"))
  return(tweets)
}

#search twitter using handle
searchTwitterHandle <- function(tw.handle, certificate.path, how.many=300) {
  tweets <-  try(userTimeline(tw.handle, n=how.many, cainfo=certificate.path), silent=T)
  if("try-error" %in% class(tweets))
    return(list(error="Oops an error occurred"))
  return(tweets)
}

#download ca cert file
downloadCACertFile <- function(){
  download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
}

# Certain words may not be relevant for your secenario to be counted as positive and negative on Hu and Liu list
# Remove them before sending the words to this list
# Fitler words
filterWords <- function(words){
  if(length(which(words %in% filter.words)) > 0)
    words <- words[-which(words %in% filter.words)]
  return(words)
}

# example
#get tweets and perform sentiment analysis
tweets <- searchTwitterHandle(twitter.handle, CERTIFICATE_PATH, 300)
summary <- performSentimentAnalysis(tweets)

