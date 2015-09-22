# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

rm (list = ls())
setwd("~/Toga/Alto")
# Revised Sentiment Analyis using Hu & Liu's library of 6,800 negative and positive words
library("twitteR")
library("plyr")
library("stringr")
library("doBy")

#Populate the list of sentiment words from Hu and Liu (http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html)
huliu.pwords <- scan('opinion-lexicon/positive-words.txt', what='character', comment.char=';')
huliu.nwords <- scan('opinion-lexicon/negative-words.txt', what='character', comment.char=';')

# Add some words
huliu.nwords <- c(huliu.nwords,'wtf','wait','waiting','epicfail', 'crash', 'bug', 'bugy', 'bugs', 'slow', 'lie')
#Remove some words
huliu.nwords <- huliu.nwords[!huliu.nwords=='sap']
huliu.nwords <- huliu.nwords[!huliu.nwords=='cloud']
#which('sap' %in% huliu.nwords)

cList <- list(twitterTag=c("#DKOM"))

getTweets <- function(tag) {
  # Get 1500 tweets - an individual is only allowed to get 1500 tweets
  return(searchTwitter(tag, n=1500))
  #since=as.character(Sys.Date()-3 - If you want to restrict this to just last few days tweets
}


getSentimentScore <- function(tweets)
{
  scores <- laply(tweets, function(singleTweet) {
    # clean up tweets with R's regex-driven global substitute, gsub()
    singleTweet <- gsub('[[:punct:]]', '', singleTweet)
    singleTweet <-  gsub('[[:cntrl:]]', '', singleTweet)
    singleTweet <- gsub('\\d+', '',  singleTweet)
    #Convert to lower case for comparision, split the tweet into single words and flatten the list
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

#Step #1

sentimentScoreDF <- data.frame()
sentimentScoreDF <- ldply(cList$twitterTag, function(twitterTag) {
  tweets <- getTweets(twitterTag)
  tweets.text <- laply(tweets,function(t)t$getText())
  sentimentScore <- getSentimentScore(tweets.text)
  sentimentScore$TwitterTag <- twitterTag
  rbind(sentimentScoreDF, sentimentScore)
})

# Get rid of tweets that have zero score and seperate +ve from -ve tweets
sentimentScoreDF$posTweets <- as.numeric(sentimentScoreDF$SentimentScore >=1)
sentimentScoreDF$negTweets <- as.numeric(sentimentScoreDF$SentimentScore <=-1)

#Summarize finidings
summaryDF <- ddply(sentimentScoreDF,"TwitterTag", summarise, 
                 TotalTweetsFetched=length(SentimentScore),
                 PositiveTweets=sum(posTweets), NegativeTweets=sum(negTweets), 
                 AverageScore=round(mean(SentimentScore),3))

summaryDF$TotalTweets <- summaryDF$PositiveTweets + summaryDF$NegativeTweets

#Get Sentiment Score
summaryDF$Sentiment  <- round(summaryDF$PositiveTweets/summaryDF$TotalTweets, 2)

#Order
orderBy(~-Sentiment,summaryDF)

write.csv(summaryDF, "DKOM-Results.csv")




#summary <- ddply(sentimentScoreDF, "TwitterTag", function(df) { MeanScore=round(mean(df$SentimentScore),3)})
summary <- ddply(sentimentScoreDF, "TwitterTag", summarise, 
      MeanScore=round(mean(SentimentScore),3), TweetCount=length(SentimentScore))

period <- seq(as.Date("2010-01-01"), by="month", length.out=24)
tweets <- llply(period, function(since){
  until <- seq(since, by="month", length.out=2)[2] 
  print(since)
  print(until)
  tt <- searchTwitter("#SALESforce", n=1500, since=since, until=until )
  print(length(tt))
  return(length(tt))
})

getAuthorized <- function() {
  cred <- OAuthFactory$new(consumerKey="g8sky1SyttNWPsgZOGAA", 
                           consumerSecret="zuJvHtN63yLSYh9Bk2Qc0kd5kGIV9mtzb7r6XPVqq8",
                           requestURL="https://api.twitter.com/oauth/request_token",
                           accessURL="https://api.twitter.com/oauth/access_token",
                           authURL="https://api.twitter.com/oauth/authorize")
  cred$handshake()
  registerTwitterOAuth(cred)
  #3739000
  save(cred, file="TwitterAanalyticsForAll.Cred")
}

loadCred <- function(){  
  cred <- load("TwitterAanalyticsForAll.Cred")
  cred$handshake()
  registerTwitterOAuth(cred)
}
