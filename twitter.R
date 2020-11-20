install.packages("twitteR")
install.packages("tm")
install.packages("ROAuth")
library(twitteR)
library(tm)
library(ROAuth)

#Extrating Data from Twitter
twitter <-OAuthFactory$new(consumerKey = "91sBvLLE5GvjHICzTwpc6c4fo", 
                           consumerSecret = "TuXL1lUlFXBPI2ZxGxKfGKh4lUdon4hofpHHjfkNWdHls11bvI", 
                           requestURL = "https://api.twitter.com/oauth/request_token", 
                           accessURL ="https://api.twitter.com/oauth/access_token", 
                           authURL = "https://api.twitter.com/oauth/authorize")
save(twitter, file = "twitter authentification.Rdata")
load("twitter authentification.Rdata")

install.packages("base64enc")
library(base64enc)
install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("91sBvLLE5GvjHICzTwpc6c4fo", #Consumer Key(API Key)
                    "TuXL1lUlFXBPI2ZxGxKfGKh4lUdon4hofpHHjfkNWdHls11bvI", #consumer Secret(API Secret)
                    "2672388621-ifxfyb8aQzRygWRuQ1dHboO5sE13T4HwzhkaFJe", #Access Token
                    "8kl9Bxz6V4geF9gSkPEiAsH1Bn4ENxO2qA03R8Nsa15QG") #Access Token Secret

#RegisterTwitterOAuth (cred)
Tweets <- userTimeline("@sundarpichai", n=1000, includeRts = TRUE)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

getwd()
setwd("C:\\Users\\91755\\Desktop\\Assignment\\16 - Text Mining")
write.csv(TweetsDF, file="twitter.csv", row.names = F)
write.csv(TweetsDF, file = "tweets.txt", row.names = F)

tweet <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\16 - Text Mining\\twitter.csv")
View(tweet)
tweet <- tweet[,1]
tweets <- VCorpus(VectorSource(tweet))
inspect(tweets[1:5])

#Cleaning the Data Set
tweets <- tm_map(tweets, content_transformer(removePunctuation))
tweets <- tm_map(tweets, content_transformer(removeNumbers))
tweets <- tm_map(tweets, content_transformer(tolower))
tweets <- tm_map(tweets, removeWords, stopwords("english"))
tweets <- tm_map(tweets, removeWords, "relnofollowtwitter")
tweets <- tm_map(tweets, removeWords, "false")
tweets <- tm_map(tweets, removeWords, "sundarpichai")
tweets <- tm_map(tweets, removeWords, "href")
inspect(tweets[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
tweets <- tm_map(tweets, content_transformer(removeURL))

tweets <- tm_map(tweets, stripWhitespace)
inspect(tweets[1:5])

#Term Document Matrix
tdm <- TermDocumentMatrix(tweets)
tdm
tdm <- as.matrix(tdm)
tdm[1:5, 1:5]

#Bar Plot
w <- rowSums(tdm)
w
w <- subset(w, w>=10)
w
barplot(w, las=2, col = rainbow(7))

#Wordcloud
library(wordcloud2)
cloud <- data.frame(names(w), w)
colnames(cloud) <- c("word", "freq")
set.seed(123)
wordcloud2(cloud, shape = "circle", size = .9)

#Sentimental Analysis
library(reshape2)
library(syuzhet)
library(scales)
library(ggplot2)
library(lubridate)
library(dplyr)

tweets <- readLines("C:\\Users\\91755\\Desktop\\Assignment\\16 - Text Mining\\tweets.csv")
s_v <- get_sentences(tweets)
str(s_v)
class(s_v)
tweets <- iconv(tweets, "UTF-8", "latin1")
tweets <- get_sentences(as.character(tweets))

sentiments_vector <- get_sentiment(s_v, method = "bing")
sentiments_vector

sum(sentiments_vector)
mean(sentiments_vector)
summary(sentiments_vector)

#Plot
plot(sentiments_vector,type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h= 0, col= "red")

#To extreact the most negative sentence
negative <- s_v[which.min(sentiments_vector)]
negative

#To extreact the most positive sentence
positive <- (s_v[which.max(sentiments_vector)])
positive
