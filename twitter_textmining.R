install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
library(twitteR)
library(ROAuth)
library(tm)

twit <- OAuthFactory$new(consumerKey='91sBvLLE5GvjHICzTwpc6c4fo', # Consumer Key (API Key)
                         consumerSecret='TuXL1lUlFXBPI2ZxGxKfGKh4lUdon4hofpHHjfkNWdHls11bvI', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(twit, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("91sBvLLE5GvjHICzTwpc6c4fo", # Consumer Key (API Key)
                    "TuXL1lUlFXBPI2ZxGxKfGKh4lUdon4hofpHHjfkNWdHls11bvI", #Consumer Secret (API Secret)
                    "2672388621-ifxfyb8aQzRygWRuQ1dHboO5sE13T4HwzhkaFJe",  # Access Token
                    "8kl9Bxz6V4geF9gSkPEiAsH1Bn4ENxO2qA03R8Nsa15QG")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('Suriya_offl', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

##checking the directory
getwd()
setwd("C:\\Users\\91755\\Documents")
write.csv(TweetsDF,file = "twitter.csv",row.names = F)

tweet<-read.csv("C:\\Users\\91755\\Documents\\twitter.csv")
View(tweet)
tweet<-tweet[,1]
tweets<-VCorpus(VectorSource(tweet))
inspect(tweets[1:3])
## cleaning dataset
tweets<-tm_map(tweets,content_transformer(tolower))
inspect(tweets[1:5])
tweets<-tm_map(tweets,content_transformer(removePunctuation))
tweets<-tm_map(tweets,content_transformer(removeNumbers))
tweets<-tm_map(tweets,removeWords,stopwords('english'))
tweets<-tm_map(tweets,removeWords,'will')
tweets<-tm_map(tweets,removeWords,'you')

removeURL<-function(x) gsub('http[[:alnum:]]*','',x)
tweets<-tm_map(tweets,content_transformer(removeURL))

tweets<-tm_map(tweets,stripWhitespace)
inspect(tweets[1:3])
## term document matrix

tdm<-TermDocumentMatrix(tweets)
tdm
tdm<-as.matrix(tdm)
tdm[1:10,1:10]

##barplot

w<-rowSums(tdm)
w<-subset(w,w>=10)
barplot(w,las=2,col = rainbow(7))

##wordcloud
library(wordcloud2)
cloud<-data.frame(names(w),w)
colnames(cloud)<-c('word','freq')
set.seed(123)
wordcloud2(cloud,shape = 'square',size = 0.9)

##sentimental analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

tw<-read.csv("C:\\Users\\91755\\Documents\\twitter.csv",header = T)
tw<-tw[,1]
str(tw)
class(tw)

tw<-iconv(tw,"UTF-8","latin1")

tw<-get_sentences(tw)

s<-get_nrc_sentiment(tw)
s
tw[95]
n<-get_sentiment(tw,method = 'bing')
n
# plot
barplot(colSums(s),col=rainbow(7))

# To extract the sentence with the most negative emotional valence
negative <- tw[which.min(n)]
negative

# and to extract the most positive sentence
positive <- tw[which.max(n)]
positive

        