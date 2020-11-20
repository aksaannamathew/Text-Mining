#Parasite Movie Review
install.packages("wordcloud2")
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")
install.packages("tm")
library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud2)

#Extraction

movie <- NULL
rev <- NULL
url <- "https://www.imdb.com/title/tt6751668/reviews"
for (i in 1:10) {
  murl <- read_html(as.character(paste(url, sep = "i")))
  rev <- murl %>%html_nodes(".show-more__control") %>% html_text()
  movie <- c(movie,rev)
}
write(movie, "movie.txt")
getwd()
View(movie1917)

#Checking
movie <- read.delim("C:\\Users\\91755\\Documents\\movie.txt")
View(movie)
movies <- VCorpus(VectorSource(movie))
inspect(movies[1:2])

#Cleaning and Clensing
movies <- tm_map(movies, content_transformer(tolower))
movies <- tm_map(movies, content_transformer(removePunctuation))
movies <- tm_map(movies, content_transformer(removeNumbers))
movies <- tm_map(movies, removeWords, stopwords("english"))
movies <- tm_map(movies, removeWords, "will")
movies <- tm_map(movies, removeWords, "can")
inspect(movies[1:5])

#Remove URLs
remove_url <- function(x) gsub('http[[:alnum:]]*','',x)
movies<-tm_map(movies,content_transformer(removeURL))

#Striping White Space
movies <- tm_map(movies, stripWhitespace)
inspect(movies[1:5])

#Term Document Matrix
tdm <- TermDocumentMatrix(movies)
tdm
tdm <- as.matrix(tdm)

#Bar Plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w>=10)
w_sub

barplot(w_sub, las=2, col = rainbow(7))

#Word Cloud
cloud <- data.frame(names(w_sub), w_sub)
colnames(cloud) <- c("word", "freq")
set.seed(123)
wordcloud2(cloud, shape = "circle", size=0.5)

#Sentimental Analysis
library(scales)
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(syuzhet)

class(movies)
str(movies)

movies <- iconv(movies, "UTF-8", "latin1")
movies <- get_sentences(movies)
movies
s <- get_nrc_sentiment(movies)
s
n <- get_sentiment(movies, method = "bing")
n

#BarPLot
barplot(colSums(s), col = rainbow(7))

#Extracte the most negative sentence
negative <- s[which.min(n)]
negative

#Extract the most positive sentence
positive <- s[which.max(n)]
positive
