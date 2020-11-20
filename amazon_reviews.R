#Amazon Reviews
install.packages("tm")
library(tm)
install.packages("wordcloud2")
library(wordcloud2)

aurl <- "https://www.amazon.in/product-reviews/B07DJCVTDN/ref=acr_arpsims_text?ie=UTF8&showViewpoints=1"
oneplus <- NULL
for (i in 1:10) {
  murl <- read_html(as.character(paste(aurl, i, sep = "=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  oneplus<- c(oneplus, rev)
}
write.table(oneplus, "oneplus7t.txt", row.names = FALSE)

oneplus <- read.delim("C:\\Users\\91755\\Desktop\\Assignment\\16 - Text Mining\oneplus7t.txt")
View(oneplus)

#Building Corpus
oneplus_corpus <- VCorpus(VectorSource(oneplus))
inspect(oneplus_corpus[1:10])

#Cleaning and Clensing
oneplus_corpus <- tm_map(oneplus_corpus, content_transformer(tolower))
oneplus_corpus <- tm_map(oneplus_corpus, content_transformer(removePunctuation))
oneplus_corpus <- tm_map(oneplus_corpus, content_transformer(removeNumbers))
oneplus_corpus <- tm_map(oneplus_corpus, removeWords, stopwords("english"))
oneplus_corpus <- tm_map(oneplus_corpus, removeWords, "this")
oneplus_corpus <- tm_map(oneplus_corpus, removeWords, "i")
inspect(oneplus_corpus[1:10])

#Remove URL
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
oneplus_corpus <- tm_map(oneplus_corpus, content_transformer(removeURL))

#Striping White Space
oneplus_corpus <- tm_map(oneplus_corpus, stripWhitespace)
inspect(oneplus_corpus[1:10])

#Term Document Matrix
tdm <- TermDocumentMatrix(oneplus_corpus)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:10]
tdm

#Bar Plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w>=10)
w_sub
barplot(w_sub, las=2, col = rainbow(10))

#Word Cloud
cloud <- data.frame(names(w), w)
colnames(cloud) <- c("word", "freq")
set.seed(123)
wordcloud2(cloud, shape = "square", size = 0.5)

#Sentimental Analysis
install.packages("syuzhet")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("reshape2")
install.packages("dplyr")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

tw <- get_sentences(as.character(oneplus_corpus))
s <- get_nrc_sentiment(tw)
s
n<- get_sentiment(tw, method = "bing")
n
#Plot
barplot(colSums(s), col = rainbow(7))

# To extract the sentence with the most negative emotional valence
negative <- s[which.min(n)]
negative

# To extract the sentence with the most positive emotional valence
postive <- s[which.max(n)]
positive
