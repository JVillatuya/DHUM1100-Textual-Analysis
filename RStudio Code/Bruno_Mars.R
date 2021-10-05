## RStudio allows you to run each line as you work with Run above.
## See results in bottom right.

## Packages are groups of functions that you can use.
## We are putting the following packages into the Library to use.
library("RDSTK")
library("readr")
library("qdap")
library("syuzhet")
library("ggplot2")
library("dplyr")
library("devtools")
library("RColorBrewer")
library("tm")
library("SnowballC")
library("wordcloud")

## make VERY sure that the following packages have loaded-- base, datasets, dplyr, ggplot2, graphics, grDevices, methods, plyr, qdap, qdapDictionaries, qdapRegex, qdapTools, RColorBrewer, RCurl, RDSTK, readr, rjson, stats, syuzhet, twitteR, utils

Bruno_Mars <- read_file("DHUM 1100/Text Analysis/assets/Bruno_Mars.txt")
View(Bruno_Mars)

docs <- Corpus(VectorSource(Bruno_Mars))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("the", "oh","its","for","yeah","got","like","caus")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

## POLARITY AND SENTIMENT
## in general (for us), sentiment is how postive, negative, or neutral a statement is.
## polarity is the degree of that sentiment
g_scores = get_nrc_sentiment(Bruno_Mars)
class(g_scores)
g_scores
g_polarity = g_scores[1,9:10]
g_sentiment = g_scores[1,1:8]

## visualize polarity
class(g_polarity)
g_polarity = data.matrix(g_polarity, rownames.force = TRUE)
barplot(g_polarity)

## visualize sentiment
class(g_sentiment)
g_sentiment = data.matrix(g_sentiment, rownames.force = TRUE)
barplot(g_sentiment)

## break it down by sentence
g_speech_sen = get_sentences(Bruno_Mars)
g_speech_sen

sentiment_vector = get_sentiment(g_speech_sen, method = "syuzhet")
## A vector is a basic data structure in R.
## It is a sequence of elements that share the same data type. 
## (logical, integer, double, character, complex, or raw data)

sentiment_vector
summary(sentiment_vector)
 boxplot(sentiment_vector)

## what was the most positive sentence in the paragraph?
max(sentiment_vector)
sentence_sentiment = data.frame(g_speech_sen, sentiment_vector)
View(sentence_sentiment)
which.max(sentence_sentiment$sentiment_vector)

## we now have an index - what can we do with this?
most_positive = sentence_sentiment[which.max(sentence_sentiment$sentiment_vector),]
## don't forget the comma!

## Wordclouds
## look over to the right and see that these packages are checked
## if one is absent then go to the console below and type install.packages("MISSINGPACKAGE")


set.seed(1234)
word_cloud = wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

ggexport(plotlist = list(word_cloud), 
         filename = "DHUM 1100/Text Analysis/plots/word_cloud.png",width=1000,height = 1000)


