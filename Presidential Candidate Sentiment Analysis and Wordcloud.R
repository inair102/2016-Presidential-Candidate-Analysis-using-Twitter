library(twitteR)
library(RJSONIO)
library(ROAuth)
library(tm)
library(streamR)
library(SnowballC)
library(Rstem)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)

#Tweets Extraction
load("my_oauth.Rdata")
filterStream(file.name = "tweet.json", track =  c("Hillary","Trump","Sanders","Carson", "Bush"), language = "en",
             timeout = 120, oauth = my_oauth)
El_tweets <- parseTweets("tweet.json", verbose = TRUE)
Elections.df <- parseTweets("tweet.json", verbose = TRUE)
dim(Elections.df)

#
#Sentiment Analysis
#

#Clean-up
clean.El_tweets <- function(text){
  words <- removePunctuation(text)
  words <- wordStem(words)
  words <- str_split(text, " ")
  return(words)
}

classify_words <- function(words, pos.words, neg.words){
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

word_classifier <- function(El_tweets, pos.words, neg.words, keyword){
  relevant <- grep(keyword, El_tweets$text, ignore.case = TRUE)
  words <- clean.El_tweets(El_tweets$text[relevant])
  scores <- unlist(lapply(words, classify_words, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores > 0))/n * 100)
  negative <- as.integer(length(which(scores < 0))/n * 100)
  neutral <- 100 - positive - negative
  cat(n, "Tweets about", keyword,":", positive, "%positive", negative, "%negative", neutral, "%neutral")
}

lexicon <- read.csv("F:/Study/3rd Sem Readings/Big Data Analytics/workshop/lexicon.csv", stringsAsFactors = F)
pos.words <- lexicon$word[lexicon$polarity == "positive"]
neg.words <- lexicon$word[lexicon$polarity == "negative"]

word_classifier(El_tweets, pos.words, neg.words, keyword = "Hillary")
word_classifier(El_tweets, pos.words, neg.words, keyword = "Trump")
word_classifier(El_tweets, pos.words, neg.words, keyword = "Sanders")
word_classifier(El_tweets, pos.words, neg.words, keyword = "Carson")
word_classifier(El_tweets, pos.words, neg.words, keyword = "Bush")

#
#Text Mining  and Plots
#

#Clean-up
myCorpus <- Corpus(VectorSource(Elections.df$text))
myCorpus <- tm_map(myCorpus, PlainTextDocument)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, removeNumbers)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myStopwords <- c(stopwords("english"), "available", "via")
myStopwords <- setdiff(myStopwords, c("r", "big"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
stemCompletion2 <- function(x,dictionary){
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary = dictionary)
  x <- paste(x, sep = "", collapse = " ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary = myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))

#count frequency mining and miners
miningCases <- lapply(myCorpusCopy, function(x){grep(as.character(x), pattern = "\\<mining")})
sum(unlist(miningCases))

minerCases <- lapply(myCorpusCopy, function(x){grep(as.character(x), pattern = "\\<miner")})
sum(unlist(minerCases))

myCorpus <- tm_map(myCorpus, content_transformer(gsub), pattern="miner", replacement="mining")

tdm <- TermDocumentMatrix(myCorpus, control = list(wordlengths = c(1, Inf)))
tdm
#Frequency of words
(freq.terms <- findFreqTerms(tdm, lowfreq = 60))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 60)
df <- data.frame(term =names(term.freq), freq = term.freq)
#Plot
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Names") + ylab("Count") + coord_flip()

#WordCloud
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")#colors
pal <- pal[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)