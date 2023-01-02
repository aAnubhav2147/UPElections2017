rm(list=ls())
setwd("/Users/udayshankar/Desktop/UPElectionsRfiles")
getwd()
library("twitteR")
library("devtools")
library("ROCR")
library("tidytext")
library("curl")
library("e1071")
library("ggplot2")
library("httr")
library("lubridate")
library("qdap")
library("ROAuth")
library("RSentiment")
library("RTextTools")
library("sentR")
library("syuzhet")
library("tm")
library("wordcloud")
library("tokenizers", lib.loc="~/Library/R/3.3/library")

consumerkey <- "kEMBMyT56PGPom9ouH2to4bR7"
consumersecret <- "iSQsvdGwpO3sMSQNmSe7IfVshJnAoF2J1fcH8c7wA3UVyOd4Nx"
accesstoken <- "1118258383-zti8cbk0tt93lItM98s5u7geLdKRmEspeB4RvIL"
accesstokensecret <- "pH6ZtwXNNR59sSM6Q5PEcsQfYcKqjBxM41ZoHSBw3y8nz"
setup_twitter_oauth(consumerkey,consumersecret,accesstoken,accesstokensecret)

up_df <- read.csv(file.choose())
up_text <- up_df$text
up_clean <- str_replace_all(up_text,"@\\w+","")
up_clean <- gsub("&amp", "", up_clean)
up_clean <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", up_clean)
up_clean <- gsub("[[:punct:]]","",up_clean)
up_clean <- gsub("[[:digit:]]", "", up_clean)
up_clean <- gsub("http\\w+", "", up_clean)
up_clean <- gsub("[ \t]{2,}", "", up_clean)
up_clean <- gsub("^\\s+|\\s+$", "", up_clean)
up_corpus <- iconv(up_clean,to="utf-8-mac")##Removes emoticons or pictures from tweets in R Console ruuning on Mac OSX environment
up_corpus <- Corpus(VectorSource(up_corpus))
up_tdm <- TermDocumentMatrix(up_corpus,control = list(removePunctuation=TRUE,stopwords=c("false","href","androida","relnofollowtwitter","ufuu","uuuu","uue","upelections","upelections2017","uppolls2017","uppolls","upelection","ueuu","uueu","ufuuau","uuuuu","uuu","uub","uucu","ucu","rtupelection","uuuueuu","uufuf","uuufu","uauu","ueu","uuue","ueuuucucu","uuauue","uufu","ufueuu","uubuf","uuuue","ufu","uuudue","uuuf","rtuu","uudu","ucueu","ueuuueu",stopwords("english")),removeNumbers=TRUE,tolower=TRUE))
up_matrix <- as.matrix(up_tdm)
up_wordfreq <- sort(rowSums(up_matrix),decreasing = TRUE)
up_df_1 <- data.frame(word=names(up_wordfreq),freq=up_wordfreq)
wordcloud(up_df_1$word,up_df_1$freq,random.order = FALSE,colors = brewer.pal(8,"Dark2"))
warnings()

