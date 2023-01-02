rm(list=ls())
setwd("/Users/udayshankar/Desktop/UPElectionsRfiles")
getwd()

##Activating Required Packages
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
library("tidyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("RCurl", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

##Setting up Twitter Authentication usin OAuth authorization
consumerkey <- "kEMBMyT56PGPom9ouH2to4bR7"
consumersecret <- "iSQsvdGwpO3sMSQNmSe7IfVshJnAoF2J1fcH8c7wA3UVyOd4Nx"
accesstoken <- "1118258383-zti8cbk0tt93lItM98s5u7geLdKRmEspeB4RvIL"
accesstokensecret <- "pH6ZtwXNNR59sSM6Q5PEcsQfYcKqjBxM41ZoHSBw3y8nz"
setup_twitter_oauth(consumerkey,consumersecret,accesstoken,accesstokensecret)


##Importing the tweets from the relevant hashtags
tweets_bjp <- searchTwitter("#bjp",n=2000)
tweets_bjp_df <- twListToDF(tweets_bjp)
write.table(tweets_bjp_df,"bjptweets@26feb.txt",sep = "\t",row.names = FALSE)
tweets_bjp_df_nohandles <- str_replace_all(tweets_bjp_df$text,"@\\w+","")
tweets_bjp_df_nohandles_norts <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweets_bjp_df)
tweets_bjp_corpus <- iconv(tweets_bjp_df_nohandles_norts,to="utf-8-mac")##A code to filter out pictures and emoticons for Mac OSX environment
tweets_bjp_corpus <- Corpus(VectorSource(tweets_bjp_corpus))##Builds the corpus for the tweets
