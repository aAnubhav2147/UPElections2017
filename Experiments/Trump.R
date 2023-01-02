
setwd("C:/Users/Tqm/Desktop/R-Work")
library(data.table)
library(twitteR)
library(httr)
api_key <- "BlUUwXP9yasjVkEPbkWKmGZZu"
api_secret <- "kiRuEGFSH7b9OrzvpmFQZ4vjalfgW2yC1Rd5k80fdevVfXCV9X"
access_token <- "748543091234213888-w9J1AdxwwhUWVSXcfuLWM0A8N37JCNx"
access_token_secret <- "oELa9htmVJEN5qUOY3VzCuvn5Imi9jljJZCEk8mEanMqI"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

trump<-userTimeline("realDonaldTrump",n=3000)

trump.df<-twListToDF(trump)
View(trump.df)
write.csv(trump.df,file='C:/Users/Tqm/Desktop/R-Work/trump.csv',row.names=F)

trump_tweet<-read.csv("trump.csv")
View(trump_tweet)
donald<-trump_tweet


#####WORDCLOUD ANALYSIS####

#WORD CLOUD
library(tm)
options(header=FALSE,stringAsFactors=FALSE,FileEncoding='latin1')
setwd("C:/Users/Tqm/Desktop/R-Work")


summary(donald$text)

#Builing Corpus
corpus<-Corpus(VectorSource(donald$text))

#Clean Text
corpus<-tm_map(corpus,tolower)

corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)

cleanset<- tm_map(corpus,removeWords,stopwords("english"))
cleanset<-tm_map(cleanset,PlainTextDocument)

#Build term document matrix

dtm<-TermDocumentMatrix(cleanset,control=list(mindWordLength=c(1,Inf)))


# inspect frequent words
findFreqTerms(dtm,lowfreq=20)

#BarPLot

termFrequency <- rowSums(as.matrix(dtm))
termFrequency<-subset(termFrequency,termFrequency>=10)
library(ggplot2)
barplot(termFrequency,las=2,col=rainbow(20))
library(wordcloud)
m<-as.matrix(dtm)

#calculate the frequency of the words and sort it descendingly by frequency.
wordFreq<-sort(rowSums(m),decreasing=TRUE)

#wordcloud
set.seed(375) #to make it reproducible
grayLevels<-gray((wordFreq+10)/(max(wordFreq)+10))

#with gray levels
wordcloud(words=names(wordFreq),freq=wordFreq,min.freq=5,max.words=100,random.order=F,colors=grayLevels)

#with colors
wordcloud(words=names(wordFreq),scale=c(7,.1),rot.per=0.2,freq=wordFreq,min.freq=5,max.words=100,random.order=F,colors=rainbow(20))



####Score Analysis####



library(tm)
options(header=FALSE,stringAsFactors=FALSE,FileEncoding='latin1')
setwd("C:/Users/Tqm/Desktop/R-Work")
trumped<-donald$text

pos=scan('C:/Users/Tqm/Desktop/R-Work/PositiveWords.txt',what='character',comment.char=';')
neg=scan('C:/Users/Tqm/Desktop/R-Work/NegativeWords.txt',what='character',comment.char=';')
score.sentiment=function(sentences,pos.words,neg.words,.progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  
  scores=laply(sentences,function(sentence,pos.words,neg.words)
    # clean up sentences with R's regex-driven global substitute, gsub():
  {
    sentence=gsub('[[:punct:]]','',sentence)
    sentence=gsub('[[:cntrl:]]','',sentence)
    sentence=gsub('\\d+','',sentence)
    
    # and convert to lower case:
    sentence=tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list=strsplit(sentence,'\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words=unlist(word.list)
    
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches=match(words,pos.words)
    neg.matches=match(words,neg.words)
    # match() returns the position of the matched term or NA
    
    # we just want a TRUE/FALSE:
    pos.matches=!is.na(pos.matches)
    neg.matches=!is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score=sum(pos.matches)-sum(neg.matches)
    return(score)
  },pos.words,neg.words,.progress=.progress)
  scores.df=data.frame(score=scores,text=sentences)
  return(scores.df)
}

analysis=score.sentiment(trumped,pos,neg)


table(analysis$score)

mean(analysis$score)

hist(analysis$score)



####Sentiment Analysis####


library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)



trumped<-as.character(trumped)
mysentiment<-get_nrc_sentiment(trumped)

head(mysentiment)

Tweet<-cbind(donald,mysentiment)



sentimentTotals <- data.frame(colSums(Tweet[,c(17:26)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
geom_bar(aes(fill = sentiment), stat = "identity") +
theme(legend.position = "none") +
xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

library(Scale)


####Plotting Trend Lines


Tweet$timestamp <- with_tz(ymd_hms(Tweet$created), "America/New_York")

posnegtime <- Tweet %>% 
group_by(timestamp = cut(timestamp, breaks="5")) %>%
summarise(negative = mean(negative),
positive = mean(positive)) %>% melt

names(posnegtime) <- c("timestamp", "sentiment", "meanvalue")
posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])

 ggplot(data = posnegtime, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
       geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
       geom_point(size = 0.5) +
       ylim(0, NA) + 
       scale_colour_manual(values = c("springgreen4", "firebrick3")) +
       theme(legend.title=element_blank(), axis.title.x = element_blank()) +
       scale_x_date(breaks = date_breaks("1 months"), 
                                       labels = date_format("%Y-%b")) +
       ylab("Average sentiment score") + 
      ggtitle("Sentiment Over Time")

####Plotting Trend Lines for weeks and for all emotions.

 Tweet$weekday<-lubridate::wday(Tweet$timestamp,label=TRUE)
 weeklysentiment <- Tweet %>% group_by(weekday) %>% 
   summarise(anger = mean(anger), 
             anticipation = mean(anticipation), 
             disgust = mean(disgust), 
             fear = mean(fear), 
             joy = mean(joy), 
             sadness = mean(sadness), 
             surprise = mean(surprise), 
             trust = mean(trust)) %>% melt
 names(weeklysentiment) <- c( "weekday","sentiment", "meanvalue")
 
 ggplot(data = weeklysentiment, aes(x=weekday ,y = meanvalue, group = sentiment)) +
   geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
   geom_point(size = 0.5) +
   ylim(0, 0.6) +
   theme(legend.title=element_blank(), axis.title.x = element_blank()) +
   ylab("Average sentiment score") + 
   ggtitle("Sentiment During the Week")






























                              