library(tm)
options(header=FALSE,stringAsFactors=FALSE,FileEncoding='latin1')
setwd("C:/Users/Tqm/Desktop/R-Work")
text<-readLines("Nikon coolpix 4300.txt")

summary(text)

#Builing Corpus
corpus<-Corpus(VectorSource(text))

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

#WORD CLOUD
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