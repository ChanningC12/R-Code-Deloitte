# Here is the sample code for analyzing the opt-out emails subject line. If the number of emails used to opt-out in 2015 & 2016 was low you can use it to run during your day. If the # of emails are high, then I would suggest running the code in the night or end of day.

# Main steps:
  # 1)	Read in file

dm1=read.csv(file="C:/Users/smusti/Documents/Sashank Musti/Intuit/Data/XML/July 27th/Output/Name/output_appdesc4.csv",head=TRUE,sep=",")
names(dm1)
desc.data <- data.frame(filename=dm1$filename,Appid=dm1$appid,words=dm1$words)
names(desc.data)
desc.data$filename<-NULL
desc_data_a <- merge(x=desc.data, y=dm_5030, by.x="Appid", by.y="App.DBID",by=NULL)
  # 2)	Load library(tm)
library(tm)
  # 3)	Read in the subject line & run the following steps

xkcd.corpus1 <- Corpus(DataframeSource(data.frame(desc_data_a$words)))
xkcd.corpus1 <- tm_map(xkcd.corpus1, removePunctuation)
xkcd.corpus1 <- tm_map(xkcd.corpus1, content_transformer(tolower))
xkcd.corpus1 <- tm_map(xkcd.corpus1,removeNumbers)
xkcd.corpus1 <- tm_map(xkcd.corpus1, function(x) removeWords(x,stopwords("english")))
dtmr1 <-DocumentTermMatrix(xkcd.corpus1, control=list(wordLengths=c(4, 15),bounds = list(global = c(10,1000))))
#dtmr1 <-DocumentTermMatrix(xkcd.corpus1)
dim(dtmr1)
  # 4)	Calculate frequency count

freqr <- colSums(as.matrix(dtmr1))
length(freqr)
ordr <- order(freqr,decreasing=TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]
findFreqTerms(dtmr,lowfreq=100)