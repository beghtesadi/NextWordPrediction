library(stringi)
library(NLP)
library(tm)
library(ggplot2)
library(RWeka)
library(reshape2)


setwd("/home/bahar/projects/NextWord")

if (!file.exists("/home/bahar/COURSERA/Data/final"))
{
  DataZip <- download.file("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
                           destfile = "Coursera-SwiftKey.zip" , method = "curl")
  
  unzip("Coursera-SwiftKey.zip")
}

# Reading the bolg, news and twitter files
blogs <- readLines("/home/bahar/COURSERA/Data/final/en_US/en_US.blogs.txt", encoding = "UTF-8" , skipNul = TRUE)
news <- readLines("/home/bahar/COURSERA/Data/final/en_US/en_US.news.txt", encoding = "UTF-8" , skipNul = TRUE)
twitter <- readLines("/home/bahar/COURSERA/Data/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

CreateCleanCorpus <- function (sampletext)
{
  #Create the corpus and do the transformations
  SampleCorpus <- VCorpus(VectorSource(sampletext))
  
  
  SampleCorpus <- tm_map(SampleCorpus, content_transformer(tolower))
  SampleCorpus <- tm_map(SampleCorpus, content_transformer(removePunctuation))
  SampleCorpus <- tm_map(SampleCorpus, content_transformer(removeNumbers))
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  SampleCorpus <- tm_map(SampleCorpus, content_transformer(removeURL))
  SampleCorpus <- tm_map(SampleCorpus, stripWhitespace)
  #SampleCorpus <- tm_map(SampleCorpus,  removeWords, stopwords("english")) 
  SampleCorpus <- tm_map(SampleCorpus, PlainTextDocument)   
  SampleCorpus
}

blogsSample <- sample(blogs , length(blogs) *0.4)
newsSample <- sample(news , length(news) *0.4)
twitterSample <- sample(twitter , length(twitter) *0.3)

blogsCorpus <- CreateCleanCorpus(blogsSample)
saveRDS(blogsCorpus, file = "./blogsCorpus0.4.RData")


newsCorpus <- CreateCleanCorpus(newsSample)
saveRDS(newsCorpus, file = "./newsCorpus0.4.RData")

twitterCorpus <- CreateCleanCorpus(twitterSample)
saveRDS(twitterCorpus, file = "./twitterCorpus0.3.RData")


