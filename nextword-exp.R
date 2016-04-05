library(stringi)
library(NLP)
library(tm)
library(ggplot2)
library(RWeka)
library(reshape2)


setwd("/home/bahar/projects/NextWord")

if (!file.exists("./final"))
{
  DataZip <- download.file("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
  destfile = "Coursera-SwiftKey.zip" , method = "curl")
  
  unzip("Coursera-SwiftKey.zip")
}

# Reading the bolg, news and twitter files
blogs <- readLines("/home/bahar/COURSERA/Data/final/en_US/en_US.blogs.txt", encoding = "UTF-8" , skipNul = TRUE)
news <- readLines("/home/bahar/COURSERA/Data/final/en_US/en_US.news.txt", encoding = "UTF-8" , skipNul = TRUE)
twitter <- readLines("/home/bahar/COURSERA/Data/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

# Counting the number of words at each file
blogsWords <- stri_count_words(blogs)
newsWords <- stri_count_words(news)
twitterWords <- stri_count_words(twitter)

#Checking the size of each file
blogSize <- file.info("/home/bahar/COURSERA/Data/final/en_US/en_US.blogs.txt")$size / 1024^2
newsSize <- file.info("./final/en_US/en_US.news.txt")$size / 1024^2
twitterSize <- file.info("./final/en_US/en_US.twitter.txt")$size / 1024^2

blogsSample <- sample(blogs , length(blogs) *0.3)
newsSample <- sample(news , length(news) *0.3)
twitterSample <- sample(twitter , length(twitter) *0.3)

SampleData <- c(blogsSample , newsSample , twitterSample)

#Create the corpus and do the transformations
SampleCorpus <- VCorpus(VectorSource(SampleData))

SampleCorpus <- tm_map(SampleCorpus, content_transformer(tolower))
SampleCorpus <- tm_map(SampleCorpus, content_transformer(removePunctuation))
SampleCorpus <- tm_map(SampleCorpus, content_transformer(removeNumbers))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
SampleCorpus <- tm_map(SampleCorpus, content_transformer(removeURL))
SampleCorpus <- tm_map(SampleCorpus, stripWhitespace)
SampleCorpus <- tm_map(SampleCorpus,  removeWords, stopwords("english")) 
#SampleCorpus <- tm_map(SampleCorpus,  removeWords, profanityWords) 
#SampleCorpus <- tm_map(SampleCorpus,  stripWhitespace)

SampleCorpus <- tm_map(SampleCorpus, PlainTextDocument)   
saveRDS(SampleCorpus, file = "./FinalCorpus.RData")
options(mc.cores=1)


BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

freq <- function(tdm)
{
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  freq_frame <- data.frame(word=names(freq), freq=freq)
  return(freq_frame)
}


TDMatrix <- TermDocumentMatrix(SampleCorpus)
TDMatrix <- removeSparseTerms(TDMatrix, 0.99)
unifreq <- freq(TDMatrix)

TDMatrix2 <- TermDocumentMatrix(SampleCorpus , control=list(tokenize=BigramTokenizer))
TDMatrix2 <- removeSparseTerms(TDMatrix2, 0.999)
biofreq <- freq(TDMatrix2)

TDMatrix3 <- TermDocumentMatrix(SampleCorpus, control=list(tokenize=TrigramTokenizer))
TDMatrix3 <- removeSparseTerms(TDMatrix3, 0.9999)
trifreq <- freq(TDMatrix3)

TDMatrix4 <- TermDocumentMatrix(SampleCorpus, control=list(tokenize=QuadgramTokenizer))
TDMatrix4 <- removeSparseTerms(TDMatrix4, 0.9999)
quadfreq <- freq(TDMatrix4)

biosplit <- as.character(biofreq$word)
biosplit <- strsplit(biosplit , " ")
temp <- do.call(rbind , biosplit)
final_bio_freq <- data.frame( temp , biofreq$freq)
saveRDS(final_bio_freq, file = "./ShinyApp/final_bio_freq.RData")


trisplit <- as.character(trifreq$word)
trisplit <- strsplit(trisplit , " ")
temp <- do.call(rbind , trisplit)
final_tri_freq <- data.frame( temp , trifreq$freq)
saveRDS(final_tri_freq, file = "./ShinyApp/final_tri_freq.RData")

quadsplit <- as.character(quadfreq$word)
quadsplit <- strsplit(quadsplit , " ")
temp <- do.call(rbind , quadsplit)
final_quad_freq <- data.frame( temp , quadfreq$freq)
saveRDS(final_quad_freq, file = "./ShinyApp/final_quad_freq.RData")


