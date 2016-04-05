library(stringi)
library(NLP)
library(tm)
library(ggplot2)
library(RWeka)
library(reshape2)
library(slam)


setwd("/home/bahar/projects/NextWord")

corpusblogs <- readRDS(file = "./blogsCorpus0.4.RData")

corpusnews <- readRDS(file ="./newsCorpus0.4.RData")
  
corpustwitter <- readRDS(file ="./twitterCorpus0.3.RData")

finalcorpus <- c(corpusblogs , corpusnews, corpustwitter, recursive=T)
saveRDS(finalcorpus, file = "./finalCorpus0.4.RData")
#finalcorpus <- readRDS(file= "./finalCorpus0.4.RData")

options(mc.cores=1)


BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

freq <- function(tdm)
{
  freq <- sort(row_sums(tdm, na.rm = TRUE),  decreasing=TRUE)
  freq_frame <- data.frame(word=names(freq), freq=freq)
  return(freq_frame)
}

TDMatrix2 <- TermDocumentMatrix(finalcorpus , control=list(tokenize=BigramTokenizer))
TDMatrix22 <- removeSparseTerms(TDMatrix2, 0.999995)
biofreq <- freq(TDMatrix22)

TDMatrix3 <- TermDocumentMatrix(finalcorpus, control=list(tokenize=TrigramTokenizer))
TDMatrix33 <- removeSparseTerms(TDMatrix3, 0.999995)
trifreq <- freq(TDMatrix33)

TDMatrix4 <- TermDocumentMatrix(finalcorpus, control=list(tokenize=QuadgramTokenizer))
TDMatrix4 <- removeSparseTerms(TDMatrix4, 0.99999)
quadfreq <- freq(TDMatrix4)

biosplit <- as.character(biofreq$word)
biosplit <- strsplit(biosplit , " ")
temp <- do.call(rbind , biosplit)
final_bio_freq <- data.frame( temp , biofreq$freq)
saveRDS(final_bio_freq, file = "./PredictNextWord/dataApp/final_bio_freq0.4.RData")

trisplit <- as.character(trifreq$word)
trisplit <- strsplit(trisplit , " ")
temp <- do.call(rbind , trisplit)
final_tri_freq <- data.frame( temp , trifreq$freq)
saveRDS(final_tri_freq, file = "./PredictNextWord/dataApp/final_tri_freq0.4.RData")


quadsplit <- as.character(quadfreq$word)
quadsplit <- strsplit(quadsplit , " ")
temp <- do.call(rbind , quadsplit)
final_quad_freq <- data.frame( temp , quadfreq$freq)
saveRDS(final_quad_freq, file = "./PredictNextWord/dataApp/final_quad_freq.RData")
