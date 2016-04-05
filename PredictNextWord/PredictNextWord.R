library(stringi)
library(NLP)
library(tm)
library(ggplot2)
library(RWeka)
library(reshape2)
library(qdap)
library(stringr)

#setwd("/home/bahar/projects/NextWord/PredictNextWord")
final_bio_freq = readRDS(file = "./dataApp/final_bio_freq0.4.RData")
final_tri_freq = readRDS(file = "./dataApp/final_tri_freq0.4.RData")

#final_quad_freq = readRDS(file = "./final_quad_freq.RData")

CleanInput <- function(text_input)
{
  cleaninput <- tolower(text_input)
  cleaninput <- removePunctuation(cleaninput)
  cleaninput <- removeNumbers(cleaninput)
  cleaninput <- str_replace_all(cleaninput, "[^[:alnum:]]", " ")
  cleaninput <- stripWhitespace(cleaninput)
  
  return(cleaninput)
}

NextWord <- function(text_input)
{

  text_input <- unlist(strsplit(text_input , " "))
  count = length(text_input)

  next_word <- "Enter a phrase"
  
  if (count <= 1)
  {
    next_word = final_bio_freq[final_bio_freq$X1 == text_input ,]$X2[1:3]
  }
  else if (count >= 2)
  {
    next_word = final_tri_freq[final_tri_freq[1] == text_input[count-1] & final_tri_freq[2] == text_input[count] ,]$X3[1:3]
  }
#   else if(count >= 3)
#   {
#     next_word = final_quad_freq[final_quad_freq[1] == text_input[count-2] & final_quad_freq[2] == text_input[count-1] &
#                                   final_quad_freq[3] == text_input[count] ,]$X4[1]
#   }
 

  cat(paste(next_word , "\n"))
  
}



