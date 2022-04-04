#import libraries 
#to do filtering and processing of our data
#install.packages("dplyr")
#to work with strings in a convenient and easier fashion
#install.packages("stringr")
#creates the structure for our tidy text data 
#install.packages("tidytext")
#to apply tokenization in our data
#install.packages("tidyr")
#to download the sentiment data
#install.packages("textdata")
#to plot the data into meaningful graphs
#install.packages("ggplot2")
#to help working with text and cleaning of the data
# install.packages('tm')
#to remove the stemming words
# install.packages('SnowballC')
#to work with the classifier
# install.packages('randomForest')
#to split the data randomly
# install.packages('caTools')
#to convert unicode to utf-8 values/character
#install.packages('rlang')
#to create clean tables
#install.packages('gt')
#adds stop words
#install.packages("stopwords")
#install.packages('sentimentr')
#load libraries
#get to know the data_library(sentimentr)
library(rlang)
library(caTools)
library(tm)
library(SnowballC)
library(tidyr)
library(dplyr)
library(stringr)  
library(tidytext)
library(textstem)
library(stopwords)

needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                       "tm", "SnowballC", "caTools", "rlang", "gt", "stopwords",
                     "sentimentr", "tidytext", "magrittr", "textstem")
#install packages in case they are not install yet
#install.packages(needed_packages)

#load packages
lapply(needed_packages, require, character.only = TRUE)

source("./preprocessing_data/remove_redundancies_and_bad_words.R")
source("./preprocessing_data/labeling_data.R")
source("./preprocessing_data/bag_of_words.R")
source("./preprocessing_data/clean_lyrics.R")

#-------------------------------------------------------------------------------
# Swear words and additional stopwords
#-------------------------------------------------------------------------------
#load dataset
data <- read.csv("./data/artists_songs.csv")


swears<-read.csv("./data/swear_words.csv")


# These additional stopwords found by preliminary analysis
additional_stopwords <- c("mmm", "gotta", "beyonc", "beyonc�" ,"hey","em", 
                          "huh", "eh", "te", "ohoh", "yeah", "oh","ya", "yo", 
                          "tu", "lo", "je","yuh", "woo", "mi", "de", "da",
                          "eheh","ayy","uhhuh","ariana", "grande", "ah","nicki",
                          "y'all","c'mon", "minaj", "whoa", "nananana", 
                          "rihanna", "eminem", "cardi", "babe", "niggas", 
                          "pre", "na", "ella", "la", "yonc�", "jhen�")


#remove unicode characters
data <- remove_redundancies_and_bad_Words(data)

dataset <- data
data1 <- data

#general information

dim(data) # (5137, 4)  --> 5137 rows, 4 columns
str(data)
summary(data)

# look at some example texts randomly from the dataset 
data %>% select(lyric) %>% sample_n(4) %>% pull()

unique(data$album) # 527 
unique(data$artist) # 20 unique artists in the dataset
data %>% View()

#------------------------------------------------------------------------------
# Corpus cleaning testing
#------------------------------------------------------------------------------

Corpus_data <- Corpus(VectorSource(data1$lyric))
substr(Corpus_data[[1]]$content, 1, 510) 

Cleaned_lyrics <- clean_lyrics(Corpus_data)
substr(Cleaned_lyrics[[1]]$content, 1, 510)

#head(Cleaned_lyrics)
# <<SimpleCorpus>>
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 6


# Remove additional stopwords and swear words
Removed_stopwords_lyrics <- clean_lyrics(Corpus_data, remove_sw = TRUE, 
                                         my_stopwords = additional_stopwords, 
                                         swear_wrd = swears$swear_words )
substr(Removed_stopwords_lyrics[[1]]$content, 1, 510)

# Lemmatization
Lemmatized_Lyrics <- clean_lyrics(Corpus_data, lemmatize = TRUE, 
                                  my_stopwords = additional_stopwords, 
                                  swear_wrd = swears$swear_words )
substr(Lemmatized_Lyrics[[1]]$content, 1, 510)

# Stemming
StemDoc_Lyrics <- clean_lyrics(Corpus_data, stemming = TRUE, 
                               my_stopwords = additional_stopwords, 
                               swear_wrd = swears$swear_words )
substr(StemDoc_Lyrics[[1]]$content, 1, 510)

#-------------------------------------------------------------------------------
# Comparing different data cleaning outputs
#-------------------------------------------------------------------------------

substr(Corpus_data[[1]]$content, 1, 510)  # Corpus_data
substr(Cleaned_lyrics[[1]]$content, 1, 510) # Cleaned text
substr(Removed_stopwords_lyrics[[1]]$content, 1, 510) # Text without stop-words including additional stop words
substr(Lemmatized_Lyrics[[1]]$content, 1, 510) # Lemmatized text
substr(StemDoc_Lyrics[[1]]$content, 1, 510) # Stemming applied to text


#-------------------------------------------------------------------------------
# The Lemmatized lyrics corpus is be used to build a Term-Document Matrix (TDM).
# Term-Document Matrix  counts the number of times every unique word is repeated 
#in each Lyrics. 
# Summation of the rows of the Term-Document Matrix results in counting the 
#number of times every term is repeated across all song lyrics
#-------------------------------------------------------------------------------

Song_Lyrics_dtm <- TermDocumentMatrix(Removed_stopwords_lyrics)
dtm_m <- as.matrix(Song_Lyrics_dtm)
dim(dtm_m) # Dimensions of the term-document matrix 

Song_Lyrics_dtm1 <- TermDocumentMatrix(Lemmatized_Lyrics)
dtm_m1 <- as.matrix(Song_Lyrics_dtm1)
dim(dtm_m1) # Dimensions of the term-document matrix


#label the lyrics column based on an overall net sentiment
labeled_dataset <- label_dataset(dataset)

#creating bag of words model to work with the classifier
bag_of_words_dataset <- bag_of_words(labeled_dataset)

#copy the rating variable to the new dataset
bag_of_words_dataset$rating = labeled_dataset$rating

# Encoding the target feature as factor
bag_of_words_dataset$rating = factor(labeled_dataset$rating, levels = c(0, 1))