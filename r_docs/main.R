################################################################################
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
#load libraries
library(rlang)
library(caTools)
library(randomForest)
library(tm)
library(SnowballC)
library(tidyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(gt)
library(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(quanteda.corpora)
library(magrittr) # needs to be run every time you start R and want to use %>%\
library(textstem)
require(wordcloud2)
require(wordcloud)
library(stopwords)
source("./r_docs/replace_unicode_characters.R")
source("./r_docs/labeling_data.R")
source("./r_docs/bag_of_words.R")
source("./r_docs/random_forest_classification.R")
source("./r_docs/display_tokenize_data.R")
source("./r_docs/display_bag_of_words_model.R")
source("./r_docs/display_cleaned_data.R")


data <- read.csv("./data/artists_songs.csv")

#remove unicode characters
data <- replace_unicode_chars(data)

#get to know the data
#dim(data) # (5147, 4)  --> 5147 rows, 4 columns
#str(data)
#summary(data)

# look at some example texts randomly from the dataset 
data %>% select(lyric) %>% sample_n(4) %>% pull()


unique(data$album) # alumns unique
unique(data$artist) # unique artists in the dataset
data %>% View()


#-------------------------------------------------------------------------------
# Remove swear words
#-------------------------------------------------------------------------------
swears<-read.csv('./data/swear_words.csv')

# These additional stopwords found by preliminary analysis
additional_stopwords <- c("mmm", "gotta", "beyonc", "beyonc�" ,"hey","em", 
                          "huh", "eh", "te", "ohoh", "yeah", "oh","ya", "yo", 
                          "tu", "lo", "je","yuh", "woo", "mi", "de", "da",
                          "eheh","ayy","uhhuh","ariana", "grande", "ah","nicki",
                          "y'all","c'mon", "minaj", "whoa", "nananana", "yay",
                          "rihanna", "eminem", "cardi", "babe", "niggas", 
                          "pre", "na", "ella", "la", "yonc�")


# Transform original dataset into "tidytext" dataset structure
tidy_lyrics <- data %>% 
  mutate(line = row_number()) %>% # to get the row_number the word of the "lyric column" belongs to. So, line column is generated such that line 1 refers to the same song the words belonged to
  unnest_tokens(word, lyric) %>% # to tokenize and reshape the data at the same time ("lyric" is the name of the column in dataset)
  filter(!word %in% swears$swear_words) %>% #Remove swear words
  filter(!word %in%  additional_stopwords) %>% #Remove  additional_stowords
  anti_join(get_stopwords()) # remove default stop words

tidy_lyrics %>% count(word, sort=TRUE)

tidy_lyrics %>% count(title,word, sort=TRUE) %>% View() #word in each song (in descending order, highest at the top)

#--------------------------------------------------------------------
# End of Swear words and additional stopwords, tf-idf
#--------------------------------------------------------------------

#loading the dataset
dataset = read.csv("./data/artists_songs.csv")

#remove unicode characters
dataset <- replace_unicode_chars(dataset)

#display table to display clean dataset
create_table_to_display_clean_dataset(dataset)

#labeling the data
labeled_dataset <- label_dataset(dataset)

#display tokenize data
create_graph_to_display_frequency_of_sentiments(labeled_dataset)

#creating bag of words model to work with the classifier
bag_of_words_dataset <- bag_of_words(labeled_dataset,
                                     additional_stopwords,
                                     swears)

head(bag_of_words_dataset)


#copy the rating variable to the new dataset
bag_of_words_dataset$rating = labeled_dataset$rating

# Encoding the target feature as factor
bag_of_words_dataset$rating = factor(labeled_dataset$rating, levels = c(0, 1))

#display bag of words
# create_table_for_bag_of_words(bag_of_words_dataset)

################################################################################

#splitting the data into training set and test set.
#creating a splitter to split the data in 70% for training set and 30% for
#the test set
split = sort(sample(nrow(bag_of_words_dataset), nrow(bag_of_words_dataset)*.7))
training_set = bag_of_words_dataset[split,]
test_set = bag_of_words_dataset[-split,]


