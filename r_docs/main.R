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
#install.packages('sentimentr')
#load libraries
library(sentimentr)
library(rlang)
library(caTools)
library(tm)
library(SnowballC)
library(tidyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(gt)
library(quanteda)
require(quanteda.corpora)
library(magrittr) # needs to be run every time you start R and want to use %>%\
library(textstem)
require(wordcloud2)
require(wordcloud)
library(stopwords)
require(kableExtra)
require(htmlwidgets)
require(webshot)
source("./r_docs/clean_data.R")
source("./r_docs/labeling_data.R")
source("./r_docs/bag_of_words.R")
source("./r_docs/display_tokenize_data.R")
source("./r_docs/display_bag_of_words_model.R")
source("./r_docs/display_cleaned_data.R")
source("./r_docs/clean_lyrics.R")

#----------------------------------------------------
# Swear words and additional stopwords
#----------------------------------------------------
data <- read.csv("./data/artists_songs.csv")

swears<-read.csv('./data/swear_words.csv')

# These additional stopwords found by preliminary analysis
additional_stopwords <- c("mmm", "gotta", "beyonc", "beyonc�" ,"hey","em", 
                          "huh", "eh", "te", "ohoh", "yeah", "oh","ya", "yo", 
                          "tu", "lo", "je","yuh", "woo", "mi", "de", "da",
                          "eheh","ayy","uhhuh","ariana", "grande", "ah","nicki",
                          "y'all","c'mon", "minaj", "whoa", "nananana", 
                          "rihanna", "eminem", "cardi", "babe", "niggas", 
                          "pre", "na", "ella", "la", "yonc�", "jhen�")


#remove unicode characters
data <- clean_data_vars(data)

#get to know the data
dim(data) # (5137, 4)  --> 5137 rows, 4 columns
str(data)
summary(data)

# look at some example texts randomly from the dataset 
data %>% select(lyric) %>% sample_n(4) %>% pull()


unique(data$album) # 527 
unique(data$artist) # 20 unique artists in the dataset
data %>% View()



#-------------------------------------------------------------------------------
# to see tidy format 
#-------------------------------------------------------------------------------



# Transform original dataset into "tidytext" dataset structure
tidy_lyrics <- data %>% 
  mutate(line = row_number()) %>% # to get the row_number the word of the "lyric column" belongs to. So, line column is generated such that line 1 refers to the same song the words belonged to
  unnest_tokens(word, lyric) %>% # to tokenize and reshape the data at the same time ("lyric" is the name of the column in dataset)
  filter(!word %in% swears$swear_words) %>% #Remove swear words
  filter(!word %in%  additional_stopwords) %>% #Remove  additional_stowords
  anti_join(get_stopwords()) # remove default stop words

tidy_lyrics %>% count(word, sort=TRUE)

tidy_lyrics %>% count(title,word, sort=TRUE) %>% View() #word in each song (in descending order, highest at the top)

#--------------------------------------------------------------------------------------------------------------------------
# Corpus cleaning testing
#--------------------------------------------------------------------------------------------------------------------------
data1 <- read.csv("./data/artists_songs.csv")
Corpus_data <- Corpus(VectorSource(data1$lyric))
substr(Corpus_data[[1]]$content, 1, 510) 

Cleaned_lyrics <- clean_lyrics(Corpus_data)
substr(Cleaned_lyrics[[1]]$content, 1, 510)

head(Cleaned_lyrics)
# <<SimpleCorpus>>
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 6


# Remove additional stopwords and swear words
Removed_stopwords_lyrics <- clean_lyrics(Corpus_data, remove_sw = TRUE, my_stopwords = additional_stopwords, swear_wrd = swears$swear_words )
substr(Removed_stopwords_lyrics[[1]]$content, 1, 510)

# Lemmatization
Lemmatized_Lyrics <- clean_lyrics(Corpus_data, lemmatize = TRUE, my_stopwords = additional_stopwords, swear_wrd = swears$swear_words )
substr(Lemmatized_Lyrics[[1]]$content, 1, 510)

# Stemming
StemDoc_Lyrics <- clean_lyrics(Corpus_data, stemming = TRUE, my_stopwords = additional_stopwords, swear_wrd = swears$swear_words )
substr(StemDoc_Lyrics[[1]]$content, 1, 510)

#--------------------------------------------------------------------------------------------------------
# Comparing different data cleaning outputs
#--------------------------------------------------------------------------------------------------------
substr(Corpus_data[[1]]$content, 1, 510)  # Corpus_data
substr(Cleaned_lyrics[[1]]$content, 1, 510) # Cleaned text
substr(Removed_stopwords_lyrics[[1]]$content, 1, 510) # Text without stop-words including additional stopwords
substr(Lemmatized_Lyrics[[1]]$content, 1, 510) # Lemmatized text
substr(StemDoc_Lyrics[[1]]$content, 1, 510) # Stemming applied to text



#-------------------------------------------------------------------------------------------------------------------------------------------
# The Lemmatized lyrics corpus is be used to build a Term-Document Matrix (TDM).
# Term-Document Matrix  counts the number of times every unique word is repeated in each Lyrics. 
# Summation of the rows of the Term-Document Matrix results in counting the number of times every term is repeated across all song lyrics
#------------------------------------------------------------------------------------------------------------------------------------------


Song_Lyrics_dtm <- TermDocumentMatrix(Removed_stopwords_lyrics)
dtm_m <- as.matrix(Song_Lyrics_dtm)
dim(dtm_m) # Dimensions of the term-document matrix 


Song_Lyrics_dtm1 <- TermDocumentMatrix(Lemmatized_Lyrics)
dtm_m1 <- as.matrix(Song_Lyrics_dtm1)
dim(dtm_m1) # Dimensions of the term-document matrix



dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
kable(head(dtm_d, 10), col.names = c("Word", "Frequency"), row.names = FALSE,
      caption = "Table 1: Most Common Terms (Cleaned Text)", align = "c") %>%
  kable_styling(full_width = F)




dtm_v1 <- sort(rowSums(dtm_m1), decreasing = TRUE)
dtm_d1 <- data.frame(word = names(dtm_v1), freq = dtm_v1)
kable(head(dtm_d1, 10), col.names = c("Lemma", "Frequency"), row.names = FALSE,
      caption = "Table 2: Most Common Terms (Lemmatized Text)", align = "c") %>%
  kable_styling(full_width = F)

webshot::install_phantomjs()
wordcloud_lyrics <- wordcloud2(dtm_d, fontFamily = "Comic Sans", size = 1.2)
saveWidget(wordcloud_lyrics,"images/wordcloud_removed_stopwords.html", selfcontained = F)
webshot("images/wordcloud_removed_stopwords.html", "images/wordcloud_removed_stopwords.png", vwidth = 900, vheight = 600, delay = 20)


wordcloud_lyrics <- wordcloud2(dtm_d1, fontFamily = "Comic Sans", size = 1.2)
saveWidget(wordcloud_lyrics,"images/wordcloud_lemmatized.html", selfcontained = F)
webshot("images/wordcloud_lemmatized.html", "images/wordcloud_lemmatized.png", vwidth = 900, vheight = 600, delay = 20)

#--------------------------------------------------------------------
# End of wordclouds
#--------------------------------------------------------------------

#loading the dataset
dataset = read.csv("./data/artists_songs.csv")

#clean data 
dataset <- clean_data_vars(dataset)

#display table to display clean dataset
create_table_to_display_clean_dataset(dataset)



#labeling the data
labeled_dataset <- label_dataset(dataset)

#display tokenize data
create_graph_to_display_frequency_of_sentiments(labeled_dataset)

#creating bag of words model to work with the classifier
bag_of_words_dataset <- bag_of_words(labeled_dataset)

#copy the rating variable to the new dataset
bag_of_words_dataset$rating = labeled_dataset$rating

# Encoding the target feature as factor
bag_of_words_dataset$rating = factor(labeled_dataset$rating, levels = c(0, 1))

#display bag of words
create_table_for_bag_of_words(bag_of_words_dataset)