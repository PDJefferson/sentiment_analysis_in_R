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

source("./r_scripts/clean_lyrics.R")
source("./r_scripts/remove_redundancies.R")

data <- read.csv("./data/artists_songs.csv")

dim(data) # 5751 4 
unique(data$album) # 538 
unique(data$artist) # 22 unique artists
summary(data)

# look at some example texts randomly from the dataset 
data %>% select(lyric) %>% sample_n(4) %>% pull()


swears<-read.csv("./data/swear_words.csv")

# These additional stopwords found by preliminary analysis
additional_stopwords <- c("mmm", "gotta", "beyonc", "beyonc�" ,"hey","em", 
                          "huh", "eh", "te", "ohoh", "yeah", "oh","ya", "yo", 
                          "tu", "lo", "je","yuh", "woo", "mi", "de", "da",
                          "eheh","ayy","uhhuh","ariana", "grande", "ah","nicki",
                          "imma","y'all","c'mon", "minaj", "whoa", "nananana", 
                          "rihanna", "eminem", "cardi", "babe", "niggas", 
                          "pre", "na", "ella", "la", "yonc�", "jhen�")




# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  doc <- gsub("'cause", " because", doc)
  return(doc)
}

# fix (expand) contractions
data$lyric <- sapply(data$lyric, fix.contractions)

data %>% View()

head(sample(stop_words$word, 30), 30)
#------------------------------------------------------------------------------
# Corpus cleaning testing
#------------------------------------------------------------------------------

# See output of original lyrics after removing unicode without (lemmatization, swear words and stopwords & additional stopwords removal )
Corpus_data <- Corpus(VectorSource(data$lyric))
substr(Corpus_data[[1]]$content, 1, 510) 

# Apply clean_lyrics function (removed punctuations)
Cleaned_lyrics <- clean_lyrics(Corpus_data)

substr(Cleaned_lyrics[[1]]$content, 1, 510)

#head(Cleaned_lyrics)
# <<SimpleCorpus>>
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 6


# Remove additional stopwords and swear words (not yet lemmatized)
Removed_stopwords_lyrics <- clean_lyrics(Corpus_data, remove_sw = TRUE, 
                                         my_stopwords = additional_stopwords, 
                                         swear_wrd = swears$swear_words )
substr(Removed_stopwords_lyrics[[1]]$content, 1, 510)



# Lemmatization
Lemmatized_Lyrics <- clean_lyrics(Corpus_data, lemmatize = TRUE, 
                                  my_stopwords = additional_stopwords, 
                                  swear_wrd = swears$swear_words )
substr(Lemmatized_Lyrics[[1]]$content, 1, 510)
#Lemmatized_Lyrics %>% View()

# Stemming
#StemDoc_Lyrics <- clean_lyrics(Corpus_data, stemming = TRUE, my_stopwords = additional_stopwords, swear_wrd = swears$swear_words )
#substr(StemDoc_Lyrics[[1]]$content, 1, 510)

#-------------------------------------------------------------------------------
# Comparing different data cleaning outputs
#-------------------------------------------------------------------------------

substr(Corpus_data[[1]]$content, 1, 510)  # Corpus_data
substr(Cleaned_lyrics[[1]]$content, 1, 510) # Cleaned text
substr(Removed_stopwords_lyrics[[1]]$content, 1, 510) # Text without stop-words including additional stop words
substr(Lemmatized_Lyrics[[1]]$content, 1, 510) # Lemmatized text
#substr(StemDoc_Lyrics[[1]]$content, 1, 510) # Stemming applied to text


# ------------------------------------------------------------------------------
# Section of removing songs less than 350 words 
#-------------------------------------------------------------------------------
#remove unicode characters
data <- remove_redundancies(data)  # --. this function not workging properly (does not remove swear words and ariana's 1st song is weird if applied before Mrunal's lemmatization function.)
data %>% View()

dim(data) # (5137, 4)  --> 5137 rows, 4 columns |   5143    4  |   5140    4  |  5142 4
# currently 5142 songs and 4 rows
unique(data$album) # 527 
unique(data$artist) # 20 unique artists in the dataset



#--------------------------------------------
#
#------------------------------------------
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations


library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function
full_word_count <- data %>%
  unnest_tokens(word, lyric) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

full_word_count[1:10,] %>%
  ungroup(num_words, title) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(title = color_tile("lightpink","lightpink")(title)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

#--------------------------------------------

#-------------------------------------------------------------------------------
# The Lemmatized lyrics corpus is be used to build a Term-Document Matrix (TDM).
# Term-Document Matrix  counts the number of times every unique word is repeated 
# in each Lyrics. 
# Summation of the rows of the Term-Document Matrix results in counting the 
# number of times every term is repeated across all song lyrics
#-------------------------------------------------------------------------------

Song_Lyrics_dtm1 <- TermDocumentMatrix(Lemmatized_Lyrics)
dtm_m1 <- as.matrix(Song_Lyrics_dtm1)
dim(dtm_m1) # Dimensions of the term-document matrix


#-----------------------------------------------------------
# LDA topic modeling
#-----------------------------------------------------------
# Manipulate the lyrics into a Document Term Matrix (DTM). A DTM is a sparse matrix which contains counts of each words in the entire dicitionary (columns) in each document (rows.) Once we have the lyrics in the form of a DTM, the LDA will be very easy. 
# ------------------------------------------------------------------------------

# write.csv(refined_artists_dataset,"your directory path\\artists_songs.csv", row.names = FALSE)
