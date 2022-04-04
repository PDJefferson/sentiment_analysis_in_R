#loading and installing packages
needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                     "tm", "SnowballC", "caTools","rlang", "gt", "stopwords", 
                     "sentimentr", "htmlwidgets", "webshot", "kableExtra", 
                     "wordcloud", "wordcloud2", "magrittr", "ggplot2", "textstem")

install.packages(needed_packages)
lapply(needed_packages, require, character.only = TRUE)

#loading r scripts
source("./preprocessing_data/remove_redundancies_and_bad_words.R")
source("./preprocessing_data/labeling_data.R")
source("./preprocessing_data/bag_of_words.R")
source("./preprocessing_data/clean_lyrics.R")
source("./plots/display_bag_of_words_model.R")
source("./plots/display_cleaned_data.R")
source("./plots/display_tokenize_data.R")

swears<-read.csv("./data/swear_words.csv")

# These additional stopwords found by preliminary analysis
additional_stopwords <- c("mmm", "gotta", "beyonc", "beyonc�" ,"hey","em", 
                          "huh", "eh", "te", "ohoh", "yeah", "oh","ya", "yo", 
                          "tu", "lo", "je","yuh", "woo", "mi", "de", "da",
                          "eheh","ayy","uhhuh","ariana", "grande", "ah","nicki",
                          "y'all","c'mon", "minaj", "whoa", "nananana", 
                          "rihanna", "eminem", "cardi", "babe", "niggas", 
                          "pre", "na", "ella", "la", "yonc�", "jhen�")

#loading the data
dataset <- read.csv("./data/artists_songs.csv")

#cleaning dataset
dataset <- remove_redundancies_and_bad_Words(dataset)

#copying the data to a new vector
dataset1 <- dataset

#display clean dataset in a table
create_table_to_display_clean_dataset(dataset)

#label the lyrics column based on an overall net sentiment
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

#vector to corpus data structure to clean data in an efficient manner
Corpus_data <- Corpus(VectorSource(dataset1$lyric))

Cleaned_lyrics <- clean_lyrics(Corpus_data)


# Remove additional stopwords and swear words
Removed_stopwords_lyrics <- clean_lyrics(Corpus_data, remove_sw = TRUE, 
                                         my_stopwords = additional_stopwords, 
                                         swear_wrd = swears$swear_words )

Song_Lyrics_dtm <- TermDocumentMatrix(Removed_stopwords_lyrics)
dtm_m <- as.matrix(Song_Lyrics_dtm)

#display common words in a table when data is cleaned
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
kable(head(dtm_d, 10), col.names = c("Word", "Frequency"), row.names = FALSE,
      caption = "Table 1: Most Common Terms (Cleaned Text)", align = "c") %>%
  kable_styling(full_width = F)

# Lemmatization
Lemmatized_Lyrics <- clean_lyrics(Corpus_data, lemmatize = TRUE, 
                                  my_stopwords = additional_stopwords, 
                                  swear_wrd = swears$swear_words )

Song_Lyrics_dtm1 <- TermDocumentMatrix(Lemmatized_Lyrics)
dtm_m1 <- as.matrix(Song_Lyrics_dtm1)

#display common words in a table after lemmatization
dtm_v1 <- sort(rowSums(dtm_m1), decreasing = TRUE)
dtm_d1 <- data.frame(word = names(dtm_v1), freq = dtm_v1)
kable(head(dtm_d1, 10), col.names = c("Lemma", "Frequency"), row.names = FALSE,
      caption = "Table 2: Most Common Terms (Lemmatized Text)", align = "c") %>%
  kable_styling(full_width = F)

#--------------------------------------------------------------------
# Wordclouds
#--------------------------------------------------------------------

#displays a wordcloud after the data has been stripped from the stopwords
webshot::install_phantomjs(force = TRUE)

wordcloud2(dtm_d, fontFamily = "Comic Sans", size = 1.2)

#displays a wordcloud after the words has been reduced to its most natural form
#or lemmatize
wordcloud2(dtm_d1, fontFamily = "Comic Sans", size = 1.2)
