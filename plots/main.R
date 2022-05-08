#loading and installing packages
needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                     "tm", "SnowballC", "caTools","rlang", "gt", "stopwords", 
                     "sentimentr", "htmlwidgets", "webshot", "kableExtra", 
                     "wordcloud", "wordcloud2", "magrittr", "ggplot2", "textstem")

#uncomment in case these packages are not install
#install.packages(needed_packages)

lapply(needed_packages, require, character.only = TRUE)

source("./plots/display_tokenize_data.R")
source("./plots/display_vector_technique.R")
source("./plots/display_cleaned_data.R")

#loading the data
dataset <- read.csv("./data/cleaned_15k_dataset.csv")

#copying the data to a new table
dataset1 <- dataset

#display clean dataset in a table
create_table_to_display_clean_dataset(dataset)

#select artists with most songs to display the frequency of sentiments
selected_artist <- dataset %>%
  group_by(artist) %>%
  count(artist) %>%
  ungroup() %>%
  filter(n >= 34)

#filter songs by artists
selected_artists_song <- dataset %>%
  filter( artist %in% selected_artist$artist)

#display tokenize data
create_graph_to_display_frequency_of_sentiments(selected_artists_song)

#loading tf_idf vectorization method
tf_idf_model <- read.csv("./data/tf_idf_dataset.csv")

#display tf_idf
create_table_for_vector_technique(tf_idf_model)

#loading bag_of_word vectorization method
bag_of_words_model <- read.csv("./data/bag_of_words_dataset.csv")

#display bag_of_words
create_table_for_vector_technique(bag_of_words_model)

#vector to corpus data structure to clean data in an efficient manner
Corpus_data <- Corpus(VectorSource(dataset1$lyric))

Song_Lyrics_dtm <- TermDocumentMatrix(Corpus_data)
dtm_m <- as.matrix(Song_Lyrics_dtm)

#display common words in a table when data is cleaned
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
kable(head(dtm_d, 10), col.names = c("Word", "Frequency"), row.names = FALSE,
      caption = "Table 1: Most Common Terms (Cleaned data)", align = "c") %>%
  kable_styling(full_width = F)

#--------------------------------------------------------------------
# Wordcloud
#--------------------------------------------------------------------

#displays a wordcloud after the data has been cleaned
webshot::install_phantomjs(force = TRUE)

wordcloud2(dtm_d, fontFamily = "Comic Sans", size = 1.2)
