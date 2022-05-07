needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                      "tm", "SnowballC", "caTools", "rlang", "gt", "stopwords",
                     "sentimentr", "tidytext", "magrittr", "textstem","qdap", 
                     "doParallel", "devtools", "cld2", "superml")
#install packages in case they are not install yet
#install.packages(needed_packages)
#devtools::install_github("saraswatmks/superml")
#install.packages("superml", dependencies=TRUE)

#load packages
lapply(needed_packages, require, character.only = TRUE)

source("./preprocessing_data/remove_redundancies_and_bad_words.R")
source("./preprocessing_data/labeling_data.R")
source("./preprocessing_data/tf_idf_model.R")
source("./preprocessing_data/clean_lyrics.R")

#-------------------------------------------------------------------------------
# Swear words and additional stopwords
#-------------------------------------------------------------------------------
#load dataset
data <- read.csv("./data/labeled_lyrics_cleaned.csv")


swears<-read.csv("./data/swear_words.csv")

# These additional stopwords found by preliminary analysis
additional_stopwords <- c("mmm", "gotta", "beyoncé", "beyonc�", "hey","em", 
                          "huh", "eh", "te", "ohoh", "yeah", "oh","ya", "yo", 
                          "tu", "lo", "je","yuh", "woo", "mi", "de", "da",
                          "eheh","ayy","uhhuh","ariana", "grande", "ah","nicki",
                          "imma","y'all","c'mon", "minaj", "whoa", "nananana", 
                          "rihanna", "eminem", "cardi", "niggas", 
                          "pre", "Pre", "na", "ella", "la", "yonc�", "jhen�" ,
                          "taylor")


#change the labels of the data
data <- data %>%
  transmute(artist = artist
            ,lyric = data$seq
            ,title = song
            ,rating = ifelse(label >= 0.67 ,
                             1 , 
                             ifelse(label <= 0.33, 0,  label)))


data <- data %>%
  filter(rating == 1 | rating == 0)

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

#removes redudancies
data <- remove_redundancies_and_bad_Words(data)

stopCluster(cl)

#get only overall positive songs
positive_songs <- data %>%
  filter(rating == 1)

#get only overall negative songs
negative_songs <- data %>%
  filter(rating == 0)

positive_songs <- sample_n(positive_songs , 7500)
negative_songs <- sample_n(negative_songs, 7500)

#to get a perfectly balanced amount of positive and negative songs
data <- rbind(positive_songs, negative_songs)
 
#shuffles the data
data = data[sample(1:nrow(data)), ]

dataset <- data
data1 <- data

write.csv(dataset, 
          "./data/cleaned_15k_dataset.csv", 
          row.names = FALSE)

#general information

dim(data) # (15000, 4)  --> 15000 rows, 4 columns
str(data)
summary(data)

# look at some example texts randomly from the dataset 
data %>% select(lyric) %>% sample_n(4) %>% pull()

unique(data$artist) # 1000+ unique artists in the dataset
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

#creating tf_idf model to work with the classifier
tf_idf_dataset <- tf_idf_vectorizer(dataset)

#copy the dependent feature to the new dataset
tf_idf_dataset$rating = dataset$rating

#Encoding the dependent feature as factor
tf_idf_dataset$rating = factor(tf_idf_dataset$rating, 
                                     levels = c(0, 1))
#saving the data
write.csv(tf_idf_dataset, 
          "./data/tf_idf_dataset.csv",
          row.names = FALSE)


#loading tf_idf_dataset 
tf_idf_dataset <- read.csv("./data/tf_idf_dataset.csv")
#unknown data
unknown_data <- read.csv("./data/banda_music.csv")

#cleaning data
unknown_data <- remove_redundancies_and_bad_Words(unknown_data)

unknown_data <- subset(unknown_data, select = -c(album))

#load clean data
cleaned_data <- read.csv("./data/cleaned_15k_dataset.csv")

#cleaning up the data
#vcorpus is a data structure that will help cleaning
#the text so we can work with our data using the bag of words model
corpus_dataset = VCorpus(VectorSource(cleaned_data$lyric))

#puts all the words in lowercases
corpus_dataset = tm_map(corpus_dataset, content_transformer(tolower))
#removes the stop words, like the, a,of etc
corpus_dataset = tm_map(corpus_dataset, removeWords, stopwords())
#removes white spaces and extra spaces
corpus_dataset = tm_map(corpus_dataset, stripWhitespace)

vector = data.frame(lyric = sapply(corpus_dataset, as.character), 
                    stringsAsFactors = FALSE)

#creates tf-idf vectorizer. only takes into account features that appear at
#least .0045 in entire data to reduce dimensionality
tfv <- TfIdfVectorizer$new(min_df = 0.00050, remove_stopwords = FALSE)

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

#we fit on data
tfv$fit(vector$lyric)

#tranforms the values into tf-idf values
tf_matrix <- tfv$transform(vector$lyric)

stopCluster(cl)

#creating a vector to work with the algorithm
#we use as.data.frame to transform in this case a matrix to a vector
dataset = as.data.frame(as.matrix(tf_matrix))




