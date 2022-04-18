# CSUN Data Mining Class  COMP541  Spring 2022
# Topic Modeling Lyrics
# Mrunal Prakash Gavali
#####################################################################

f <- file.choose()
data <- read.csv(f)

head(data)
str(data)


# Loading packages 
library(tidyverse) #data manipulation and visualization
library(tidytext)  #tidy text analysis
library(topicmodels) #  #topic modeling functions
library(stringr) #common string functions
library(scales) #used for percent scale on confusion table

# COMMAND ----------
data %>% View()


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


# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
data$lyric  <- sapply(data$lyric , removeSpecialChars)

data %>% View()




# convert everything to lower case
data$lyric <- sapply(prince$lyrics, tolower)

str(data[139, ]$lyric, nchar.max = 300)

#get facts about the full dataset
summary(data)


swears<-read.csv("./data/swear_words.csv")

# These additional stopwords found by preliminary analysis
additional_stopwords <- c("mmm", "gotta", "beyonc", "beyonc�" ,"hey","em", 
                          "huh", "eh", "te", "ohoh", "yeah", "oh","ya", "yo", 
                          "tu", "lo", "je","yuh", "woo", "mi", "de", "da",
                          "eheh","ayy","uhhuh","ariana", "grande", "ah","nicki",
                          "imma","y'all","c'mon", "minaj", "whoa", "nananana", 
                          "rihanna", "eminem", "cardi", "babe", "niggas", 
                          "pre", "na", "ella", "la", "yonc�", "jhen�")


head(sample(stop_words$word, 15), 15)


#unnest and remove stop, undesirable and short words
data_words_filtered <- data %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% additional_stopwords) %>%
  filter(!word %in% swears$swear_words) %>%
  filter(nchar(word) > 3)

class(data_words_filtered) # [1] "data.frame"
dim(data_words_filtered) # [1] 320788      4

data %>% View()

#set size of plot
options(repr.plot.height = 400, repr.plot.width = 1000, repr.plot.res = 100)

# look at some example texts
data %>% select(lyrics) %>% sample_n(4,seed=1234) %>% pull()

#Lyrics need to be of type character
data$lyric<-sapply(data$lyric,as.character)

#Split by album, this will become clearer later
data.album<-split(data,data$album)
unique(data$album) # 538 alumns unique


#  manipulate the lyrics into a Document Term Matrix (DTM). 
# A DTM is a sparse matrix which contains counts of each words in the entire dicitionary (columns) in each document (rows.) 
# Once we have the lyrics in the form of a DTM, the LDA will be very easy.

# ---------------------------------------------------------------------------------
#   \%\>\%   We loaded into R via the `dplyr` package and is originally part of the `magrittr` package. It is used to chain funcitons together that would otherwise be nested. In some cases it is easier to read than nesting many functions. 
# A simple example is that the following two expressions are equivalent:

# head(sort(data$artist),10)

# data$artist%>%sort%>%head(10)

album_names<-names(data.album)
by_song<-c()
for(ii in 1:length(data.album)){
  album<-rowid_to_column(tibble(text=data.album[[ii]]$lyric,
                                title=album_names[ii]),var='song')
  by_song<-rbind(by_song,album)
}

by_song<-by_song %>%
  unite(document,c('title','song'),sep="_",remove=T)

by_song


# ---------------------------------------------------------------------------------
# word counts 
# ---------------------------------------------------------------------------------
by_song_word <- by_song %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_song_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()


# Bag of words
#cleaning up the data
#vcorpus is a data structure that will help cleaning
#the text so we can work with our data using bag of words
corpus_dataset = VCorpus(VectorSource(data$Consumer_complaint_narrative))
#puts all the words in lowercases
corpus_dataset = tm_map(corpus_dataset, content_transformer(tolower))
#removes all the numbers in our text
corpus_dataset = tm_map(corpus_dataset, removeNumbers)
#removes all the punctuations in our text
corpus_dataset = tm_map(corpus_dataset, removePunctuation)
#removes the stop words, like the, a, etc
corpus_dataset = tm_map(corpus_dataset, removeWords, stopwords())
#words that are alike like loved and love will be take it as one
corpus_dataset = tm_map(corpus_dataset, stemDocument)
#removes white spaces and extra spaces
corpus_dataset = tm_map(corpus_dataset, stripWhitespace)

#----------------------------------------------------------------------


# Add stop words

add_stop<-c("la","it's","don't","that's","yeah","ya","uh","ye","ra","yo")
stop_words_add<-add_row(stop_words,
                        word = add_stop)
stop_words_add<-stop_words_add[-which(stop_words_add[,1]=='me'),]

word_counts <- by_song_word %>%
  anti_join(stop_words_add) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

head(word_counts)

# document_matrix
songs_dtm <- word_counts %>%
  cast_dtm(document, word, n)

control <- list(burnin = 500, iter = 1000, keep = 100, seed = 2500)

install.packages("devtools", type = "win.binary")

invisible(lapply( file.path( "https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions", c("topicmodels2LDAvis.R", "optimal_k.R") ), devtools::source_url ))

opt.k = optimal_k(songs_dtm, max.k=30, control=control,drop.seed = FALSE)
opt.k
song_lda = LDA(songs_dtm, k = as.numeric(opt.k), method="Gibbs", control=control)

#Interpret LDA
#------
#  Interpreting the LDA output is the most important part of the analysis and takes more care and time.

#First we will try go get an idea of what topics were created by printing the top 10 terms per topic. Do you see any patterns?
#  ```{r}
#output top 10 terms per topic
lda_inf = posterior(song_lda)
topics.hp = topics(song_lda,1)
terms.hp = terms(song_lda, 10)
print(terms.hp[])

install.packages("reshape2")
song_topics <- tidy(song_lda, matrix = "beta")
top_n(song_topics, 10)

top_terms <- song_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



song_gamma <- tidy(song_lda, matrix = "gamma")

song_gamma <- song_gamma %>%
  separate(document, c("title", "song"), sep = "_", convert = TRUE)

top_n(song_gamma, 20)


song_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma,fill=factor(topic))) +
  geom_boxplot() +
  facet_wrap(~ title)+
  scale_y_sqrt()
# ------------------------------------
#    Goal
# 1. Apply textstat to find 
# - readability score 
# - lexical diversity
# - difficult word
# - word frequency per song


# Word Frequency: number of words per song
# Word Length: average length of individual words in a text
# Lexical Diversity: number of unique words used in a text (song vocabulary)
# Lexical Density: the number of unique words divided by the total number of words (word repetition)

# 
#-------------------------------------
dim(data) #  5751    4  --> dimensions
names(data) #  "artist" "title"  "lyric"  "album"  --> attributes
str(data) # the structure is revealed here

attributes(data) # name, class, etc.,

data[1:5, ] # The first five

# list of undesirable words that need to be removed manually 

undesirable_words <- c("fuckin", "mmm", "baby", "fuck", "ass", "bitches", "bitch", "gotta",
                        "damn", "shit", "beyonc", "hey","em", "huh", "eh", "te", "ohoh",
                       "tu", "lo", "je","yuh", "woo", "mi", "de", "da", "eheh","ayy","uhhuh",
                       "whoa", "nananana", "rihanna", "eminem", "cardi", "babe", "niggas", "pre", "na", "ella"  )