needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                      "tm", "SnowballC", "caTools", "rlang", "gt", "stopwords",
                     "sentimentr", "tidytext", "magrittr", "textstem","qdap", 
                     "doParallel", "devtools", "cld2", "superml")

#install packages in case they are not install yet (uncomment the next lines)
#install.packages(needed_packages)
#devtools::install_github("saraswatmks/superml")
#install.packages("superml", dependencies=TRUE)

#load packages
lapply(needed_packages, require, character.only = TRUE)

source("./preprocessing_data/remove_redundancies_and_bad_words.R")
source("./preprocessing_data/labeling_data.R")
source("./preprocessing_data/tf_idf_model.R")
source("./preprocessing_data/bag_of_words.R")

#load dataset
dataset <- read.csv("./data/labeled_lyrics_cleaned.csv")

#additional swear words found by preliminary analysis
swears <- read.csv("./data/swear_words.csv")

#additional stopwords found by preliminary analysis
additional_stopwords <- read.csv("./data/additional_stop_words.csv")


#change the labels of the data
dataset  <- dataset  %>%
  transmute(artist = artist
            ,lyric = data$seq
            ,title = song
            ,rating = ifelse(label >= 0.67 ,
                             1 , 
                             ifelse(label <= 0.33, 0,  label)))

#remove any songs that do not have a label rating of 0 or 1
dataset  <- dataset %>%
  filter(rating == 1 | rating == 0)


#cleans the lyric column by using this function
dataset  <- remove_redundancies_and_bad_Words(dataset)

#The following steps are to get a even amount of positive and negative songs
#get only overall positive songs
positive_songs <- dataset  %>%
  filter(rating == 1)

#get only overall negative songs
negative_songs <- dataset %>%
  filter(rating == 0)

#extract 7500 songs from each postive and negative data frame
positive_songs <- sample_n(positive_songs , 7500)
negative_songs <- sample_n(negative_songs, 7500)

#to get a perfectly balanced amount of positive and negative songs
dataset <- rbind(positive_songs, negative_songs)
 
#shuffle the data to randomize the positions of positive and negative songs
dataset = dataset[sample(1:nrow(dataset)), ]

#save the cleaned dataset to use it later
write.csv(dataset, 
          "./data/cleaned_15k_dataset.csv", 
          row.names = FALSE)

#general information
dim(dataset) # (15000, 4)  --> 15000 rows, 4 columns
str(dataset)
summary(dataset)

# look at some example texts randomly from the dataset 
dataset %>% select(lyric) %>% sample_n(4) %>% pull()

unique(dataset$artist) # 1000+ unique artists in the dataset
dataset %>% View()

#creating tf_idf vector to work with the classifier
tf_idf_dataset <- tf_idf_vectorizer(dataset)

#copy the dependent feature to the vectorizer data
tf_idf_dataset$rating = dataset$rating

#Encoding the dependent feature as factor
tf_idf_dataset$rating = factor(tf_idf_dataset$rating, 
                                     levels = c(0, 1))

#saving the tf_idf data to user it later
write.csv(tf_idf_dataset, 
          "./data/tf_idf_dataset.csv",
          row.names = FALSE)

#creating a bag of words vector to work with the classifier
bag_of_word_dataset <- bag_of_words(dataset)

#copy the dependent feature to the vectorizer data
bag_of_word_dataset$rating = dataset$rating

#Encoding the dependent feature as fsactor
bag_of_word_dataset$rating = factor(bag_of_word_dataset$rating, 
                               levels = c(0, 1))

#saving the bag of words data to use it later
write.csv(bag_of_word_dataset, 
          "./data/bag_of_words_dataset.csv",
          row.names = FALSE)