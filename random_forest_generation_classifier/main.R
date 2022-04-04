needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                     "tm", "SnowballC", "caTools", "rlang", "gt", "stopwords",
                     "sentimentr", "tidytext", "magrittr", "textstem", "randomForest")

#install packages in case they are not install yet
#install.packages(needed_packages)

#load packages
lapply(needed_packages, require, character.only = TRUE)

source("./preprocessing_data/remove_redundancies_and_bad_words.R")
source("./preprocessing_data/labeling_data.R")
source("./preprocessing_data/bag_of_words.R")
source("./preprocessing_data/clean_lyrics.R")
source("./random_forest_generation_classifier/random_forest_classification.R")

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

dataset <- remove_redundancies_and_bad_Words(dataset)

#label the lyrics column based on an overall net sentiment
labeled_dataset <- label_dataset(dataset)

#creating bag of words model to work with the classifier
bag_of_words_dataset <- bag_of_words(labeled_dataset)

#copy the rating variable to the new dataset
bag_of_words_dataset$rating = labeled_dataset$rating

# Encoding the target feature as factor
bag_of_words_dataset$rating = factor(labeled_dataset$rating, levels = c(0, 1))

#training  model

#splitting the data into training set and test set.
#creating a splitter to split the data in 80% for training set and 20% for
#the test set
split = sort(sample(nrow(bag_of_words_dataset), nrow(bag_of_words_dataset)*.8))
training_set = bag_of_words_dataset[split,]
test_set = bag_of_words_dataset[-split,]

#training model using random forest classifier
classifier <- random_forest_classifier(training_set)

#predicting test results
y_pred = predict(classifier, newdata = test_set[-ncol(test_set)])

# Making the Confusion Matrix to compare results
confusion_matrix = table(test_set[, ncol(test_set)], y_pred)