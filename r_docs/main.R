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
#to create the bag of words
# install.packages('SnowballC')
#to work with the classifier
# install.packages('randomForest')
#to split the data randomly
# install.packages('caTools')

library(caTools)
library(randomForest)
library(tm)
library(SnowballC)
library(tidyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
source("./r_docs/labeling_data.R")
source("./r_docs/bag_of_words.R")
source("./r_docs/random_forest_classification.R")

#loading the dataset
dataset = read.csv("./data/artists_songs.csv")

#labeling the data
labeled_dataset <- label_dataset(dataset)

#creating bag of words to work with the classifier
bag_of_words_dataset <- bag_of_words(labeled_dataset)

################################################################################
#training our model

#splitting the data into training set and test set
#creating splitter, which will split the data in 70% for training set and 30% for
#the test set
split = sort(sample(nrow(bag_of_words_dataset), nrow(bag_of_words_dataset)*.7))
training_set = bag_of_words_dataset[split,]
test_set = bag_of_words_dataset[-split,]

#training model using random forest classifier
classifier <- random_forest_classifier(training_set)

#predicting test results
y_pred = predict(classifier, newdata = test_set[-6485])

# Making the Confusion Matrix to compare results
confusion_matrix = table(test_set[, 6485], y_pred)


