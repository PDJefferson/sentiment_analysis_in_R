needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                     "tm", "SnowballC", "caTools", "rlang", "gt", "stopwords",
                     "sentimentr", "tidytext", "magrittr", "textstem", "randomForest"
                    ,"qdap")

#install packages in case they are not install yet
#install.packages(needed_packages)
  
#load packages 
lapply(needed_packages, require, character.only = TRUE)

source("./preprocessing_data/remove_redundancies_and_bad_words.R")
source("./preprocessing_data/labeling_data.R")
source("./preprocessing_data/bag_of_words.R")
source("./random_forest_generation_classifier/random_forest_classification.R")

swears <- read.csv("./data/swear_words.csv")

# These additional stopwords found by preliminary analysis
additional_stopwords <- c("mmm", "gotta", "beyonce", "beyonc�", "hey","em", 
                          "huh", "eh", "te", "ohoh", "yeah", "oh","ya", "yo", 
                          "tu", "lo", "je","yuh", "woo", "mi", "de", "da",
                          "eheh","ayy","uhhuh","ariana", "grande", "ah","nicki",
                          "imma","y'all","c'mon", "minaj", "whoa", "nananana", 
                          "rihanna", "eminem", "cardi", "niggas", 
                          "pre", "Pre", "na", "ella", "la", "yonc�", "jhen�" )

#loading the data
dataset <- read.csv("./data/artists_songs.csv")

#replaces unicodes for their ascii values and removes redudancies
dataset <- remove_redundancies_and_bad_Words(dataset)

#labels the dataset
labeled_dataset <- label_dataset(dataset)


#creates the bag of words model
bag_of_words_dataset = bag_of_words(labeled_dataset)

#copying the rating var into the bof dataset
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

#Making the Confusion Matrix to compare results
confusion_matrix = table(test_set[, ncol(test_set)], y_pred)

#Accuracy shows the amount of correctly predictions
accuracy_val = multiply_by(divide_by(confusion_matrix[1] + confusion_matrix[4], 
                   nrow(test_set))
                   , 100) 
cat(accuracy_val, "% lyrics label were predicted correctly", sep = '')

#Precision shows the amount of predicted positive songs that were correctly
precision_val = multiply_by(divide_by(confusion_matrix[1], 
                                      confusion_matrix[2] + confusion_matrix[1])
                            , 100)

cat(precision_val, "% of lyrics that aroused overall negative feelings from the 
predicted negatives, were predicted correctly", sep = '')

#sensitivity shows the amount of positive overall examples that were predicted
#accurately
sensitivity = multiply_by(divide_by(confusion_matrix[1], 
                                    confusion_matrix[3] + confusion_matrix[1])
                          , 100)
cat(sensitivity, "% of lyrics that aroused negative feelings only, 
    were predicted accurrately", sep = '')

#fp rate shows the amount of negative values predictive incorrectly
fp_rate = multiply_by(divide_by(confusion_matrix[2], 
                                confusion_matrix[2] + confusion_matrix[4])
                      , 100)
cat(fp_rate, "% of lyrics that were predicted as arousing positive feelings, 
    were predicted wrongly", sep = '')

#specificity show the amount of negative feeling songs that were predicted correctly
specificity = 100 - fp_rate
cat(specificity, "% of lyrics that aroused overall positive feelings from the 
predicted negatives, were predicted correctly", sep = '')