needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                     "tm", "SnowballC", "caTools", "rlang", "gt", "stopwords",
                     "sentimentr", "tidytext", "magrittr", "textstem", "e1071"
                     ,"qdap", "rpud", "caret")

#install packages in case they are not install yet
#install.packages(needed_packages)

#load packages 
lapply(needed_packages, require, character.only = TRUE)


#creates the tf_idf_data
tf_idf_dataset = read.csv("./data/tf_idf_dataset.csv")

# Encoding the target feature as factor
tf_idf_dataset$rating = factor(tf_idf_dataset$rating, levels = c(0, 1))

#training  model

#splitting the data into training set and test set.
#creating a splitter to split the data in 80% for training set and 20% for
#the test set
split = sort(sample(nrow(tf_idf_dataset), nrow(tf_idf_dataset)*.75))
training_set = tf_idf_dataset[split,]
test_set = tf_idf_dataset[-split,]

#training model using svm
classifier <- svm(formula = training_set$rating ~ .,
                  data = training_set[-ncol(training_set)],
                  type= "C-classification",
                  kernel = "linear")

#predicting test results
y_pred = predict(classifier, newdata = test_set[-ncol(test_set)])

#Making the Confusion Matrix to compare results
confusion_matrix = table(test_set[, ncol(test_set)], y_pred)

#get better configurations for svm 
classifier = train(y = training_set$rating, x = training_set[-ncol(training_set)], 
                   data = training_set[-ncol(training_set)],
                   method = "svmLinearWeights")

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