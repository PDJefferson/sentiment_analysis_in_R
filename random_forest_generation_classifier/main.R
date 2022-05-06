needed_packages <- c("dplyr", "stringr", "tidytext", "tidyr", "textdata", 
                     "tm", "caTools", "gt","tidytext", "magrittr","randomForest"
                    ,"qdap", "doParallel", "superml", "devtools", "ROCR", "cvms",
                    "caret", "ggimage", "rsvg")

#install packages in case they are not install yet
#install.packages(needed_packages)

#load packages 
lapply(needed_packages, require, character.only = TRUE)

source("./random_forest_generation_classifier/random_forest_classification.R")

#loading the preprocessed data
tf_idf_dataset <- read.csv("./data/tf_idf_dataset.csv")

# Encoding the target feature as factor
tf_idf_dataset$rating = factor(tf_idf_dataset$rating, 
                                     levels = c(0, 1))

#splitting the data into training set and test set.
#creating a splitter to split the data in 80% for training set and 20% for
#the test set
split = sort(sample(nrow(tf_idf_dataset), nrow(tf_idf_dataset)*.8))
training_set = tf_idf_dataset[split,]
test_set = tf_idf_dataset[-split,]

#run model with parallel processing using doparallel library to speed up the
#Process

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

start.time <- proc.time()

#training model using random forest classifier
classifier = randomForest(x = training_set[-ncol(training_set)],
                          y = training_set$rating,
                          ntree = 2501,
                          sampsize = 7000,
                          type = "classification",
                          do.trace = 25)

stop.time <- proc.time()

run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)

cl <- makePSOCKcluster(detectCores() - 3)
registerDoParallel(cl)

#get better settings for random forest classifier, takes a lot to compute values
classifier_best_setting = train(y = training_set$rating, 
                                x = training_set[-ncol(training_set)], 
                   data = training_set[-ncol(training_set)],
                   method = "rf")

stopCluster(cl)



#predicting test results
y_pred_test_set = predict(classifier, newdata = test_set[-ncol(test_set)])

#Making the Confusion Matrix to compare results
confusion_matrix = table("target" = test_set$rating, "predicted" = y_pred_test_set)

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
specificity = multiply_by(divide_by(confusion_matrix[4], 
                        (confusion_matrix[2] + confusion_matrix[4])),
                        100)
cat(specificity, "% of lyrics that aroused overall positive feelings from the 
predicted negatives, were predicted correctly", sep = '')

#mean of precision and recall
f_score = divide_by(multiply_by(2, multiply_by(precision_val, sensitivity)),
                    precision_val + sensitivity)

cat(f_score, "% harmonic mean of precision and recall", sep = '')

#plot confusion matrix for test set
cfm <- as_tibble(confusion_matrix)


plot_confusion_matrix(cfm,
                      target_col = "target", 
                      prediction_col = "predicted",
                      counts_col = "n")

#check how the amount of trees affect the accuracy of the predictions
plot(classifier)

#plot roc curve
pred <- prediction(predictions = as.numeric(y_pred_test_set), 
                   labels = as.numeric(test_set$rating))
perf <- performance(pred,"tpr","fpr")

plot(perf , colorize = TRUE) +
  title("ROC CURVE")

#test on unknown data
