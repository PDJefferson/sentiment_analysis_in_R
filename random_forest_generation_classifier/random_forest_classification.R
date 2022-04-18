#random forest classification
random_forest_classifier <- function(training_set){
  classifier = randomForest(x = training_set[-ncol(training_set)],
                            y = training_set$rating,
                            ntree = 1000)
  return(classifier)
}