#random forest classification
random_forest_classifier <- function(training_set){
  classifier = randomForest(x = training_set[-ncol(training_set)],
                            y = training_set$rating,
                            ntree = 500)
  return(classifier)
}