#random forest classification

random_forest_classifier <- function(training_set){
  classifier = randomForest(x = training_set[-6485],
                            y = training_set$rating,
                            ntree = 100)
  return(classifier)
}