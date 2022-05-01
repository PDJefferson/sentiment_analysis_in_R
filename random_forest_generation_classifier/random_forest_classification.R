#random forest classification
random_forest_classifier <- function(training_set){
  classifier = randomForest(x = training_set[-ncol(training_set)],
                            y = training_set$rating,
                            ntree = 5001,
                            do.trace = 25,
                            mtry = 40,
                            type = 'classification')
  
  return(classifier)
}