#random forest classification
random_forest_classifier <- function(training_set){
  classifier = randomForest(x = training_set[-ncol(training_set)],
                            y = training_set$rating,
                            type = "classification",
                            ntree = 2501,
                            mtry = 50,
                            do.trace = 25)
  
  return(classifier)
}