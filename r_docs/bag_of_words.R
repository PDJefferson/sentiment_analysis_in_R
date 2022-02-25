#creating bag of words

bag_of_words <- function(dataset_original) {
  
  #cleaning up the data
  #vcorpus is a data structuture that will help cleaning
  #the text so we can work with our data using bag of words
  corpus_dataset = VCorpus(VectorSource(dataset_original$lyric))
  #puts all the words in lowercases
  corpus_dataset = tm_map(corpus_dataset, content_transformer(tolower))
  #removes all the numbers in our text
  corpus_dataset = tm_map(corpus_dataset, removeNumbers)
  #removes all the punctuations in our text
  corpus_dataset = tm_map(corpus_dataset, removePunctuation)
  #removes the stop words, like the, a, etc
  corpus_dataset = tm_map(corpus_dataset, removeWords, stopwords())
  #words that are alike like loved and love will be take it as one
  corpus_dataset = tm_map(corpus_dataset, stemDocument)
  #removes white spaces and extra spaces
  corpus_dataset = tm_map(corpus_dataset, stripWhitespace)
  
  #Creating the Bag of Words model
  #creates a table with columns that are all the words that we can find in the review
  #rows represent each review
  #creates a sparse matrix with all of the features
  #sparcity means in this case the words that do not appear frequently
  dtm = DocumentTermMatrix(corpus_dataset)
  
  #removes the words that do not appear frequently, filter non-frequent words that
  #we want to remove from our data
  dtm = removeSparseTerms(dtm, 0.999)
  
  #creating a vector to work with the algorithm
  #we use as.data.frame to tranform in this case a matrix to a vector
  dataset = as.data.frame(as.matrix(dtm))
  
  #we copy the rating variable to the new dataset
  dataset$rating = dataset_original$rating
  
  # Encoding the target feature as factor
  dataset$rating = factor(dataset$rating, levels = c(0, 1))
  
  return(dataset)
}