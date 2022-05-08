#creating bag of words
bag_of_words <- function(dataset_original) {
  
  #cleaning up the data
  #vcorpus is a data structure that will help cleaning
  #the text so we can work with the data using the bag of words model
  corpus_dataset = VCorpus(VectorSource(dataset$lyric))
  
  #puts all the words in lowercases
  corpus_dataset = tm_map(corpus_dataset, content_transformer(tolower))
  #removes white spaces and extra spaces
  corpus_dataset = tm_map(corpus_dataset, stripWhitespace)
  
  
  #Creating the Bag of Words model.
  dtm = DocumentTermMatrix(corpus_dataset)
  
  #removes the words that do not appear frequently, 
  #filter non-frequent words that
  #don't add any meaning to our data
  dtm = removeSparseTerms(dtm, 0.9951)
  
  #creating a vector to work with the algorithm
  #we use as.data.frame to transform in this case a matrix to a data frame
  bag_of_word_dataset = as.data.frame(as.matrix(dtm))
  
  return(dataset)
}