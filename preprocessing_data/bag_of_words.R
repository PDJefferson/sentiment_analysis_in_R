#creating bag of words
bag_of_words <- function(dataset_original) {
  
  #cleaning up the data
  #vcorpus is a data structure that will help cleaning
  #the text so we can work with our data using the bag of words model
  corpus_dataset = VCorpus(VectorSource(dataset_original$lyric))
  
  #puts all the words in lowercases
  corpus_dataset = tm_map(corpus_dataset, content_transformer(tolower))
  #removes the stop words, like the, a,of etc
  corpus_dataset = tm_map(corpus_dataset, removeWords, stopwords())
  #removes white spaces and extra spaces
  corpus_dataset = tm_map(corpus_dataset, stripWhitespace)
  
  
  #Creating the Bag of Words model.
  #creates a table with columns that are all the words that 
  #we can find in the lyric.
  #rows represent each lyric.
  #creates a sparse matrix with all of the words as the variable for the table
  #sparcity means in this case the words that do not appear frequently
  dtm = DocumentTermMatrix(corpus_dataset)
  
  #removes the words that do not appear frequently, 
  #filter non-frequent words that
  #don't add any meaning to our data
  dtm = removeSparseTerms(dtm, 0.999)
  
  #creating a vector to work with the algorithm
  #we use as.data.frame to transform in this case a matrix to a vector
  dataset = as.data.frame(as.matrix(dtm))
  
  return(dataset)
}