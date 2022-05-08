#creating the tf_idf_vector model
tf_idf_vectorizer <- function(dataset_original) {
 
 
  #vcorpus is a data structure that will help cleaning
  #the text so we can work with our data ussing this vector technique
  corpus_dataset = VCorpus(VectorSource(dataset_original$lyric))
  #puts all the words in lowercases
  corpus_dataset = tm_map(corpus_dataset, content_transformer(tolower))
  #removes white spaces and extra spaces
  corpus_dataset = tm_map(corpus_dataset, stripWhitespace)
  
  #converts corpus data structure to a vector
  vector = data.frame(lyric = sapply(corpus_dataset, as.character), 
                      stringsAsFactors = FALSE)
  
  #creates tf-idf vectorizer.It only takes into account features that appear at
  #least .0050 in entire data to reduce dimensions
  tfv <- TfIdfVectorizer$new(min_df = 0.0050, remove_stopwords = FALSE)
  
  #parallel processing to speed up the process
  cl <- makePSOCKcluster(detectCores() - 1)
  registerDoParallel(cl)
  
  #fits the data with the parameters chosen using the formula for tf-idf
  tfv$fit(vector$lyric)
   
  #tranforms the values into tf-idf values
  tf_matrix <- tfv$transform(vector$lyric)
  
  stopCluster(cl)
  
  #we use as.data.frame to transform the matrix to a data frame
  dataset = as.data.frame(as.matrix(tf_matrix))
  
  return(dataset)
}