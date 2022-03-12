clean_lyrics <- function(Corpus_data, remove_sw = FALSE, my_stopwords = NULL, swear_wrd = NULL, lemmatize = FALSE, stemming = FALSE){
  # Correcting abbreviations
  correct_verb <- content_transformer(function (x , pattern) gsub(pattern, "ing", x))
  Corpus_data <- tm_map(Corpus_data, correct_verb, "in'")
  # Converting the text to lower case
  Corpus_data <- tm_map(Corpus_data, content_transformer(tolower))
  # Removing the numbers
  Corpus_data <- tm_map(Corpus_data, removeNumbers)
  if(remove_sw==TRUE | lemmatize == TRUE | stemming == TRUE){
    # Removing English common stop-words + "come on"
    Corpus_data <- tm_map(Corpus_data, removeWords, c(stopwords("english"), "come on"))
  }

  
  Corpus_data <- tm_map(Corpus_data, removePunctuation)
  
  # Eliminating extra white spaces
  Corpus_data <- tm_map(Corpus_data, stripWhitespace)
  
  # Removing personalized stop-words
  Corpus_data <- tm_map(Corpus_data, removeWords, my_stopwords)
  
  
  # Eliminating extra white spaces
  Corpus_data <- tm_map(Corpus_data, stripWhitespace)
  
  # Remove swear words
  Corpus_data <- tm_map(Corpus_data, removeWords, swear_wrd)
  
  if(lemmatize == TRUE){
    Corpus_data <- tm_map(Corpus_data, lemmatize_strings)
  }
  
  if(stemming == TRUE){
    #words that are alike like loved and love will be take it as one
    Corpus_data <- tm_map(Corpus_data,stemDocument)
  }
  
  
  
  return(Corpus_data)
  
  
}