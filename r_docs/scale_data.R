################################################################################
#tokenize the dataset
tokenize_dataset <- function(dataset) {
 
  #breaking each word in the lyric and keeping track of which lyric
  #the word belongs
  tokenized_dataset <- dataset %>%
    group_by(artist) %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, lyric)
  
  #comparing artists lyrics with bing sentiment and counting the
  #the sentiments (by filtering between positive and negative ) of 
  #each song by its respective word in their songs
  scaled_artist_sentiment_analysis_through_words<- tokenized_dataset %>%
    inner_join(get_sentiments("bing")) %>%
    count(artist,title,album, word,sentiment, sort = FALSE) %>%
    ungroup()
  
  #encoding categorical data, which will result on the
  #sentiment value to become either 0 or 1.
  #1 for positive and 0 for negative
  scaled_artist_sentiment_analysis_through_words$sentiment = 
    factor(scaled_artist_sentiment_analysis_through_words$sentiment, 
           levels = c("positive", "negative"),
           labels = c("1", "0"))
  
  return(scaled_artist_sentiment_analysis_through_words)
}