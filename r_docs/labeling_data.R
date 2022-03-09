################################################################################
#tokenizing and labeling the dataset
label_dataset <- function(dataset) {
  
  #breaking each word in the lyric and keeping track of which lyric
  #the word belongs
  tokenized_dataset <- dataset %>%
    mutate(linenumber = row_number()) %>%
    unnest_tokens(word, lyric)
  
  #comparing artists lyrics with bing sentiment and counting the
  #the sentiments (by filtering between positive and negative ) of 
  #each song by its respective word in their songs
  normalized_artist_sentiment_analysis_through_words <- tokenized_dataset %>%
    inner_join(get_sentiments("bing")) %>%
    ungroup()
  
  #encoding categorical data, which will result on the
  #sentiment value to become either 0 or 1.
  #1 for positive and 0 for negative
  normalized_artist_sentiment_analysis_through_words$sentiment = 
    factor(normalized_artist_sentiment_analysis_through_words$sentiment, 
           levels = c("positive", "negative"),
           labels = c(1, 0))
  
  #grouping each word by the linenumber and title to rate
  #the song based on the frequency
  #of the sentiments(if there are more positive(1) sentiments we give it a 1
  #and 0 otherwise
  labeled_dataset = normalized_artist_sentiment_analysis_through_words %>%
    group_by(title,linenumber) %>%
    transmute(rating = names(sort(summary(sentiment), 
                                  decreasing = TRUE)[1:1])) %>%
    count(rating,title, sort = FALSE) %>%
    ungroup()

  #joining the ratings with their respective lyrics using its title
  labeled_dataset <- dataset %>%
      inner_join(labeled_dataset[,1:3], by="title")
      
  #finally, to ensure there are no duplicates of lyrics in our data, we use
  #distinct to remove it.
  labeled_dataset = distinct(labeled_dataset, 
                             lyric, title, album,
                             .keep_all = TRUE)
    
  return(subset(labeled_dataset, select = -c(linenumber)))
}
