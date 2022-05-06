create_graph_to_display_frequency_of_sentiments <- function(dataset) {
  
  #separating each word from a single row/lyric while
  #keeping track of the lyric
  tokenized_dataset <- dataset %>%
    group_by(artist) %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, lyric)
    
  #we get the words that have in common with bing and use the line number to 
  #to keep track of the song. we separate the sentiment positive and negative by
  #pivot wider and finally we get the net sentiment of the lyric
  prepare_tokenized_dataset <- tokenized_dataset %>%
    inner_join(get_sentiments("bing")) %>%
    count(artist, song_number = linenumber %/% 4, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  ggplot(prepare_tokenized_dataset, aes(song_number, sentiment, fill= artist)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~artist, ncol = 4, scales= "free_x")
  
}