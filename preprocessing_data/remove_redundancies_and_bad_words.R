remove_redundancies_and_swear_Words <- function(dataset) {
  
  #remove swear words from the dataset
  dataset <- dataset %>%
    mutate(lyric = extract_profanity_terms(get_sentences(lyric), 
                                        profanity_list = swears$swear_words))
  
  swear_words_list <- unlist(dataset$lyric$profanity, 
                             recursive = TRUE, 
                             use.names = TRUE)
  swear_words_list <- paste(unique(swear_words_list), collapse = "|")
  
  dataset <- dataset %>%
    mutate(lyric = trimws(sub(swear_words_list , "" ,lyric$sentence )))
  
  #remove additional stop words
  #adds a | to separate each word as trimws uses that to recognize each word
  #separately.
  additional_words <- paste(additional_stopwords, collapse = "|")
  dataset <- dataset %>%
    mutate(lyric = trimws(sub(additional_words, "" , lyric)))
  
  
  #remove songs with few lyrics
  few_lyrics_remover = ifelse(nchar(dataset$lyric) < 350, TRUE, FALSE)
  dataset = dataset[!few_lyrics_remover,]
  
  #replace unicode characters with its ascii equivalent
  dataset <- dataset %>%
    mutate(artist= chr_unserialise_unicode(artist)
           ,title = chr_unserialise_unicode(title)
           ,lyric = chr_unserialise_unicode(lyric)
           ,album = chr_unserialise_unicode(album)) %>%
    mutate(lyric = gsub("\uFFFD", "" , lyric, fixed =TRUE)) %>%
    mutate(lyric = str_remove(lyric, "<U+FFFD>"))
  
  return(dataset)
}