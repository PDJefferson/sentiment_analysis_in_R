clean_data_vars <- function(dataset) {
  
  #remove swear words from the dataset
  dataset <- dataset %>%
    mutate(lyric = extract_profanity_terms
            (lyric, profanity_list = swears$swear_words)) %>%
    mutate(lyric =  lyric$sentence)
 
  #remove additional stop words
  #adds a | to separate each word as trimws uses that to recognize each word
  #separately.
  additional_words <- paste(additional_stopwords, collapse = "|")
  dataset <- dataset %>%
    mutate(lyric = trimws(sub(additional_words, "" ,lyric)))
  
  
  #lemmatize words, which means reduce the word to its most natural form
  dataset <- dataset %>%
    mutate(lyric = lemmatize_strings(lyric))
  
  
  #remove songs with few lyrics
  few_lyrics_remover = ifelse(nchar(dataset$lyric) < 350, TRUE, FALSE)
  dataset = dataset[!few_lyrics_remover,]
  
  #replace unicode characters with its ascii equivalent
  dataset <- dataset %>%
    mutate(artist= chr_unserialise_unicode(artist)
           ,title = chr_unserialise_unicode(title)
           ,lyric = chr_unserialise_unicode(lyric)
           ,album = chr_unserialise_unicode(album))
  
  return(dataset)
  
}