remove_redundancies_and_bad_Words <- function(dataset) {
  
  #detect non-english songs to remove it
  non_english_songs_remover <- ifelse(detect_language(dataset$lyric) != "en", 
                                      TRUE, 
                                      FALSE)
 
  dataset = dataset[!non_english_songs_remover,]
  
  # function to expand contractions in an English-language source
  fix.contractions <- function(doc) {
    # "won't" is a special case as it does not expand to "wo not"
    doc <- gsub("won't", "will not", doc)
    doc <- gsub("can't", "can not", doc)
    doc <- gsub("n't", " not", doc)
    doc <- gsub("i'll", "i will", doc)
    doc <- gsub("i've", "i have", doc)
    doc <- gsub("i'm", "i am", doc)
    doc <- gsub("i'd", "i would", doc)
    doc <- gsub("in'", "ing", doc)
    doc <- gsub("woulda", "would have", doc)
      
    # 's could be 'is' or could be possessive: it has no expansion
    doc <- gsub("'s", "", doc)
    doc <- gsub("'cause", " because", doc)
    return(doc)
  }
  
  # fix (expand) contractions
  dataset$lyric <- sapply(dataset$lyric, fix.contractions)
  
  #remove songs with few lyrics
  few_lyrics_remover = ifelse(nchar(dataset$lyric) < 350, TRUE, FALSE)
  dataset = dataset[!few_lyrics_remover,]
  
  #converts words to lowercase,lemmatize words
  #replaces extra contractions, removes numbers and punctuations
  dataset <- dataset %>%
    mutate(lyric = sapply(lyric, tolower)) %>%
    mutate(lyric = replace_contraction(lyric)) %>%
    mutate(lyric = removePunctuation(lyric)) %>%
    mutate(lyric = removeNumbers(lyric)) %>%
    mutate(lyric = lemmatize_strings(lyric)) %>%
    mutate(lyric = sapply(lyric, tolower))
    
   
  #traverses the data and extracts every profanity word encountered in 
  #each row\song
  dataset <- dataset %>%
    mutate(lyric = extract_profanity_terms(get_sentences(lyric), 
                                          profanity_list = swears$swear_words))
  #saving the swear words into this vector
  swear_words_list <- unlist(dataset$lyric$profanity,
                             recursive = TRUE,
                             use.names = TRUE)
  
  #removes duplicates of same word
  swear_words_list <- paste(unique(swear_words_list))
  
  
  #passes all the stop words to this vector so that we can later remove it from
  #our data
  stopwords_list = paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_list = paste0('\\b', stopwords_list, '\\b')
  
  #removes stop words and additional white spaces
  dataset <- dataset %>%
    mutate(lyric = str_remove_all(lyric, stopwords_list)) %>%
    mutate(lyric = stripWhitespace(lyric))
    
  
  #removes the swear words that have in common with the list
  dataset <- dataset %>%
    mutate(lyric = removeWords(lyric$sentence, swear_words_list))
   
  #remove additional stop words
  dataset <- dataset %>%
    mutate(lyric = removeWords(lyric, additional_stopwords$stop_words))
  
  return(dataset)
}