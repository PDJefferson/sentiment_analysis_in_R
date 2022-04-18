remove_redundancies_and_bad_Words <- function(dataset) {
    names_removal <- c("beyonc", "beyonc�","yonc" ,"jhen", "ros")
    
    #replace unicode characters with its ascii equivalent
    dataset <- dataset %>%
      mutate(artist= chr_unserialise_unicode(artist)
             ,title = chr_unserialise_unicode(title)
             ,lyric = chr_unserialise_unicode(lyric)
             ,album = chr_unserialise_unicode(album)) %>%
      mutate(lyric = gsub("\uFFFD", "" , lyric, fixed =TRUE)) %>%
      mutate(lyric = str_remove(lyric, "<U+FFFD>"))
    
    #remove songs with few lyrics
    few_lyrics_remover = ifelse(nchar(dataset$lyric) < 350, TRUE, FALSE)
    dataset = dataset[!few_lyrics_remover,]
    
    # function to expand contractions in an English-language source
    fix.contractions <- function(doc) {
      # "won't" is a special case as it does not expand to "wo not"
      doc <- gsub("won't", "will not", doc)
      doc <- gsub("can't", "can not", doc)
      doc <- gsub("n't", " not", doc)
      doc <- gsub("'ll", " will", doc)
      doc <- gsub("'re", " are", doc)
      doc <- gsub("'ve", " have", doc)
      doc <- gsub("'m", " am", doc)
      doc <- gsub("'d", " would", doc)
      doc <- gsub("i'd", "i would", doc)
      doc <- gsub("id", "i would", doc)
      doc <- gsub("in'", "ing", doc)
      doc <- gsub("ive", "i have", doc)
      doc <- gsub("im" , "i am", doc)
      # 's could be 'is' or could be possessive: it has no expansion
      doc <- gsub("'s", "", doc)
      doc <- gsub("'cause", " because", doc)
      return(doc)
    }
    
    # fix (expand) contractions
    dataset$lyric <- sapply(dataset$lyric, fix.contractions)
    
    #converts words to lowercase,lemmatize words
    #replaces extra contractions, removes numbers and punctuation
    dataset <- dataset %>%
      mutate(lyric = replace_contraction(lyric)) %>%
      mutate(lyric = removePunctuation(lyric)) %>%
      mutate(lyric = removeNumbers(lyric)) %>%
      mutate(lyric = lemmatize_strings(lyric))
      
    #traverses the data and extracts every profanity word encountered in 
    #each row\song
    dataset <- dataset %>%
      mutate(lyric = extract_profanity_terms(get_sentences(lyric), 
                                          profanity_list = swears$swear_words))
    #converting list to vector
    swear_words_list <- unlist(dataset$lyric$profanity, 
                               recursive = TRUE, 
                               use.names = TRUE)
    #removing duplicates of same word
    swear_words_list <- paste(unique(swear_words_list))
    
    #finally removes the swear words that have in common with the list
    dataset <- dataset %>%
      mutate(lyric = removeWords(lyric$sentence, swear_words_list))
   
    #remove additional stop words
    dataset <- dataset %>%
      mutate(lyric = removeWords(lyric, additional_stopwords))
    
    #removes names
    #adds a | to separate each word as gsub uses that to recognize each word
    #separately.
    additional_words <- paste(names_removal, collapse = "|")
    dataset <- dataset %>%
      mutate(lyric = sub("^\\s+", "", gsub(additional_words, 
                                           "", 
                                           lyric, 
                                           ignore.case=TRUE)))
  
  return(dataset)
}