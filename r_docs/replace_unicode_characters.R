replace_unicode_chars <- function(dataset) {
  
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