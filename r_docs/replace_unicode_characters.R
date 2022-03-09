replace_unicode_chars <- function(dataset) {
  
  #remove songs with few lyrics
  few_lyrics_remover = ifelse(nchar(dataset$lyric) < 350, TRUE, FALSE)
  dataset = dataset[!few_lyrics_remover,]
  
  #replace unicode characters with empty characters
  dataset <- dataset %>%
    mutate(title = str_replace(string = title ,
                                pattern ="<U\\+\\w+>" ,
                                replacement =  "")
           ,lyric = str_replace(string = lyric, 
                                pattern = "<U\\+\\w+>" ,
                                replacement =  "")
           ,album = str_replace(string = album, 
                                pattern = "<U\\+\\w+>", 
                                replacement =  ""))
  #remove replacement character(�) that gets created after removing 
  #the unicode chars
  dataset <- dataset %>%
    mutate(title = str_replace(string = title ,
                               pattern ="\uFFFD" ,
                               replacement =  "")
           ,lyric = str_replace(string = lyric, 
                                pattern = "\uFFFD" ,
                                replacement =  "")
           ,album = str_replace(string = album, 
                                pattern = "\uFFFD", 
                                replacement =  ""))
  
  
}