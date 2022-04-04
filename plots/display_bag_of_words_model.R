create_table_for_bag_of_words <- function(bag_of_words_dataset) {
  
  
  data_temp =  bag_of_words_dataset[1:21,6264:6294]
  
  data_temp %>%
    gt() %>%
    tab_header(
      title = md("**bag of words model**"),
      subtitle = md("table shows the first 21 lyrics with a few selected words")
    )
}