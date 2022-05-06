create_table_for_tf_idf_model <- function(tf_idf_dataset) {
  
  
  data_temp =  tf_idf_dataset[1:20,1:11]
  
  data_temp$rating = tf_idf_dataset[1:20, 1511]
  
  data_temp %>%
    gt() %>%
    tab_header(
      title = md("**tf idf model**"),
      subtitle = md("table shows the first 20 songs with a few selected words")
    )
}