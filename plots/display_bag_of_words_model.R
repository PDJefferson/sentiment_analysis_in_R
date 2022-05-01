create_table_for_tf_idf_model <- function(tf_idf_dataset) {
  
  
  data_temp =  tf_idf_dataset[1:21,1:20]
  
  data_temp %>%
    gt() %>%
    tab_header(
      title = md("**tf idf model**"),
      subtitle = md("table shows the first 21 songs with a few selected words")
    )
}