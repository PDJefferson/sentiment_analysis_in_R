create_table_for_vector_technique <- function(dataset) {
  
  
  data_temp =  dataset[1:20,1:11]
  
  data_temp$rating = dataset[1:20, ncol(dataset)]
  
  data_temp %>%
    gt() %>%
    tab_header(
      title = md("**Vectorization technique**"),
      subtitle = md("table shows the first 20 songs with a few selected words")
    )
}