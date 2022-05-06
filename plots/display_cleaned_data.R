create_table_to_display_clean_dataset <- function(dataset) {
  
  data_temp =  dataset %>%
    sample_n(4) 
  
  data_temp %>%
    gt() %>%
    tab_header(
      title = md("**Clean dataset**"),
      subtitle = md("table shows a few lyrics after having been cleaned")
    )
}
