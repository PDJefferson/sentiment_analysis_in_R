create_table_to_display_clean_dataset <- function(dataset) {
  
  data_temp =  data %>%
    sample_n(3) 
  
  data_temp %>%
    gt() %>%
    tab_header(
      title = md("**Clean dataset**"),
      subtitle = md("table shows the first 21 lyrics after having been clean")
    )
}
