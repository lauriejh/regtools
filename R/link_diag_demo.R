link_diag_demo <- function(data_diag, data_demo, id_col= "id"){
  linked_data <- data_diag |>
    dplyr::inner_join(data_demo, by = id_col)
  message("After joining diagnostic and demographic data: ")
  print(skimr::skim(linked_data))
  return(linked_data)
}
