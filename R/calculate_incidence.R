calculate_incidence <- function(count_data, pop_data, joining_var, diag_count_col = "diag_count", pop_count_col = "pop_count"){
  incidence_data <- count_data |>
    dplyr::left_join(pop_data, by = joining_var) |>
    dplyr::mutate(prev_rate = diag_count_col/pop_count_col)
}
