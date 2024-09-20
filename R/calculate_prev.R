calculate_prev <- function(count_data, pop_data, joining_var, diag_count_col = "diag_count", pop_count_col = "pop_count", cumulative = TRUE){
  if(cumulative)
  incidence_data <- count_data |>
      left_join(pop_data, by = joining_var) |>
      mutate(prev_rate = diag_count_col/pop_count_col)
}
