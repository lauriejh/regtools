calculate_incidence <- function(count_data,
                                pop_data,
                                joining_var,
                                diag_count_col = "diag_count",
                                pop_count_col = "pop_count"){
  #Necessary that it is only the first occurrence for incidence rates (new cases)

  #For cumulative incidence:
  #new cases in a period/ population at risk at start of the period (only diseased free population)

  #For incidence rate:
  #number of new diagnoses/total person-time at risk (need to account for left-truncation and censoring etc...)
   incidence_data <- count_data |>
    dplyr::left_join(pop_data, by = joining_var) |>
    dplyr::mutate(prev_rate = diag_count_col/pop_count_col)
}
