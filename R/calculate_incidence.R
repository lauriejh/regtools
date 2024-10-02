calculate_incidence <- function(linked_data, # needs to be only first occurrence (first time diagnosis)
                                type,
                                id_col = "id",
                                date_col = "date",
                                pop_data,
                                pop_col = "pop_count", #population at risk
                                person_time = "person_time", #number used for denominator
                                time_p = NULL,
                                grouping_vars = NULL,
                                only_counts = FALSE,
                                suppression = TRUE,
                                suppression_treshold = 5){


  ## Input validation ####
  stopifnot("Requires linked and population dataset"= !is.null(linked_data), !is.null(pop_data))

  if(!all(grouping_vars %in% names(linked_data))) {
    stop("Your data must contain the specified 'grouping variables'.")
  }

  if(!id_col %in% names(linked_data)) {
    stop("Your data must contain the specified 'id' column.")
  }

  ## Dataset should only contain new cases for correct computation of incidence statistics
  message("To correctly calculate incidence rates, the provided dataset should only contain new/first time diagnoses.")
  new_cases <- readline(prompt = "Have you verified that the provided dataset fulfills this requirement? (yes/no): ")
  if (tolower(new_cases) == "yes") {
    message("Computing incidence calculations...")
  } else {
    stop("The dataset should only contain first time/new diagnoses.")
  }

  ##Person-time need to be numeric
  stopifnot("Person-time value needs to be a numeric value. " = is.numeric(person_time), length(person_time) == 1)


  ##### If cumulative type is specified, then require time_p. Otherwise consider that all the dates in the dataset are the period of interest ####

  if(type == "cumulative"){
    if(!is.null(time_p)){
      message("cumulative")
      linked_data <- linked_data |>
        dplyr::filter(.data[[date_col]] >= time_p[1],
                      .data[[date_col]] <= time_p[2])
      } else {
      message("No time-period has been provided. Computations done in all of the available dates in the dataset.")
      linked_data <- linked_data
      }
  }

  #### Suppression helper function ####

  suppress_values <- function(data, columns, threshold) {
    data <- data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(columns), ~ ifelse(. <= threshold, NA, .)))
  }

  ##Group by specified grouping variables ####
  if (!is.null(grouping_vars)) {
    data_grouped <- linked_data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(grouping_vars)))
  } else {
    data_grouped <- linked_data
  }

  ##Calculate counts ####
  id_col_sym <- rlang::sym(id_col)

  count_data <- data_grouped |>
    dplyr::summarise(incidence_cases = dplyr::n_distinct(!!id_col_sym),
                     .groups = 'drop')


  ## Suppression ####
  if (suppression){
    count_data_suppressed <- suppress_values(data = count_data, columns = c("incidence_cases"), threshold = suppression_treshold)
  } else {
    warning("No suppression. Confidentiality cannot be assured.")
    count_data_suppressed <- count_data
  }

  ## Intermediate results: only diagnostic counts ####
  if (only_counts){
    return(count_data_suppressed)
  }


  #For cumulative incidence:
  #new cases in a period/ population at risk at start of the period (only diseased free population)

  #For incidence rate:
  #number of new diagnoses/total person-time at risk (need to account for left-truncation and censoring etc...)
}
