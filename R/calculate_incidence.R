#' Calculate incidence rates
#'
#' @description
#'The `calculate_incidence()` function calculates incidence rates based on the given diagnostic and demographic information.
#'Incidence represents the number of new cases of a given diagnosis that exist in a population of interest at a specified point or period in time.
#'
#' @param linked_data Dataset containing relevant diagnostic and demographic information
#' @param type Can either be cumulative or rate
#' @param id_col Name (character) of the ID column in the data set (unique personal identifier). Default is "id".
#' @param date_col Name (character) of the date column in the data set. Default is "date".
#' @param pop_data Dataset containing relevant population information.
#' @param pop_col Name (character) of the column containing population counts in the population dataset.
#' @param person_time_data  Dataset containing relevant person-time information.
#' @param person_time_col Name (character) of the column containing person-time counts in the person-time dataset.
#' @param time_p  Time period or time point. For time period, specify as a range. For time point, single numerical value. Useful to calculate either point or period prevalence.
#' @param grouping_vars Optional character vector including grouping variables for the aggregation of diagnostic counts (eg. sex, education).
#' @param only_counts Return only diagnostic count, instead of prevalence rates. Default is set to FALSE.
#' @param suppression Apply suppression to results (intermediate and rates) in order to maintain statistical confidentiality.
#' @param suppression_treshold Threshold for suppression, default is set to 5 (NPR standard).
#'
#' @returns Prevalence rate table
#' @export
#' @import logger
#'
calculate_incidence <- function(linked_data, # needs to be only first occurrence (first time diagnosis)
                                type,
                                id_col = "id",
                                date_col = "date",
                                pop_data = NULL,
                                pop_col = "pop_count", #population at risk
                                person_time_data = NULL,
                                person_time_col = NULL, #number used for denominator
                                time_p = NULL,
                                grouping_vars = NULL,
                                only_counts = FALSE,
                                suppression = TRUE,
                                suppression_treshold = 5){


  ## Input validation ####
  stopifnot("Requires linked dataset"= !is.null(linked_data))

  if(!all(grouping_vars %in% names(linked_data))) {
    stop("Your data must contain the specified 'grouping variables'.")
  }

  if(!id_col %in% names(linked_data)) {
    stop("Your data must contain the specified 'id' column.")
  }

  supported_types <- c("cumulative", "rate")
  stopifnot("Incidence type not supported. Please specify 'cumulative' for computing cumulative incidence, or 'rate' for incidence rate." = type %in% supported_types)


  ## Dataset should only contain new cases for correct computation of incidence statistics
  message("To correctly calculate incidence rates, the provided dataset should only contain new/first time diagnoses.")
  new_cases <- readline(prompt = "Have you verified that the provided dataset fulfills this requirement? (yes/no): ")
  if (tolower(new_cases) == "yes") {
    message("Computing incidence calculations...")
  } else {
    stop("The dataset should only contain first time/new diagnoses.")
  }

  ##Person-time need to be numeric
  if(type == "rate"){
    stopifnot("To compute incidence rates it is necessary to provide person-time value" = !is.null(person_time_data))
  }

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


  #For cumulative incidence:####
  #new cases in a period/ population at risk at start of the period (only diseased free population)

  #For incidence rate:####
  #number of new diagnoses/total person-time at risk (need to account for left-truncation and censoring etc...)

  if(type == "cumulative"){
    cumulative_incidence <- count_data |>
      dplyr::left_join(pop_data, by = grouping_vars) |>
      dplyr::mutate(cum_incidence = incidence_cases/.data[[pop_col]])
    return(cumulative_incidence)
  } else if (type == "rate"){
    incidence_rate <- count_data |>
      dplyr::left_join(person_time_data, by = grouping_vars) |>
      dplyr::mutate(incidence_rate = (incidence_cases/.data[[person_time_col]]) * 1000)
    return(incidence_rate)
  }


}
