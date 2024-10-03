#' Calculate prevalence rates
#'
#' @description
#'The `calculate_prevalence()` function calculates prevalence rates based on the given diagnostic and demographic information.
#'Prevalence represents the number of cases of a given diagnosis that exist in a population of interest at a specified point or period in time.
#'
#' @param linked_data Dataset containing relevant diagnostic and demographic information
#' @param id_col Name (character) of the ID column in the data set (unique personal identifier). Default is "id".
#' @param pop_data Dataset containing relevant population information.
#' @param pop_col Name (character) of the column containing population counts in the population dataset.
#' @param time_p Optional time period or time point. Useful to calculate either point or period prevalence.
#' @param grouping_vars Optional character vector including grouping variables for the aggregation of diagnostic counts (eg. sex, education).
#' @param only_counts Return only diagnostic count, instead of prevalence rates. Default is set to FALSE.
#' @param suppression Apply suppression to results (intermediate and rates) in order to maintain statistical confidentiality.
#' @param suppression_treshold Threshold for suppression, default is set to 5 (NPR standard).
#' @return Prevalence rate table
#'
#' @export
#'

calculate_prevalence <- function(linked_data,
                           id_col = "id",
                           date_col = "date",
                           pop_data,
                           pop_col = "pop_count",
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

  ##### If time_p is specified, filter data ####
  if (!is.null(time_p)) {
    linked_data <- linked_data |>
      dplyr::filter(.data[[date_col]] >= time_p[1],
             .data[[date_col]] <= time_p[2])
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
    dplyr::summarise(unique_id = dplyr::n_distinct(!!id_col_sym),
                     total_events = dplyr::n(), .groups = 'drop')

  ## Suppression ####
  if (suppression){
    count_data_suppressed <- suppress_values(data = count_data, columns = c("unique_id", "total_events"), threshold = suppression_treshold)
  } else {
    warning("No suppression. Confidentiality cannot be assured.")
    count_data_suppressed <- count_data
  }

  ## Intermediate results: only diagnostic counts ####
  if (only_counts){
    return(count_data_suppressed)
  }

  ##Join with population and calculate rates ####
  prevalence <- count_data_suppressed |>
      dplyr::left_join(pop_data, by = grouping_vars) |>
      dplyr::mutate(prev_rate = unique_id/.data[[pop_col]])
  return(prevalence)
}
