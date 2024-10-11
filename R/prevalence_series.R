#' Calculate prevalence series rates
#'
#' @param linked_data Dataset containing relevant diagnostic and demographic information
#' @param time_points Time points.
#' @param id_col Name (character) of the ID column in the data set (unique personal identifier). Default is "id".
#' @param date_col Name (character) of the date column in the data set. Default is "date".
#' @param pop_data  Dataset containing relevant population information.
#' @param pop_col Name (character) of the column containing population counts in the population dataset.
#' @param grouping_vars Optional character vector including grouping variables for the aggregation of diagnostic counts (eg. sex, education).
#' @param only_counts Return only diagnostic count, instead of prevalence rates. Default is set to FALSE.
#' @param suppression Apply suppression to results (intermediate and rates) in order to maintain statistical confidentiality.
#' @param suppression_treshold Threshold for suppression, default is set to 5 (NPR standard).
#'
#' @return Prevalence rate rates for specified time points
#' @export
#'
prevalence_series <- function(linked_data,
                              time_points,
                              id_col = "id",
                              date_col = "date",
                              pop_data,
                              pop_col = "pop_count",
                              grouping_vars = NULL,
                              only_counts = FALSE,
                              suppression = TRUE,
                              suppression_treshold = 5){

  ### Input validation ####
  stopifnot("Requires linked and population dataset"= !is.null(linked_data), !is.null(pop_data))

  if(!all(grouping_vars %in% names(linked_data))) {
    stop("Your data must contain the specified 'grouping variables'.")
  }

  if(!id_col %in% names(linked_data)) {
    stop("Your data must contain the specified 'id' column.")
  }


### Cycle through specified time points
  prevalence_series_ls <- purrr::map(time_points, function(time_p) {
    regtools::calculate_prevalence(
      linked_data = linked_data,
      id_col = id_col,
      date_col = date_col,
      pop_data = pop_data,
      pop_col = pop_col,
      time_p = time_p,
      grouping_vars = grouping_vars,
      only_counts = only_counts,
      suppression = suppression,
      suppression_treshold = suppression_treshold
    )
  })

  prevalence_series_df <- dplyr::bind_rows(prevalence_series_ls)
}
