#' Calculate prevalence rates
#'
#' @param linked_data Dataset containing relevant diagnostic and demographic information
#' @param id_col Name (character) of the ID column in the data set (unique personal identifier). Default is "id".
#' @param pop_data Dataset containing relevant population information.
#' @param pop_col Name (character) of the column containing population counts in the population dataset.
#' @param time_period ?? Optional
#' @param grouping_vars Optional character vector including grouping variables for the aggregation of diagnostic counts (eg. sex, education).
#' @param only_counts Return only diagnostic count, instead of prevalence rates. Default is set to FALSE.
#' @return Prevalence rate table
#' @importFrom rlang sym
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n_distinct
#' @importFrom dplyr n
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#'
#' @export
#'

calculate_prev <- function(linked_data,
                           id_col = "id",
                           pop_data,
                           pop_col = "pop_count",
                           time_period = NULL,
                           grouping_vars = NULL,
                           only_counts = FALSE){

  ##Input validation ####
  stopifnot("Requires linked and population dataset"= !is.null(linked_data), !is.null(pop_data))

  if(!all(grouping_vars %in% names(linked_data))) {
    stop("Your data must contain the specified 'grouping variables'.")
  }

  if(!id_col %in% names(linked_data)) {
    stop("Your data must contain the specified 'id' column.")
  }

  ###Filter for time period? Or how to establish that it is period prevalence instead of point prevalence? ####


  ##Group by specified grouping variables ####
  if (!is.null(grouping_vars)) {
    data_grouped <- linked_data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(grouping_vars)))
  } else {
    data_grouped <- linked_data
  }

  ##Calculate rates ####
  id_col_sym <- rlang::sym(id_col)
  count_data <- data_grouped |>
    dplyr::summarise(unique_id = dplyr::n_distinct(!!id_col_sym),
                     total_events = dplyr::n(), .groups = 'drop')

  if (only_counts){
    return(count_data)
  }

  ##Join with population and calculate rates ####
  prevalence <- count_data |>
      dplyr::left_join(pop_data, by = grouping_vars) |>
      dplyr::mutate(prev_rate = unique_id/.data[[pop_col]])
}
