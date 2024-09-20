#' Count diagnostic cases
#'
#' @param data Minimal tidy dataset containing both diagnostic and demographic information.
#' @param grouping_vars Character vector including grouping variables for the aggregation of diagnostic counts (eg. sex, education).
#' @param id_col Name (character) of the ID column in the data set (unique personal identifier). Default is "id".
#' @return Dataset containing the diagnostic counts with the specified grouping
#' @importFrom rlang sym
#' @importFrom dplyr group_by_at
#' @importFrom dplyr summarise
#' @importFrom dplyr n_distinct
#' @importFrom dplyr n
#'
#' @export
#'

count_diag <- function(data, grouping_vars = NULL, id_col = "id"){
  ##Input validation: grouping variables and id in provided dataset
  if(!all(grouping_vars %in% names(data))) {
    stop("Your data must contain the specified 'grouping variables'.")
  }

  if(!id_col %in% names(data)) {
    stop("Your data must contain the specified 'id' column.")
  }

  ##Count distinct id cases, grouped with given grouping_vars
  id_col_sym <- rlang::sym(id_col)
  count_data <- data |>
    dplyr::group_by_at(grouping_vars) |>
    dplyr::summarise(unique_id = dplyr::n_distinct(!!id_col_sym),
                     total_events = dplyr::n(), .groups = 'drop')

}
