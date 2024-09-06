#' Filter demographic data by selected filtering parameters
#'
#' @param data Data frame containing pre-processed demographic data
#' @param filter_param Named list containing filtering parameters. The names in the list are the column names and the values are vectors of values to keep.
#'
#' @return Filtered demographic dataframe containing only relevant observations based on the filtering parameters.
#' @importFrom purrr reduce
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @export
#'

filter_demo <- function(data, filter_param){
  stopifnot("Specified variables do not exist in your dataset." = names(filter_param) %in% colnames(data))

  ####Keep only observations that comply with filtering parameters####
  filtered_data <- purrr::reduce(names(filter_param), function(data, col) {
    data |>  dplyr::filter(!!rlang::sym(col) %in% filter_param[[col]])
    }, .init = data)

  ####need to give a summary of the things that changed, n cases and other####
}
