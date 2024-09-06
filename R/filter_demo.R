#' Filter demographic data by selected filtering parameters
#'
#' @param data Data frame containing pre-processed demographic data
#' @param filter_param Named list containing filtering parameters. The names in the list are the column names and the values are vectors of values to keep.
#' @param rm_na Removes observations that have NA in the filtered columns.
#'
#' @return Filtered demographic dataframe containing only relevant observations based on the filtering parameters.
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @importFrom tidyselect all_of
#' @importFrom purrr reduce
#' @importFrom rlang sym
#' @export
#'

filter_demo <- function(data, filter_param, rm_na = TRUE){
  stopifnot("Specified variables do not exist in your dataset." = names(filter_param) %in% colnames(data))

  ####Keep only observations that comply with filtering parameters####
  filtered_data <- purrr::reduce(names(filter_param), function(df, col) {
    df |>  dplyr::filter(!!rlang::sym(col) %in% filter_param[[col]])
  }, .init = data)

  ####Check for NAs####
  if (rm_na == TRUE) {
    message("Removing observations containing NAs")
    cols <- names(filter_param)
    filtered_data_na <- filtered_data |>
      dplyr::filter(!dplyr::if_any(tidyselect::all_of(cols), ~ is.na(.)))
  }
  else{
    message("Not removing NAs")
    filtered_data_na <- filtered_data
    }
  return(filtered_data_na)
}
