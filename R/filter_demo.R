#' Filter demographic data by selected filtering parameters
#'
#' @param data Data frame containing pre-processed demographic data
#' @param filter_param Named list containing filtering parameters. The names in the list are the column names and the values are vectors of values to keep.
#' @param rm_na Removes observations that have NA in the filtered columns.
#'
#' @return Filtered demographic dataframe containing only relevant observations based on the filtering parameters.
#' @importFrom dplyr filter
#' @importFrom purrr reduce
#' @importFrom rlang sym
#' @importFrom tidyr drop_na
#' @importFrom skimr skim
#'
#' @export
#'

filter_demo <- function(data, filter_param, rm_na = TRUE){
  stopifnot("Specified variables do not exist in your dataset." = names(filter_param) %in% colnames(data))

  ####Keep only observations that comply with filtering parameters####
  filtered_data <- purrr::reduce(names(filter_param), function(df, col) {
    df |>  dplyr::filter(!!rlang::sym(col) %in% filter_param[[col]])
  }, .init = data)
  message(paste0("Original dataset had "), nrow(data), " observations.")
  cat("\n")
  message(paste0("\u2022", nrow(filtered_data), " observations fullfilled the selected filtering parameters: "))
  Sys.sleep(2)
  print(data |> skim())

  ####Remove NAs####
  if (rm_na == TRUE) {
    message("Removing observations containing NAs...")
    n_missing_sum <- data |>
      summarize(across(everything(), ~ sum(is.na(.))))
    print(n_missing_sum)
    Sys.sleep(1)
    filtered_data <- filtered_data |>
      tidyr::drop_na()
    message("\u2022 After removing NAs, the dataset has ", nrow(filtered_data), " observations.")
  }
  return(filtered_data)
}
