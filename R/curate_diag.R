#' Curate diagnostic data
#'
#' @param data Data frame containing pre-processed and validated diagnostic data (check minimum requirements in documentation)
#' @param min_diag Numerical value, minimum amount of diagnostic events
#' @param first_diag Option to summarize information keeping only the first recorded diagnostic event. Default set to TRUE.
#' @param id_col Character string containing the name of the ID column in data set, default is "id"
#' @param code_col Character string containing the name of the code column in data set, default is "icd_code"
#' @param date_col Character string containing the name of date column in data set, default is "date"
#'
#' @return Curated diagnostic data: minimum diagnostic events, and first ever diagnosis information
#' @importFrom dplyr group_by_at
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom rlang sym
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr first
#' @importFrom dplyr across
#'
#' @export
#'
#'
curate_diag <- function(data, min_diag = 1, first_diag = TRUE, id_col = "id", code_col = "icd_code", date_col = "date"){
  stopifnot("The specified id column does not exist in the dataset" = id_col %in% colnames(data))
  stopifnot("The specified code column does not exist in the dataset" = code_col %in% colnames(data))
  stopifnot("The specified date column does not exist in the dataset" = date_col %in% colnames(data))

  ####Keep only observations that comply with minimum diagnostic events####
  filtered_data_min <- data |>
    dplyr::group_by_at(c(id_col, code_col)) |> #could include option for time grouping variable, certain amount of cases in the same year or irrelevant of year?
    dplyr::filter(dplyr::n() >= min_diag)

  ####Summarize first diagnostic information####
  if (first_diag){
    message("Summarizing first diagnostic event information...")
    id_col_sym <- rlang::sym(id_col)
    code_col_sym <- rlang::sym(code_col)
    date_col_sym <- rlang::sym(date_col)
    filtered_data_min <- filtered_data_min |>
      dplyr::arrange(!!id_col_sym, !!date_col_sym) |>
      dplyr::group_by(!!id_col_sym) |>
      dplyr::summarise(code = dplyr::first(!!code_col_sym),
                     y_diagnosis_first = min(!!date_col_sym),
                     diagnosis_count = dplyr::n(),
                     dplyr::across(!c(!!code_col_sym, !!date_col_sym), first),
                     .groups = 'drop')
  }
  return(filtered_data_min)
}
