#' Harmonize old municipality codes in Norway to 2024 municipality codes
#'
#' @param data Data containing old municipality codes
#' @param municipality_col Name of column containing original municipality codes
#' @param fylke Also want fylke name in 2024?
#'
#' @returns Harmonized codes in 2024
#' @export
#'
harmonize_municipality_codes <- function(data, municipality_col, fylke = TRUE){
  corresp_table <- readRDS("data/kommuner-1994-2024-processed.rds")

  data_harmonized <- data |>
    dplyr::left_join(corresp_table, by = setNames("original_code", municipality_col))

  data_harmonized <- data_harmonized |> dplyr::select(-harmonized_code_clean, -original_name, -start_year, -end_year)

  if (fylke == F){
    data_harmonized <- data_harmonized |> dplyr::select(-fylke_code, -fylke_name)
  }

  return(data_harmonized)
}
