#' Validate and filter diagnostic data
#'
#' @param data Data frame containing pre-processed diagnostic data (check minimum requirements in documentation)
#' @param codes Character vector including ICD-10 codes to validate and filter diagnostic data
#' @param code_col String containing the name of column containing the diagnostic codes
#' @param min_diag Numerical value, minimum amount of diagnostic events
#'
#' @return Filtered diagnostic data frame containing only relevant observations based on diagnostic codes of interest.
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr group_by_at
#' @importFrom dplyr summarise
#'
#' @export
#'

filter_diag <- function(data, codes, id_col = "id", code_col = "icd_code", min_diag){

  stopifnot("The specified code column does not exist in the dataset" = code_col %in% colnames(data))

  ####Check if desired code exists in ICD-10####
  load("data/npr.rda")

  message(paste0("Checking that code exists in file: "))
  codes_found <- purrr::map_lgl(codes, function(code) {
    any(purrr::map_lgl(npr, ~ code %in% .x))
  })

  if (!(all(codes_found))) {
    missing_codes <- codes[!codes_found]
    stop(paste(paste(missing_codes, collapse = ", "), "codes not valid"))
  } else {
    message("Selected ICD-10 codes are valid \u2713")
    cat("\n")
  }

  ####Check if desired code exists in data set and filter####
  message("Filtering data by selected ICD-10 codes...")
  if (!(all(codes %in% data[[code_col]]))){
    stop("Not all your codes were found in the dataset")
  } else{
    filtered_data <- data |>
      dplyr::filter(.data[[code_col]] %in% codes)
  }
  message("\u2713")

  ####Keep only observations that comply with minimum diagnostic####
  message("Filtering data by specified minimum diagnostic events...")
  filtered_data_min <- filtered_data |>
    dplyr::group_by_at(c(id_col, code_col)) |> #could include option for time grouping variable, certain amount of cases in the same year or irrelevant of year?
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::filter(count >=min_diag)

  message("\u2713")
  return(filtered_data_min)
  #give some information to the user that can be useful: number of ids (rows), year/date span, what do the codes refer to?
}
