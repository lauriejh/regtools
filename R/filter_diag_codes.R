#' Validate and filter diagnostic data by selected ICD-10 codes
#'
#' @param data Data frame containing pre-processed diagnostic data (check minimum requirements in documentation)
#' @param codes Character vector including ICD-10 codes to validate and filter diagnostic data
#' @param id_col Name of ID column in data set, default is "id"
#' @param code_col String containing the name of column containing the diagnostic codes
#' @param log_path File path of the log file to be used
#'
#'
#' @return Filtered diagnostic data frame containing only relevant observations based on diagnostic codes of interest.
#'
#' @export
#'

filter_diag <- function(data, codes, id_col = "id", code_col = "icd_code", log_path = NULL){

  ##### Set up logging #####
  log_threshold(DEBUG)
  log_formatter(formatter_glue)

  if (is.null(log_path) || !file.exists(log_path)){
    if(!dir.exists("log")){
      dir.create("log")
    }
    formatted_date <- format(Sys.Date(), "%d_%m_%Y")
    log_appender(appender_file(glue::glue("log/filter_diag_{formatted_date}.log")))
    log_info("Log file does not exist in specified path: {log_path}. Created file in log directory")
    message("Log file does not exist in specified path. Creating .log file in log directory")
    cat("\n")
  } else {
    log_appender(appender_file(log_path))
  }

  ##### Columns exist #####

  if(!code_col %in% colnames(data)){
    log_error("The specified code column does not exist in the dataset")
    stop(glue::glue("The specified code column does not exist in the dataset"))
  }

  if(!id_col %in% colnames(data)){
    log_error("The specified id column does not exist in the dataset")
    stop(glue::glue("The specified id column does not exist in the dataset"))
  }


  #### Check if desired code exists in ICD-10 ####
  load("data/npr.rda")

  message("Checking that code exists in ICD-10 code list...")

  codes_found <- purrr::map_lgl(codes, function(code) {
    any(purrr::map_lgl(npr, ~ code %in% .x))
  })

  if (!(all(codes_found))) {
    missing_codes <- codes[!codes_found]
    log_error("{paste(missing_codes, collapse = ', ')} code(s) not valid")
    stop(paste(paste(missing_codes, collapse = ", "), "code(s) not valid"))
    } else {
    cat(crayon::green("Selected ICD-10 codes are valid \u2713\n"))
    log_info("Selected ICD-10 codes ({paste(codes, collapse = ', ')}) are valid")
    cat("\n")
  }

  ####Check if desired code exists in data set and filter####
  message("Filtering data by selected ICD-10 codes...")
  cat("\n")
  if (!(all(codes %in% data[[code_col]]))){
    cat(crayon::yellow(glue::glue("Warning: The following codes are not found in the dataset: {paste(codes[!codes %in% data[[code_col]]], collapse = ', ')}")))
    log_warn("The following codes are not found in the dataset: {paste(codes[!codes %in% data[[code_col]]], collapse = ', ')}")

    filtered_data <- data |>
      dplyr::filter(.data[[code_col]] %in% codes)
  } else{
    filtered_data <- data |>
      dplyr::filter(.data[[code_col]] %in% codes)
  }
  cat("\n")
  cat(crayon::green("Diagnostic dataset succesfully filtered\n"))
  return(filtered_data)
  #give some information to the user that can be useful: number of ids (rows), year/date span, what do the codes refer to?
}
