#' Validate and filter diagnostic data by selected ICD-10 codes
#'
#' @param data Data frame containing pre-processed diagnostic data (check minimum requirements in documentation)
#' @param codes Character vector including ICD-10 codes to validate and filter diagnostic data
#' @param pattern_codes Character vector including general pattern of desired codes. For example: F84 will return all codes starting with F84.
#' @param id_col Name of ID column in data set, default is "id"
#' @param code_col String containing the name of column containing the diagnostic codes
#' @param log_path File path of the log file to be used
#'
#'
#' @return Filtered diagnostic data frame containing only relevant observations based on diagnostic codes of interest.
#'
#' @export
#'

filter_diag <- function(data, codes = NULL, pattern_codes = NULL, id_col = "id", code_col = "icd_code", log_path = NULL){

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
    cli::cli_alert_warning("Log file does not exist in specified path. Creating .log file in log directory")
    cat("\n")
  } else {
    log_appender(appender_file(log_path))
  }

  ##### Validate Input #####

  if(!code_col %in% colnames(data)){
    log_error("The specified code column does not exist in the dataset")
    cli::cli_abort("The specified code column does not exist in the dataset")
  }

  if(!id_col %in% colnames(data)){
    log_error("The specified id column does not exist in the dataset")
    cli::cli_abort("The specified id column does not exist in the dataset")
  }


  if(!is.null(pattern_codes) && !is.null(codes)){
    log_error("Only one of 'pattern_codes' or 'codes' should be specified.")
    cli::cli_abort("Only one of 'pattern_codes' or 'codes' should be specified.")
  }

  #### Check if desired code exists in ICD-10 ####

  message("Checking that code exists in ICD-10 code list...")

  if(!is.null(pattern_codes)){
    type_code <- "pattern"
    codes_found <- purrr::map_lgl(pattern_codes, function(code) {
      any(purrr::map_lgl(npr, ~ code %in% .x))
    })
  } else if (is.null(pattern_codes)){
    type_code <- "exact"
    codes_found <- purrr::map_lgl(codes, function(code){
      any(purrr::map_lgl(npr, ~ code %in% .x))
    })
  }


  if (!(all(codes_found))) {
    missing_codes <- switch(
      type_code,
      "pattern" = pattern_codes[!codes_found],
      "exact"   = codes[!codes_found])
    log_error("{paste(missing_codes, collapse = ', ')} code(s) not valid")
    stop(paste0(paste0(missing_codes, collapse = ", "), " code(s) not valid"))
    } else {
      valid_codes <- switch(
        type_code,
        "pattern" = pattern_codes,
        "exact"   = codes)
      cli::cli_alert_success("Selected ICD-10 codes are valid: {paste(valid_codes, collapse = ', ')}")
      log_info("Selected ICD-10 codes ({paste(valid_codes, collapse = ', ')}) are valid")
      cat("\n")
    }

  ####Check if desired code exists in data set and filter####
  message("Filtering data by selected ICD-10 codes...")
  if (!(all(codes %in% data[[code_col]]))){
    cli::cli_alert_warning("Warning: The following codes are not found in the dataset: {paste(codes[!codes %in% data[[code_col]]], collapse = ', ')}")
    log_warn("The following codes are not found in the dataset: {paste(codes[!codes %in% data[[code_col]]], collapse = ', ')}")

    if(!is.null(pattern_codes)){
      pattern <- paste0("^(", paste(pattern_codes, collapse = "|"), ")")
      filtered_data <- data |>
        dplyr::filter(stringr::str_detect(!!rlang::sym(code_col), pattern))
      } else if (is.null(pattern_codes)){
      filtered_data <- data |>
        dplyr::filter(.data[[code_col]] %in% codes)
      }

  } else{
    if(!is.null(pattern_codes)){
      pattern <- paste0("^(", paste(pattern_codes, collapse = "|"), ")")
      filtered_data <- data |>
        dplyr::filter(stringr::str_detect(!!rlang::sym(code_col), pattern))
    } else if (is.null(pattern_codes)){
      filtered_data <- data |>
        dplyr::filter(.data[[code_col]] %in% codes)
    }
  }

  ###### Summary data #####
  cli::cli_h1("")
  cat(crayon::green$bold("Diagnostic dataset succesfully filtered\n"))
  cat("\n")
  cli::cli_alert_info("Filtered {.val {nrow(data) - nrow(filtered_data)}} rows ({.strong {round((nrow(data) - nrow(filtered_data)) / nrow(data) * 100, 1)}%} removed)")
  cli::cli_h1("Data Summary")
  cli::cli_h3("After filtering:")
  cli::cli_alert_info("Remaining number of rows: {.val {nrow(filtered_data)}}")
  cli::cli_alert_info("Remaining number of columns: {.val {ncol(filtered_data)}}")
  cli::cli_alert_info("Unique IDs in dataset: {.val {dplyr::n_distinct(filtered_data[[id_col]])}}")
  cli::cli_alert_info("ICD-10 codes in dataset: {.pkg {unique(filtered_data$code, fromLast = T)}}")
  cat("\n")
  cat(utils::str(filtered_data))

  # Logs
  log_with_separator("Diagnostic dataset '{substitute(data)}' succesfully filtered")
  log_info("Remaining number of rows: {nrow(filtered_data)}")
  log_info("Remaining number of columns: {ncol(filtered_data)}")
  log_info("Unique IDs in dataset: {dplyr::n_distinct(filtered_data[[id_col]])}")
  log_info("ICD-10 codes in dataset: {paste(unique(filtered_data$code, fromLast = T), collapse = ', ')}")
  log_formatter(formatter_pander)
  log_info(sapply(filtered_data, class))

  return(filtered_data)
}
