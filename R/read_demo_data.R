
#' Read and validate the structure of demographic individual-level data
#'
#' @description
#'`read_demo_data()` validates the general structure and minimum column requirements for demographic individual-level data.
#' The input data sets must be CSV, RDS, RDA or .SAV files.
#' @param file_path A character string. File path to the demographic data to read. Supports CSV, RDS, RDA and .SAV files.
#' @param data_type A character string. Demographic data can either be of type "t_variant" or "t_invariant", necessary to check correct data structure characteristics.
#' @param id_col A character string. Name of ID column in data set. Default is "id".
#' @param date_col A character string. Name of date column in data set, default is "date".
#' @param log_path A character string. Path to the log file to append function logs. Default is `NULL`.
#' * If `NULL`, a new directory `/log` and file is created in the current working directory.
#' @param ... Additional arguments passed to methods or underlying functions.
#'
#' @return A data frame with the validated minimum requirements for demographic data.
#' @examples
#' # Read and validate CSV file for varying individual level demographic data
#' demo_csv <- system.file("extdata", "invar_data.csv", package = "regtools")
#' log_file <- tempfile()
#' cat("Example log file", file = log_file)
#'
#' demo_data_validated <- read_demo_data(demo_csv, data_type = "t_invariant",
#' id_col = "id", log_path = log_file)
#'
#' @export
#' @import logger
#'

read_demo_data <- function(file_path, data_type = c("t_variant", "t_invariant"), id_col = "id", date_col = "date", log_path = NULL, ...) {

  ##### Set up logging #####
  log_threshold(DEBUG)
  log_formatter(formatter_glue)

  if (is.null(log_path) || !file.exists(log_path)){
    if(!dir.exists("log")){
      dir.create("log")
    }
    formatted_date <- format(Sys.Date(), "%d_%m_%Y")
    log_appender(appender_file(glue::glue("log/read_demo_data_{formatted_date}.log")))
    log_info("Log file does not exist in specified path: {log_path}. Created file in log directory")
    cli::cli_alert_warning("Log file does not exist in specified path. Creating .log file in log directory")
  } else {
    log_appender(appender_file(log_path))
  }


  ##### Data type and extension #####

  if(missing(data_type)) {
    log_error("Data type not specified.")
    stop("Data type not specified.")
  }

  if(!data_type %in% c("t_variant", "t_invariant")){
    log_error("{data_type} not supported.")
    stop(glue::glue("{data_type} not supported."))
  }

  file_extension <- tolower(tools::file_ext(file_path))
  supported_types <- c("csv", "rds", "rda", "sav")

  ###### Check file existence and type #####
  if(!file.exists(file_path)){
    log_error("Diagnositc file does not exist in the specified path: {file_path}")
    stop("File does not exist in the specified path.")
  }


  if(!file_extension %in% supported_types){
    log_error("{file_extension}. File type not supported. Please provide a .csv, .rds, or .sav file.")
    stop("File type not supported. Please provide a .csv, .rds, or .sav file.")
  }

  ###### Read files #####
  cat("\n")
  message(glue::glue("Reading {file_path} file..."))
  data <- switch(file_extension,
                 csv = utils::read.csv(file_path, ...),
                 rds = readRDS(file_path),
                 rda = load(file_path),
                 sav = haven::read_sav(file_path, ...),
                 stop("Unsupported file type"))

  cli::cli_alert_success("Succesfully read file: {file_path}")
  log_info("Succesfully read file: {file_path}")

  ###### Check columns id #####
  message("Checking column requirements:")
  log_info("Checking column requirements:")

  id_column <- which(names(data) == id_col)

  if (length(id_column) == 0) {
    log_error("The dataset must contain a column named {id_col}")
    stop(glue::glue("The dataset must contain a column named {id_col}"))
  }

  if (!is.character(data[[id_column]])) {
    log_error("The {id_column} column must be of type character.")
    stop("The 'ID' or 'id' column must be of type character.")
  }

  cli::cli_alert_success("ID column")
  log_info("ID column \u2713")

  ###### Time variant: Check date column #####
  if (data_type == "t_variant"){
    log_info("Specified data type: t_variant")
    message("Data type: time variant. Checking requirements...")
    date_column <- which(names(data) == date_col)
    if (length(date_column) == 0) {
      log_error("The dataset must contain a column named: {date_col}")
      stop(glue::glue("The dataset must contain a column named: {date_col}"))
    }
    if (!lubridate::is.Date(data[[date_column]]) && !is.numeric(data[[date_column]])) {
      log_error("The 'date' column must be of type date or numeric")
      stop("The 'date' column must be of type date or numeric")
    }
  }
  cli::cli_alert_success("Date column")
  cat("\n")
  log_info("Date column \u2713")

  ###### Time invariant: check ID duplicates #####
  if (data_type == "t_invariant"){
    log_info("Specified data type: t_invariant")
    message("Data type: time invariant. Checking requirements...")
    if (any(duplicated(data[[id_column]]))) {
      stop("The dataset contains duplicate IDs. Verify that this dataset only containts persistent characteristics.")
      log_error("The dataset contains duplicate IDs. Verify that this dataset only containts persistent characteristics.")
    }
    log_info("No duplicate IDs \u2713")
    cli::cli_alert_success("No duplicate IDs")
    cat("\n")
  }

  ###### Summary data #####
  log_with_separator(glue::glue("Demographic dataset '{file_path}' succesfully read and columns validated"))
  cli::cli_h1("")
  cat(crayon::green$bold("Demographic dataset succesfully read and columns validated\n"))
  cli::cli_h1("Data Summary")
  cat("\n")
  cli::cli_alert_info("Number of rows: {.val {nrow(data)}}. Number of columns: {.val {ncol(data)}}.")
  cli::cli_alert_info("Unique IDs in dataset: {.val {dplyr::n_distinct(data[[id_col]])}}.")
  cat("\n")
  cat(utils::str(data))
  log_info("Data Summary: ")
  log_info("Number of rows: {nrow(data)}")
  log_info("Numner of columns: {ncol(data)}")
  log_formatter(formatter_pander)
  log_info(sapply(data, class))

  return(dplyr::as_tibble(data))
}
