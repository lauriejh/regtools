#' Read and validate the structure of diagnostic data
#'
#' @description
#'`read_data_diagnostic()` validates the general structure and minimum column requirements for diagnostic data.
#' The input data sets must be CSV, RDS, RDA or .SAV files.
#'
#' @param file_path File path of the diagnostic data to be read. Supports CSV, RDS, RDA and .SAV files.
#' @param id_col Name of ID column in data set, default is "id"
#' @param date_col Name of date column in data set, default is "date"
#' @param code_col Name of diagnostic codes column in data set, default is "code"
#' @param log_path File path of the log file
#' @param ... Optional extra parameters for specifying correct reading of CSV and .SAV files
#'
#' @return A data frame with the validated minimum requirements for diagnostic data
#' @export
#' @import logger
#'
#'

read_diag_data <- function(file_path, id_col = "id", date_col = "date", code_col = "code", log_path, ...) {
  file_extension <- tolower(tools::file_ext(file_path))
  supported_types <- c("csv", "rds", "rda", "sav")

  ##### Set up logging #####
  if (!file.exists(log_path)){
    if(!dir.exists("log")){
      dir.create("log")
    }
    formatted_date <- format(Sys.Date(), "%d_%m_%Y")
    log_appender(appender_file(glue::glue("log/read_diag_data_{formatted_date}.log")))
    log_info("Log file does not exist in specified path: {log_path}. Created file in new log directory")
    message("Log file does not exist in specified path. Creating .log file in new log directory")
  } else {
    log_appender(appender_file(log_path))
  }


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
  data <- switch(file_extension,
                 csv = utils::read.csv(file_path, ...),
                 rds = readRDS(file_path),
                 rda = load(file_path),
                 sav = haven::read_sav(file_path, ...),
                 stop("Unsupported file type"))

  message("\u2713")
  cat("\n")
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

  message("ID \u2713")
  log_info("ID column \u2713")


  ###### Check columns code #####
  code_column <- which(names(data) == code_col)
  if (length(code_column) == 0) {
    log_error("The dataset must contain a column named: {code_col}")
    stop(glue::glue("The dataset must contain a column named: {code_col}"))
  }
  if (!is.character(data[[code_column]])) {
    log_error("The {code_column} column must be of type character.")
    stop("The 'code' column must be of type character.")
  }

  message("Code \u2713")
  log_info("Code column \u2713")

  ###### Check date column #####
  date_column <- which(names(data) == date_col)
  if (length(date_column) == 0) {
    log_error("The dataset must contain a column named: {date_col}")
    stop(glue::glue("The dataset must contain a column named: {date_col}"))
  }
  if (!lubridate::is.Date(data[[date_column]]) && !is.numeric(data[[date_column]])) {
    log_error("The {date_column} must be of type date or numeric.")
    stop("The 'date' column must be of type date or numeric.")
  }

  message("Date \u2713")
  log_info("Date column \u2713")

  ###### Extra columns #####
  required_columns <- c(date_col, id_col, code_col)
  extra_columns <- setdiff(names(data), required_columns)

  if (length(extra_columns) > 0) {
    cat("The dataset contains additional columns: ", paste(extra_columns, collapse = ", "), "\n")
    log_warnings("The dataset contains additional columns")
    remove_extra <- readline(prompt = "Do you want to remove these extra columns? (yes/no): ")

    if (tolower(remove_extra) == "yes") {
      data <- data[, required_columns, drop = FALSE]
      message("Extra columns removed.")
      log_info("User decided to remove extra columns.")
    } else {
      warning("The dataset contains extra columns that were not removed.")
      log_warning("The dataset contains extra columns that were not removed by the user.")
    }
  }
  log_with_separator("Diagnostic dataset succesfully read and columns validated")
  return(data)
}



