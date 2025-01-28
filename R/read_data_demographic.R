
#' Read and validate the structure of demographic data
#'
#' @description
#'`read_demo_data()` validates the general structure and minimum column requirements for demographic data.
#' The input data sets must be CSV, RDS, RDA or .SAV files.
#' @param file_path File path of the demographic data to be read. Supports CSV, RDS, RDA and .SAV files.
#' @param data_type Demographic data can either be of type "t_variant" or "t_invariant", necessary to check correct data structure characteristics.
#' @param id_col Name of ID column in data set, default is "id"
#' @param date_col Name of date column in data set, default is "date"
#' @param log_path File path of the log file to be used .
#' @param ... Optional extra parameters for specifying correct reading of CSV and .SAV files
#'
#' @return A data frame with the validated minimum requirements for demographic data
#' @export
#' @import logger
#'

read_demo_data <- function(file_path, data_type, id_col = "id", date_col = "date", log_path = NULL, ...) {

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
    message("Log file does not exist in specified path. Creating .log file in log directory")
    cat("\n")
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
  message("Date \u2713")
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
    message("No duplicate IDs \u2713")
    cat("\n")
  }

  ###### Summary data #####

  log_with_separator(glue::glue("Demographic dataset '{file_path}' succesfully read and columns validated"))
  cat(crayon::green$bold("Demographic dataset succesfully read and columns validated\n"))
  cat("\n")
  cat(crayon::green(glue::glue("Data Summary: \n Number of rows: {nrow(data)}. Number of columns: {ncol(data)}\n")))
  cat("\n")
  cat("\n")
  cat(utils::str(data))
  log_info("Data Summary: ")
  log_info("Number of rows: {nrow(data)}")
  log_info("Numner of columns: {ncol(data)}")
  log_formatter(formatter_pander)
  log_info(sapply(data, class))

  return(data)
}
