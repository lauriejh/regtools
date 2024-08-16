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
#' @param ... Optional extra parameters for specifying correct reading of CSV and .SAV files
#'
#' @return A data frame with the validated minimum requirements for diagnostic data
#' @importFrom haven read_sav
#' @importFrom utils read.csv
#' @importFrom lubridate is.Date
#' @export
#'
#'

read_diag_data <- function(file_path, id_col = "id", date_col = "date", code_col = "code", ...) {
  file_extension <- tolower(tools::file_ext(file_path))
  supported_types <- c("csv", "rds", "rda", "sav")

  ###### Check file existance and type #####
  stopifnot("File does not exist in the specified path." = file.exists(file_path))

  stopifnot("File type not supported. Please provide a .csv, .rds, or .sav file." = file_extension %in% supported_types)


  ###### Read files #####
  cat("\n")
  message("Reading file...")
  data <- switch(file_extension,
                 csv = utils::read.csv(file_path, ...),
                 rds = readRDS(file_path),
                 rda = load(file_path),
                 sav = haven::read_sav(file_path, ...),
                 stop("Unsupported file type"))

  message("\u2713")
  cat("\n")
  message("Checking column requirements:")


  ###### Check columns id #####
  id_column <- which(names(data) == id_col)

  if (length(id_column) == 0) {
    stop(paste0("The dataset must contain a column named: ", id_col))
  }

  if (!is.character(data[[id_column]])) {
    stop("The 'ID' or 'id' column must be of type character.")
  }

  message("ID \u2713")


  ###### Check columns code #####
  code_column <- which(names(data) == code_col)
  if (length(code_column) == 0) {
    stop(paste0("The dataset must contain a column named: ", code_col))
  }
  if (!is.character(data[[code_column]])) {
    stop("The 'code' column must be of type character.")
  }

  message("Code \u2713")

  ###### Check date column #####
  date_column <- which(names(data) == date_col)
  if (length(date_column) == 0) {
    stop(paste0("The dataset must contain a column named: ", date_col))
  }
  if (!lubridate::is.Date(data[[date_column]]) && !is.numeric(data[[date_column]])) {
    stop("The 'date' column must be of type date")
  }

  message("Date \u2713")

  ###### Extra columns #####
  required_columns <- c(date_col, id_col, code_col)
  extra_columns <- setdiff(names(data), required_columns)

  if (length(extra_columns) > 0) {
    cat("The dataset contains additional columns: ", paste(extra_columns, collapse = ", "), "\n")
    remove_extra <- readline(prompt = "Do you want to remove these extra columns? (yes/no): ")

    if (tolower(remove_extra) == "yes") {
      data <- data[, required_columns, drop = FALSE]
      message("Extra columns removed.")
    } else {
      warning("The dataset contains extra columns that were not removed.")
    }
  }

  return(data)
}



