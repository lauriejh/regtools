
#' Read and validate the structure of demographic data
#'
#' @description
#'`read_data_demographic()` validates the general structure and minimum column requirements for demographic data.
#' The input data sets must be CSV, RDS, RDA or .SAV files.
#' @param file_path File path of the demographic data to be read. Supports CSV, RDS, RDA and .SAV files.
#' @param data_type Demographic data can either be of type "t_variant" or "t_invariant", necessary to check correct data structure characteristics.
#' @param id_col Name of ID column in data set, default is "id"
#' @param date_col Name of date column in data set, default is "date"
#' @param ... Optional extra parameters for specifying correct reading of CSV and .SAV files
#'
#' @return A data frame with the validated minimum requirements for demographic data
#' @importFrom haven read_sav
#' @importFrom utils read.csv
#' @importFrom lubridate is.Date
#' @export
#'

read_demo_data <- function(file_path, data_type, id_col = "id", date_col = "date", ...) {

  if(missing(data_type)) {
    stop("Data type not specified")
  }

  file_extension <- tolower(tools::file_ext(file_path))
  supported_types <- c("csv", "rds", "rda", "sav")

  ###### Check file existence and type #####
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
                 stop("Unsupported file type."))

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
  Sys.sleep(1)
  message("ID \u2713")
  Sys.sleep(1)

  ###### Time variant: Check date column #####
  if (data_type == "t_variant"){
    message("Data type: time variant. Checking requirements...")
    date_column <- which(names(data) == date_col)
    if (length(date_column) == 0) {
      stop(paste0("The dataset must contain a column named: ", date_col))
    }
    if (!lubridate::is.Date(data[[date_column]]) && !is.numeric(data[[date_column]])) {
      stop("The 'date' column must be of type date or numeric")
    }
    Sys.sleep(1)
    message("Date \u2713")}
  Sys.sleep(1)

  ###### Time invariant: check ID duplicates #####
  if (data_type == "t_invariant"){
    message("Data type: time invariant Checking requirements...")
    if (any(duplicated(data[[id_column]]))) {
      stop("The dataset contains duplicate IDs. Verify that this dataset only containts persistent characteristics.")
    }
    Sys.sleep(1)
    message("No duplicate IDs \u2713")
  }

  return(data)
}
