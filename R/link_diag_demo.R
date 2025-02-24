#' Link diagnostic and demographic datasets using unique personal identifiers
#'
#' @param data_diag Data frame containing pre-processed and pre-validated diagnostic data (check minimum requirements in documentations)
#' @param data_demo_inv Data frame containing validated time-invariant demographic data (check minimum requirements in documentations)
#' @param data_demo_var Data frame containing validated time-variant demographic data (check minimum requirements in documentations)
#' @param id_col Name (character) of the ID column in the data set (unique personal identifier), default is "id"
#' @param date_col Name (character) of the date column in the data set, in case of using time-variant data.
#' @param log_path File path of the log file to be used
#' @return Linked dataset including relevant diagnostic and demographic characteristics.
#' @export
#' @import logger
#'
link_diag_demo <- function(data_diag, data_demo_inv = NULL, data_demo_var = NULL, id_col= "id", date_col = "year", log_path = NULL){

  ##### Set up logging #####
  log_threshold(DEBUG)
  log_formatter(formatter_glue)

  if (is.null(log_path) || !file.exists(log_path)){
    if(!dir.exists("log")){
      dir.create("log")
    }
    formatted_date <- format(Sys.Date(), "%d_%m_%Y")
    log_appender(appender_file(glue::glue("log/link_diag_demo_{formatted_date}.log")))
    log_info("Log file does not exist in specified path: {log_path}. Created file in log directory")
    cli::cli_alert_warning("Log file does not exist in specified path. Creating .log file in log directory")
    cat("\n")
  } else {
    log_appender(appender_file(log_path))
  }


  ###Input validation#####
  # include diag validation s
  if(is.null(data_demo_inv) && is.null(data_demo_var)) {
    log_error("At least one of data_demo_inv or data_demo_var must be provided")
    stop("At least one of data_demo_inv or data_demo_var must be provided")
  }

  if(!is.null(data_demo_var) && !all(c(id_col,date_col) %in% names(data_demo_var))) {
    log_error("data_demo_var must contain the specified 'date' and 'id' columns")
    stop("data_demo_var must contain the specified 'date' and 'id' columns")
  }


  ###Main linking ####
  linked_df <- data_diag
  joined_datasets <- c(substitute(data_diag))

  if(!is.null(data_demo_inv)){
    if(!id_col %in% names(data_demo_inv)){
      log_error("{data_demo_inv} must contain specified 'id' column")
      stop(glue::glue("{data_demo_inv} must contain specified 'id' column"))
    }
    message("Joining diagnostic data with time-invariant demographic data...")
    linked_df <- linked_df |>
      dplyr::inner_join(data_demo_inv, by = id_col)

    joined_datasets <- c(joined_datasets, substitute(data_demo_inv))
    cli::cli_alert_success("Datasets succesfully linked: {paste(joined_datasets, collapse = ', ')}")
    log_info("Datasets succesfully linked: {paste(joined_datasets, collapse = ', ')}")
  }

  ##add check for 'date' column in last linked_df or directly from diag data

  if(!is.null(data_demo_var)){
    if(!id_col %in% names(data_demo_var)){
      log_error("{data_demo_var} must contain specified 'id' column")
      stop(glue::glue("{data_demo_var} must contain specified 'id' column"))
    }
    message("Joining with time-variant demographic data...")
    linked_df <- linked_df |>
      dplyr::inner_join(data_demo_var, by = c(id_col, date_col))

    joined_datasets <- c(joined_datasets, substitute(data_demo_var))
    cli::cli_alert_success("Datasets succesfully linked: {paste(joined_datasets, collapse = ', ')}")
    log_info("Datasets succesfully linked: {paste(joined_datasets, collapse = ', ')}")
  }

  linked_df <- linked_df |> janitor::clean_names() # consider removing it

  ###### Summary data #####
  cli::cli_h1("Data Summary")
  cli::cli_alert_info("After joining added {.val {ncol(linked_df)-ncol(data_diag)}} columns to '{substitute(data_diag)}': {setdiff(names(linked_df), names(data_diag))}")
  log_info("After joining added {ncol(linked_df)-ncol(data_diag)} columns to '{substitute(data_diag)}': {paste(setdiff(names(linked_df), names(data_diag)), collapse = ', ')}")

  cli::cli_alert_info("Rows in '{substitute(data_diag)}': {.val {nrow(data_diag)}}")
  log_info("Rows in '{substitute(data_diag)}': {nrow(data_diag)}")

  if(!is.null(data_demo_var)){
    cli::cli_alert_info("Rows in '{substitute(data_demo_var)}': {.val {nrow(data_demo_var)}}")
    log_info("Rows in '{substitute(data_demo_var)}': {nrow(data_demo_var)}")
    if(!is.null(data_demo_inv)){
      cli::cli_alert_info("Rows in '{substitute(data_demo_inv)}': {.val {nrow(data_demo_inv)}}")
      log_info("Rows in '{substitute(data_demo_inv)}': {nrow(data_demo_inv)}")
    }
  } else if (is.null(data_demo_var)){
    cli::cli_alert_info("Rows in '{substitute(data_demo_inv)}': {.val {nrow(data_demo_inv)}}")
    log_info("Rows in '{substitute(data_demo_inv)}': {nrow(data_demo_inv)}")
  }

  cli::cli_alert_success("Total matched rows: {.val {nrow(linked_df)}}")
  log_info("Total matched rows: {nrow(linked_df)}")

  log_formatter(formatter_pander)
  log_info(sapply(linked_df, class))

  return(linked_df)
}
