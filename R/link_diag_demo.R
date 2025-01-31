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
  if(is.null(data_demo_inv) && is.null(data_demo_var)) {
    log_error("At least one of data_demo_inv or data_demo_var must be provided")
    stop("At least one of data_demo_inv or data_demo_var must be provided")
  }

  if(!is.null(data_demo_var) && !all(c(id_col,date_col) %in% names(data_demo_var))) {
    log_error("At least one of data_demo_inv or data_demo_var must be provided")
    stop("data_demo_var must contain the specified 'date' and 'id' columns")
  }


  ###Main linking ####
  if(!is.null(data_demo_inv)){
    if(!id_col %in% names(data_demo_inv)){
      log_error("{data_demo_inv} must contain specified 'id' column")
      stop(glue::glue("{data_demo_inv} must contain specified 'id' column"))
    }
    message("Joining diagnostic data with time-invariant demographic data...")
    linked_df <- data_diag |>
      dplyr::inner_join(data_demo_inv, by = id_col)
    cli::cli_alert_success("Join ready: diag and demo_inv")
  }

  ##add check for 'date' column in last linked_df or directly from diag data
  if(!is.null(data_demo_var)){
    if(!id_col %in% names(data_demo_inv)){
      log_error("{data_demo_var} must contain specified 'id' column")
      stop(glue::glue("{data_demo_var} must contain specified 'id' column"))
    }
    message("Joining with time-variant demographic data...")
    linked_df <- linked_df |>
      dplyr::inner_join(data_demo_var, by = c(id_col, date_col))
    cli::cli_alert_success("Join ready: diag and demo_inv")
  }

  linked_df <- linked_df |> janitor::clean_names()
  return(linked_df)
}
