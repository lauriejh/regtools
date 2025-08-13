#' Calculate incidence rates
#'
#' @description
#'The `calculate_incidence()` function calculates incidence rates based on the given diagnostic and demographic information.
#'Incidence represents the number of new cases of a given diagnosis that exist in a population of interest at a specified point or period in time.
#'
#' @param linked_data A data frame containing linked relevant diagnostic and demographic information. Should include only first time diagnosis, see 'curate_diag'
#' @param type Character string. Valid options are "cumulative" or "rate".
#' * If "cumulative",
#' * If "rate",
#' @param id_col A character string. Name of ID (unique personal identifier) column in `linked_data`. Default is "id".
#' @param date_col A character string. Name  of the date column in `linked_data`. Default is "date".
#' @param pop_data A data frame containing corresponding population at risk information.
#' @param pop_col A character string. Name of the column containing population counts in `pop_data`.
#' @param person_time_data  A data frame containing corresponding person-time information.
#' @param person_time_col A character string. Name of the column containing person-time counts in `person_time_data`.
#' @param time_p  A numeric value or numeric vector. Time point or time period used to calculate the incidence.
#' * For time period, specify as a range. The first value of the vector is the period's lower bound, and the second element is the period's upper bound. Example:  `time_p = c(2010,2015)`
#' * For time point, single numeric value. Example: `time_p = 2010`
#' @param grouping_vars Character vector (optional). Grouping variables for the aggregation of diagnostic counts (e.g. sex, education).
#' @param only_counts Logical. Only want diagnostic counts? Default is `FALSE`.
#' * If `TRUE`, return only counts.
#' @param suppression Logical. Suppress results (counts and rates) in order to maintain statistical confidentiality? Default is `TRUE`.
#' * If `TRUE`, applies primary suppression (NA) to any value under the threshold defined by `suppression_threshold`
#' @param suppression_threshold Integer. Threshold used for suppression, default is set to 5 (NPR standard).
#' @param log_path A character string. Path to the log file to append function logs. Default is `NULL`.
#' * If `NULL`, a new directory `/log` and file is created in the current working directory.
#'
#' @returns Incidence table
#' @examples
#' log_file <- tempfile()
#' cat("Example log file", file = log_file)
#'
#' pop_df <- tibble::tibble(year = "2012-2013", population = 4500)
#' linked_df <- linked_df |> dplyr::rename("year"= "diag_year")
#'
#' incidence_df <- calculate_incidence(linked_df,
#'   type = "cumulative",
#'   id_col = "id",
#'   date_col = "year",
#'   pop_data = pop_df,
#'   pop_col = "population",
#'   time_p = c(2012,2013),
#'   only_counts = FALSE,
#'   suppression = TRUE,
#'   suppression_threshold = 10,
#'   log_path = log_file)
#'
#'
#' @export
#' @import logger
#'
calculate_incidence <- function(linked_data,
                                type = c("cumulative", "rate"),
                                id_col = "id",
                                date_col = "date",
                                pop_data = NULL,
                                pop_col = "pop_count",
                                person_time_data = NULL,
                                person_time_col = NULL,
                                time_p = NULL,
                                grouping_vars = NULL,
                                only_counts = FALSE,
                                suppression = TRUE,
                                suppression_threshold = 5,
                                log_path = NULL){






  ##### Set up logging #####
  log_threshold(DEBUG)
  log_formatter(formatter_glue)

  if (is.null(log_path) || !file.exists(log_path)){
    if(!dir.exists("log")){
      dir.create("log")
    }
    formatted_date <- format(Sys.Date(), "%d_%m_%Y")
    log_appender(appender_file(glue::glue("log/calculate_incidence_{formatted_date}.log")))
    log_info("Log file does not exist in specified path: {log_path}. Created file in log directory")
    cli::cli_alert_warning("Log file does not exist in specified path. Creating .log file in log directory")
    cat("\n")
  } else {
    log_appender(appender_file(log_path))
  }

  ## Input validation ####
  if(is.null(linked_data)){
    log_error("Requires linked dataset")
    cli::cli_abort("Requires linked dataset")
  }

  if(!all(grouping_vars %in% names(linked_data))) {
    log_error("The linked dataset must contain the specified 'grouping variables': {paste(grouping_vars, collapse = ', ')}")
    cli::cli_abort("The linked dataset must contain the specified 'grouping variables': {grouping_vars}")
  }

  if(!id_col %in% names(linked_data)) {
    log_error("The linked dataset must contain the specified 'id' column: {id_col}")
    cli::cli_abort("The linked dataset must contain the specified 'id' column: {id_col}")
  }

  supported_types <- c("cumulative", "rate")
  if(!type %in% supported_types){
    log_error("{type} not supported. Please specify 'cumulative' for computing cumulative incidence, or 'rate' for incidence rate.")
    cli::cli_abort("{type} not supported. Please specify 'cumulative' for computing cumulative incidence, or 'rate' for incidence rate.")
  }

  ## Dataset should only contain new cases for correct computation of incidence statistics
  cli::cli_alert_warning("To correctly calculate incidence rates, the provided dataset should only contain new/first time diagnoses.")

  ##Person-time need to be numeric
  if(type == "rate" && is.null(person_time_data)){
    log_error("To compute incidence rates it is necessary to provide person-time values")
    cli::cli_abort("To compute incidence rates it is necessary to provide person-time values")
  }

  if(type == "rate"){
    linked_data <- linked_data |>
      dplyr::filter(.data[[date_col]] == time_p)
  }

  ##### If cumulative type is specified, then require time_p. Otherwise consider that all the dates in the dataset are the period of interest ####

  if(type == "cumulative"){
    if(!is.null(time_p)){
      linked_data <- linked_data |>
        dplyr::filter(.data[[date_col]] >= time_p[1],
                      .data[[date_col]] <= time_p[2])
      } else {
        cli::cli_alert_warning("No time-period has been provided. Computations done in all of the available dates in the dataset.")
        log_warn("No time-period has been provided. Computations done in all of the available dates in the dataset.")
        linked_data <- linked_data
      }
  }

  #### Suppression helper function ####
  suppress_values <- function(data, columns, threshold) {
    data <- data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(columns), ~ ifelse(. <= threshold, NA, .)))
    n_removed <- data |> dplyr::filter(dplyr::if_any(columns, is.na)) |>
      nrow()
    cli::cli_alert_success("Suppressed counts using {.strong {suppression_threshold}} threshold")
    cli::cli_alert_info("Removed {.val {n_removed}} cells out of {nrow(data)}")
    log_info("Suppressed counts using {suppression_threshold} threshold Removed {n_removed} cells out of {nrow(data)}")
    return(data)
  }

  ##Group by specified grouping variables ####
  if (!is.null(grouping_vars)) {
    data_grouped <- linked_data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(grouping_vars)))
  } else {
    data_grouped <- linked_data
  }

  ##Calculate counts ####
  id_col_sym <- rlang::sym(id_col)

  print(dim(data_grouped))

  count_data <- data_grouped |>
    dplyr::summarise(year = paste(as.character(time_p), collapse = '-'),
                     incidence_cases = dplyr::n_distinct(!!id_col_sym),
                     .groups = 'drop')

  print(dim(count_data))

  ## Suppression ####
  if (suppression){
    count_data_suppressed <- suppress_values(data = count_data, columns = c("incidence_cases"), threshold = suppression_threshold)
  } else {
    count_data_suppressed <- count_data
    cli::cli_alert_warning("No suppression. Confidentiality cannot be assured.")
    log_warn("No suppression. Confidentiality cannot be assured.")
  }

  ## Intermediate results: only diagnostic counts ####
  if (only_counts){
    return(count_data_suppressed)
  }



  # Check mapping, in case some missing data in pop

  check_mapping <- function(df1, df2, by_cols){
    missing_in_df2 <- dplyr::anti_join(df1, df2, by =  by_cols)
    if(nrow(missing_in_df2) > 0) {
      cat("\n")
      cli::cli_alert_warning("Warning: there are {nrow(missing_in_df2)} cells missing from {substitute(df2)}. Join with population dataset doesn't have a 'one-to-one' relationship")
      log_warn("here are {nrow(missing_in_df2)} cells missing from {substitute(df2)}")}
  }


  #For cumulative incidence:####
  #new cases in a period/ population at risk at start of the period (only disease free population)

  #For incidence rate:####
  #number of new diagnoses/total person-time at risk (need to account for left-truncation and censoring etc...)

  if(type == "cumulative"){
    check_mapping(count_data_suppressed, pop_data, by_cols = c(grouping_vars, date_col))
    incidence <- count_data_suppressed |>
      dplyr::left_join(pop_data, by = grouping_vars) |>
      dplyr::mutate(cum_incidence = incidence_cases/.data[[pop_col]])
    cat("\n")
    cli::cli_alert_success(crayon::green("Cumulative incidence ready"))
    log_info("Cumulative incidence ready")
    return(incidence)
  } else if (type == "rate"){
    check_mapping(count_data_suppressed, person_time_data, by_cols = c(grouping_vars, date_col))
    incidence <- count_data_suppressed |>
      dplyr::left_join(person_time_data, by = grouping_vars) |>
      dplyr::mutate(incidence_rate = incidence_cases/.data[[person_time_col]])
    cat("\n")
    cli::cli_alert_success(crayon::green("Incidence rates ready"))
    log_info("Incidence rates ready")
    return(incidence)
  }

  ###### Summary #####
  cli::cli_h1("Summary")
  cli::cli_alert_info("Diagnostic and demographic data: {.pkg {substitute(linked_data)}}")
  cli::cli_alert_info("Population data: {.pkg {substitute(pop_data)}}")
  cli::cli_alert_info("Grouped by variables: {.pkg {grouping_vars}}")
  cli::cli_alert_info("For time point/period:  {.val {time_p}}")


  # Logs
  log_with_separator("Summary")
  log_info("Diagnostic and demographic data: {substitute(linked_data)}")
  log_info("Population data: {substitute(pop_data)}")
  log_info("Grouped by variables: {paste(grouping_vars, collapse = ', ')}")
  log_info("For time point/period: {time_p}")

}
