#' Calculate prevalence rates
#'
#' @description
#'The `calculate_prevalence()` function calculates prevalence rates based on the given diagnostic and demographic information.
#'Prevalence represents the number of cases of a given diagnosis that exist in a population of interest at a specified point or period in time.
#'
#' @param linked_data Dataset containing relevant diagnostic and demographic information
#' @param id_col Name (character) of the ID column in the data set (unique personal identifier). Default is "id".
#' @param date_col Name (character) of the date column in the data set. Default is "date".
#' @param pop_data Dataset containing relevant population information.
#' @param pop_col Name (character) of the column containing population counts in the population dataset.
#' @param time_p  Time period or time point. For time period, specify as a range. For time point, single numerical value. Useful to calculate either point or period prevalence.
#' @param grouping_vars Optional character vector including grouping variables for the aggregation of diagnostic counts (eg. sex, education).
#' @param only_counts Return only diagnostic count, instead of prevalence rates. Default is set to FALSE.
#' @param suppression Apply suppression to results (intermediate and rates) in order to maintain statistical confidentiality.
#' @param suppression_treshold Threshold for suppression, default is set to 5 (NPR standard).
#' @param log_path File path of the log file to be used
#' @return Prevalence rate table
#'
#' @export
#' @import logger

calculate_prevalence <- function(linked_data,
                                 id_col = "id",
                                 date_col = "date",
                                 pop_data = NULL,
                                 pop_col = "pop_count",
                                 time_p,
                                 grouping_vars = NULL,
                                 only_counts = FALSE,
                                 suppression = TRUE,
                                 suppression_treshold = 5,
                                 log_path= NULL){



  ##### Set up logging #####
  log_threshold(DEBUG)
  log_formatter(formatter_glue)

  if (is.null(log_path) || !file.exists(log_path)){
    if(!dir.exists("log")){
      dir.create("log")
    }
    formatted_date <- format(Sys.Date(), "%d_%m_%Y")
    log_appender(appender_file(glue::glue("log/calculate_prevalence_{formatted_date}.log")))
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

  if(!date_col %in% names(linked_data) & !date_col %in% names(pop_data)){
    log_error("The population and linked data must include the same specified 'date' column: {date_col}")
    cli::cli_abort("The population and linked data must include the same specified 'date' column: {date_col}")
  }

  #### Suppression helper function ####
  suppress_values <- function(data, columns, threshold) {
    data <- data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(columns), ~ ifelse(. <= threshold, NA, .)))
    n_removed <- data |> dplyr::filter(dplyr::if_any(columns, is.na)) |>
      nrow()
    cli::cli_alert_success("Suppressed counts using {.strong {suppression_treshold}} treshold")
    cli::cli_alert_info("Removed {.val {n_removed}} cells out of {nrow(data)}")
    log_info("Suppressed counts using {suppression_treshold} treshold. Removed {n_removed} cells out of {nrow(data)}")
    return(data)
  }


  ##### Check for time-point or period and filter ####
  message("Computing prevalence rates/counts...")

  if (length(time_p) == 1){
    filtered_data <- linked_data |>
      dplyr::filter(.data[[date_col]] == time_p)
  } else if (length(time_p) ==2){
    filtered_data <- linked_data |>
      dplyr::filter(.data[[date_col]] >= time_p[1],
                    .data[[date_col]] <= time_p[2])
  } else {
    log_error("Time input should be either a single year or a vector of two years for a range")
    cli::cli_abort("Time input should be either a single year or a vector of two years for a range")
  }


  ## Group by specified grouping variables ####
  if (!is.null(grouping_vars)) {
    data_grouped <- filtered_data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(grouping_vars)))
  } else {
    data_grouped <- linked_data
  }

  ## Calculate counts ####
  id_col_sym <- rlang::sym(id_col)
  count_data <- data_grouped |>
    dplyr::summarise(year = paste(as.character(time_p), collapse = '-'),
                     unique_id = dplyr::n_distinct(!!id_col_sym),
                     total_events = dplyr::n(), .groups = 'drop')

  ## Suppression ####
  if (suppression){
    count_data_suppressed <- suppress_values(data = count_data, columns = c("unique_id", "total_events"), threshold = suppression_treshold)
  } else {
    count_data_suppressed <- count_data
    log_warn("No suppression. Confidentiality cannot be assured.")
    cli::cli_alert_warning("No suppression. Confidentiality cannot be assured.")
  }

  ## Intermediate results: only diagnostic counts ####
  if (only_counts){
    cat("\n")
    log_info("Prevalence counts ready")
    cli::cli_alert_success(crayon::green("Prevalence counts ready!"))
    return(count_data_suppressed)
  }

  ## Join with population and calculate rates ####

  # Check data type of date_col and transform for successful joining
  if(!is.character(pop_data[[date_col]])){
    pop_data[[date_col]] <- as.character(pop_data[[date_col]])
  }


  # Check mapping, in case some missing data in pop

  missing_in_pop <- dplyr::anti_join(count_data_suppressed, pop_data, by =  c(grouping_vars, date_col))

  if(nrow(missing_in_pop) > 0) {
    cat("\n")
    log_warn("here are {nrow(missing_in_pop)} cells missing from {substitute(pop_data)}")
    cli::cli_alert_warning("Warning: there are {nrow(missing_in_pop)} cells missing from {substitute(pop_data)}. Join with population dataset doesn't have a 'one-to-one' relationship")
    }


  prevalence <- tryCatch({
    count_data_suppressed |>
      dplyr::left_join(pop_data, by = c(grouping_vars, date_col), relationship = "one-to-one") |>
      dplyr::mutate(prev_rate = unique_id/.data[[pop_col]])
      },
    error = function(e){
      logger::log_warn("Relationship between population dataset and counts is not one-to-one")
      cli::cli_alert_danger("Relationship between population dataset and counts is not one-to-one")
      count_data_suppressed |>
        dplyr::left_join(pop_data, by = c(grouping_vars, date_col)) |>
        dplyr::mutate(prev_rate = unique_id/.data[[pop_col]])
    })

  cat("\n")
  cli::cli_alert_success(crayon::green("Prevalence rates ready!"))
  log_info("Prevalence rates ready")


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

  return(prevalence)
}

