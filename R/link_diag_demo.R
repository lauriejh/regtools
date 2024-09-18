#' Link diagnostic and demographic datasets using unique personal identifiers
#'
#' @param data_diag Data frame containing pre-processed and pre-validated diagnostic data (check minimum requirements in documentations)
#' @param data_demo_inv Data frame containing validated time-invariant demographic data (check minimum requirements in documentations)
#' @param data_demo_var Data frame containing validated time-variant demographic data (check minimum requirements in documentations)
#' @param id_col Name (character) of the ID column in the data set (unique personal identifier), default is "id"
#' @param date_col Name (character) of the date column in the data set, in case of using time-variant data.
#'
#' @return Linked dataset including relevant diagnostic and demographic characteristics.
#' @importFrom dplyr inner_join
#' @importFrom skimr skim
#' @importFrom janitor clean_names
#' @export
#'
link_diag_demo <- function(data_diag, data_demo_inv = NULL, data_demo_var = NULL, id_col= "id", date_col = "year"){
  ###Input validation#####
  if(is.null(data_demo_inv) && is.null(data_demo_var)) {
    stop("At least one of data_demo_inv or data_demo_var must be provided")
  }

  if(!is.null(data_demo_var) && !all(c(id_col,date_col) %in% names(data_demo_var))) {
    stop("data_demo_var must contain the specified 'date' and 'id' columns")
  }


  ###Linkage ####
  if(!is.null(data_demo_inv)){
    stopifnot("data_demo_inv must contain specified 'id' column." = id_col %in%  names(data_demo_inv))
    message("Joining diagnostic data with time-invariant demographic data...")
    linked_df <- data_diag |>
      dplyr::inner_join(data_demo_inv, by = id_col)
    message("Join ready: diag and demo_inv")
  }

  ##add check for 'date' column in last linked_df or directly from diag data
  if(!is.null(data_demo_var)){
    message("Joining with time-variant demographic data...")
    linked_df <- linked_df |>
      dplyr::inner_join(data_demo_var, by = c(id_col, date_col))
    message("Join ready: diag and demo_inv")
  }

  linked_df <- linked_df |> janitor::clean_names()
  return(linked_df)
}
