#' Filter demographic data by selected filtering parameters
#'
#' @param data Data frame containing pre-processed demographic data
#' @param data_type Type of demographic data: "t_variant" or "t-variant"
#' @param filter_param Named list containing filtering parameters. The names in the list are the column names and the values are vectors of values to keep.
#' @param id_col Optional flag, necessary for "any" filtering option.
#' @param rm_na Removes observations that have NA in the non-filtered columns.
#' @param data_type Type of demographic data: "t_variant" or "t-variant"
#' @param any Filtering option, any year. Default = FALSE.
#'
#' @return Filtered demographic dataframe containing only relevant observations based on the filtering parameters.
#' @importFrom dplyr filter
#' @importFrom purrr reduce
#' @importFrom rlang sym
#' @importFrom skimr skim
#' @importFrom tidyselect everything
#' @importFrom dplyr across
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#'
#' @export
#'

filter_demo <- function(data, data_type, filter_param, id_col = NULL, any = FALSE, rm_na = TRUE){

  ###Validate input ####
  stopifnot("Specified variables do not exist in your dataset." = names(filter_param) %in% colnames(data))

  if(missing(data_type)) {
    stop("Data type not specified")
  }

  ###Helper functions####
  remove_na <- function(data){
    n_missing <- data |>
      dplyr::summarize(dplyr::across(tidyselect::everything(), ~ sum(is.na(.))))
    if(sum(n_missing) > 0){
      message(paste("Removing ", sum(n_missing), "observations containing NAs: "))
      print(n_missing)
      Sys.sleep(1)
      data_no_na <- data |>
        tidyr::drop_na()
      message(paste("\u2022 After removing NAs, the dataset has ", nrow(data_no_na), " observations."))
      } else {
        message("The dataset has no NAs or they are coded in a different format.")
      }
    return(data_no_na)
  }

  do_filter <- function(data, filter_param, id_col = NULL, any = FALSE){
    filtered_data <- purrr::reduce(names(filter_param), function(df, col) {
      if(any){
        df |>
          dplyr::group_by(!!rlang::sym(id_col)) |>
          dplyr::filter(any(!!rlang::sym(col) %in% filter_param[[col]])) |>
          dplyr::ungroup()
      } else {
        df |>  dplyr::filter(!!rlang::sym(col) %in% filter_param[[col]])
      }
    }, .init = data)
    }


  ####Main filtering####
  if(data_type == "t_invariant"){
    message("Filtering time-invariant dataset...")
    filtered_data <- do_filter(data, filter_param)
  } else if (data_type == "t_variant"){
    message("Filtering time-variant dataset...")
    filtered_data <- do_filter(data, filter_param, id_col, any)
  } else {
    stop("Invalid data type specified ")
  }

  message(paste("Original dataset had ", nrow(data), " observations."))
  cat("\n")
  message(paste("\u2022", nrow(filtered_data), " observations fulfilled the selected filtering parameters: "))
  Sys.sleep(2)
  print(filtered_data |> skimr::skim())

  ####NA filtering####

  if(rm_na) {
    filtered_data <- remove_na(filtered_data)
  }
  return(filtered_data)
}
