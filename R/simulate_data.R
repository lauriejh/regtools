#' Create simulated diagnostic dataset
#'
#' @param data_type "diagnostic", "t_varying, "t_unvarying"
#' @param population_size Target size of simulated diagnostic dataset
#' @param prefix_ids ID prefix
#' @param length_ids Desired length of unique identifiers
#' @param family_codes ICD-10 family
#' @param pattern "increase", "decrease" or "random" pattern for incidence
#' @param prevalence Prevalence
#' @param dates Dates
#' @param incidence Incidence(s) for period
#' @param sex_vector How should sex be coded?
#' @param y_birth Years of birth to be included
#' @param filler_codes Filler codes to be used
#' @param filler_y_birth What other years of birth to be used as fillers
#' @param unvarying_query
#' @param region_level
#' @param date_unvarying_codes
#' @param year_expand_varying
#'
#' @returns Simulated diagnostic dataset
#' @export
#'

simulate_data <- function(data_type, population_size, prefix_ids, length_ids, family_codes, pattern, prevalence = NULL, dates, incidence = NULL, sex_vector, y_birth, filler_codes, filler_y_birth,
                          unvarying_query, region_level = "kommune", date_unvarying_codes = NULL, year_expand_varying){

  ### Helper functions ---------------------------------------------------------
  unique_ids <- function(population_size, prefix = "P000", length_id = 6) {

    max_length <- 10^length_id
    if (population_size > max_length){
      cli::cli_abort("Population size exceed possible number of unique ID
                   combinations. Try increasing 'length_id'.")
    }

    random_numbers <- sample(0:(max_length-1), population_size)

    unique_ids <- sprintf(paste0("%0", length_id, "d"), random_numbers)
    paste0(prefix, unique_ids)
  }



  icd10_codes <- function(family){
    load("simulate/npr.rda")
    pattern <- paste0("^(", paste(family, collapse = "|"), ")")

    matching_codes <- purrr::map(npr[, 1:4], function(x){
      x[grepl(pattern, x, ignore.case = TRUE)]
    })
    matching_codes <- unlist(matching_codes)
  }

  number_cases <- function(population_size, pattern = "random", prevalence = NULL, dates, incidence = NULL){

    periods <- length(dates)


    if (!is.null(prevalence)){
      prevalence_cases <- round(population_size * prevalence)
      if (pattern == "random"){
        prob_equal = rep.int(1 / periods, periods)
        incidence_cases <- as.vector(rmultinom(1, size = prevalence_cases, prob = prob_equal))
      } else if (pattern == "increase"){
        prob_increasing <- 1:periods
        incidence_cases <- as.vector(rmultinom(1, size = prevalence_cases, prob = prob_increasing))
      } else if (pattern == "decrease") {
        prob_decrease <- rev(1:periods)
        incidence_cases <- as.vector(rmultinom(1, size = prevalence_cases, prob = prob_decrease))
      }
      n_cases_list <- list(incidence_cases = incidence_cases,
                           prevalence_cases = round(population_size * prevalence),
                           dates = dates)

    }

    if(!is.null(incidence)){
      incidence_cases <- purrr::map_dbl(incidence, ~round(population_size*.x))
      prevalence <- sum(incidence_cases)
      n_cases_list <- list(incidence_cases = incidence_cases,
                           prevalence_cases = prevalence,
                           dates = dates)
    }

    return(n_cases_list)
  }

  generate_cases <- function(ids, codes, cases_list, sex, y_birth){

    prevalence_cases <- cases_list[["prevalence_cases"]]

    cases_df <- dplyr::tibble(
      id = sample(ids, prevalence_cases, replace = FALSE),
      code = sample(codes, prevalence_cases, replace = TRUE),
      sex = as.factor(sample(sex, prevalence_cases, replace = TRUE)),
      y_birth = sample(y_birth, prevalence_cases, replace= TRUE))

    date_df <- purrr::map2_dfr(
      cases_list$dates,
      cases_list$incidence_cases,
      ~ dplyr::tibble(date = rep(.x, .y))
    )

    cases_df <- cbind(cases_df, date_df)
    return(cases_df)
  }


  get_classifications <- function(queries, date = NULL) {

    if (length(queries) == 1) {
      search <- klassR::SearchKlass(queries)
      class_code <- search$klass_nr[1]
      codes_ssb <- klassR::GetKlass(klass = class_code,
                                    language = "en",
                                    date = date) |>
        dplyr::select(code, name)
      return(codes_ssb)
    }

    #Multiple queries
    result_list <- purrr::map(queries, function(query) {
      search <- klassR::SearchKlass(query)
      class_code <- search$klass_nr[1]
      codes_ssb <- klassR::GetKlass(klass = class_code,
                                    language = "en",
                                    date = date) |>
        dplyr::select(code, name)
      return(codes_ssb)
    })

    names(result_list) <- queries

    return(result_list)
  }

  add_var_ssb <- function(data, new_info) {

    size <- nrow(data)
    if (is.data.frame(new_info)) {
      sampled_idx <- sample(nrow(new_info), size = size, replace = TRUE)
      data[["code"]] <- new_info$code[sampled_idx]
      data[["description"]] <- new_info$name[sampled_idx]

    } else if (is.list(new_info)) {
      for (i in seq_along(new_info)) {
        curr_info <- new_info[[i]]
        prefix <- names(new_info)[i]
        new_code_col <- glue::glue("{prefix}_code")
        new_desc_col <- glue::glue("{prefix}_description")
        sampled_idx <- sample(nrow(curr_info), size = size, replace = TRUE)
        data[[new_code_col]] <- curr_info$code[sampled_idx]
        data[[new_desc_col]] <- curr_info$name[sampled_idx]
      }
    }

    return(data)
  }


  construct_yearly <- function(data, years_expand){
    id_df <- data |> dplyr::select(id)
    expanded_x <- id_df |>
      dplyr::mutate(
        year = list(years_expand)) |>
      tidyr::unnest(year)
  }


  assign_regions <- function(data, regions) {
    data  |>
      dplyr::group_by(id)  |>
      tidyr::nest() |>
      dplyr::mutate(regions = purrr::map(data, ~{
        n_years <- nrow(.x)
        num_moves <- sample(1:3, 1)
        change_years <- sort(sample(1:n_years, num_moves - 1))

        municipality_assignment <- rep(sample(regions$code, num_moves), times = c(change_years, n_years) - c(0, change_years))
        .x |> mutate(code = municipality_assignment)
      })) |>
      tidyr::unnest(cols = c(data, regions), names_repair = "universal")
  }



  ### Main function call -------------------------------------------------------

  unique_id_vector <- unique_ids(population_size, prefix = prefix_ids, length_id = length_ids)
  cli::cli_alert_success("Succesfully generated unique id vector")
  icd10_vector <- icd10_codes(family = family_codes)
  cli::cli_alert_success("Succesfully generated ICD-10 code vector")
  list_cases <- number_cases(population_size = population_size, pattern = pattern, prevalence = prevalence, dates = dates, incidence = incidence)
  cli::cli_alert_success("Succesfully generated number of incidence and prevalence cases")
  diagnostic_df <- generate_cases(ids = unique_id_vector, codes = icd10_vector, cases_list = list_cases, sex = sex_vector, y_birth = y_birth)
  cli::cli_alert_success("Sucessfully generated relevant diagnostic cases!")


  ## Generate non-relevant cases
  other_codes <- icd10_codes(family = filler_codes)
  new_codes <- setdiff(other_codes, icd10_vector)

  used_ids <- diagnostic_df$id
  new_ids <- setdiff(unique_id_vector, used_ids)

  random_incidence <- number_cases(length(new_ids), pattern = "random", prevalence = 1, dates = list_cases$dates)
  filler_diagnostic_df <- generate_cases(new_ids, new_codes, random_incidence, sex = c(1,2), y_birth= filler_y_birth)

  full_diag_df <- rbind(filler_diagnostic_df, diagnostic_df)
  rows <- sample(nrow(full_diag_df))
  full_diag_df <- full_diag_df[rows, ]

  if (data_type == "diagnostic"){
    final_simulated_data <- full_diag_df |> dplyr::select(id, code, date)
    cli::cli_alert_success("Sucessfully generated full diagnostic dataset!")
  } else if (data_type == "t_unvarying"){
    t_unvarying <- full_diag_df |> dplyr::select(id, sex, y_birth)
    unvarying_codes <- get_classifications(queries = unvarying_query) #can be list or dataframe
    final_simulated_data <- add_var_ssb(t_unvarying, unvarying_codes)
    cli::cli_alert_success("Sucessfully generated full time-unvarying dataset!")
  } else if (data_type == "t_varying"){
    t_varying <- full_diag_df |> dplyr::select(id) |> construct_yearly(years_expand = year_expand_varying)
    kommune_codes <- get_classifications(queries = region_level, date = date_unvarying_codes) |> select(code)
    t_varying_regions <- assign_regions(t_varying, kommune_codes) |>
      dplyr::select(id, 'year...2', code)

    final_simulated_data <- t_varying_regions |>
      dplyr::rename(year = `year...2`) |>
      dplyr::ungroup()
    cli::cli_alert_success("Sucessfully generated full time-varying dataset!")
  }

  return(final_simulated_data)
}
