#' Create simulated diagnostic, time-varying and time-unvarying data
#'
#' @param population_size pop
#' @param prefix_ids
#' @param length_ids
#' @param family_codes
#' @param pattern
#' @param prevalence
#' @param diag_years
#' @param incidence
#' @param sex_vector
#' @param y_birth
#' @param filler_codes
#' @param filler_y_birth
#' @param unvarying_query
#' @param unvarying_codes
#' @param region_level
#' @param region_codes
#' @param filler_region_codes
#' @param residence_years
#' @param date_classifications
#'
#' @returns
#' @export
#'
simulate_data <- function(population_size, prefix_ids, length_ids, family_codes,
                             pattern, prevalence = NULL, diag_years, incidence = NULL,
                             sex_vector, y_birth,
                             filler_codes, filler_y_birth,
                             unvarying_query, unvarying_codes = NULL,
                             region_level = "kommune", region_codes, filler_region_codes, residence_years,
                             date_classifications = NULL){


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

    if (length(matching_codes) == 0){
      cli::cli_abort("No ICD-10 codes found matching: {family}")
    }

    return(matching_codes)
  }

  number_cases <- function(population_size, pattern = "random", prevalence = NULL, diag_years, incidence = NULL){

    periods <- length(diag_years)


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
                           diag_years = diag_years)

    }

    if(!is.null(incidence)){
      incidence_cases <- purrr::map_dbl(incidence, ~round(population_size*.x))
      prevalence <- sum(incidence_cases)
      n_cases_list <- list(incidence_cases = incidence_cases,
                           prevalence_cases = prevalence,
                           diag_years = diag_years)
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
      cases_list$diag_years,
      cases_list$incidence_cases,
      ~ dplyr::tibble(diag_year = rep(.x, .y))
    )

    cases_df <- cbind(cases_df, date_df)
    return(cases_df)
  }


  get_classifications <- function(queries, date = NULL) {

    if (length(queries) == 1) {
      if(queries == "kommune"){
        search <- klassR::search_klass("kommuneinndeling")
        class_code <- search$klass_nr[1]
        codes_ssb <- klassR::GetKlass(klass = class_code,
                                      language = "en",
                                      date = date) |>
          dplyr::select(code, name)
        return(codes_ssb)
      } else if (queries == "fylke"){
        search <- klassR::search_klass("fylkesinndeling")
        class_code <- search$klass_nr[1]
        codes_ssb <- klassR::GetKlass(klass = class_code,
                                      language = "en",
                                      date = date) |>
          dplyr::select(code, name)
        return(codes_ssb)
      } else {
        search <- klassR::search_klass(queries)
        class_code <- search$klass_nr[1]
        codes_ssb <- klassR::GetKlass(klass = class_code,
                                      language = "en",
                                      date = date) |>
          dplyr::select(code, name)
        return(codes_ssb)
      }
    }

    #Multiple queries
    result_list <- purrr::map(queries, function(query) {
      search <- klassR::search_klass(query)
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

  add_unvarying_ssb <- function(data, new_info, user_codes) {

    size <- nrow(data)
    if (is.data.frame(new_info)) {
      sampled_idx <- sample(nrow(new_info), size = size, replace = TRUE)
      data[["code"]] <- new_info$code[sampled_idx]
      data[["description"]] <- new_info$name[sampled_idx]

    } else if (is.list(new_info) && user_codes == F) {
      for (i in seq_along(new_info)) {
        curr_info <- new_info[[i]]
        prefix <- names(new_info)[i]
        new_code_col <- glue::glue("{prefix}_code")
        new_desc_col <- glue::glue("{prefix}_description")
        sampled_idx <- sample(nrow(curr_info), size = size, replace = TRUE)
        data[[new_code_col]] <- curr_info$code[sampled_idx]
        data[[new_desc_col]] <- curr_info$name[sampled_idx]
      }
    } else if (is.list(new_info) && user_codes == T) {
      new_cols <- purrr::imap(new_info, function(codes, var_name){
        sample(codes, size = nrow(data), replace = TRUE)
      })
      data <- cbind(data, new_cols)
    }

    return(data)
  }

  construct_yearly <- function(data, years_expand){
    expanded_x <- data |>
      dplyr::mutate(
        residence_y = list(years_expand)) |>
      tidyr::unnest(residence_y)
  }


  assign_regions <- function(data, regions) {
    if (is.vector(regions)){
      codes <- regions
    } else if (is.data.frame(regions)){
      codes <- regions$code
    }

    varying_data_codes <- data |>
      dplyr::group_by(id)  |>
      dplyr::group_modify(~ {
        n_years <- nrow(.x)
        num_moves <- sample(1:3, 1)

        if (num_moves == 1) {
          region_assignment <- rep(sample(codes, 1, replace = TRUE), n_years)
        } else {
          moving_year <- sort(sample(1:(n_years - 1), num_moves - 1))
          spans <- c(moving_year, n_years) - c(0, moving_year)
          region_assignment <- rep(sample(codes, num_moves, replace = TRUE), times = spans)
        }

        .x$region_code <- region_assignment
        return(.x)
      })  |>
      dplyr::ungroup()

    return(varying_data_codes)
  }

  ### Main function call -------------------------------------------------------

  ##  Add relevant diagnostic codes, and some of the time unvarying information
  unique_id_vector <- unique_ids(population_size, prefix = prefix_ids, length_id = length_ids)
  icd10_vector <- icd10_codes(family = family_codes)
  list_cases <- number_cases(population_size = population_size, pattern = pattern, prevalence = prevalence, diag_years = diag_years, incidence = incidence)
  cli::cli_alert_info("Creating relevant cases with the following characteristics:")
  cli::cli_ul(c("Population size = {population_size}",
                "Prefix IDs = {prefix_ids}",
                "Length IDs = {length_ids}",
                "ICD-10 relevant codes = {family_codes}",
                "Pattern of incidence = {pattern}",
                "Prevalence = {prevalence}",
                "Diagnostic years = {diag_years}",
                "Incidence = {incidence}",
                "Coding sex = {sex_vector}",
                "Relevant years of birth = {y_birth}"
  ))
  cat("\n")
  diagnostic_df <- generate_cases(ids = unique_id_vector, codes = icd10_vector,
                                  cases_list = list_cases, sex = sex_vector, y_birth = y_birth) # in here

  ## Include relevant time-unvarying data

  if (is.null(unvarying_codes)){
    unvarying_codes <- get_classifications(queries = unvarying_query) #can be list or dataframe
    relevant_cases_unvar <- add_unvarying_ssb(diagnostic_df, unvarying_codes, user_codes = F)
  } else if (!is.null(unvarying_codes)){
    relevant_cases_unvar <- add_unvarying_ssb(diagnostic_df, unvarying_codes, user_codes = T)
  }


  ## Assign regions of residence
  cases_df_region <- dplyr::tibble(residence_y = sample(residence_years, list_cases[["prevalence_cases"]], replace = TRUE),
                                   region_code = sample(region_codes, list_cases[["prevalence_cases"]], replace = TRUE))

  relevant_cases_unvar_region <- cbind(relevant_cases_unvar, cases_df_region)

  ## Generate non-relevant cases

  cli::cli_alert_info("Creating filler cases with the following characteristics:")
  cli::cli_ul(c("Filler ICD-10 codes = {filler_codes}",
                "Filler years of birth = {filler_y_birth}",
                "Pattern for filler incidence = 'random'",
                "Number of filler cases to generate = {length(unique_id_vector) - length(relevant_cases_unvar_region$id)}"
  ))
  cat("\n")
  cli::cli_alert_warning("This process can take some minutes...")


  other_codes <- icd10_codes(family = filler_codes)
  new_codes <- setdiff(other_codes, icd10_vector)

  used_ids <- relevant_cases_unvar_region$id
  new_ids <- setdiff(unique_id_vector, used_ids)

  random_incidence <- number_cases(length(new_ids), pattern = "random", prevalence = 1, diag_years = list_cases$diag_years)
  filler_diagnostic_df <- generate_cases(new_ids, new_codes, random_incidence, sex = sex_vector, y_birth= filler_y_birth)

  ## Add filler unvarying codes

  filler_diagnostic_df <- add_unvarying_ssb(filler_diagnostic_df, unvarying_codes, user_codes = F)


  all_cases <- relevant_cases_unvar |>
    dplyr::bind_rows(filler_diagnostic_df) |>
    construct_yearly(years_expand = residence_years)

  # Add filler varying codes

  if (is.null(filler_region_codes)){
    filler_region_codes <- get_classifications(queries = region_level, date = date_classifications) |> dplyr::select(code)
  }

  all_cases <- assign_regions(all_cases, filler_region_codes)

  # Update with relevant codes for diagnostic cases

  all_cases_updated <-dplyr::rows_update(all_cases, relevant_cases_unvar_region, by = c("id", "residence_y"), unmatched = "ignore")

  unvar_df <- all_cases_updated |> dplyr::select(-residence_y, -region_code, -code, -diag_year) |> dplyr::distinct()

  var_df <- all_cases_updated |> dplyr::select(id, residence_y, region_code)

  diag_df <- all_cases_updated |> dplyr::select(id, code, diag_year) |> dplyr::distinct()

  all_cases_updated_list <- list(unvar_df = unvar_df, var_df = var_df, diag_df = diag_df)
  cat("\n")
  cli::cli_alert_success("Succesfully generated diagnostic, time-varying and time-unvarying datasets!")

  return(all_cases_updated_list)

}
