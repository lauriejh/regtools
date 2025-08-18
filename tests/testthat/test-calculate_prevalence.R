test_that("writes to log", {
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  pop_df <- tibble::tibble(year = "2012-2020", population = 30024)
  linked_df <- linked_df |> dplyr::rename("year"= "diag_year")

  prevalence_df <- calculate_prevalence(linked_df,
                                        id_col = "id",
                                        date_col = "year",
                                        pop_data = pop_df,
                                        pop_col = "population",
                                        time_p = c(2012,2020),
                                        CI = TRUE,
                                        CI_level = 0.95,
                                        only_counts = FALSE,
                                        suppression = TRUE,
                                        log_path = l_path)

  # Test that logging works
  expect_true(file.exists(l_path))
  expect_gt(file.info(l_path)$size, 0)

})


test_that("creates dir and file log", {
  td <- withr::local_tempdir()
  withr::local_dir(td)
  pop_df <- tibble::tibble(year = "2012-2020", population = 30024)
  linked_df <- linked_df |> dplyr::rename("year"= "diag_year")
  prevalence_df <- calculate_prevalence(linked_df,
                                        id_col = "id",
                                        date_col = "year",
                                        pop_data = pop_df,
                                        pop_col = "population",
                                        time_p = c(2012,2020),
                                        CI = TRUE,
                                        CI_level = 0.95,
                                        only_counts = FALSE,
                                        suppression = TRUE,
                                        log_path = NULL)


  expect_true("log" %in% list.files(td))
  expect_length(list.files(file.path(td, "log")), 1)
})


# Input validation

test_that("input validation works", {
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  pop_df <- tibble::tibble(year = "2012-2020", population = 30024)
  linked_df <- linked_df |> dplyr::rename("year"= "diag_year")

  # Test code col
  expect_error(calculate_prevalence(linked_data = linked_df,
                                    id_col = "ids",
                                    date_col = "year",
                                    pop_data = pop_df,
                                    pop_col = "population",
                                    time_p = c(2012,2020),
                                    CI = TRUE,
                                    CI_level = 0.95,
                                    only_counts = FALSE,
                                    suppression = TRUE,
                                    log_path = l_path), "The linked dataset must contain the specified 'id' column: ids")

  # Test date col
  expect_error(calculate_prevalence(linked_data = linked_df,
                                    id_col = "id",
                                    date_col = "years",
                                    pop_data = pop_df,
                                    pop_col = "population",
                                    time_p = c(2012,2020),
                                    CI = TRUE,
                                    CI_level = 0.95,
                                    only_counts = FALSE,
                                    suppression = TRUE,
                                    log_path = l_path), "The population and linked data must include the same specified 'date' column: years")

})



# Suppression

test_that("Suppression works", {
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  pop_df <- tibble::tibble(year = "2012-2020", population = 30024)
  linked_df <- linked_df |> dplyr::rename("year"= "diag_year")

  expect_message(calculate_prevalence(linked_data = linked_df,
                                      id_col = "id",
                                      date_col = "year",
                                      pop_data = pop_df,
                                      pop_col = "population",
                                      time_p = c(2012,2020),
                                      CI = TRUE,
                                      CI_level = 0.95,
                                      only_counts = FALSE,
                                      suppression = TRUE,
                                      suppression_threshold = 10,
                                      log_path = l_path), "Suppressed counts using 10 threshold")

  # Also know that total relevant diagnostic cases in linked_df is 691
  suppressed <- calculate_prevalence(linked_data = linked_df,
                                     id_col = "id",
                                     date_col = "year",
                                     pop_data = pop_df,
                                     pop_col = "population",
                                     time_p = c(2012,2020),
                                     CI = TRUE,
                                     CI_level = 0.95,
                                     only_counts = FALSE,
                                     suppression = TRUE,
                                     suppression_threshold = 700,
                                     log_path = l_path)

  n_removed <- suppressed |> dplyr::filter(is.na(unique_id)) |> nrow()

  expect_gt(n_removed, 0)

})

# Confidence interval

test_that("calculates CI when it should", {
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  pop_df <- tibble::tibble(year = "2012-2020", population = 30024)
  linked_df <- linked_df |> dplyr::rename("year"= "diag_year")

  prevalence_df <- calculate_prevalence(linked_df,
                                        id_col = "id",
                                        date_col = "year",
                                        pop_data = pop_df,
                                        pop_col = "population",
                                        time_p = c(2012,2020),
                                        only_counts = FALSE,
                                        suppression = TRUE,
                                        suppression_threshold = 10,
                                        CI = TRUE,
                                        CI_level = 0.99,
                                        log_path = l_path)

  # Should include at least two col with ci

  expect_gt(length(which(stringr::str_starts(colnames(prevalence_df), "ci"))), 1)

})


# Time period vs point


# Only counts

test_that("only gives out counts", {
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  pop_df <- tibble::tibble(year = "2012-2020", population = 30024)
  linked_df <- linked_df |> dplyr::rename("year"= "diag_year")

  prevalence_df <- calculate_prevalence(linked_df,
                                        id_col = "id",
                                        date_col = "year",
                                        pop_data = pop_df,
                                        pop_col = "population",
                                        time_p = c(2012,2020),
                                        only_counts = TRUE,
                                        suppression = TRUE,
                                        log_path = l_path)

  # Should not have any prev_rate column

  expect_false("prev_rate" %in% colnames(prevalence_df))
  expect_equal(ncol(prevalence_df), 3)

})
