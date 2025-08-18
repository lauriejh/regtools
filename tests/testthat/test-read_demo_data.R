test_that("reads a supported format into the same reference .rds and log file", {

  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log") # Temporal log path, should delete itself at the end
  test_csv <- system.file("extdata", "invar_data.csv", package = "regtools") #Read example csv

  # Test CSV reading and validation
  output_csv <- read_demo_data(file_path = test_csv,
                               data_type = "t_invariant",
                               id_col = "id",
                               date_col = "diag_year",
                               log_path = l_path)

  output_csv$sex <- as.factor(output_csv$sex)
  expect_equal(output_csv, invar_df)

  # Test that logging works
  expect_true(file.exists(l_path))
  expect_gt(file.info(l_path)$size, 0)

})

test_that("Error when missing required columns", {
  td <- withr::local_tempdir()
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")

  # create and save wrong CSV
  wrong_csv <- var_df
  wrong_csv$id <- NULL
  wrong_path <- file.path(td, "wrong.csv")

  utils::write.csv(wrong_csv, wrong_path, row.names = FALSE)

  expect_error(
    read_demo_data(wrong_path, data_type = "t_variant", id_col = "id", date_col = "year_varying", log_path = l_path), "The dataset must contain a column named id")
})

test_that("Error when unsupported file extensions and nonexistent files", {
  td <- withr::local_tempdir()
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")

  # create and save xslx file
  tp <- file.path(td, "invar.xlsx")
  file.create(tp)

  expect_error(
    read_demo_data(tp, data_type = "t_invariant", id_col = "id", date_col = "year_varying", log_path = l_path), "File type not supported. Please provide a .csv, .rds, or .sav file.")

})

test_that("Error when not valid data type given", {
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  expect_error(
    read_demo_data(invar_df, data_type = "demographic", id_col = "id", date_col = "year_varying", log_path = l_path), "demographic not supported.")

})

test_that("Error duplicated ids when invariant type", {
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  sample_var_df <- head(var_df, 100)
  td <- withr::local_tempdir()
  t_path <- file.path(td, "sample_var_df.csv")
  utils::write.csv(sample_var_df, t_path, row.names = FALSE)

  expect_error(
    read_demo_data(t_path, data_type = "t_invariant", id_col = "id", date_col = "year_varying", log_path = l_path), "The dataset contains duplicate IDs. Verify that this dataset only containts persistent characteristics.")
})

test_that("CLI stable for sample CSV", {
  log_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  test_csv <- system.file("extdata", "invar_data.csv", package = "regtools")
  expect_snapshot({
    invisible(read_demo_data(file_path = test_csv,
                             data_type = "t_invariant",
                             id_col = "id",
                             date_col = "diag_year",
                             log = log_path))
  }, transform = snap_redact_paths)
})
