test_that("reads a supported format into the same reference .rds and log file", {

  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log") # Temporal log path, should delete itself at the end
  test_csv <- system.file("extdata", "diag_data.csv", package = "regtools") #Read example csv

  # Test CSV reading and validation
  output_csv <- read_diag_data(file_path = test_csv,
                               id_col = "id", date_col = "diag_year",
                               log_path = l_path)

  expect_equal(output_csv, diag_df)

  # Test that logging works
  expect_true(file.exists(l_path))
  expect_gt(file.info(l_path)$size, 0)


})


test_that("Error when missing required columns", {
  td <- withr::local_tempdir() #init temp directory
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")

  # create and save wrong CSV
  wrong_csv <- diag_df
  wrong_csv$id <- NULL
  wrong_path <- file.path(td, "wrong.csv")

  utils::write.csv(wrong_csv, wrong_path, row.names = FALSE)

  expect_error(
    read_diag_data(wrong_path, id_col = "id", date_col = "diag_year", code_col = "code", log_path = l_path), "The dataset must contain a column named id")
})

test_that("Error when unsupported file extensions and nonexistent files", {
  td <- withr::local_tempdir()
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")

  # create and save xslx file
  tp <- file.path(td, "diag.xlsx")
  file.create(tp)

  expect_error(
    read_diag_data(tp, id_col = "id", date_col = "diag_year", code_col = "code", log_path = l_path), "File type not supported. Please provide a .csv, .rds, or .sav file.")

})

test_that("CLI stable for sample CSV", {
  l_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  test_csv <- system.file("extdata", "diag_data.csv", package = "regtools")
  expect_snapshot({
    invisible(read_diag_data(file_path = test_csv,
                             id_col = "id", date_col = "diag_year", code_col = "code", log_path = l_path))
  }, transform = snap_redact_paths)
})




