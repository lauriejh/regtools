test_that("reads a supported format into the same reference .rds and log file", {

  log_path <- withr::local_tempfile(fileext = ".log", lines = "Test log") # Temporal log path, should delete itself at the end
  test_csv <- system.file("extdata", "invar_data.csv", package = "regtools") #Read example csv

  # Test CSV reading and validation
  output_csv <- read_demo_data(file_path = test_csv,
                               data_type = "t_invariant",
                               id_col = "id",
                               date_col = "diag_year",
                               log = log_path)

  output_csv$sex <- as.factor(output_csv$sex)
  expect_equal(output_csv, invar_df)

  # Test that logging works
  expect_true(file.exists(log_path))
  expect_gt(file.info(log_path)$size, 0)

  # Test that input validation works


})

test_that("CLI stable for sample CSV", {  # need to figure out what to do with paths
  log_path <- withr::local_tempfile(fileext = ".log", lines = "Test log")
  test_csv <- system.file("extdata", "invar_data.csv", package = "regtools")
  expect_snapshot({
    invisible(read_demo_data(file_path = test_csv,
                             data_type = "t_invariant",
                             id_col = "id",
                             date_col = "diag_year",
                             log = log_path))
  })
})


# skip on os (anything not windows, if not path formatter will not work)

