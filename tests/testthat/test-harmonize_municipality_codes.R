test_that("error when col name not in dataset", {
  expect_error(
    harmonize_municipality_codes(data = kommuner_2016, municipality_col = "code_municipality"),
    "The dataset must contain a column named code_municipality")
})


test_that("stable output for sample kommuner 2016", {
  expect_snapshot({harmonize_municipality_codes(data = kommuner_2016,
                                                municipality_col = "code")
    })
})

test_that("nrows output same as nrows as input", {
  sample_var_df <- head(var_df, sample(c(50:200), 1))
  harmonized_test <- harmonize_municipality_codes(sample_var_df, municipality_col = "varying_code")
  expect_equal(nrow(sample_var_df), nrow(harmonized_test))
})
