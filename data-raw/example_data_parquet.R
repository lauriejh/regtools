arrow::write_dataset(diag_df, path = "./inst/extdata/dataset_diag.parquet", format = "parquet")
arrow::write_dataset(invar_df, path = "./inst/extdata/dataset_invar.parquet", format = "parquet")
arrow::write_dataset(var_df, path = "./inst/extdata/dataset_var.parquet", format = "parquet")
