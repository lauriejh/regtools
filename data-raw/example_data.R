
# Example data ------------------------------------------------------------

log_file <- tempfile()
cat("Temp log file", file = log_file)

#males and females, follow up year 2020. years of birth 2010-2018, diagnosis years 2012-2020. Period prevalence in population of 2.3%
set.seed(123)
simulated_list <- synthetic_data(population_size = 30024,
                                prefix_ids = "P000",
                                length_ids = 6,
                                family_codes = c("F45", "F84"),
                                pattern = "increase",
                                prevalence = .023,
                                diag_years  = c(2012:2020),
                                sex_vector = c(0,1),
                                y_birth = c(2010:2018),
                                filler_codes = "F",
                                filler_y_birth = c(2000:2009),
                                unvarying_codes = list("innvandringsgrunn" = c("ARB", "NRD", "UKJ")),
                                unvarying_codes_filler = list("innvandringsgrunn" = c("FAMM", "UTD")),
                                varying_query = "kommuner",
                                date_classifications = "2016-01-01"
)


invar_df <- simulated_list$invar_df
var_df <- simulated_list$var_df
diag_df <- simulated_list$diag_df


usethis::use_data(invar_df, overwrite = TRUE)
usethis::use_data(var_df, overwrite = TRUE)
usethis::use_data(diag_df, overwrite = TRUE)




filtered_inv <- filter_demo(invar_df,
                            data_type = "t_invariant",
                            filter_param = list("y_birth"= c(2010:2018), "innvandringsgrunn" = c("ARB", "NRD", "UKJ")),
                            any= FALSE,
                            rm_na = FALSE,
                            log_path = log_file)

filtered_diag <- filter_diag(diag_df,
                             pattern_codes = c("F45", "F84"),
                             id_col = "id",
                             code_col = "code",
                             log_path = log_file)

curated_diag <- curate_diag(filtered_diag,
                            min_diag = 1,
                            first_diag = TRUE,
                            id_col = "id",
                            code_col = "code",
                            date_col = "diag_year",
                            log_path = log_file)


# Link --------------------------------------------------------

linked_df <- link_diag_demo(data_diag = curated_diag,
                            data_demo_inv = filtered_inv,
                            id_col = "id",
                            log_path = log_file)


usethis::use_data(linked_df, overwrite = TRUE)

