
# Example data ------------------------------------------------------------

# Get approx population size for out population of interest

#Østfold, males and females, follow up year 2020. years of birth 2010-2018, diagnosis years 2012-2020. Period prevalence in population of 2.3%
population_ssb <- get_population_ssb(regions = "fylker", years = c(2020), ages = c(2:10), by_sex = F,  aggregate_age = TRUE)

population_ostfold <- population_ssb |> dplyr::filter(region_code == 31 & age == "Total")

simulated_list <- simulate_data(population_size = population_ostfold$population,
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
                                varying_query = "fylke"
)


invar_df <- simulated_list$unvar_df
var_df <- simulated_list$var_df
diag_df <- simulated_list$diag_df


usethis::use_data(invar_df, overwrite = TRUE)
usethis::use_data(var_df, overwrite = TRUE)
usethis::use_data(diag_df, overwrite = TRUE)
