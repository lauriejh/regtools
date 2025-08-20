## code to prepare `kommuner_2016` dataset

search <- klassR::search_klass("kommuneinndeling")
class_code <- search$klass_nr[1]
kommuner_2016 <- klassR::GetKlass(klass = class_code,
                                    language = "en",
                                    date = "2016-01-01") |>
  dplyr::select(code, name)



usethis::use_data(kommuner_2016, overwrite = TRUE)
