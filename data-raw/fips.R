## code to prepare `fips` dataset goes here
library(tigris)
library(dplyr)
library(readr)

fips <- tigris::fips_codes %>%
  dplyr::distinct(state, .keep_all = TRUE) %>%
  dplyr::filter(state_code < 58) %>%
  dplyr::select(-county_code, -county)

fips <- readr::type_convert(fips)

usethis::use_data(fips, overwrite = TRUE)
