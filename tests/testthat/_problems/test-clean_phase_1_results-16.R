# Extracted from test-clean_phase_1_results.R:16

# prequel ----------------------------------------------------------------------
library(testthat)
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("janitor")
testthat::skip_if_not_installed("readr")
testthat::skip_if_not_installed("stringr")
testthat::skip_if_not_installed("humaniformat")
library(dplyr)
library(janitor)
library(readr)
library(stringr)
library(humaniformat)

# test -------------------------------------------------------------------------
df_empty <- data.frame()
expect_error(clean_phase_1_results(df_empty, verbose = TRUE), "Required columns are missing")
