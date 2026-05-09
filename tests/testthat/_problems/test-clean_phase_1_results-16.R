# Extracted from test-mysterycall_clean_phase1.R:16

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
expect_error(mysterycall_clean_phase1(df_empty, verbose = TRUE), "Required columns are missing")
