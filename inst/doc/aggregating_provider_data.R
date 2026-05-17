## ----setup, include=FALSE-----------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  eval = eval_vignettes
)

if (eval_vignettes) {
  library(mysterycall)
}

## ----eval=FALSE---------------------------------------------------------------
# roster <- mysterycall_search_by_taxonomy(
#   "Gynecologic Oncology",
#   states = c("CO", "WY"),
#   write_snapshot = FALSE,
#   notify = FALSE
# ) |>
#   mysterycall_validate_npi()

## ----eval=FALSE---------------------------------------------------------------
# clinician_data <- mysterycall_get_clinician_data(roster)
# 
# # Assert the enrichment table is unique before joining
# mysterycall_assert_unique_keys(clinician_data, key_cols = "npi",
#                                 label = "cms_clinician_data")
# 
# analysis_base <- mysterycall_safe_left_join(
#   left          = roster,
#   right         = clinician_data,
#   by            = "npi",
#   label_left    = "roster",
#   label_right   = "cms_clinician_data",
#   min_coverage  = 0.90,
#   suffix        = c("_roster", "_clinician")
# )

## ----eval=FALSE---------------------------------------------------------------
# site_table <- analysis_base |>
#   dplyr::distinct(npi, address, .keep_all = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# analysis_with_geo <- mysterycall_safe_left_join(
#   left         = analysis_base,
#   right        = site_geography,
#   by           = c("npi", "address"),
#   label_left   = "analysis_base",
#   label_right  = "site_geography",
#   min_coverage = 0.95
# )

## ----eval=FALSE---------------------------------------------------------------
# phase2_clean <- clean_phase_2_data("phase2_results.xlsx")
# 
# analysis_ready <- mysterycall_safe_left_join(
#   left               = analysis_with_geo,
#   right              = phase2_clean,
#   by                 = c("npi", "name"),
#   label_left         = "analysis_with_geo",
#   label_right        = "phase2_outcomes",
#   expect_unique_right = FALSE,
#   min_coverage       = NULL   # not all providers are called in every wave
# )

## -----------------------------------------------------------------------------
# mysterycall::physicians |>
#   dplyr::count(subspecialty, sort = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# analysis_ready |>
#   dplyr::count(subspecialty, medicaid_accepted)

## ----eval=FALSE---------------------------------------------------------------
# table_generate_overall(
#   analysis_ready,
#   vars = c("subspecialty", "medicaid_accepted", "appointment_offered")
# )

