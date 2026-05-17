## ----setup, include=FALSE-----------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
library(magrittr)
knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  eval = eval_vignettes
)

if (eval_vignettes) {
  library(mysterycall)
}

## -----------------------------------------------------------------------------
# sample_names <- tibble::tibble(
#   first = c("Jane", "Maria", "Alex"),
#   last = c("Smith", "Garcia", "Johnson")
# )
# 
# sample_names

## ----eval=FALSE---------------------------------------------------------------
# results <- mysterycall_search_and_process_npi(
#   data = sample_names,
#   notify = FALSE
# )

## ----eval=FALSE---------------------------------------------------------------
# results <- mysterycall_search_and_process_npi(
#   data = sample_names,
#   enumeration_type = "ind",
#   limit = 10,
#   country_code = "US",
#   filter_credentials = c("MD", "DO"),
#   notify = FALSE
# )

## ----eval=FALSE---------------------------------------------------------------
# results <- mysterycall_search_and_process_npi(
#   data = sample_names,
#   limit = 10,
#   dest_dir = "npi_chunks",
#   accumulate_path = "npi_results.csv",
#   resume = TRUE,
#   progress_log = "npi_progress.csv",
#   progress_log_format = "csv",
#   heartbeat_seconds = 30,
#   notify = FALSE
# )

## ----eval=FALSE---------------------------------------------------------------
# dplyr::glimpse(results)
# 
# results |>
#   dplyr::count(search_term, sort = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# results_unique <- results |>
#   dplyr::distinct(npi, .keep_all = TRUE)
# 
# results_unique |>
#   dplyr::count(taxonomies_desc, sort = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # Parse names returned from NPPES into structured components
# parsed_names <- do.call(rbind, lapply(
#   paste(results_unique$basic_first_name, results_unique$basic_last_name),
#   mysterycall_parse_physician_name
# ))
# 
# low_conf <- parsed_names[parsed_names$confidence == "low", ]
# if (nrow(low_conf)) message(nrow(low_conf), " low-confidence name parses — review manually")
# 
# # Validate phone numbers against the provider's reported state
# phone_checks <- do.call(rbind, Map(
#   mysterycall_validate_phone,
#   phone_str      = results_unique$addresses_telephone_number,
#   practice_state = results_unique$addresses_state
# ))
# 
# table(phone_checks$validity_flag)

## ----eval=FALSE---------------------------------------------------------------
# clinician_data <- mysterycall_get_clinician_data(results_unique)
# 
# roster_enriched <- mysterycall_safe_left_join(
#   left         = results_unique,
#   right        = clinician_data,
#   by           = "npi",
#   label_left   = "npi_search_results",
#   label_right  = "cms_clinician_data",
#   min_coverage = 0.80
# )

