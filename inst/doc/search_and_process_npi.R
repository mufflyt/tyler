eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
library(magrittr)
knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  eval = eval_vignettes
)

if (eval_vignettes) {
  library(tyler)
}

sample_names <- tibble::tibble(
  first = c("Jane", "Maria", "Alex"),
  last = c("Smith", "Garcia", "Johnson")
)

sample_names

# results <- search_and_process_npi(
#   data = sample_names,
#   notify = FALSE
# )

# results <- search_and_process_npi(
#   data = sample_names,
#   enumeration_type = "ind",
#   limit = 10,
#   country_code = "US",
#   filter_credentials = c("MD", "DO"),
#   notify = FALSE
# )

# results <- search_and_process_npi(
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

# dplyr::glimpse(results)
# 
# results |>
#   dplyr::count(search_term, sort = TRUE)

# results_unique <- results |>
#   dplyr::distinct(npi, .keep_all = TRUE)
# 
# results_unique |>
#   dplyr::count(taxonomies_desc, sort = TRUE)

# clinician_data <- retrieve_clinician_data(results_unique)
