eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  eval = eval_vignettes
)

if (eval_vignettes) {
  library(tyler)
}

# roster <- search_by_taxonomy(
#   "Gynecologic Oncology",
#   states = c("CO", "WY"),
#   write_snapshot = FALSE,
#   notify = FALSE
# ) |>
#   validate_and_remove_invalid_npi()

# clinician_data <- retrieve_clinician_data(roster)
# 
# analysis_base <- roster |>
#   dplyr::left_join(
#     clinician_data,
#     by = "npi",
#     suffix = c("_roster", "_clinician")
#   )

# site_table <- analysis_base |>
#   dplyr::distinct(npi, address, .keep_all = TRUE)

# analysis_with_geo <- analysis_base |>
#   dplyr::left_join(
#     site_geography,
#     by = c("npi", "address")
#   )

# phase2_clean <- clean_phase_2_data("phase2_results.xlsx")
# 
# analysis_ready <- analysis_with_geo |>
#   dplyr::left_join(
#     phase2_clean,
#     by = c("npi", "name")
#   )

tyler::physicians |>
  dplyr::count(subspecialty, sort = TRUE)

# analysis_ready |>
#   dplyr::count(subspecialty, medicaid_accepted)

# table_generate_overall(
#   analysis_ready,
#   vars = c("subspecialty", "medicaid_accepted", "appointment_offered")
# )
