## ----include = FALSE----------------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
library(magrittr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = eval_vignettes
)

## ----setup, include = FALSE---------------------------------------------------
# if (eval_vignettes) {
#   library(mysterycall)
# }

## ----include = FALSE----------------------------------------------------------
# # Load required packages when evaluating examples
# if (eval_vignettes) {
#   library(mysterycall)
#   library(dplyr)
#   library(tidyr)
#   library(readr)
#   library(npi)
#   library(stringr)
# }

## ----eval=FALSE---------------------------------------------------------------
# obgyn_taxonomy <- mysterycall::taxonomy %>%
#   dplyr::filter(stringr::str_detect(`Classification`, stringr::fixed("GYN", ignore_case = TRUE))) %>%
#   dplyr::select(Code, Specialization)

## ----include=TRUE, message=TRUE, warning=TRUE, echo=TRUE, eval=FALSE----------
# taxonomy_descriptions <- c(
#   "Hospice and Palliative Medicine",
#   "Gynecologic Oncology"
# )
# 
# data <- mysterycall_search_by_taxonomy(taxonomy_to_search = taxonomy_descriptions)

## ----include=TRUE, message=FALSE, warning=FALSE, eval=FALSE-------------------
# # Providers by gender and state
# dplyr::count(data, basic_gender, addresses_state, sort = TRUE)
# 
# # Providers by taxonomy description
# dplyr::count(data, taxonomies_desc, sort = TRUE)
# 
# # Check for multiple matches per NPI (cross-state enrollment)
# dplyr::count(data, npi, sort = TRUE) |> dplyr::filter(n > 1)

## ----eval=FALSE---------------------------------------------------------------
# # OBGYN subspecialty taxonomy descriptions and their short codes
# obgyn_subspecialties <- c(
#   "Obstetrics & Gynecology, Female Pelvic Medicine and Reconstructive Surgery" = "FPM",
#   "Obstetrics & Gynecology, Gynecologic Oncology"                              = "ONC",
#   "Obstetrics & Gynecology, Maternal & Fetal Medicine"                         = "MFM",
#   "Obstetrics & Gynecology, Reproductive Endocrinology"                        = "REI"
# )
# 
# roster_clean <- data |>
#   dplyr::distinct(npi, .keep_all = TRUE) |>
# 
#   # Keep only OBGYN subspecialist rows
#   dplyr::filter(taxonomies_desc %in% names(obgyn_subspecialties)) |>
# 
#   # Standardize fields
#   dplyr::mutate(
#     # 5-digit ZIP only
#     zip5               = stringr::str_sub(addresses_postal_code, 1, 5),
#     # First initial of middle name only
#     basic_middle_name  = stringr::str_sub(basic_middle_name, 1, 1),
#     # Remove punctuation from name fields
#     dplyr::across(
#       c(basic_first_name, basic_last_name, basic_middle_name),
#       ~ stringr::str_remove_all(.x, "[[:punct:]]")
#     ),
#     # Parse enumeration year
#     enumeration_year   = as.integer(
#       format(as.Date(basic_enumeration_date), "%Y")
#     ),
#     # Expand single-letter gender code
#     gender = dplyr::case_match(
#       basic_gender,
#       "F" ~ "Female",
#       "M" ~ "Male",
#       .default = "Unknown"
#     ),
#     # Short subspecialty code via named-vector lookup
#     subspecialty = obgyn_subspecialties[taxonomies_desc]
#   ) |>
# 
#   # Rename to common schema
#   dplyr::rename(
#     first_name = basic_first_name,
#     last_name  = basic_last_name,
#     middle_name = basic_middle_name,
#     city       = addresses_city,
#     state      = addresses_state,
#     phone      = addresses_telephone_number
#   ) |>
# 
#   dplyr::select(npi, first_name, middle_name, last_name,
#                 gender, subspecialty, city, state, zip5,
#                 phone, enumeration_year, taxonomies_desc)

## ----eval=FALSE---------------------------------------------------------------
# phone_checks <- do.call(rbind, Map(
#   mysterycall_validate_phone,
#   phone_str      = roster_clean$phone,
#   practice_state = roster_clean$state
# ))
# 
# table(phone_checks$validity_flag)
# #> valid                  missing   invalid_format
# #> 412                    18            7

