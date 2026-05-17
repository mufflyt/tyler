## ----setup, include=FALSE-----------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = eval_vignettes)
if (eval_vignettes) library(mysterycall)

## ----load-pkg, eval=FALSE-----------------------------------------------------
# library(mysterycall)
# library(dplyr)

## ----show-academic-patterns, eval=FALSE---------------------------------------
# # View all built-in academic patterns (40+ keywords)
# mysterycall_academic_patterns()

## ----show-government-patterns, eval=FALSE-------------------------------------
# # View all built-in government/military patterns
# mysterycall_government_patterns()

## ----extend-patterns, eval=FALSE----------------------------------------------
# # Add "regional medical center" to the academic list without losing any built-ins
# my_academic <- c(mysterycall_academic_patterns(), "regional medical center", "academic health")
# 
# # Add a state health department abbreviation to the government list
# my_gov <- c(mysterycall_government_patterns(), "state health dept", "chc ")
# 
# roster <- roster |>
#   dplyr::mutate(
#     practice_setting = mysterycall_classify_practice_setting(
#       org_name,
#       academic_patterns   = my_academic,
#       government_patterns = my_gov
#     )
#   )

## ----replace-patterns, eval=FALSE---------------------------------------------
# # Replace academic patterns entirely with a minimal set for a corrections study
# corrections_academic <- c("prison hospital", "correctional medical center")
# 
# roster <- roster |>
#   dplyr::mutate(
#     practice_setting = mysterycall_classify_practice_setting(
#       org_name,
#       academic_patterns = corrections_academic   # government_patterns uses built-in default
#     )
#   )

## ----classification-examples, eval=FALSE--------------------------------------
# test_names <- c(
#   "University of Colorado Medical Center",  # Academic: "university" and "medical center" match
#   "VA Medical Center Denver",               # Government: "va " matches before "medical center" is tested
#   "Denver Women's ENT Associates LLC",      # Private Practice: no academic or government match
#   "Johns Hopkins Medicine",                 # Academic: "johns hopkins" in built-in list
#   "Walter Reed National Military Medical Center", # Government: "walter reed" and "military" match
#   "Vanderbilt-Ingram Cancer Center",        # Academic: "vanderbilt" and "cancer center" match
#   "",                                       # Unknown: blank input
#   NA_character_                             # Unknown: NA input
# )
# 
# mysterycall_classify_practice_setting(test_names)
# #> [1] "Academic"         "Government"       "Private Practice"
# #> [4] "Academic"         "Government"       "Academic"
# #> [7] "Unknown"          "Unknown"

## ----na-label, eval=FALSE-----------------------------------------------------
# mysterycall_classify_practice_setting(NA_character_, na_label = NA_character_)
# #> [1] NA

## ----apply-to-roster, eval=FALSE----------------------------------------------
# roster <- roster |>
#   dplyr::mutate(
#     practice_setting = mysterycall_classify_practice_setting(org_name)
#   )
# 
# # Check the distribution — expect Private Practice to dominate
# table(roster$practice_setting, useNA = "always")

## ----load-ruca-crosswalk, eval=FALSE------------------------------------------
# library(readr)
# 
# ruca_crosswalk <- read_csv(
#   "data-raw/ruca2010revised.csv",
#   col_types = cols(
#     ZIP_CODE = col_character(),  # keep leading zeros: "01234" not 1234
#     RUCA1    = col_double()
#   )
# ) |>
#   dplyr::select(zip5 = ZIP_CODE, ruca_code = RUCA1) |>
#   dplyr::distinct(zip5, .keep_all = TRUE)  # one row per ZIP

## ----prepare-zip, eval=FALSE--------------------------------------------------
# roster <- roster |>
#   dplyr::mutate(
#     zip5 = stringr::str_sub(addresses_postal_code, 1L, 5L)
#   )

## ----join-ruca, eval=FALSE----------------------------------------------------
# roster_with_ruca <- mysterycall_safe_left_join(
#   left         = roster,
#   right        = ruca_crosswalk,
#   by           = "zip5",
#   label_left   = "roster",
#   label_right  = "USDA RUCA crosswalk",
#   min_coverage = 0.90   # warn if >10% of providers have unmatched ZIPs
# )

## ----inspect-unmatched, eval=FALSE--------------------------------------------
# unmatched <- dplyr::anti_join(roster, ruca_crosswalk, by = "zip5")
# table(unmatched$addresses_state)

## ----classify-ruca, eval=FALSE------------------------------------------------
# roster_with_ruca <- roster_with_ruca |>
#   dplyr::mutate(
#     geography = mysterycall_classify_ruca(ruca_code)
#   )
# 
# table(roster_with_ruca$geography, useNA = "always")

## ----ruca-as-factor, eval=FALSE-----------------------------------------------
# roster_with_ruca <- roster_with_ruca |>
#   dplyr::mutate(
#     geography_f = mysterycall_classify_ruca(ruca_code, as_factor = TRUE)
#   )
# 
# levels(roster_with_ruca$geography_f)
# #> [1] "Urban"    "Suburban" "Rural"    "Unknown"
# 
# # Now ggplot2 and MASS::polr() will use the correct order automatically

## ----ruca-custom-breaks, eval=FALSE-------------------------------------------
# # Two-tier: Metro (RUCA 1-3) vs Non-Metro (RUCA 4-10)
# roster_with_ruca <- roster_with_ruca |>
#   dplyr::mutate(
#     geography_2tier = mysterycall_classify_ruca(
#       ruca_code,
#       urban_max    = 3,
#       suburban_max = 3,   # no Suburban tier; anything >3 is Rural
#       labels       = c("Metro", "Metro", "Non-Metro"),  # labels[1] and [2] collapse
#       na_label     = "Unknown"
#     )
#   )

## ----ruca-two-tier, eval=FALSE------------------------------------------------
# roster_with_ruca <- roster_with_ruca |>
#   dplyr::mutate(
#     geography_metro = dplyr::case_when(
#       is.na(ruca_code)  ~ "Unknown",
#       ruca_code <= 3    ~ "Metro",
#       TRUE              ~ "Non-Metro"
#     )
#   )

## ----ruca-coverage-check, eval=FALSE------------------------------------------
# n_total    <- nrow(roster_with_ruca)
# n_missing  <- sum(is.na(roster_with_ruca$ruca_code))
# pct_missing <- round(n_missing / n_total * 100, 1)
# 
# message(sprintf(
#   "%d of %d providers (%.1f%%) had no RUCA code (unmatched ZIP).",
#   n_missing, n_total, pct_missing
# ))

## ----assign-region-examples, eval=FALSE---------------------------------------
# states <- c("CO", "Texas", "New York", "california", "Massachusetts", NA)
# 
# # US Census Bureau regions
# mysterycall_assign_region(states, system = "census")
# #> [1] "West"      "South"     "Northeast" "West"      "Northeast" "Unknown"
# 
# # ACOG Districts
# mysterycall_assign_region(states, system = "acog")
# #> [1] "District VIII" "District VII"  "District II"   "District IX"
# #> [5] "District I"    "Unknown"
# 
# # AAO-HNS Districts
# mysterycall_assign_region(states, system = "aao_hns")
# #> [1] "District 7" "District 6" "District 2" "District 8"
# #> [5] "District 1" "Unknown"

## ----apply-region, eval=FALSE-------------------------------------------------
# roster <- roster |>
#   dplyr::mutate(
#     census_region = mysterycall_assign_region(addresses_state, system = "census"),
#     acog_district = mysterycall_assign_region(addresses_state, system = "acog")
#   )
# 
# # Summary table
# table(roster$census_region, useNA = "always")

## ----territories, eval=FALSE--------------------------------------------------
# mysterycall_assign_region(c("PR", "GU", "VI"), system = "acog")
# #> [1] "District IV" "Unknown"     "District IV"   # GU is AAO-HNS only
# 
# mysterycall_assign_region(c("PR", "GU", "VI"), system = "census")
# #> [1] "Unknown" "Unknown" "Unknown"

## ----classify-med-school, eval=FALSE------------------------------------------
# schools <- c(
#   "University of Colorado School of Medicine",   # US_MD
#   "Philadelphia College of Osteopathic Medicine",# US_DO: "osteopathic" matches DO list
#   "McGill University Faculty of Medicine",        # CAN_MD: "mcgill" in Canadian list
#   "Ross University School of Medicine",           # IMG: "ross university" in IMG list
#   "Universidad Nacional Autonoma de Mexico",      # IMG: "universidad" matches IMG list
#   NA_character_                                   # Unknown
# )
# 
# mysterycall_classify_medical_school(schools)
# #> [1] "US_MD"   "US_DO"   "CAN_MD"  "IMG"     "IMG"     "Unknown"
# 
# # Apply to a roster column
# roster <- roster |>
#   dplyr::mutate(
#     med_school_type = mysterycall_classify_medical_school(medical_school_name)
#   )

## ----credential-intersection, eval=FALSE--------------------------------------
# # Correct: classify by school name; use credential only as a secondary check
# roster <- roster |>
#   dplyr::mutate(
#     med_school_type = mysterycall_classify_medical_school(medical_school_name),
#     # Flag potential mismatches: DO credential but school classified as US_MD
#     credential_mismatch = (basic_credential == "DO") & (med_school_type == "US_MD")
#   )
# 
# # If credential_mismatch > 0, investigate: school name may be missing or malformed
# sum(roster$credential_mismatch, na.rm = TRUE)

## ----impute-age, eval=FALSE---------------------------------------------------
# roster <- roster |>
#   dplyr::mutate(
#     age_imputed = mysterycall_impute_age(
#       graduation_year,
#       ref_year   = 2023L,  # pin to your data collection year
#       age_offset = 27L     # validated median age at US med school graduation
#     )
#   )

## ----age-warnings, eval=FALSE-------------------------------------------------
# # Example showing how warnings fire
# mysterycall_impute_age(
#   c(1980, 2010, 2028, 1920),  # 2028 is future; 1920 gives age > 90
#   ref_year   = 2023L,
#   age_offset = 27L
# )
# # Warning: 1 graduation year(s) exceed ref_year (2023) and will produce NA ages.
# # Warning: 1 imputed age(s) outside [25, 90] set to NA. Check graduation years.
# #> [1] 70 40 NA NA

## ----age-category, eval=FALSE-------------------------------------------------
# roster <- roster |>
#   dplyr::mutate(
#     age_category = mysterycall_age_category(
#       age_imputed,
#       breaks    = c(30, 40, 50, 60, 70),  # creates 6 bins
#       as_factor = TRUE   # ordered factor for correct table ordering
#     )
#   )
# 
# levels(roster$age_category)
# #> [1] "<30"     "30-39"   "40-49"   "50-59"   "60-69"   "70+"     "Unknown"
# 
# table(roster$age_category, useNA = "always")

## ----custom-age-breaks, eval=FALSE--------------------------------------------
# roster <- roster |>
#   dplyr::mutate(
#     age_4cat = mysterycall_age_category(
#       age_imputed,
#       breaks = c(40, 50, 60),
#       labels = c("<40", "40-49", "50-59", "60+"),
#       as_factor = TRUE
#     )
#   )

## ----genderize-prep, eval=FALSE-----------------------------------------------
# roster_for_genderize <- roster |>
#   dplyr::rename(first_name = basic_first_name)

## ----genderize-run, eval=FALSE------------------------------------------------
# # Run this interactively ONCE, then save the output
# genderized <- mysterycall_genderize(
#   data_or_path  = roster_for_genderize,
#   output_dir    = "data-processed/genderized",  # cache to a stable directory
#   output_format = "csv"
# )

## ----genderize-threshold, eval=FALSE------------------------------------------
# # Load the cached genderized output
# genderized <- readr::read_csv("data-processed/genderized/genderized_20231015120000_roster.csv")
# 
# # Apply a probability threshold before using the gender prediction
# genderized <- genderized |>
#   dplyr::mutate(
#     gender_api = dplyr::if_else(
#       probability >= 0.90,
#       gender,
#       NA_character_   # treat low-confidence predictions as missing
#     )
#   )

## ----combine-gender, eval=FALSE-----------------------------------------------
# genderized <- genderized |>
#   dplyr::mutate(
#     # Keep NPPES gender when available; fall back to Genderize.io when missing
#     gender_combined = dplyr::case_when(
#       basic_gender %in% c("M", "F")     ~ basic_gender,          # NPPES takes precedence
#       probability >= 0.90 & !is.na(gender) ~ toupper(substr(gender, 1, 1)),  # "male" -> "M"
#       TRUE                              ~ NA_character_
#     )
#   )
# 
# # Summarise coverage improvement
# before <- mean(is.na(roster$basic_gender))
# after  <- mean(is.na(genderized$gender_combined))
# message(sprintf(
#   "Gender coverage: %.1f%% missing before genderize; %.1f%% missing after.",
#   before * 100, after * 100
# ))

## ----gender-precheck, eval=FALSE----------------------------------------------
# # Always inspect before standardising
# table(roster$gender_combined, useNA = "always")
# #>    F    M    NA
# #>  142  155    3
# 
# # If you see anything unexpected (e.g., "non-binary", "unknown", "X"),
# # decide how to handle it before calling prepare_table1_vars().

## ----prepare-table1-example, eval=FALSE---------------------------------------
# analysis_ready <- genderized |>
#   mysterycall_prepare_table1_vars(
#     grad_year_col = "graduation_year",   # will compute age_imputed and age_category
#     gender_col    = "gender_combined",   # will compute gender_std
#     setting_col   = "practice_setting",  # will copy to setting_std
#     region_col    = "census_region",     # will copy to region_std
#     ref_year      = 2023L                # pin to data collection year
#   )
# 
# # New columns added:
# # age_imputed, age_category, gender_std, setting_std, region_std
# dplyr::glimpse(analysis_ready |> dplyr::select(age_imputed, age_category,
#                                                  gender_std, setting_std, region_std))

## ----stratified-sample, eval=FALSE--------------------------------------------
# set.seed(42L)  # for documentation; pass seed= instead in production
# 
# pilot_roster <- mysterycall_stratified_sample(
#   data        = roster,
#   group_col   = "subspecialty",
#   n_per_group = 30L,
#   seed        = 42L
# )
# 
# # Verify balance
# table(pilot_roster$subspecialty)
# #> General Otolaryngology       Head and Neck Oncology   Laryngology
# #>                      30                           30          30
# 
# # Groups returned in full when smaller than n_per_group
# nrow(pilot_roster)  # may be < 30 * n_groups if any group was small

## ----check-duplicates, eval=FALSE---------------------------------------------
# flagged <- mysterycall_check_duplicates(
#   call_log,
#   id_col    = "npi",
#   max_calls = 2L   # each physician should appear at most twice (one per insurance arm)
# )
# 
# # How many unique physicians were over-called?
# attr(flagged, "n_flagged")
# #> [1] 4
# 
# # Inspect the worst offenders
# head(flagged[, c("npi", "physician_name", "call_date", "insurance", "n_calls")])

## ----dedup-keep-first, eval=FALSE---------------------------------------------
# # Keep only the first 2 calls per NPI (chronological order)
# call_log_clean <- call_log |>
#   dplyr::arrange(npi, call_date) |>
#   dplyr::group_by(npi) |>
#   dplyr::slice_head(n = 2L) |>
#   dplyr::ungroup()

## ----dedup-exclude-all, eval=FALSE--------------------------------------------
# flagged_npis <- unique(flagged$npi)
# call_log_clean <- dplyr::filter(call_log, !npi %in% flagged_npis)
# 
# message(sprintf(
#   "Excluded %d physicians (%d calls) from analysis due to over-calling.",
#   length(flagged_npis),
#   sum(call_log$npi %in% flagged_npis)
# ))

## ----build-synthetic-roster, eval=FALSE---------------------------------------
# set.seed(2023L)
# n <- 300L
# 
# subspecialties <- c(
#   "General Otolaryngology", "Laryngology", "Head and Neck Oncology",
#   "Rhinology", "Otology / Neurotology", "Pediatric Otolaryngology"
# )
# 
# org_names <- c(
#   # Academic
#   "University of Colorado Medical Center", "Vanderbilt University Medical Center",
#   "UCSF Department of Otolaryngology", "Ohio State Wexner Medical Center",
#   "Mount Sinai Health System",
#   # Government
#   "VA Medical Center Denver", "Walter Reed National Military Medical Center",
#   "Indian Health Service Hospital",
#   # Private Practice
#   "Denver ENT Associates LLC", "Rocky Mountain Otolaryngology PA",
#   "Midwest ENT Group", "Pacific Ear Nose Throat",
#   "Suburban Otolaryngology Practice", "Southwest ENT Specialists LLC"
# )
# 
# states_vec <- c(
#   "CO", "TX", "CA", "NY", "FL", "OH", "GA", "IL", "NC", "PA",
#   "WA", "AZ", "MA", "MI", "TN", "VA", "MO", "WI", "MN", "OR"
# )
# 
# # Simulate 9-digit ZIP codes (NPPES format)
# zip_base <- c(
#   "80204", "78201", "90001", "10001", "33101",
#   "43201", "30301", "60601", "27601", "19101",
#   "98101", "85001", "02101", "48201", "37201",
#   "22201", "63101", "53201", "55101", "97201"
# )
# 
# roster_raw <- data.frame(
#   npi                  = paste0("1", sprintf("%09d", seq_len(n))),
#   basic_first_name     = sample(c("James", "Emily", "Michael", "Sarah", "David",
#                                    "Jennifer", "Robert", "Lisa", "William", "Karen",
#                                    "Christopher", "Linda", "Matthew", "Jessica",
#                                    "Ashley", "Ryan", "Mia", "Jordan"), n, replace = TRUE),
#   basic_last_name      = paste0("Physician_", seq_len(n)),
#   basic_gender         = sample(c("M", "F", NA_character_), n,
#                                  replace = TRUE, prob = c(0.55, 0.40, 0.05)),
#   basic_credential     = sample(c("MD", "DO", "MD, PhD", "DO, MPH"), n,
#                                  replace = TRUE, prob = c(0.75, 0.15, 0.08, 0.02)),
#   org_name             = sample(org_names, n, replace = TRUE),
#   addresses_state      = sample(states_vec, n, replace = TRUE),
#   addresses_postal_code = paste0(
#     sample(zip_base, n, replace = TRUE),
#     "-",
#     sprintf("%04d", sample(0:9999, n, replace = TRUE))
#   ),
#   taxonomies_desc      = sample(subspecialties, n, replace = TRUE),
#   graduation_year      = sample(1975:2018, n, replace = TRUE),
#   medical_school_name  = sample(c(
#     "University of Colorado School of Medicine",
#     "Philadelphia College of Osteopathic Medicine",
#     "McGill University Faculty of Medicine",
#     "Ross University School of Medicine",
#     "Harvard Medical School",
#     "Stanford University School of Medicine",
#     "Midwestern University Chicago College of Osteopathic Medicine"
#   ), n, replace = TRUE, prob = c(0.35, 0.10, 0.05, 0.05, 0.20, 0.20, 0.05)),
#   stringsAsFactors = FALSE
# )

## ----pipeline-setting, eval=FALSE---------------------------------------------
# roster_raw <- roster_raw |>
#   dplyr::mutate(
#     practice_setting = mysterycall_classify_practice_setting(org_name)
#   )
# 
# table(roster_raw$practice_setting)

## ----pipeline-ruca-join, eval=FALSE-------------------------------------------
# # Extract 5-digit ZIP from NPPES 9-digit format
# roster_raw <- roster_raw |>
#   dplyr::mutate(zip5 = substr(addresses_postal_code, 1L, 5L))
# 
# # Tiny crosswalk for demonstration
# ruca_crosswalk_demo <- data.frame(
#   zip5      = c("80204", "78201", "90001", "10001", "33101",
#                 "43201", "30301", "60601", "27601", "19101",
#                 "98101", "85001", "02101", "48201", "37201",
#                 "22201", "63101", "53201", "55101", "97201"),
#   ruca_code = c(1, 1, 1, 1, 1,
#                 1, 1, 1, 1, 1,
#                 1, 1, 2, 1, 1,
#                 1, 1, 2, 2, 2),
#   stringsAsFactors = FALSE
# )
# 
# roster_with_ruca <- mysterycall_safe_left_join(
#   left         = roster_raw,
#   right        = ruca_crosswalk_demo,
#   by           = "zip5",
#   label_left   = "roster",
#   label_right  = "ruca_crosswalk",
#   min_coverage = 0.80
# )

## ----pipeline-ruca-classify, eval=FALSE---------------------------------------
# roster_with_ruca <- roster_with_ruca |>
#   dplyr::mutate(
#     geography    = mysterycall_classify_ruca(ruca_code),
#     geography_f  = mysterycall_classify_ruca(ruca_code, as_factor = TRUE)
#   )

## ----pipeline-region, eval=FALSE----------------------------------------------
# roster_with_ruca <- roster_with_ruca |>
#   dplyr::mutate(
#     census_region = mysterycall_assign_region(addresses_state, system = "census"),
#     aao_district  = mysterycall_assign_region(addresses_state, system = "aao_hns")
#   )

## ----pipeline-med-school, eval=FALSE------------------------------------------
# roster_with_ruca <- roster_with_ruca |>
#   dplyr::mutate(
#     med_school_type = mysterycall_classify_medical_school(medical_school_name)
#   )
# 
# table(roster_with_ruca$med_school_type)

## ----pipeline-age, eval=FALSE-------------------------------------------------
# roster_with_ruca <- roster_with_ruca |>
#   dplyr::mutate(
#     age_imputed  = mysterycall_impute_age(graduation_year, ref_year = 2023L),
#     age_category = mysterycall_age_category(age_imputed, as_factor = TRUE)
#   )

## ----pipeline-genderize, eval=FALSE-------------------------------------------
# # Step 7a: Prepare first_name column required by mysterycall_genderize()
# roster_for_gender <- roster_with_ruca |>
#   dplyr::rename(first_name = basic_first_name)
# 
# # Step 7b: Query Genderize.io API (run ONCE; cache output)
# # NOTE: Requires internet access; consumes API quota (1000 names/month free tier)
# genderized <- mysterycall_genderize(
#   data_or_path  = roster_for_gender,
#   output_dir    = "data-processed/genderized",
#   output_format = "csv"
# )
# 
# # Step 7c: Apply probability threshold and combine with NPPES gender
# genderized <- genderized |>
#   dplyr::mutate(
#     gender_api = dplyr::if_else(
#       !is.na(probability) & probability >= 0.90,
#       toupper(substr(gender, 1L, 1L)),  # "male" -> "M", "female" -> "F"
#       NA_character_
#     ),
#     # NPPES takes precedence; Genderize fills in missing values only
#     gender_combined = dplyr::case_when(
#       basic_gender %in% c("M", "F")  ~ basic_gender,
#       !is.na(gender_api)             ~ gender_api,
#       TRUE                           ~ NA_character_
#     )
#   ) |>
#   # Restore the original column name before prepare_table1_vars()
#   dplyr::rename(basic_first_name = first_name)

## ----pipeline-table1-prep, eval=FALSE-----------------------------------------
# # If you skipped step 7, substitute genderized with roster_with_ruca
# # and set gender_col = "basic_gender"
# 
# analysis_ready <- genderized |>
#   mysterycall_prepare_table1_vars(
#     grad_year_col = "graduation_year",
#     gender_col    = "gender_combined",
#     setting_col   = "practice_setting",
#     region_col    = "census_region",
#     ref_year      = 2023L
#   )

## ----pipeline-select, eval=FALSE----------------------------------------------
# final_roster <- analysis_ready |>
#   dplyr::select(
#     # Identifiers
#     npi,
#     basic_first_name, basic_last_name,
#     # Practice characteristics
#     taxonomies_desc,
#     setting_std,          # Academic / Government / Private Practice
#     med_school_type,      # US_MD / US_DO / CAN_MD / IMG
#     # Geography
#     addresses_state,
#     census_region,
#     aao_district,
#     geography,            # Urban / Suburban / Rural
#     # Demographics
#     age_imputed,
#     age_category,
#     gender_std            # Male / Female / Unknown
#   )

## ----pipeline-glimpse, eval=FALSE---------------------------------------------
# dplyr::glimpse(final_roster)

## ----pipeline-table1, eval=FALSE----------------------------------------------
# t1 <- mysterycall_table1(
#   final_roster,
#   covariates = c(
#     "gender_std",
#     "age_category",
#     "setting_std",
#     "geography",
#     "census_region",
#     "med_school_type"
#   ),
#   stratify_by = "taxonomies_desc",
#   variable_labels = c(
#     gender_std    = "Sex",
#     age_category  = "Age category",
#     setting_std   = "Practice setting",
#     geography     = "Urban/Rural",
#     census_region = "Census region",
#     med_school_type = "Medical school type"
#   )
# )
# 
# # Print the table
# print(t1)
# 
# # Or access the underlying tibble for knitr::kable() formatting
# knitr::kable(t1$table, caption = "Table 1. Provider characteristics by subspecialty.")

## ----session-info, eval=FALSE-------------------------------------------------
# sessionInfo()

