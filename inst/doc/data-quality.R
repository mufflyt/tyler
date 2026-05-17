## ----setup, include = FALSE---------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  eval      = eval_vignettes
)
library(mysterycall)

## ----phone-basic--------------------------------------------------------------
# # A well-formed Colorado number
# mysterycall_validate_phone("(303) 555-1234")
# 
# # Missing number
# mysterycall_validate_phone(NA_character_)
# 
# # Non-NANP format (too few digits)
# mysterycall_validate_phone("555-1234")

## ----phone-state--------------------------------------------------------------
# # Colorado provider with a New York area code
# mysterycall_validate_phone("(212) 555-0199", practice_state = "CO")

## ----phone-roster-------------------------------------------------------------
# roster <- data.frame(
#   npi            = c("1234567890", "0987654321", "1111111111"),
#   phone          = c("(720) 555-0101", "999-0000", "(303) 867-5309"),
#   practice_state = c("CO",            "CO",        "CO"),
#   stringsAsFactors = FALSE
# )
# 
# # vectorised: one call per row, returns a data frame
# phone_results <- do.call(rbind, Map(
#   mysterycall_validate_phone,
#   phone_str      = roster$phone,
#   practice_state = roster$practice_state
# ))
# 
# cbind(roster["npi"], phone_results[, c("phone_e164_valid", "phone_validity_flag")])

## ----parse-single-------------------------------------------------------------
# mysterycall_parse_physician_name("Smith, Jane, Jr.")

## ----parse-vector-------------------------------------------------------------
# raw_names <- c(
#   "Maria de la Cruz",
#   "Smith, John, Jr.",
#   "Robert Smith DO",
#   "Linda Do",
#   NA_character_
# )
# 
# parsed <- mysterycall_parse_physician_name(raw_names)
# parsed[, c("first_name", "last_name", "suffix", "parse_confidence", "parse_warnings")]

## ----parse-physicians---------------------------------------------------------
# # Parse the 4,659 physician names from the bundled roster
# parsed_physicians <- mysterycall_parse_physician_name(
#   mysterycall::physicians$name
# )
# 
# # Confidence breakdown
# table(parsed_physicians$parse_confidence)

## ----parse-validate-----------------------------------------------------------
# validated <- mysterycall_validate_parsed_names(parsed)
# 
# # Overview of quality issues
# table(validated$quality_issue, useNA = "ifany")
# 
# # Rows that are not valid (missing first or last name)
# validated[!validated$is_valid, c("first_name", "last_name",
#                                   "parse_confidence", "quality_issue")]

## ----parse-format-------------------------------------------------------------
# mysterycall_format_physician_name(
#   first_name     = "Jane",
#   last_name      = "Smith",
#   middle_name    = "A.",
#   suffix         = "MD",
#   format         = "last_first",
#   include_suffix = TRUE
# )
# 
# # Vectorised
# mysterycall_format_physician_name(
#   first_name  = c("John",  "Maria"),
#   last_name   = c("Smith", "Cruz"),
#   suffix      = c("MD",    NA),
#   format      = "first_last"
# )

## ----parse-test---------------------------------------------------------------
# tbl <- mysterycall_test_name_parser()
# cat(sprintf("%d / %d high-confidence valid parses\n",
#             sum(tbl$is_valid & tbl$parse_confidence == "high"),
#             nrow(tbl)))

## ----join-left----------------------------------------------------------------
# calls <- data.frame(
#   npi  = c("1111111111", "2222222222", "3333333333"),
#   date = as.Date(c("2025-01-10", "2025-01-11", "2025-01-12")),
#   stringsAsFactors = FALSE
# )
# 
# demographics <- data.frame(
#   npi    = c("1111111111", "2222222222"),
#   gender = c("Female", "Male"),
#   stringsAsFactors = FALSE
# )
# 
# # 2 of 3 left rows match → coverage = 66.7 % < 95 % threshold → error
# tryCatch(
#   suppressMessages(mysterycall_safe_left_join(
#     left         = calls,
#     right        = demographics,
#     by           = "npi",
#     label_left   = "call_log",
#     label_right  = "demographics",
#     min_coverage = 0.95
#   )),
#   error = function(e) message("Caught: ", conditionMessage(e))
# )
# 
# # Lower threshold to match the actual coverage
# result <- suppressMessages(mysterycall_safe_left_join(
#   left         = calls,
#   right        = demographics,
#   by           = "npi",
#   label_left   = "call_log",
#   label_right  = "demographics",
#   min_coverage = 0.60
# ))
# result

## ----join-types---------------------------------------------------------------
# left2  <- data.frame(npi = 1111111111L, stringsAsFactors = FALSE)
# right2 <- data.frame(npi = "1111111111", x = 42L, stringsAsFactors = FALSE)
# 
# suppressMessages(mysterycall_safe_left_join(
#   left2, right2, by = "npi",
#   label_left  = "left",
#   label_right = "right",
#   min_coverage = 0
# ))

## ----join-assert--------------------------------------------------------------
# dupes <- data.frame(
#   npi    = c("1111111111", "1111111111"),
#   source = c("NPPES", "CMS"),
#   stringsAsFactors = FALSE
# )
# 
# # Error: found 1 duplicate group
# tryCatch(
#   mysterycall_assert_unique_keys(dupes, key_cols = "npi", label = "demographics"),
#   error = function(e) message("Caught: ", conditionMessage(e))
# )
# 
# # Pass dedupe = TRUE to drop duplicates instead:
# clean <- suppressMessages(
#   mysterycall_assert_unique_keys(dupes, key_cols = "npi",
#                                   label = "demographics", dedupe = TRUE)
# )
# nrow(clean)

## ----join-variants------------------------------------------------------------
# # Inner join — only matched rows; defaults to min_coverage = 0.90
# result_inner <- suppressMessages(mysterycall_safe_inner_join(
#   left        = calls,
#   right       = demographics,
#   by          = "npi",
#   label_left  = "call_log",
#   label_right = "demographics",
#   min_coverage = 0.60
# ))
# nrow(result_inner)   # 2 rows: only matched NPIs
# 
# # Semi join — keep left rows that match; no right columns added
# matched_calls <- suppressMessages(mysterycall_safe_semi_join(
#   left        = calls,
#   right       = demographics,
#   by          = "npi",
#   label_left  = "call_log",
#   label_right = "demographics",
#   min_coverage = 0.60
# ))
# matched_calls   # 2 rows, same columns as calls
# 
# # Anti join — keep left rows with NO match
# unmatched_calls <- suppressMessages(mysterycall_safe_anti_join(
#   left        = calls,
#   right       = demographics,
#   by          = "npi",
#   label_left  = "call_log",
#   label_right = "demographics"
# ))
# unmatched_calls   # 1 row: NPI 3333333333

## ----pipeline-----------------------------------------------------------------
# library(dplyr)
# 
# # ── 1. Parse physician names ──────────────────────────────────────────────────
# roster <- mysterycall::physicians |>
#   select(NPI, name, subspecialty)
# 
# parsed_names <- mysterycall_parse_physician_name(roster$name)
# roster <- bind_cols(roster, parsed_names)
# 
# low_conf <- filter(roster, parse_confidence == "low")
# message(sprintf("%d name(s) flagged as low-confidence (%.1f%%)",
#                 nrow(low_conf), 100 * nrow(low_conf) / nrow(roster)))
# 
# # ── 2. Validate parsed names ──────────────────────────────────────────────────
# validated_names <- mysterycall_validate_parsed_names(parsed_names)
# issues <- filter(validated_names, !is.na(quality_issue))
# message(sprintf("%d name quality issue(s) detected:", nrow(issues)))
# print(table(issues$quality_issue))
# 
# # ── 3. Illustrate a safe join with a synthetic demographics sidecar ───────────
# # In a real study this would be CMS Care Compare or similar
# demographics_sidecar <- data.frame(
#   NPI    = roster$NPI[1:4000],       # 4,000 of 4,659 have demographics
#   gender = sample(c("Female", "Male", "Unknown"),
#                   4000, replace = TRUE, prob = c(.7, .28, .02)),
#   stringsAsFactors = FALSE
# )
# 
# roster_enriched <- suppressMessages(
#   mysterycall_safe_left_join(
#     left         = roster,
#     right        = demographics_sidecar,
#     by           = "NPI",
#     label_left   = "physicians",
#     label_right  = "demographics_sidecar",
#     min_coverage = 0.85   # expect ≥ 85 % match rate
#   )
# )
# 
# # Coverage summary
# cat(sprintf(
#   "Roster rows: %d  |  Enriched rows: %d  |  Coverage: %.1f%%\n",
#   nrow(roster), nrow(roster_enriched),
#   100 * mean(!is.na(roster_enriched$gender))
# ))

