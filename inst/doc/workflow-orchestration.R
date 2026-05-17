## ----setup, include=FALSE-----------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = eval_vignettes)
if (eval_vignettes) library(mysterycall)

## ----load-pkg, eval=FALSE-----------------------------------------------------
# library(mysterycall)

## ----one-call, eval=FALSE-----------------------------------------------------
# result <- mysterycall_run_workflow(
#   taxonomy_terms      = c("207V00000X", "207VB0002X"),
#   name_data           = provider_names_df,
#   phase1_data         = raw_roster,
#   lab_assistant_names = c("Alice", "Bob", "Carol", "Dana", "Eve"),
#   output_directory    = "/network/study/phase1_output",
#   phase2_data         = "/network/study/returned_workbooks",
#   phase2_output_directory = "/network/study/phase2_output",
#   quality_check_path  = "/network/study/qa/quality_table.csv",
#   all_states          = state.abb,
#   verbose             = TRUE
# )

## ----phase1-params, eval=FALSE------------------------------------------------
# result <- mysterycall_clean_phase1(
#   phase1_data      = raw_roster,        # required: raw data frame or file path
#   output_directory = "/study/output",   # required: directory for CSV + audit JSON
#   output_format    = "csv",             # "csv" (default), "rds", or "parquet"
#   verbose          = TRUE,              # print step-by-step progress
#   duplicate_rows   = TRUE,             # TRUE = one row per insurance scenario
#   id_seed          = 20240101L,         # integer seed for reproducible ID generation
#   parent_cohort_hash = NULL             # link to a previous run's hash (optional)
# )

## ----phase1-flag-example, eval=FALSE------------------------------------------
# # How many rows were assigned a generated ID?
# sum(result$processing_flag_generated_id, na.rm = TRUE)
# 
# # Which rows have no last name?
# result[result$processing_flag_no_last_name, c("provider_first_name",
#                                                "provider_last_name_legal_name",
#                                                "npi")]

## ----phase1-attrs, eval=FALSE-------------------------------------------------
# # The cohort hash uniquely identifies this batch
# cohort_id <- attr(result, "cohort_hash")
# cat("Cohort hash:", cohort_id, "\n")
# 
# # The path to the JSON file on disk
# audit_path <- attr(result, "audit_trail_path")
# cat("Audit trail written to:", audit_path, "\n")
# 
# # Read the audit trail back for inspection
# audit <- jsonlite::read_json(audit_path, simplifyVector = TRUE)
# cat("Completeness — NPI:  ", audit$quality_metrics$completeness_npi, "\n")
# cat("Completeness — Phone:", audit$quality_metrics$completeness_phone, "\n")

## ----phase1-error, eval=FALSE-------------------------------------------------
# # This will fail with a clear error message
# bad_data <- data.frame(
#   provider_first_name = "Jane",
#   provider_last_name_legal_name = "Smith",
#   phone = "303-555-0100"
#   # 'npi' column is absent
# )
# 
# mysterycall_clean_phase1(
#   phase1_data      = bad_data,
#   output_directory = tempdir()
# )
# #> Error in mysterycall_clean_phase1():
# #>   Required column 'npi' not found in phase1_data.
# #>   Found columns: provider_first_name, provider_last_name_legal_name, phone
# #>   Please rename or add the 'npi' column before calling this function.

## ----npi-validate, eval=FALSE-------------------------------------------------
# validated <- mysterycall_validate_npi(
#   data       = phase1_result,
#   npi_column = "npi"    # default; change if your column has a different name
# )
# 
# # The function adds a logical column 'npi_valid'
# table(validated$npi_valid, useNA = "ifany")
# #>
# #>  FALSE   TRUE   <NA>
# #>     14   1678      2

## ----npi-dups, eval=FALSE-----------------------------------------------------
# dup_report <- mysterycall_check_duplicates(
#   data       = validated,
#   id_column  = "npi",
#   name_cols  = c("provider_first_name", "provider_last_name_legal_name")
# )
# 
# # Rows where npi_count > 1 are duplicate providers
# dups <- dup_report[dup_report$npi_count > 1, ]
# cat(nrow(dups), "rows are duplicate NPI entries\n")
# 
# # Remove duplicates before splitting
# clean_roster <- validated[!duplicated(validated$npi) | is.na(validated$npi), ]

## ----split-params, eval=FALSE-------------------------------------------------
# workbooks <- mysterycall_split_and_save(
#   data                = clean_roster,
#   lab_assistant_names = c("Alice", "Bob", "Carol", "Dana", "Eve"),
#   output_directory    = "/study/workbooks",
#   split_insurance_order = c("Medicaid", "Private"),
#   seed                = 1978L       # default; controls assignment shuffle
# )

## ----workload-bar, echo=TRUE, eval=TRUE, fig.width=5, fig.height=2.8, fig.cap="Example round-robin workload distribution across five callers for a 198-provider roster. Differences of at most 1 provider guarantee balanced calling burden."----
callers   <- c("Alice", "Bob", "Carol", "Dana", "Eve")
n_total   <- 198L
base_load <- n_total %/% length(callers)
extra     <- n_total %% length(callers)
loads     <- c(rep(base_load + 1L, extra), rep(base_load, length(callers) - extra))

wb_df <- data.frame(
  caller = factor(callers, levels = callers),
  n      = loads
)

ggplot2::ggplot(wb_df, ggplot2::aes(x = caller, y = n, fill = caller)) +
  ggplot2::geom_col(width = 0.6) +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.4, size = 3.5,
                      fontface = "bold") +
  ggplot2::scale_fill_brewer(palette = "Set2", guide = "none") +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)),
                               limits = c(0, NA)) +
  ggplot2::labs(x = NULL, y = "Providers assigned") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

## ----split-verify, eval=FALSE-------------------------------------------------
# # Read all workbooks back and combine
# all_workbooks <- lapply(workbooks$file_paths, readxl::read_excel)
# combined      <- dplyr::bind_rows(all_workbooks)
# 
# # Check 1: same number of rows as the input
# stopifnot(nrow(combined) == nrow(clean_roster))
# 
# # Check 2: no NPI appears in more than one workbook
# npi_counts <- table(combined$npi)
# if (any(npi_counts > 1)) {
#   warning("Some NPIs appear in more than one workbook!")
#   print(npi_counts[npi_counts > 1])
# } else {
#   message("Split verified: every provider appears in exactly one workbook.")
# }

## ----rename-cols, eval=FALSE--------------------------------------------------
# # Standardise column names across all returned workbooks
# renamed <- mysterycall_rename_columns(
#   data           = raw_returned_workbook,
#   target_strings = c("appt", "wait", "insurance", "outcome", "caller"),
#   new_names      = c("appointment_offered", "wait_days",
#                      "insurance_type", "call_outcome", "caller_name")
# )

## ----rename-warning, eval=FALSE-----------------------------------------------
# # If no column contains "wait", you get:
# #> Warning in mysterycall_rename_columns():
# #>   Target substring 'wait' did not match any column name.
# #>   Available columns: npi, provider_last_name_legal_name, appointment_offered, ...
# #>   Proceeding without renaming for this target.

## ----phase2-clean, eval=FALSE-------------------------------------------------
# phase2_result <- mysterycall_clean_phase2(
#   data             = "/study/returned_workbooks",  # directory of Excel files
#   output_directory = "/study/phase2_output",
#   target_strings   = c("appt", "wait", "insurance", "outcome", "caller"),
#   new_names        = c("appointment_offered", "wait_days",
#                        "insurance_type", "call_outcome", "caller_name"),
#   verbose          = TRUE
# )

## ----phase2-full, eval=FALSE--------------------------------------------------
# # Simulate two messy returned workbooks
# alice_wb <- data.frame(
#   NPI          = c("1234567893", "9876543210"),
#   `Last Name`  = c("Smith", "Jones"),
#   `Appt Offered?` = c("Yes", "No"),
#   `Wait (days)` = c(14L, NA_integer_),
#   check.names  = FALSE
# )
# 
# bob_wb <- data.frame(
#   npi            = c("1122334455", "5544332211"),
#   provider_lname = c("Garcia", "Patel"),
#   appointment_offered = c("YES", "offered"),
#   wait_days           = c(7L, 21L)
# )
# 
# # Phase 2 standardises both:
# # After mysterycall_rename_columns() each workbook will have:
# #   npi, provider_last_name_legal_name, appointment_offered, wait_days

## ----quality-params, eval=FALSE-----------------------------------------------
# mysterycall_save_quality_table(
#   data        = phase2_result,
#   output_path = "/study/qa/quality_table.csv",
#   strata_col  = "insurance_type"   # optional: stratify rows by payer
# )

## ----quality-flag, eval=FALSE-------------------------------------------------
# qa <- read.csv("/study/qa/quality_table.csv")
# 
# # Flag callers below 80% completion
# qa$flag <- ifelse(qa$completion_pct < 80, "REVIEW", "OK")
# 
# # Print callers needing review
# knitr::kable(
#   qa[qa$flag == "REVIEW", ],
#   caption = "Callers below 80% completion threshold"
# )

## ----coverage-params, eval=FALSE----------------------------------------------
# not_contacted <- mysterycall_not_contacted_states(
#   data       = phase2_result,
#   all_states = state.abb           # all 50 states, or a custom list
# )

## ----coverage-table, eval=FALSE-----------------------------------------------
# # Quick cross-tabulation of states by contact status
# contacted_states <- unique(phase2_result$state[
#   !is.na(phase2_result$call_outcome)
# ])
# 
# status_vec <- setNames(
#   ifelse(state.abb %in% contacted_states, "Contacted", "Not contacted"),
#   state.abb
# )
# 
# print(table(status_vec))
# #>
# #>  Contacted Not contacted
# #>         43             7

## ----coverage-filter, eval=FALSE----------------------------------------------
# # Which specific states need follow-up?
# cat("States with zero completed calls:\n")
# cat(paste(not_contacted$state, collapse = ", "), "\n")
# #> States with zero completed calls:
# #> AK, MT, ND, SD, VT, WV, WY
# 
# # Subset the roster to those states for a targeted second calling wave
# followup_roster <- phase2_result[
#   phase2_result$state %in% not_contacted$state, ]
# 
# cat(nrow(followup_roster), "providers in under-contacted states\n")

## ----workflow-list, eval=FALSE------------------------------------------------
# names(result)
# #>  [1] "phase1_result"           "validated_npi"
# #>  [3] "workbook_paths"          "phase2_result"
# #>  [5] "quality_table"           "not_contacted_states"
# #>  [7] "workflow_summary"        "cohort_hash"
# #>  [9] "audit_trail_path"        "run_timestamp"

## ----workflow-full, eval=FALSE------------------------------------------------
# result <- mysterycall_run_workflow(
#   taxonomy_terms      = c(
#     "207V00000X",   # Obstetrics & Gynecology
#     "207VB0002X",   # Urogynecology
#     "207VX0201X"    # Gynecologic Oncology
#   ),
#   name_data           = name_crosswalk_df,
#   phase1_data         = raw_nppes_export,
#   lab_assistant_names = c("Alice Nguyen", "Bob Patel",
#                           "Carol Smith", "Dana Lee", "Eve Gonzalez"),
#   output_directory    = "/network/obgyn_study/phase1",
#   phase2_data         = "/network/obgyn_study/returned_workbooks",
#   phase2_output_directory = "/network/obgyn_study/phase2",
#   quality_check_path  = "/network/obgyn_study/qa/quality_check.csv",
#   all_states          = state.abb,
#   verbose             = TRUE
# )

## ----workflow-logged, eval=FALSE----------------------------------------------
# result <- mysterycall_run_workflow_logged(
#   # ... same parameters as mysterycall_run_workflow() ...
#   log_file = "/network/obgyn_study/logs/workflow_run.jsonl"
# )

## ----workflow-summary, eval=FALSE---------------------------------------------
# knitr::kable(
#   result$workflow_summary,
#   caption = "Row counts through the pipeline"
# )

## ----workflow-check, eval=FALSE-----------------------------------------------
# # Programmatic pre-analysis checks
# stopifnot(
#   result$workflow_summary$rows_retained[1] == 100,
#   mean(result$validated_npi$npi_valid, na.rm = TRUE) > 0.95,
#   result$cohort_hash == "a3f8c1d2..."   # paste the hash from your lab notebook
# )

## ----preflight, eval=FALSE----------------------------------------------------
# mysterycall_preflight_check(phase1_data = raw_roster)

## ----large-study, eval=FALSE--------------------------------------------------
# # Recommended settings for large studies
# result <- mysterycall_run_workflow_logged(
#   phase1_data      = large_roster,
#   output_format    = "parquet",   # faster than CSV for > 5,000 rows
#   duplicate_rows   = TRUE,
#   verbose          = FALSE,       # suppress console output in batch scripts
#   log_file         = "/logs/workflow.jsonl"
# )

## ----business-days-scalar, echo=TRUE, eval=TRUE-------------------------------
# How many business days between a call on a Monday and an appointment on
# Friday of the following week?  (Calendar days = 11; business days = 10)
bd <- mysterycall_count_business_days(
  start_date = "2024-03-04",   # Monday
  end_date   = "2024-03-15"    # Friday two weeks later
)
cat("Business days:", bd, "\n")

## ----business-days-df, echo=TRUE, eval=TRUE-----------------------------------
# Apply to a data frame of completed calls
calls_df <- data.frame(
  npi       = paste0("NPI-", 1:6),
  insurance = c("Medicaid", "Private", "Medicaid", "Private", "Medicare", "Uninsured"),
  call_date = as.Date(c("2024-03-04", "2024-03-04", "2024-03-11",
                         "2024-03-11", "2024-03-18", "2024-03-18")),
  appt_date = as.Date(c("2024-03-25", "2024-03-12", "2024-04-01",
                         "2024-03-19", "2024-03-27", "2024-04-08"))
)

# mysterycall_business_days() column params: call_col, appt_col, result_col
calls_with_bd <- mysterycall_business_days(
  data       = calls_df,
  call_col   = "call_date",
  appt_col   = "appt_date",
  result_col = "wait_days"
)

knitr::kable(
  calls_with_bd[, c("npi", "insurance", "call_date", "appt_date", "wait_days")],
  col.names = c("NPI", "Insurance", "Call date", "Appt date", "Wait (business days)"),
  caption   = "Business-day wait times computed by `mysterycall_business_days()`."
)

## ----caller-reliability, echo=TRUE, eval=TRUE---------------------------------
set.seed(7)
# Long-format audit sample: each record appears twice — once per caller
n_records <- 30L
audit_long <- data.frame(
  record_id  = rep(1:n_records, 2),
  caller     = rep(c("Alice", "Bob"), each = n_records),
  appt_offered = c(
    sample(c("Yes", "No"), n_records, replace = TRUE, prob = c(0.72, 0.28)),
    sample(c("Yes", "No"), n_records, replace = TRUE, prob = c(0.68, 0.32))
  )
)

rel <- mysterycall_caller_reliability(
  data        = audit_long,
  caller_col  = "caller",
  outcome_col = "appt_offered",
  type        = "kappa"
)

print(rel)

## ----call-productivity, echo=TRUE, eval=TRUE----------------------------------
set.seed(8)
n_rows <- 80L
prod_df <- data.frame(
  caller           = sample(c("Alice", "Bob", "Carol", "Dana"),
                             n_rows, replace = TRUE),
  call_date        = sample(seq(as.Date("2024-02-01"), as.Date("2024-03-31"),
                                 by = "day"), n_rows, replace = TRUE),
  appointment_offered = sample(c(1L, 1L, 1L, 0L), n_rows, replace = TRUE)
)

prod <- mysterycall_call_productivity(
  data        = prod_df,
  caller_col  = "caller",
  date_col    = "call_date",
  outcome_col = "appointment_offered"
)

knitr::kable(
  prod$summary,
  caption = "Per-caller productivity from `mysterycall_call_productivity()`."
)

