## ----setup, include=FALSE-----------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = eval_vignettes)
if (eval_vignettes) library(mysterycall)

## ----load-pkg, eval=FALSE-----------------------------------------------------
# library(mysterycall)

## ----synth-data, eval=FALSE---------------------------------------------------
# set.seed(2024)
# n <- 200
# 
# analysis_data <- data.frame(
#   # Provider characteristics
#   age_category      = sample(c("< 40", "40-49", "50-59", "60+"),
#                              n, replace = TRUE,
#                              prob = c(0.20, 0.35, 0.30, 0.15)),
#   gender_std        = sample(c("Female", "Male"),
#                              n, replace = TRUE, prob = c(0.55, 0.45)),
#   subspecialty      = sample(
#     c("General OB/GYN", "Urogynecology", "Gynecologic Oncology",
#       "Maternal-Fetal Medicine", "Reproductive Endocrinology"),
#     n, replace = TRUE,
#     prob = c(0.45, 0.15, 0.15, 0.15, 0.10)
#   ),
#   practice_setting  = sample(c("Academic", "Private", "Community Health Center",
#                                "Hospital-Based"),
#                              n, replace = TRUE,
#                              prob = c(0.30, 0.40, 0.15, 0.15)),
#   region_std        = sample(c("Northeast", "Midwest", "South", "West"),
#                              n, replace = TRUE,
#                              prob = c(0.22, 0.26, 0.36, 0.16)),
#   ruca_category     = sample(c("Urban", "Large Rural", "Small Rural", "Isolated"),
#                              n, replace = TRUE,
#                              prob = c(0.75, 0.12, 0.08, 0.05)),
# 
#   # Call outcome variables
#   insurance_type    = sample(c("Medicaid", "Private"),
#                              n, replace = TRUE),
#   appt_offered      = sample(c("Yes", "No", NA_character_),
#                              n, replace = TRUE, prob = c(0.60, 0.35, 0.05)),
#   wait_days         = pmax(0L, as.integer(
#     ifelse(runif(n) < 0.05, NA_integer_,
#            round(rnorm(n, mean = 18, sd = 12)))
#   ))
# )

## ----table1-params, eval=FALSE------------------------------------------------
# tbl1 <- mysterycall_table1(
#   data             = analysis_data,
#   covariates       = c("age_category", "gender_std", "subspecialty",
#                        "practice_setting", "region_std", "ruca_category"),
#   stratify_by      = "insurance_type",
#   include_overall  = TRUE,
#   cont_stats       = "median_iqr",    # "mean_sd", "median_iqr", or c("mean_sd","median_iqr")
#   digits           = 1,               # decimal places for percentages
#   p_value          = TRUE,            # adds chi-sq / Kruskal-Wallis p-value column
#   min_cell         = 5,               # use Fisher's exact test when expected cell < 5
#   variable_labels  = c(
#     age_category     = "Provider age, years",
#     gender_std       = "Physician sex",
#     subspecialty     = "Subspecialty",
#     practice_setting = "Practice setting",
#     region_std       = "US Census region",
#     ruca_category    = "Rurality (RUCA category)"
#   )
# )
# 
# print(tbl1)

## ----table1-type, eval=FALSE--------------------------------------------------
# # Force wait_days to be treated as continuous (it has many unique values
# # but is stored as integer, so auto-detection is correct here)
# # Force age_category to be treated as categorical even though it looks ordinal
# analysis_data$age_category <- factor(
#   analysis_data$age_category,
#   levels = c("< 40", "40-49", "50-59", "60+"),
#   ordered = TRUE
# )

## ----gtsummary-params, eval=FALSE---------------------------------------------
# tbl1_gt <- mysterycall_table1_gtsummary(
#   data         = analysis_data,
#   vars         = c("age_category", "gender_std", "subspecialty",
#                    "practice_setting", "region_std", "ruca_category"),
#   strata_col   = "insurance_type",
#   label_list   = list(
#     age_category     ~ "Provider age, years",
#     gender_std       ~ "Physician sex",
#     subspecialty     ~ "Subspecialty",
#     practice_setting ~ "Practice setting",
#     region_std       ~ "US Census region",
#     ruca_category    ~ "Rurality (RUCA category)"
#   ),
#   missing      = "no",     # "no" = omit missing row; "ifany" = show if any missing
#   percent      = "column", # "column" (default), "row", or "cell"
#   overall_last = TRUE      # place Overall column after strata, not before
# )

## ----gtsummary-chain, eval=FALSE----------------------------------------------
# tbl1_gt |>
#   gtsummary::bold_labels() |>           # bold the variable name rows
#   gtsummary::add_n() |>                 # add total N per stratum
#   gtsummary::add_p() |>                 # add chi-sq / Kruskal-Wallis p-values
#   gtsummary::modify_caption(
#     "**Table 1.** Baseline Characteristics of Sampled Providers by Insurance Type"
#   )

## ----gtsummary-word, eval=FALSE-----------------------------------------------
# tbl1_gt |>
#   gtsummary::bold_labels() |>
#   gtsummary::add_n() |>
#   gtsummary::as_flex_table() |>
#   flextable::save_as_docx(
#     path = "/study/tables/table1_baseline.docx"
#   )

## ----table-overall, eval=FALSE------------------------------------------------
# mysterycall_table_overall(
#   input_file_path   = "/study/phase2_output/analysis_ready.rds",
#   output_directory  = "/study/tables/qa",
#   title             = "Mystery Caller Study — QA Snapshot 2024-03-15",
#   selected_columns  = c("age_category", "gender_std", "subspecialty",
#                         "practice_setting", "region_std", "ruca_category",
#                         "appt_offered", "wait_days"),
#   label_translations = c(
#     age_category     = "Provider age",
#     gender_std       = "Physician sex",
#     appt_offered     = "Appointment offered",
#     wait_days        = "Days until next appointment"
#   )
# )
# #> Writing QA table to /study/tables/qa/qa_snapshot_20240315.pdf
# #> Done. Table has 8 variables and 1688 observations.

## ----pct-table, eval=FALSE----------------------------------------------------
# pct_tbl <- mysterycall_table_percentages(
#   data        = analysis_data[!is.na(analysis_data$appt_offered), ],
#   row_var     = "subspecialty",
#   col_var     = "insurance_type",
#   outcome_var = "appt_offered",
#   outcome_val = "Yes",      # the value that counts as "success"
#   direction   = "row",
#   conf_level  = 0.95,
#   digits      = 1
# )
# 
# knitr::kable(
#   pct_tbl,
#   caption = "Table 2. Appointment Acceptance Rate (%) by Subspecialty and Insurance Type"
# )

## ----prop-table, eval=FALSE---------------------------------------------------
# # Create a wait-time category
# analysis_data$wait_category <- cut(
#   analysis_data$wait_days,
#   breaks  = c(-Inf, 7, 14, 28, Inf),
#   labels  = c("< 1 week", "1-2 weeks", "2-4 weeks", "> 4 weeks"),
#   right   = TRUE
# )
# 
# prop_tbl <- mysterycall_table_proportion(
#   data        = analysis_data[!is.na(analysis_data$wait_days), ],
#   row_var     = "wait_category",
#   col_var     = "subspecialty",
#   conf_level  = 0.95
# )
# 
# knitr::kable(
#   prop_tbl,
#   caption = "Table 3. Wait-Time Category by Subspecialty (N and proportion)"
# )

## ----disparity-kable, eval=FALSE----------------------------------------------
# library(kableExtra)
# 
# # Assume 'disparity_result' is the output of mysterycall_disparities_table()
# disp_tbl <- disparity_result$formatted_table
# 
# # Identify the reference rows (OR == "Ref." or 1.00 with no CI)
# ref_rows <- which(disp_tbl$adjusted_or == "Ref.")
# 
# knitr::kable(
#   disp_tbl,
#   col.names = c("Variable", "Category", "Medicaid %",
#                 "Private %", "Adjusted OR", "95% CI", "p-value"),
#   caption   = "Table 2. Adjusted Odds Ratios for Appointment Acceptance by Insurance Type"
# ) |>
#   kableExtra::kable_styling(full_width = FALSE) |>
#   kableExtra::row_spec(ref_rows, bold = TRUE, italic = TRUE) |>
#   kableExtra::footnote(
#     general = "OR = odds ratio; CI = confidence interval. Reference categories
#     shown in bold italics. Models adjusted for physician sex, subspecialty,
#     practice setting, US Census region, and rurality (RUCA category).",
#     threeparttable = TRUE
#   )

## ----disparity-stars, eval=FALSE----------------------------------------------
# # Extract numeric p-values (assumes a 'p_numeric' column in the result)
# disp_tbl$stars <- dplyr::case_when(
#   disp_tbl$p_numeric < 0.001 ~ "***",
#   disp_tbl$p_numeric < 0.01  ~ "**",
#   disp_tbl$p_numeric < 0.05  ~ "*",
#   TRUE                        ~ ""
# )
# 
# # Combine with formatted p-value for display
# disp_tbl$p_display <- paste0(
#   formatC(disp_tbl$p_numeric, format = "f", digits = 3),
#   disp_tbl$stars
# )

## ----disparity-csv, eval=FALSE------------------------------------------------
# # Export the full disparity table to CSV for the supplement
# write.csv(
#   disp_tbl,
#   file      = "/study/tables/supplement_table_S1_disparity.csv",
#   row.names = FALSE,
#   na        = ""    # journals prefer empty strings to "NA" in CSV supplements
# )
# 
# # Optionally, also write an Excel version for non-R collaborators
# openxlsx::write.xlsx(
#   disp_tbl,
#   file       = "/study/tables/supplement_table_S1_disparity.xlsx",
#   sheetName  = "Disparity Table",
#   colNames   = TRUE,
#   rowNames   = FALSE
# )

## ----pdf-basic, eval=FALSE----------------------------------------------------
# mysterycall_write_table_pdf(
#   object   = tbl1,        # arsenal tableby object from mysterycall_table1()
#   filename = "/study/tables/table1_baseline.pdf"
# )

## ----pdf-tinytex, eval=FALSE--------------------------------------------------
# # Install TinyTeX (one-time setup, ~200 MB download)
# install.packages("tinytex")
# tinytex::install_tinytex()
# 
# # Then re-run the table export
# mysterycall_write_table_pdf(
#   object   = tbl1,
#   filename = "/study/tables/table1_baseline.pdf"
# )

## ----pdf-html-alt, eval=FALSE-------------------------------------------------
# # knitr::kable() does not require LaTeX
# html_table <- knitr::kable(
#   as.data.frame(summary(tbl1, text = "html")),
#   format  = "html",
#   caption = "Table 1. Baseline Characteristics"
# )
# 
# # Save to standalone HTML file
# writeLines(
#   c(
#     "<!DOCTYPE html><html><body>",
#     as.character(html_table),
#     "</body></html>"
#   ),
#   con = "/study/tables/table1_baseline.html"
# )

## ----results-para, eval=FALSE-------------------------------------------------
# paragraph <- mysterycall_write_results_paragraph(
#   model_result    = poisson_model_result,   # output of mysterycall_fit_model()
#   disparity_table = disp_tbl,              # output of mysterycall_disparities_table()
#   outcome_label   = "appointment acceptance",
#   insurance_var   = "insurance_type",
#   ref_level       = "Private",
#   alt_level       = "Medicaid"
# )
# 
# cat(paragraph)

## ----e2e-prep, eval=FALSE-----------------------------------------------------
# # These preparations mirror what mysterycall_prepare_table1_vars() does
# # (run that function on your real data instead)
# analysis_data$age_category <- factor(
#   analysis_data$age_category,
#   levels = c("< 40", "40-49", "50-59", "60+")
# )
# analysis_data$appt_offered_binary <- as.integer(
#   analysis_data$appt_offered == "Yes"
# )

## ----e2e-table1, eval=FALSE---------------------------------------------------
# table1_final <- mysterycall_table1(
#   data            = analysis_data,
#   covariates      = c("age_category", "gender_std", "practice_setting",
#                       "region_std", "ruca_category", "insurance_type"),
#   stratify_by     = "subspecialty",
#   include_overall = TRUE,
#   cont_stats      = "median_iqr",
#   digits          = 1,
#   p_value         = TRUE,
#   min_cell        = 5,
#   variable_labels = c(
#     age_category     = "Provider age, years",
#     gender_std       = "Physician sex",
#     practice_setting = "Practice setting",
#     region_std       = "US Census region",
#     ruca_category    = "Rurality (RUCA category)",
#     insurance_type   = "Insurance type assigned"
#   )
# )
# 
# # Export to PDF
# mysterycall_write_table_pdf(
#   object   = table1_final,
#   filename = "/study/tables/table1_by_subspecialty.pdf"
# )

## ----e2e-table2, eval=FALSE---------------------------------------------------
# # First compute the disparity estimates
# # (assumes mysterycall_fit_model() has already been run — see stats vignette)
# disparity_result <- mysterycall_disparities_table(
#   data          = analysis_data[!is.na(analysis_data$appt_offered), ],
#   outcome_var   = "appt_offered_binary",
#   insurance_var = "insurance_type",
#   ref_level     = "Private",
#   covariates    = c("age_category", "gender_std", "subspecialty",
#                     "practice_setting", "region_std", "ruca_category")
# )
# 
# # Format and print
# knitr::kable(
#   disparity_result$formatted_table,
#   col.names = c("Variable", "Category", "Medicaid % (n)",
#                 "Private % (n)", "Unadjusted OR",
#                 "Adjusted OR", "95% CI", "p-value"),
#   caption   = paste(
#     "Table 2. Appointment Acceptance Rates and Adjusted Odds Ratios",
#     "by Insurance Type Among Obstetrics & Gynecology Providers"
#   )
# ) |>
#   kableExtra::kable_styling(full_width = FALSE) |>
#   kableExtra::footnote(
#     general = paste(
#       "OR = odds ratio; CI = confidence interval.",
#       "Adjusted models include physician sex, subspecialty, practice setting,",
#       "US Census region, and RUCA rurality category as covariates."
#     ),
#     threeparttable = TRUE
#   )
# 
# # Export to CSV for journal supplement
# write.csv(
#   disparity_result$formatted_table,
#   file      = "/study/tables/table2_disparity.csv",
#   row.names = FALSE,
#   na        = ""
# )

## ----e2e-table3, eval=FALSE---------------------------------------------------
# # Summarise wait times by insurance type using the Poisson model summary
# # (assumes mysterycall_fit_wait_model() has been run — see stats vignette)
# wait_summary <- analysis_data |>
#   dplyr::filter(!is.na(wait_days)) |>
#   dplyr::group_by(insurance_type) |>
#   dplyr::summarise(
#     n            = dplyr::n(),
#     median_days  = median(wait_days),
#     q1           = quantile(wait_days, 0.25),
#     q3           = quantile(wait_days, 0.75),
#     pct_gt28days = round(mean(wait_days > 28) * 100, 1),
#     .groups      = "drop"
#   ) |>
#   dplyr::mutate(
#     median_iqr = paste0(median_days, " [", q1, "–", q3, "]")
#   )
# 
# knitr::kable(
#   wait_summary[, c("insurance_type", "n", "median_iqr", "pct_gt28days")],
#   col.names = c("Insurance Type", "N", "Median days [IQR]", "% > 28 days"),
#   caption   = paste(
#     "Table 3. Days to Next Available Appointment by Insurance Type",
#     "(Completed Calls with Appointment Offered)"
#   )
# )

## ----e2e-pdf, eval=FALSE------------------------------------------------------
# # Table 1 was already exported in Step 1.
# # Export Table 2 (disparity) to PDF:
# mysterycall_write_table_pdf(
#   object   = disparity_result$arsenal_object,
#   filename = "/study/tables/table2_disparity.pdf"
# )
# 
# # Table 3 (wait times) is a simple data frame, not an arsenal object;
# # use knitr → rmarkdown → PDF for it:
# rmarkdown::render(
#   input       = "/study/scripts/table3_waittimes.Rmd",
#   output_file = "/study/tables/table3_waittimes.pdf",
#   params      = list(data_path = "/study/phase2_output/analysis_ready.rds")
# )
# 
# message(
#   "All three tables exported.\n",
#   "  Table 1: /study/tables/table1_by_subspecialty.pdf\n",
#   "  Table 2: /study/tables/table2_disparity.pdf\n",
#   "  Table 3: /study/tables/table3_waittimes.pdf\n",
#   "  Table 2 CSV supplement: /study/tables/table2_disparity.csv"
# )

## ----max-min-table, echo=TRUE, eval=TRUE--------------------------------------
set.seed(3)
subspecialties <- sample(
  c("General OB/GYN", "MFM", "GYN Oncology", "REI", "FPMRS", "General OB/GYN",
    "General OB/GYN", "MFM", "General OB/GYN", "REI"),
  size = 120, replace = TRUE
)

# Most common subspecialty
cat("Most common:\n")
print(mysterycall_max_table(subspecialties))

# Least common subspecialty
cat("\nLeast common:\n")
print(mysterycall_min_table(subspecialties))

## ----format-pct, echo=TRUE, eval=TRUE-----------------------------------------
rates <- c(0.91, 0.638, 0.54, 0.00, 1.00, 0.085)
knitr::kable(
  data.frame(
    raw_rate     = rates,
    formatted_1d = mysterycall_format_pct(rates, digits = 1),
    formatted_0d = mysterycall_format_pct(rates, digits = 0)
  ),
  col.names = c("Raw proportion", "1 decimal place", "0 decimal places"),
  caption   = "`mysterycall_format_pct()` output for typical acceptance rates."
)

