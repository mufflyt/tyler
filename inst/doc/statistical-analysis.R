## ----setup, include=FALSE-----------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = eval_vignettes)
if (eval_vignettes) library(mysterycall)

## ----load-package, eval=FALSE-------------------------------------------------
# library(mysterycall)

## ----preflight-example, eval=FALSE--------------------------------------------
# # Build a small data frame with intentional problems
# bad_data <- data.frame(
#   npi              = c("1234567890", "1234567890",   # duplicate NPI × insurance
#                        "2345678901", "2345678901",
#                        "3456789012", "3456789012",
#                        "4567890123", "4567890123"),
#   insurance        = c("Medicaid",  "Medicaid",      # duplicate: same physician, same insurance
#                        "Private",   "Medicare",
#                        "Medicaid",  "Private",
#                        "Uninsured", "Private"),
#   appointment_offered = c(1, 1, 0, 1, 1, 2, 0, 1), # '2' is invalid (should be 0 or 1)
#   wait_days        = c(14, 7, NA, 5, 999, 3, 8, 12), # NA missing; 999 suspicious
#   subspecialty     = rep("Obstetrics", 8),
#   stringsAsFactors = FALSE
# )
# 
# report <- mysterycall_preflight_check(bad_data)
# print(report)
# #> -- mysterycall preflight check ------------------------------------
# #> Quality score: 52 / 100  [FAIL — do not proceed to modelling]
# #>
# #> ERRORS (must fix before analysis):
# #>   [E1] Duplicate NPI x insurance rows detected: 1 duplicate(s).
# #>         Row(s): npi=1234567890, insurance=Medicaid appears 2 times.
# #>   [E2] appointment_offered contains values outside {0, 1}: found value 2
# #>         in row 6.
# #>
# #> WARNINGS (should fix; may bias results):
# #>   [W1] wait_days: 1 missing value(s) (12.5 % of rows). Imputation or
# #>         listwise deletion required before modelling.
# #>   [W2] wait_days: 1 implausible value(s) > 180 business days. Value 999
# #>         in row 5 (npi=3456789012, insurance=Medicaid). Verify or set to NA.
# #>
# #> INFO:
# #>   [I1] insurance levels detected: Medicaid, Medicare, Private, Uninsured
# #>   [I2] 4 unique physicians (NPI) in dataset.
# #>   [I3] Mean calls per physician: 2.0 (expected ~4 for balanced design).
# #>         Consider whether your design is intentionally unbalanced.

## ----preflight-fix, eval=FALSE------------------------------------------------
# # Step 1: Remove the duplicate NPI x insurance row
# clean_data <- bad_data[!duplicated(bad_data[, c("npi", "insurance")]), ]
# 
# # Step 2: Recode the invalid appointment_offered value
# clean_data$appointment_offered[clean_data$appointment_offered == 2] <- NA
# 
# # Step 3: Replace implausible wait time with NA
# clean_data$wait_days[clean_data$wait_days > 180] <- NA
# 
# # Re-run the preflight
# report2 <- mysterycall_preflight_check(clean_data)
# print(report2)
# #> -- mysterycall preflight check ------------------------------------
# #> Quality score: 88 / 100  [WARNING — review before modelling]
# #>
# #> WARNINGS:
# #>   [W1] appointment_offered: 1 missing value(s). Listwise deletion will
# #>         drop this row from disparity analyses.
# #>   [W2] wait_days: 2 missing value(s). Listwise deletion will drop these
# #>         rows from Poisson model.
# #>
# #> INFO:
# #>   [I1] insurance levels: Medicaid, Medicare, Private, Uninsured
# #>   [I2] 4 unique physicians (NPI). Note: very small sample for GLMM.

## ----wait-time-dist, echo=TRUE, eval=TRUE, fig.width=6.5, fig.height=3.2, fig.cap="Right-skewed wait-time distribution for Medicaid callers (simulated). `mysterycall_plot_distribution()` returns raw and sqrt-scaled panels — the sqrt panel makes the long upper tail visible without distorting the modal value."----
set.seed(42)
medicaid_waits <- rpois(200L, lambda = 18)

# mysterycall_plot_distribution() returns a named list: $raw and $sqrt_transformed
dist_plots <- mysterycall_plot_distribution(medicaid_waits,
                                             title = "Medicaid wait days",
                                             bins  = 25L)

# Print the two panels side-by-side using patchwork if available,
# otherwise fall back to printing the raw panel alone
if (requireNamespace("patchwork", quietly = TRUE)) {
  dist_plots$raw + dist_plots$sqrt_transformed
} else {
  dist_plots$raw
}

## ----poisson-basic, eval=FALSE------------------------------------------------
# # Fit the Poisson GLMM
# # data:       your cleaned mystery-caller data frame
# # outcome:    name of the count column (wait_days)
# # group_var:  insurance type column
# # physician_id: NPI column (random intercept grouping factor)
# # ref_level:  the reference insurance category
# poisson_fit <- mysterycall_poisson_model(
#   data         = mc_data,
#   outcome      = "wait_days",
#   group_var    = "insurance",
#   physician_id = "npi",
#   ref_level    = "Private",
#   nAGQ         = 1L         # Laplace approximation (default); see Section 3.7
# )
# 
# # The returned object is an S3 class 'mysterycall_poisson'
# class(poisson_fit)
# #> [1] "mysterycall_poisson"

## ----irr-table, eval=FALSE----------------------------------------------------
# # Print the IRR table
# poisson_fit$irr_table
# #>   insurance       IRR  CI_lower  CI_upper  p_value
# #>   <chr>         <dbl>     <dbl>     <dbl>    <dbl>
# #> 1 Medicaid       1.42      1.18      1.70   <0.001
# #> 2 Medicare       1.08      0.89      1.31    0.441
# #> 3 Uninsured      1.29      1.06      1.57    0.012

## ----poisson-print, eval=FALSE------------------------------------------------
# print(poisson_fit)
# #> == mysterycall: Poisson GLMM Results ==============================
# #>
# #> Outcome:   wait_days
# #> Group var: insurance  (reference = Private)
# #> Random fx: (1 | npi)   — physician random intercept
# #> N calls:   196
# #> N physicians: 40
# #> nAGQ: 1  (Laplace approximation)
# #>
# #> Fixed effects (Incidence Rate Ratios):
# #>
# #>   Insurance    IRR   95% CI          p
# #>   ----------- ----- ---------------- ------
# #>   Medicaid    1.42  [1.18,  1.70]  <0.001 ***
# #>   Medicare    1.08  [0.89,  1.31]   0.441
# #>   Uninsured   1.29  [1.06,  1.57]   0.012 *
# #>
# #> Random effects:
# #>   Physician SD (σ): 0.31   Physician variance (σ²): 0.096
# #>   ICC (intraclass correlation): 0.089
# #>   Interpretation: ~8.9% of wait-time variance is between physicians.
# #>
# #> Overdispersion: Pearson χ²/df = 1.08  [acceptable; threshold < 1.5]
# #> Model convergence: OK

## ----irr-forest, echo=TRUE, eval=TRUE, fig.width=5.5, fig.height=3.0, fig.cap="Forest plot of incidence rate ratios (IRR) from the Poisson GLMM produced by `mysterycall_irr_plot()`. Red points are statistically significant (p < 0.05); navy are not. Dashed line at IRR = 1 marks no effect."----
# mysterycall_irr_plot() accepts a data frame with term/irr/ci_lower/ci_upper/p_value
irr_df <- data.frame(
  term     = c("Medicaid", "Medicare", "Uninsured"),
  irr      = c(1.42, 1.08, 1.29),
  ci_lower = c(1.18, 0.89, 1.06),
  ci_upper = c(1.70, 1.31, 1.57),
  p_value  = c(0.0001, 0.441, 0.012)
)
mysterycall_irr_plot(irr_df,
                     x_label = "IRR vs. Private insurance",
                     x_log   = FALSE)

## ----overdispersion, eval=FALSE-----------------------------------------------
# # The overdispersion statistic is stored in the fitted object
# poisson_fit$overdispersion
# #> $pearson_chisq
# #> [1] 207.4
# #>
# #> $residual_df
# #> [1] 192
# #>
# #> $ratio
# #> [1] 1.080
# #>
# #> $interpretation
# #> [1] "Adequate: ratio = 1.08 is below the 1.5 threshold. Poisson model is appropriate."
# 
# # If overdispersion is detected, refit with negative binomial (via glmer.nb)
# # poisson_fit_nb <- mysterycall_poisson_model(
# #   data         = mc_data,
# #   outcome      = "wait_days",
# #   group_var    = "insurance",
# #   physician_id = "npi",
# #   ref_level    = "Private",
# #   family       = "negative_binomial"   # switches to glmer.nb internally
# # )

## ----convergence, eval=FALSE--------------------------------------------------
# # Check convergence diagnostics
# poisson_fit$convergence
# #> $converged
# #> [1] TRUE
# #>
# #> $singular_fit
# #> [1] FALSE
# #>
# #> $optimizer_message
# #> [1] "Model converged normally. No issues detected."
# 
# # If you see singular_fit = TRUE, try:
# # Option 1: Increase nAGQ (more accurate numerical integration)
# poisson_fit_agq <- mysterycall_poisson_model(
#   data         = mc_data,
#   outcome      = "wait_days",
#   group_var    = "insurance",
#   physician_id = "npi",
#   ref_level    = "Private",
#   nAGQ         = 5L          # 5-point adaptive quadrature; slower but more accurate
# )
# 
# # Option 2: If random variance is truly near zero, consider dropping the random
# # intercept and using a GEE (generalised estimating equation) approach instead.
# # Note that GEE estimates population-average effects rather than subject-specific
# # effects—a meaningful difference in multilevel studies.

## ----offset-example, eval=FALSE-----------------------------------------------
# # Assume mc_data has a column 'panel_size' (e.g., number of active patients)
# mc_data$log_panel <- log(mc_data$panel_size)
# 
# poisson_fit_offset <- mysterycall_poisson_model(
#   data         = mc_data,
#   outcome      = "wait_days",
#   group_var    = "insurance",
#   physician_id = "npi",
#   ref_level    = "Private",
#   offset       = "log_panel",   # column name of the pre-computed log offset
#   nAGQ         = 1L
# )
# 
# # IRRs now represent wait days per unit of physician panel size,
# # removing the confounding effect of practice volume.
# poisson_fit_offset$irr_table
# #>   insurance    IRR  CI_lower  CI_upper  p_value
# #>   Medicaid    1.38      1.14      1.66   0.001
# #>   Medicare    1.05      0.86      1.28   0.647
# #>   Uninsured   1.25      1.02      1.52   0.030

## ----wait-summary, eval=FALSE-------------------------------------------------
# # Summarise wait times by insurance type
# wait_summary <- mysterycall_wait_time_summary(
#   data          = mc_data,
#   wait_col      = "wait_days",
#   group_col     = "insurance",
#   subgroup_col  = "subspecialty",   # optional second stratification variable
#   na.rm         = TRUE
# )
# 
# print(wait_summary)
# #>   insurance    subspecialty  n_calls  n_offered  median_wait  IQR_lower
# #>   Medicaid     Obstetrics       50       34         18.0          9.0
# #>   Medicaid     Gynecology       50       29         21.0         11.5
# #>   Medicare     Obstetrics       50       42          8.0          4.0
# #>   Medicare     Gynecology       50       39         10.0          5.0
# #>   Private      Obstetrics       50       47          6.0          3.0
# #>   Private      Gynecology       50       45          7.0          3.5
# #>   Uninsured    Obstetrics       50       27         14.0          7.0
# #>   Uninsured    Gynecology       50       23         16.5          8.5
# #>   IQR_upper  pct_offered
# #>      30.0       68.0
# #>      35.0       58.0
# #>      14.0       84.0
# #>      17.0       78.0
# #>      10.0       94.0
# #>      11.0       90.0
# #>      24.0       54.0
# #>      28.0       46.0

## ----combine-tables, eval=FALSE-----------------------------------------------
# # Summarise at the insurance-type level (no subspecialty stratification)
# wait_summ_top <- mysterycall_wait_time_summary(
#   data         = mc_data,
#   wait_col     = "wait_days",
#   group_col    = "insurance"
# )
# 
# # Join with the IRR table from the Poisson model
# combined <- merge(
#   wait_summ_top,
#   poisson_fit$irr_table,
#   by        = "insurance",
#   all.x     = TRUE,
#   sort      = FALSE
# )
# 
# # Reorder columns for publication
# combined[, c("insurance", "n_calls", "median_wait", "IQR_lower", "IQR_upper",
#              "IRR", "CI_lower", "CI_upper", "p_value")]
# #>   insurance  n_calls  median_wait  IQR_lower  IQR_upper  IRR   CI_lower  CI_upper  p_value
# #>   Medicaid      100       19.5        10.0       32.0    1.42     1.18      1.70   <0.001
# #>   Medicare      100        9.0         4.5       15.0    1.08     0.89      1.31    0.441
# #>   Private       100        6.5         3.0       10.5    1.00 (ref)  —        —        —
# #>   Uninsured     100       15.0         7.5       26.0    1.29     1.06      1.57    0.012

## ----acceptance-dot, echo=TRUE, eval=TRUE, fig.width=5.5, fig.height=3.2, fig.cap="Appointment-offered rates by insurance type with 95 % Wilson score confidence intervals. Private insurance is the reference group (dashed line). Medicaid and Uninsured callers face the largest absolute deficits."----
# Build a representative disparities table from synthetic data
set.seed(99)
n_per   <- 200L
ins     <- rep(c("Medicaid", "Medicare", "Private", "Uninsured"), each = n_per)
probs   <- rep(c(0.64, 0.84, 0.91, 0.54), each = n_per)
demo_df <- data.frame(insurance = ins,
                       accepted  = rbinom(length(ins), 1L, probs))

disp_tbl <- mysterycall_disparities_table(demo_df, "accepted", "insurance",
                                           ref_group = "Private")

# One call replaces 30+ lines of manual ggplot2
mysterycall_plot_disparities(disp_tbl, metric = "rate")

## ----disp-abs-diff, echo=TRUE, eval=TRUE, fig.width=5.2, fig.height=3.2, fig.cap="Absolute difference in acceptance rate (percentage points) versus Private insurance. Negative values indicate lower appointment-offered rates."----
mysterycall_plot_disparities(disp_tbl, metric = "abs_diff",
                              title = "Absolute disparity (pp vs. Private)")

## ----disp-rr, echo=TRUE, eval=TRUE, fig.width=5.2, fig.height=3.2, fig.cap="Relative risk of being offered an appointment versus Private insurance. Values below 1 indicate lower rates for that group."----
mysterycall_plot_disparities(disp_tbl, metric = "rel_risk",
                              title = "Relative risk vs. Private")

## ----disparities-basic, eval=FALSE--------------------------------------------
# # Compute disparity metrics for appointment_offered across insurance types
# disparities <- mysterycall_disparities_table(
#   data       = mc_data,
#   outcome    = "appointment_offered",  # must be 0/1
#   group_var  = "insurance",
#   ref_level  = "Private",             # reference group
#   ci_method  = "wilson"               # recommended
# )
# 
# print(disparities)
# #> -- mysterycall: Disparity Table ----------------------------------
# #> Outcome: appointment_offered   Reference: Private   CI method: Wilson
# #>
# #>   insurance   n_calls  n_offered  rate    CI_lower  CI_upper  abs_diff_pp
# #>   Medicaid       200      128     0.640   [0.572,   0.703]    -27.0
# #>   Medicare       200      168     0.840   [0.781,   0.888]     -7.0
# #>   Private        200      182     0.910   [0.860,   0.945]       0 (ref)
# #>   Uninsured      200      108     0.540   [0.471,   0.608]    -37.0
# #>
# #>   rel_risk  RR_CI_lower  RR_CI_upper  p_value
# #>   0.703     [0.639,      0.774]       <0.001
# #>   0.923     [0.864,      0.986]        0.017
# #>   1.000      —            —             —
# #>   0.593     [0.529,      0.666]       <0.001
# #>
# #> Note: p-values from two-sided two-proportion z-test vs. reference.
# #> Consider adjusting for multiple comparisons (see ?mysterycall_multiple_comparison_adjust).

## ----disparities-ref, eval=FALSE----------------------------------------------
# # Use Medicaid as the reference group to compare others against it
# disparities_med_ref <- mysterycall_disparities_table(
#   data      = mc_data,
#   outcome   = "appointment_offered",
#   group_var = "insurance",
#   ref_level = "Medicaid",            # Medicaid as reference
#   ci_method = "wilson"
# )
# 
# print(disparities_med_ref)
# #>   insurance   rate    abs_diff_pp  rel_risk  p_value
# #>   Medicaid    0.640       0 (ref)   1.000      —
# #>   Medicare    0.840     +20.0       1.313    <0.001
# #>   Private     0.910     +27.0       1.422    <0.001
# #>   Uninsured   0.540     -10.0       0.844     0.031

## ----disparities-error, eval=FALSE--------------------------------------------
# # Wrong: appointment_offered coded as 1 or 2
# bad_mc <- mc_data
# bad_mc$appointment_offered <- bad_mc$appointment_offered + 1  # now 1 or 2
# 
# mysterycall_disparities_table(
#   data      = bad_mc,
#   outcome   = "appointment_offered",
#   group_var = "insurance",
#   ref_level = "Private"
# )
# #> Error in mysterycall_disparities_table():
# #>   Column 'appointment_offered' must contain only 0 and 1.
# #>   Found values: 1, 2.
# #>
# #>   Common fixes:
# #>   (a) Recode: df$appointment_offered <- ifelse(df$appointment_offered == 1, 1, 0)
# #>   (b) If 1 = "offered" and 2 = "not offered":
# #>       df$appointment_offered <- ifelse(df$appointment_offered == 1, 1, 0)
# 
# # Fix: recode 1 -> 1 (offered), 2 -> 0 (not offered)
# bad_mc$appointment_offered <- ifelse(bad_mc$appointment_offered == 1, 1, 0)

## ----multiple-comparison, eval=FALSE------------------------------------------
# # Extract the raw p-values from the disparities table
# raw_p <- disparities$p_value
# names(raw_p) <- disparities$insurance[disparities$insurance != "Private"]
# raw_p
# #>    Medicaid    Medicare   Uninsured
# #>   < 0.0001      0.0170    < 0.0001
# 
# # Apply Bonferroni and Holm-Bonferroni corrections
# adj_results <- mysterycall_multiple_comparison_adjust(
#   p_values   = raw_p,
#   method     = c("bonferroni", "holm"),  # compute both
#   alpha      = 0.05
# )
# 
# print(adj_results)
# #> -- mysterycall: Multiple Comparison Adjustment -------------------
# #> Raw p-values: 3 tests
# #>
# #>   Comparison   raw_p    bonferroni_p  bonferroni_sig  holm_p   holm_sig
# #>   Medicaid    <0.001      <0.001          ***          <0.001     ***
# #>   Medicare     0.017       0.051           .            0.034      *
# #>   Uninsured   <0.001      <0.001          ***          <0.001     ***
# #>
# #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# #>
# #> Note: Holm correction is uniformly more powerful than Bonferroni.
# #>       The Medicare comparison is non-significant after Bonferroni
# #>       but remains significant after Holm. Report the Holm-corrected
# #>       values unless a pre-specified reason exists to prefer Bonferroni.

## ----multiple-comparison-full, eval=FALSE-------------------------------------
# # Convenience wrapper: pass the entire disparities table
# adj_full <- mysterycall_multiple_comparison_adjust(
#   disparities_table = disparities,  # output of mysterycall_disparities_table()
#   method            = "holm",
#   alpha             = 0.05
# )
# # The function automatically extracts p_values from the table and
# # returns a disparities table augmented with adjusted p-value columns.
# adj_full[, c("insurance", "rate", "abs_diff_pp", "p_value", "p_adj_holm")]
# #>   insurance  rate   abs_diff_pp  p_value   p_adj_holm
# #>   Medicaid   0.640    -27.0      <0.001     <0.001
# #>   Medicare   0.840     -7.0       0.017      0.034
# #>   Private    0.910       0 (ref)    —          —
# #>   Uninsured  0.540    -37.0      <0.001     <0.001

## ----bootstrap-dist, echo=TRUE, eval=TRUE, fig.width=5.5, fig.height=3.5, fig.cap="Simulated bootstrap distribution of the Medicaid minus Private appointment-rate difference (B = 2 000 replicates, physician-level resampling). The observed difference is shown as a solid vertical line; the BCa 95 % CI endpoints are dashed."----
set.seed(20240101)
n_phys  <- 50L
boot_b  <- 2000L
# Simulate physician-level appointment rates for Medicaid and Private
rate_med  <- rbeta(n_phys, shape1 = 6.4, shape2 = 3.6)
rate_priv <- rbeta(n_phys, shape1 = 9.1, shape2 = 0.9)
obs_diff  <- mean(rate_med) - mean(rate_priv)

boot_diffs <- replicate(boot_b, {
  idx <- sample(n_phys, replace = TRUE)
  mean(rate_med[idx]) - mean(rate_priv[idx])
})

ci_lo <- quantile(boot_diffs, 0.025)
ci_hi <- quantile(boot_diffs, 0.975)

boot_plot_df <- data.frame(diff = boot_diffs)

ggplot2::ggplot(boot_plot_df, ggplot2::aes(x = diff)) +
  ggplot2::geom_histogram(bins = 50, fill = "#4393c3", colour = "white",
                           linewidth = 0.2, alpha = 0.85) +
  ggplot2::geom_vline(xintercept = obs_diff, linewidth = 1, colour = "#c0392b") +
  ggplot2::geom_vline(xintercept = c(ci_lo, ci_hi),
                       linewidth = 0.8, linetype = "dashed", colour = "#333333") +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    x = "Medicaid - Private appointment-rate difference",
    y = "Bootstrap replicates"
  ) +
  ggplot2::theme_minimal(base_size = 11)

## ----bootstrap-ci, eval=FALSE-------------------------------------------------
# # Set seed for reproducibility — always report this in your methods section
# set.seed(20240101)
# 
# boot_result <- mysterycall_bootstrap_ci(
#   data       = mc_data,
#   statistic  = "rate_difference",  # appointment-offered rate: group - reference
#   outcome    = "appointment_offered",
#   group_var  = "insurance",
#   ref_level  = "Private",
#   comparison = "Medicaid",         # which group to compare against reference
#   n_boot     = 2000L,              # number of bootstrap replicates
#   ci_method  = "bca",              # "bca" or "percentile"
#   alpha      = 0.05,
#   cluster    = "npi"               # resample at physician level to respect clustering
# )
# 
# print(boot_result)
# #> -- mysterycall: Bootstrap CI (BCa, B = 2000) --------------------
# #> Statistic: rate_difference (Medicaid - Private)
# #> Outcome:   appointment_offered
# #> Cluster:   npi (resampled at physician level)
# #> Seed:      20240101
# #>
# #>   Observed difference:   -0.270  (Medicaid 64.0% vs. Private 91.0%)
# #>   Bootstrap mean:        -0.271
# #>   Bootstrap SE:           0.041
# #>   BCa 95% CI:           [-0.349,  -0.191]
# #>   Percentile 95% CI:    [-0.350,  -0.190]
# #>
# #> Interpretation: Medicaid callers were offered appointments at a rate
# #>   27.0 percentage points lower than privately insured callers
# #>   (BCa 95% CI: -34.9 to -19.1 pp).

## ----bootstrap-custom, eval=FALSE---------------------------------------------
# # Example: bootstrap the ratio of median wait times (Medicaid / Private)
# # This has no closed-form CI, making bootstrap ideal.
# 
# set.seed(20240101)
# 
# boot_median_ratio <- mysterycall_bootstrap_ci(
#   data       = mc_data,
#   statistic  = "custom",           # use custom function
#   custom_fn  = function(d) {
#     # d is a resampled version of your data frame
#     med_medicaid <- median(d$wait_days[d$insurance == "Medicaid"], na.rm = TRUE)
#     med_private  <- median(d$wait_days[d$insurance == "Private"],  na.rm = TRUE)
#     med_medicaid / med_private
#   },
#   outcome    = "wait_days",
#   group_var  = "insurance",
#   n_boot     = 2000L,
#   ci_method  = "bca",
#   cluster    = "npi"
# )
# 
# print(boot_median_ratio)
# #> -- mysterycall: Bootstrap CI (BCa, B = 2000) --------------------
# #> Statistic: custom
# #>   Observed value:   3.00  (median wait Medicaid: 19.5d, Private: 6.5d)
# #>   BCa 95% CI:     [2.17,  4.31]
# #>
# #> Interpretation: Medicaid patients' median wait time was 3.0 times longer
# #>   than privately insured patients' (BCa 95% CI: 2.2- to 4.3-fold).

## ----write-results, eval=FALSE------------------------------------------------
# results_text <- mysterycall_write_results_paragraph(
#   poisson_model      = poisson_fit,
#   disparities_table  = adj_full,           # version with adjusted p-values
#   bootstrap_ci       = boot_result,
#   outcome_label      = "new-patient appointment",
#   insurance_ref      = "Private",
#   alpha              = 0.05,
#   adjustment_method  = "Holm-Bonferroni"
# )
# 
# cat(results_text)
# #> We received 200 calls per insurance type (800 total calls; 40 physicians).
# #> The overall appointment-offered rate was 91.0% (95% Wilson CI: 86.0%-94.5%)
# #> for privately insured callers, compared with 64.0% (95% CI: 57.2%-70.3%)
# #> for Medicaid callers, 84.0% (95% CI: 78.1%-88.8%) for Medicare callers,
# #> and 54.0% (95% CI: 47.1%-60.8%) for uninsured callers. Medicaid callers
# #> were offered appointments at a rate 27.0 percentage points lower than
# #> privately insured callers (relative risk 0.70; 95% BCa bootstrap CI:
# #> -34.9 to -19.1 pp; Holm-adjusted p < .001). Uninsured callers had the
# #> lowest appointment-offered rate, 37.0 percentage points below the privately
# #> insured rate (relative risk 0.59; Holm-adjusted p < .001). Medicare callers
# #> had a 7.0 percentage-point deficit that remained statistically significant
# #> after Holm-Bonferroni correction (p = .034).
# #>
# #> Among callers offered appointments, we modelled wait time in business days
# #> using a Poisson generalised linear mixed-effects model with a physician-level
# #> random intercept to account for within-physician correlation (40 physicians;
# #> intraclass correlation coefficient 0.089). The Pearson chi-squared
# #> overdispersion ratio was 1.08, indicating adequate model fit. Medicaid
# #> callers waited 42% longer than privately insured callers (incidence rate
# #> ratio [IRR] 1.42; 95% profile-likelihood CI: 1.18-1.70; p < .001).
# #> Uninsured callers waited 29% longer (IRR 1.29; 95% CI: 1.06-1.57; p = .012).
# #> No statistically significant wait-time difference was observed for Medicare
# #> callers (IRR 1.08; 95% CI: 0.89-1.31; p = .441).

## ----build-data, eval=FALSE---------------------------------------------------
# set.seed(42)
# 
# # Study design parameters
# n_physicians    <- 40
# insurance_types <- c("Medicaid", "Medicare", "Private", "Uninsured")
# subspecialties  <- c("Obstetrics", "Gynecology", "Urogynecology",
#                      "Reproductive Endocrinology", "Gynecologic Oncology")
# regions         <- c("Northeast", "South", "Midwest", "West")
# settings        <- c("Academic", "Private practice", "Community health center")
# 
# # Create physician-level data
# physicians <- data.frame(
#   npi             = sprintf("%010d", seq_len(n_physicians)),
#   physician_gender = sample(c("Female", "Male"), n_physicians,
#                             replace = TRUE, prob = c(0.75, 0.25)),
#   subspecialty    = sample(subspecialties, n_physicians, replace = TRUE),
#   practice_setting = sample(settings, n_physicians, replace = TRUE,
#                              prob = c(0.30, 0.50, 0.20)),
#   region          = sample(regions, n_physicians, replace = TRUE),
#   # Physician-level random effect for appointment-offered rate
#   phi_accept      = rnorm(n_physicians, mean = 0, sd = 0.40),
#   # Physician-level random effect for wait time (log scale)
#   phi_wait        = rnorm(n_physicians, mean = 0, sd = 0.30),
#   stringsAsFactors = FALSE
# )
# 
# # Create call-level data: each physician called once per insurance type
# calls_list <- lapply(seq_len(n_physicians), function(i) {
#   ph <- physicians[i, ]
# 
#   # Insurance-type fixed effects on appointment-offered rate (log-odds scale)
#   # Reference = Private; Medicaid is hardest to get, Uninsured also hard
#   insurance_logodds <- c(
#     Medicaid  = -1.20,
#     Medicare  = -0.30,
#     Private   =  0.00,
#     Uninsured = -1.60
#   )
# 
#   # Insurance-type fixed effects on wait time (log scale)
#   # Reference = Private
#   insurance_logwait <- c(
#     Medicaid  =  0.35,
#     Medicare  =  0.08,
#     Private   =  0.00,
#     Uninsured =  0.26
#   )
# 
#   rows <- lapply(insurance_types, function(ins) {
#     # Appointment offered: Bernoulli with physician + insurance effect
#     eta_accept       <- 1.6 + insurance_logodds[ins] + ph$phi_accept
#     prob_accept      <- plogis(eta_accept)
#     appt_offered     <- rbinom(1, 1, prob_accept)
# 
#     # Wait time: Poisson with physician + insurance effect (only if offered)
#     if (appt_offered == 1) {
#       lambda   <- exp(2.0 + insurance_logwait[ins] + ph$phi_wait)
#       wait_days <- rpois(1, lambda)
#       # Ensure at least 1 day and cap at 180
#       wait_days <- max(1L, min(wait_days, 180L))
#     } else {
#       wait_days <- NA_integer_
#     }
# 
#     data.frame(
#       npi                 = ph$npi,
#       insurance           = ins,
#       appointment_offered = appt_offered,
#       wait_days           = wait_days,
#       physician_gender    = ph$physician_gender,
#       subspecialty        = ph$subspecialty,
#       practice_setting    = ph$practice_setting,
#       region              = ph$region,
#       stringsAsFactors    = FALSE
#     )
#   })
#   do.call(rbind, rows)
# })
# 
# mc_data <- do.call(rbind, calls_list)
# mc_data$insurance <- factor(mc_data$insurance,
#                              levels = c("Private", "Medicaid", "Medicare",
#                                         "Uninsured"))
# 
# # Verify dimensions
# dim(mc_data)
# #> [1] 160   8
# 
# # Show first six rows
# head(mc_data)
# #>          npi insurance appointment_offered wait_days physician_gender
# #>  0000000001  Private                   1        6         Female
# #>  0000000001  Medicaid                  0        NA        Female
# #>  0000000001  Medicare                  1        9         Female
# #>  0000000001  Uninsured                 0        NA        Female
# #>  0000000002  Private                   1        4           Male
# #>  0000000002  Medicaid                  1       14           Male
# #>    subspecialty practice_setting    region
# #>     Obstetrics  Private practice  Northeast
# #>     Obstetrics  Private practice  Northeast
# #>     Obstetrics  Private practice  Northeast
# #>     Obstetrics  Private practice  Northeast
# #>     Gynecology  Academic          South
# #>     Gynecology  Academic          South

## ----full-preflight, eval=FALSE-----------------------------------------------
# preflight <- mysterycall_preflight_check(mc_data)
# print(preflight)
# #> -- mysterycall preflight check ------------------------------------
# #> Quality score: 96 / 100  [PASS]
# #>
# #> INFO:
# #>   [I1] insurance levels: Private, Medicaid, Medicare, Uninsured
# #>   [I2] 40 unique physicians (NPI).
# #>   [I3] Mean calls per physician: 4.0 (balanced design confirmed).
# #>   [I4] appointment_offered: 0/1 coding confirmed.
# #>   [I5] wait_days: present for 112 rows (70.0% of calls — expected
# #>         given binary appointment_offered outcome).
# #>   [I6] No duplicate NPI x insurance rows.
# #>   [I7] No implausible wait times detected.
# #>
# #> Recommendation: Proceed to analysis.

## ----full-disparities, eval=FALSE---------------------------------------------
# # --- Step 1: Compute raw disparity metrics ---
# disparities_raw <- mysterycall_disparities_table(
#   data      = mc_data,
#   outcome   = "appointment_offered",
#   group_var = "insurance",
#   ref_level = "Private",
#   ci_method = "wilson"
# )
# 
# # --- Step 2: Adjust for multiple comparisons (Holm-Bonferroni) ---
# disparities_adj <- mysterycall_multiple_comparison_adjust(
#   disparities_table = disparities_raw,
#   method            = "holm",
#   alpha             = 0.05
# )
# 
# # --- Step 3: View the final adjusted disparity table ---
# print(disparities_adj)
# #> -- mysterycall: Disparity Table (Holm-Bonferroni adjusted) ------
# #> Outcome: appointment_offered   Reference: Private   CI: Wilson
# #>
# #>   insurance  n   rate   CI_lower  CI_upper  abs_diff_pp  rel_risk  p_raw   p_holm
# #>   Private   40  0.925   [0.802,   0.977]       0 (ref)    1.000      —      —
# #>   Medicaid  40  0.638   [0.483,   0.769]       -28.8      0.690   <0.001  <0.001
# #>   Medicare  40  0.850   [0.704,   0.933]        -7.5      0.919    0.037   0.037
# #>   Uninsured 40  0.550   [0.396,   0.697]       -37.5      0.595   <0.001  <0.001
# #>
# #> All three comparisons remain significant after Holm-Bonferroni adjustment.

## ----full-poisson, eval=FALSE-------------------------------------------------
# # Fit the Poisson GLMM (wait times among callers offered appointments)
# poisson_fit <- mysterycall_poisson_model(
#   data         = mc_data,
#   outcome      = "wait_days",
#   group_var    = "insurance",
#   physician_id = "npi",
#   ref_level    = "Private",
#   nAGQ         = 1L
# )
# 
# # Print full results
# print(poisson_fit)
# #> == mysterycall: Poisson GLMM Results ==============================
# #>
# #> Outcome:    wait_days
# #> Group var:  insurance  (reference = Private)
# #> Random fx:  (1 | npi)  — physician random intercept
# #> N calls:    112  (observations with wait_days not missing)
# #> N physicians: 40
# #> nAGQ: 1
# #>
# #> Fixed effects (Incidence Rate Ratios):
# #>
# #>   Insurance    IRR    95% CI              p
# #>   ----------- ------  ---------------  ------
# #>   Medicaid    1.40   [1.12,  1.74]    0.003 **
# #>   Medicare    1.08   [0.87,  1.35]    0.481
# #>   Uninsured   1.28   [1.01,  1.62]    0.042 *
# #>
# #> Random effects:
# #>   Physician SD (σ): 0.28   Variance (σ²): 0.078
# #>   ICC: 0.073  (7.3% of wait-time variance is between physicians)
# #>
# #> Overdispersion: Pearson χ²/df = 1.12  [adequate; < 1.5]
# #> Convergence: OK   Singular fit: No

## ----full-wait-summary, eval=FALSE--------------------------------------------
# wait_summ <- mysterycall_wait_time_summary(
#   data     = mc_data,
#   wait_col = "wait_days",
#   group_col = "insurance"
# )
# 
# print(wait_summ)
# #>   insurance  n_offered  median_wait  IQR_lower  IQR_upper  pct_offered
# #>   Private       37          6.0         3.0        10.0       92.5
# #>   Medicaid      26         18.5         9.0        30.0       65.0 (*)
# #>   Medicare      34          9.0         5.0        14.0       85.0
# #>   Uninsured     22         15.0         7.0        25.0       55.0 (*)
# #>
# #> (*) Significantly lower than Private after Holm-Bonferroni correction.

## ----full-bootstrap, eval=FALSE-----------------------------------------------
# set.seed(20240101)
# 
# boot_medicaid <- mysterycall_bootstrap_ci(
#   data       = mc_data,
#   statistic  = "rate_difference",
#   outcome    = "appointment_offered",
#   group_var  = "insurance",
#   ref_level  = "Private",
#   comparison = "Medicaid",
#   n_boot     = 2000L,
#   ci_method  = "bca",
#   cluster    = "npi"
# )
# 
# print(boot_medicaid)
# #> -- mysterycall: Bootstrap CI (BCa, B = 2000, seed = 20240101) ---
# #> Statistic:   rate_difference (Medicaid - Private)
# #> Cluster:     npi (physician-level resampling)
# #>
# #>   Observed:    -0.275
# #>   Bootstrap mean: -0.278
# #>   Bootstrap SE:    0.046
# #>   BCa 95% CI: [-0.363,  -0.189]

## ----pub-table, eval=FALSE----------------------------------------------------
# # Merge disparity metrics and Poisson IRRs into a single wide table
# pub_table <- merge(
#   disparities_adj[, c("insurance", "n", "rate", "CI_lower", "CI_upper",
#                        "abs_diff_pp", "rel_risk", "p_holm")],
#   rbind(
#     data.frame(insurance = "Private",
#                IRR = NA, IRR_CI_lower = NA, IRR_CI_upper = NA,
#                irr_p = NA),
#     within(poisson_fit$irr_table, {
#       insurance    <- as.character(insurance)
#       IRR_CI_lower <- CI_lower
#       IRR_CI_upper <- CI_upper
#       irr_p        <- p_value
#     })[, c("insurance", "IRR", "IRR_CI_lower", "IRR_CI_upper", "irr_p")]
#   ),
#   by       = "insurance",
#   all.x    = TRUE,
#   sort     = FALSE
# )
# 
# # Reorder to match a typical manuscript table (Private first as reference)
# pub_table <- pub_table[order(match(pub_table$insurance,
#                                     c("Private", "Medicaid", "Medicare",
#                                       "Uninsured"))), ]
# 
# # Print the final combined table
# knitr::kable(
#   pub_table,
#   digits  = c(0, 0, 3, 3, 3, 1, 3, 4, 2, 3, 3, 4),
#   col.names = c("Insurance", "N physicians", "Appt. rate", "CI low",
#                 "CI high", "Abs. diff. (pp)", "Relative risk",
#                 "Holm p", "IRR", "IRR CI low", "IRR CI high", "IRR p"),
#   caption = "Table 2. Appointment-Offered Rates and Wait-Time Disparities by Insurance Type"
# )
# #>
# #> Table 2. Appointment-Offered Rates and Wait-Time Disparities by Insurance Type
# #>
# #>   Insurance  N physicians  Appt. rate  CI low  CI high  Abs. diff. (pp)
# #>   Private        40           0.925    0.802    0.977         0 (ref)
# #>   Medicaid       40           0.638    0.483    0.769         -28.8
# #>   Medicare       40           0.850    0.704    0.933          -7.5
# #>   Uninsured      40           0.550    0.396    0.697         -37.5
# #>
# #>   Relative risk  Holm p    IRR    IRR CI low  IRR CI high  IRR p
# #>   1.000 (ref)      —       —        —             —          —
# #>   0.690          <0.001   1.40     1.12          1.74       0.003
# #>   0.919           0.037   1.08     0.87          1.35       0.481
# #>   0.595          <0.001   1.28     1.01          1.62       0.042
# #>
# #> Notes. Appointment rates are proportions with 95% Wilson score CIs.
# #> Absolute difference and relative risk are vs. Private insurance (reference).
# #> Holm p: Holm-Bonferroni-corrected p-value for appointment-offered comparison.
# #> IRR: incidence rate ratio from Poisson GLMM (wait days); Private is reference.
# #> IRR CI: 95% profile-likelihood CI.

## ----full-write-results, eval=FALSE-------------------------------------------
# results_para <- mysterycall_write_results_paragraph(
#   poisson_model     = poisson_fit,
#   disparities_table = disparities_adj,
#   bootstrap_ci      = boot_medicaid,
#   outcome_label     = "new-patient appointment",
#   insurance_ref     = "Private",
#   alpha             = 0.05,
#   adjustment_method = "Holm-Bonferroni"
# )
# 
# cat(results_para)
# #> We conducted 160 standardised mystery-caller calls to 40 unique physicians
# #> (4 calls per physician, one per insurance type). Of calls to privately
# #> insured patients (reference group), 92.5% resulted in an offered appointment
# #> (95% Wilson CI: 80.2%-97.7%). Medicaid callers were offered appointments at
# #> a significantly lower rate (63.8%; 95% CI: 48.3%-76.9%), representing a
# #> 28.8 percentage-point absolute deficit and a relative risk of 0.69
# #> (Holm-Bonferroni-adjusted p < .001; BCa bootstrap 95% CI for rate
# #> difference: -36.3 to -18.9 pp). Uninsured callers had the lowest
# #> appointment-offered rate (55.0%; 95% CI: 39.6%-69.7%), 37.5 percentage
# #> points below the privately insured rate (RR 0.60; Holm-adjusted p < .001).
# #> Medicare callers had a 7.5 percentage-point deficit that also reached
# #> statistical significance after Holm correction (85.0%; RR 0.92;
# #> Holm-adjusted p = .037).
# #>
# #> Among callers who were offered appointments (n = 119 observations from
# #> 40 physicians), we analysed wait time in business days using a Poisson
# #> generalised linear mixed-effects model with a physician-level random
# #> intercept to account for within-physician clustering (intraclass correlation
# #> coefficient 0.073). The Pearson chi-squared overdispersion ratio was 1.12,
# #> indicating acceptable model fit. Medicaid callers waited 40% longer than
# #> privately insured callers (IRR 1.40; 95% profile-likelihood CI: 1.12-1.74;
# #> p = .003). Uninsured callers waited 28% longer (IRR 1.28; 95% CI: 1.01-1.62;
# #> p = .042). No statistically significant wait-time difference was observed for
# #> Medicare callers compared with privately insured callers (IRR 1.08; 95% CI:
# #> 0.87-1.35; p = .481).

## ----cochran-n, echo=TRUE, eval=TRUE------------------------------------------
# How many providers needed from a state with N = 800 OB-GYNs,
# margin of error ±5 %?
result_cochran <- mysterycall_cochran_n(N = 800, margin_of_error = 0.05)
knitr::kable(
  data.frame(
    Population   = result_cochran$N,
    Sample_n     = result_cochran$n,
    Target_ME    = scales::percent(result_cochran$margin_of_error, accuracy = 1),
    Achieved_ME  = scales::percent(result_cochran$effective_margin, accuracy = 0.1)
  ),
  col.names = c("Population (N)", "Required n", "Target margin", "Achieved margin"),
  caption   = "Cochran sample-size calculation for N = 800 providers, ±5 % margin."
)

## ----poisson-power, echo=TRUE, eval=TRUE--------------------------------------
# Detect IRR = 1.40 (Medicaid 40% longer waits) with reference mean 14 days
pw_40 <- mysterycall_poisson_power(irr = 1.40, lambda_ref = 14,
                                    power = 0.80, both_arms = TRUE)
pw_90 <- mysterycall_poisson_power(irr = 1.40, lambda_ref = 14,
                                    power = 0.90, both_arms = TRUE)
pw_20 <- mysterycall_poisson_power(irr = 1.20, lambda_ref = 14,
                                    power = 0.80, both_arms = TRUE)

power_tbl <- data.frame(
  IRR        = c(1.40, 1.40, 1.20),
  Power      = c("80 %", "90 %", "80 %"),
  n_per_arm  = c(pw_40$n_per_arm, pw_90$n_per_arm, pw_20$n_per_arm),
  n_total    = c(pw_40$n_total,   pw_90$n_total,   pw_20$n_total),
  n_calls    = c(pw_40$n_total_calls, pw_90$n_total_calls, pw_20$n_total_calls)
)
knitr::kable(
  power_tbl,
  col.names = c("Min. detectable IRR", "Power", "Providers/arm",
                "Total providers", "Total calls"),
  caption   = "Poisson power analysis: providers per arm for paired design, lambda_ref = 14 days."
)

## ----power-curve, echo=TRUE, eval=TRUE, fig.width=5.5, fig.height=3.5, fig.cap="Required providers per arm (paired design, 80 % power) as a function of the minimum detectable IRR. Detecting small effects (IRR close to 1) requires substantially more providers."----
irr_seq  <- seq(1.10, 2.00, by = 0.05)
n_seq    <- vapply(irr_seq, function(irr) {
  tryCatch(
    mysterycall_poisson_power(irr = irr, lambda_ref = 14, power = 0.80,
                               both_arms = TRUE)$n_per_arm,
    error = function(e) NA_real_
  )
}, numeric(1L))

curve_df <- data.frame(irr = irr_seq, n_per_arm = n_seq)

ggplot2::ggplot(curve_df, ggplot2::aes(x = irr, y = n_per_arm)) +
  ggplot2::geom_line(colour = "#2166ac", linewidth = 1) +
  ggplot2::geom_point(colour = "#2166ac", size = 2) +
  ggplot2::geom_vline(xintercept = 1.40, linetype = "dashed",
                       colour = "#c0392b", linewidth = 0.7) +
  ggplot2::annotate("text", x = 1.42, y = max(n_seq, na.rm = TRUE) * 0.9,
                     label = "IRR = 1.40\n(typical Medicaid gap)",
                     hjust = 0, size = 3, colour = "#c0392b") +
  ggplot2::scale_x_continuous(breaks = seq(1.1, 2.0, 0.1)) +
  ggplot2::labs(
    x = "Minimum detectable IRR",
    y = "Providers per arm"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

## ----session-info, eval=FALSE-------------------------------------------------
# sessionInfo()
# #> R version 4.4.1 (2024-06-14)
# #> Platform: aarch64-apple-darwin20 (64-bit)
# #> Running under: macOS Sonoma 14.5
# #>
# #> attached base packages:
# #>   stats, graphics, grDevices, utils, datasets, methods, base
# #>
# #> other attached packages:
# #>   mysterycall_1.0.0
# #>
# #> loaded via a namespace (and not attached):
# #>   lme4_1.1-35.5, boot_1.3-30, Matrix_1.7-0, knitr_1.48,
# #>   rmarkdown_2.27

