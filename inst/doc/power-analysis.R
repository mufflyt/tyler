## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 6,
  fig.height = 4,
  out.width = "95%"
)

## ----load---------------------------------------------------------------------
library(mysterycall)

## ----cochran-basic, echo=TRUE, eval=TRUE--------------------------------------
# 800 OB-GYNs in target region — need acceptance rate within ±5 percentage points
res_800 <- mysterycall_cochran_n(N = 800, margin_of_error = 0.05)
cat(sprintf(
  "N = %d providers, target ME ±%.0f%%: sample n = %d (achieved ME = ±%.1f%%)\n",
  res_800$N, res_800$margin_of_error * 100,
  res_800$n, res_800$effective_margin * 100
))

## ----cochran-grid, echo=TRUE, eval=TRUE---------------------------------------
# Show how n changes across directory sizes and margin-of-error targets
grid <- expand.grid(
  N  = c(100, 200, 400, 800, 1600),
  me = c(0.03, 0.05, 0.10)
)

grid$n <- mapply(
  function(N, me) mysterycall_cochran_n(N, me)$n,
  grid$N, grid$me
)

pivot_tbl <- stats::reshape(
  grid,
  idvar = "N", timevar = "me", direction = "wide"
)
names(pivot_tbl) <- c("Directory size (N)", "n at ±3%", "n at ±5%", "n at ±10%")

knitr::kable(
  pivot_tbl,
  caption = "Required sample sizes across directory sizes and precision targets."
)

## ----poisson-basic, echo=TRUE, eval=TRUE--------------------------------------
pw <- mysterycall_poisson_power(
  irr       = 1.40,   # Medicaid wait is 40% longer
  lambda_ref = 14,    # BCBS mean = 14 business days
  alpha      = 0.05,
  power      = 0.80,
  both_arms  = TRUE   # paired design
)

cat(sprintf(
  "IRR = %.2f | lambda_ref = %.0f d | lambda_trt = %.0f d\n",
  pw$irr, pw$lambda_ref, pw$lambda_trt
))
cat(sprintf(
  "n per arm = %d | total providers = %d | total calls = %d\n",
  pw$n_per_arm, pw$n_total, pw$n_total_calls
))

## ----poisson-table, echo=TRUE, eval=TRUE--------------------------------------
scenarios <- list(
  list(irr = 1.20, power = 0.80, label = "Small effect (IRR 1.20), 80% power"),
  list(irr = 1.40, power = 0.80, label = "Moderate effect (IRR 1.40), 80% power"),
  list(irr = 1.40, power = 0.90, label = "Moderate effect (IRR 1.40), 90% power"),
  list(irr = 1.60, power = 0.80, label = "Large effect (IRR 1.60), 80% power"),
  list(irr = 2.00, power = 0.80, label = "Very large effect (IRR 2.00), 80% power")
)

pw_rows <- lapply(scenarios, function(s) {
  res <- mysterycall_poisson_power(
    irr        = s$irr,
    lambda_ref = 14,
    power      = s$power,
    both_arms  = TRUE
  )
  data.frame(
    Scenario       = s$label,
    IRR            = s$irr,
    Power          = sprintf("%.0f%%", s$power * 100),
    `n per arm`    = res$n_per_arm,
    `Total providers` = res$n_total,
    `Total calls`  = res$n_total_calls,
    check.names    = FALSE
  )
})

pw_tbl <- do.call(rbind, pw_rows)

knitr::kable(
  pw_tbl,
  caption = paste(
    "Poisson power analysis: providers per arm for a paired design,",
    "lambda_ref = 14 days, alpha = 0.05."
  )
)

## ----icc, echo=TRUE, eval=TRUE------------------------------------------------
# No clustering vs. ICC = 0.10 (moderate) with 3 calls per office on average
pw_no_icc  <- mysterycall_poisson_power(irr = 1.40, lambda_ref = 14)
pw_icc_010 <- mysterycall_poisson_power(irr = 1.40, lambda_ref = 14,
                                         icc = 0.10, calls_per_cluster = 3)
pw_icc_020 <- mysterycall_poisson_power(irr = 1.40, lambda_ref = 14,
                                         icc = 0.20, calls_per_cluster = 3)

icc_tbl <- data.frame(
  ICC            = c("0 (none)", "0.10 (moderate)", "0.20 (high)"),
  DEFF           = c(pw_no_icc$design_effect,
                     pw_icc_010$design_effect,
                     pw_icc_020$design_effect),
  `n per arm`    = c(pw_no_icc$n_per_arm,
                     pw_icc_010$n_per_arm,
                     pw_icc_020$n_per_arm),
  `Total calls`  = c(pw_no_icc$n_total_calls,
                     pw_icc_010$n_total_calls,
                     pw_icc_020$n_total_calls),
  check.names    = FALSE
)

knitr::kable(
  icc_tbl,
  digits  = 2,
  caption = paste(
    "Effect of within-office correlation (ICC) on required sample size.",
    "IRR = 1.40, lambda_ref = 14 d, 80% power, m = 3 calls/office."
  )
)

## ----power-curve, echo=TRUE, eval=TRUE, fig.cap="Required providers per arm (paired design, 80% power, lambda_ref = 14 d) as a function of the minimum detectable IRR."----
mysterycall_equation_figure(
  lambda0   = 14,
  irr_seq   = seq(1.10, 2.0, by = 0.05),
  alpha     = 0.05,
  power     = 0.80,
  both_arms = TRUE
)

## ----power-curve-90, echo=TRUE, eval=TRUE, fig.cap="Power curves comparing 80% and 90% power targets."----
library(ggplot2)

irr_seq <- seq(1.10, 2.0, by = 0.05)

pw_80 <- vapply(irr_seq, function(irr)
  mysterycall_poisson_power(irr, lambda_ref = 14, power = 0.80, both_arms = TRUE)$n_per_arm,
  numeric(1))

pw_90 <- vapply(irr_seq, function(irr)
  mysterycall_poisson_power(irr, lambda_ref = 14, power = 0.90, both_arms = TRUE)$n_per_arm,
  numeric(1))

curve_df <- rbind(
  data.frame(irr = irr_seq, n = pw_80, power = "80%"),
  data.frame(irr = irr_seq, n = pw_90, power = "90%")
)

ggplot(curve_df, aes(x = irr, y = n, color = power, linetype = power)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("80%" = "#2C3E50", "90%" = "#C0392B")) +
  labs(
    title   = "Poisson power curves by target power level",
    subtitle = "Paired design, lambda_ref = 14 days, alpha = 0.05",
    x       = "Minimum detectable IRR",
    y       = "Required providers per arm",
    color   = "Power",
    linetype = "Power"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

## ----lambda-sensitivity, echo=TRUE, eval=TRUE, fig.cap="Required sample size per arm for IRR = 1.40 across different reference wait-time assumptions."----
lambda_vals <- c(5, 7, 10, 14, 21, 30)

n_by_lambda <- vapply(lambda_vals, function(lam)
  mysterycall_poisson_power(irr = 1.40, lambda_ref = lam, power = 0.80,
                             both_arms = TRUE)$n_per_arm,
  numeric(1))

lam_df <- data.frame(lambda_ref = lambda_vals, n_per_arm = n_by_lambda)

ggplot(lam_df, aes(x = lambda_ref, y = n_per_arm)) +
  geom_col(fill = "#2166AC", alpha = 0.8) +
  geom_text(aes(label = n_per_arm), vjust = -0.4, size = 3.5) +
  labs(
    title   = "Sample size sensitivity to reference wait time",
    subtitle = "IRR = 1.40, 80% power, paired design",
    x       = "Reference-arm mean wait time (days)",
    y       = "Required providers per arm"
  ) +
  theme_minimal(base_size = 12)

## ----combined, echo=TRUE, eval=TRUE-------------------------------------------
n_cochran <- mysterycall_cochran_n(N = 800, margin_of_error = 0.05)$n
n_poisson <- mysterycall_poisson_power(irr = 1.40, lambda_ref = 14,
                                        power = 0.80, both_arms = TRUE)$n_total

cat(sprintf("Cochran (proportion, ±5%% ME):  n = %d\n", n_cochran))
cat(sprintf("Poisson power (IRR 1.40, 80%%): n = %d\n", n_poisson))
cat(sprintf("Required study size:           n = %d\n", max(n_cochran, n_poisson)))

## ----reporting, echo=TRUE, eval=TRUE------------------------------------------
cochran_res <- mysterycall_cochran_n(N = 800, margin_of_error = 0.05)
poisson_res <- mysterycall_poisson_power(irr = 1.40, lambda_ref = 14,
                                          power = 0.80, both_arms = TRUE)

cat(sprintf(paste(
  "Of the %d obstetrician-gynecologists in the study directory,",
  "a minimum sample of %d providers was required for the acceptance-rate",
  "outcome to achieve a margin of error of ±%.0f%% (Cochran 1977).",
  "For the wait-time count outcome, a paired-design Poisson power analysis",
  "(IRR = %.2f, lambda_ref = %.0f d, alpha = 0.05, power = 80%%) indicated",
  "that %d providers would be sufficient (n_total_calls = %d)."),
  cochran_res$N,
  cochran_res$n,
  cochran_res$margin_of_error * 100,
  poisson_res$irr,
  poisson_res$lambda_ref,
  poisson_res$n_total,
  poisson_res$n_total_calls
))

