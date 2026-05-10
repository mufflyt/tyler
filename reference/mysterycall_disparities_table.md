# Compute Disparity Metrics Across Groups

For each level of a grouping column (e.g., insurance type), compute
acceptance rates, confidence intervals, and disparity metrics relative
to a reference group.

## Usage

``` r
mysterycall_disparities_table(
  data,
  outcome_col,
  group_col,
  ref_group = NULL,
  ci_method = c("wilson", "exact", "wald"),
  alpha = 0.05
)
```

## Arguments

- data:

  A data.frame containing the study data.

- outcome_col:

  Character scalar. Name of the binary outcome column (values must be 0
  or 1, NAs are dropped).

- group_col:

  Character scalar. Name of the grouping column (e.g., insurance type).

- ref_group:

  Character scalar or NULL. The reference group label. If NULL, the
  first factor level is used when `group_col` is a factor, otherwise the
  most frequent level.

- ci_method:

  Character. Method for computing the rate confidence interval. One of
  `"wilson"` (default), `"exact"`, or `"wald"`.

- alpha:

  Numeric in (0, 1). Significance level; CIs use `1 - alpha` confidence.
  Default 0.05.

## Value

A data.frame with class
`c("mysterycall_disparities_table", "data.frame")` sorted by rate
descending, with the reference group printed first. Columns:

- group:

  Group label.

- n:

  Total observations in group.

- n_accepted:

  Sum of outcome_col (excluding NAs).

- rate:

  Acceptance rate (n_accepted / n).

- lower_ci:

  Lower bound of rate CI.

- upper_ci:

  Upper bound of rate CI.

- abs_diff:

  Absolute difference in percentage points from ref_group rate (0 for
  reference).

- rel_risk:

  Rate relative to ref_group rate (1 for reference).

- rr_lower:

  Lower bound of RR 95% CI (NA for reference).

- rr_upper:

  Upper bound of RR 95% CI (NA for reference).

- p_value:

  Two-proportion z-test p-value vs. reference (NA for reference).

- p_value_fmt:

  Formatted p-value string.

## Details

Relative risk confidence intervals are computed via the log method:
`SE_log_rr = sqrt(1/n_accepted - 1/n + 1/ref_n_accepted - 1/ref_n)`. A
continuity correction of 0.5 is added when `n_accepted` is 0.

## Examples

``` r
set.seed(1)
df <- data.frame(
  insurance = sample(c("Medicaid", "Private", "Medicare"), 120, replace = TRUE),
  accepted  = rbinom(120, 1, 0.5)
)
mysterycall_disparities_table(df, "accepted", "insurance")
#> Disparity table (3 groups)
#> Group                     n  n_acc     Rate   95% CI           Abs.Diff  RR (95% CI)           p-value
#> ---------------------------------------------------------------------------------------------------- 
#> Private                  44     21    47.7%   33.8%-62.1%         (ref)  1.00 (ref)            (ref)
#> Medicaid                 38     18    47.4%   32.5%-62.7%       -0.4 pp  0.99 (0.63-1.57)      0.974
#> Medicare                 38     18    47.4%   32.5%-62.7%       -0.4 pp  0.99 (0.63-1.57)      0.974
```
