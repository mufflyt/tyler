# Bootstrap Confidence Intervals for a Summary Statistic

For each level of an optional grouping column (or for the whole
dataset), draws `n_boot` bootstrap samples with replacement, computes a
summary statistic, and returns percentile confidence intervals.

## Usage

``` r
mysterycall_bootstrap_ci(
  data,
  outcome_col,
  group_col = NULL,
  n_boot = 2000L,
  seed = NULL,
  alpha = 0.05,
  stat = c("proportion", "mean", "median")
)
```

## Arguments

- data:

  A data.frame.

- outcome_col:

  Character scalar. Name of the outcome column. When
  `stat = "proportion"`, values must be 0/1.

- group_col:

  Character scalar or NULL. Name of a grouping column. If NULL, a single
  "Overall" row is returned.

- n_boot:

  Positive integer (\>= 100). Number of bootstrap replicates. Default
  2000L.

- seed:

  Integer or NULL. If not NULL, passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before
  bootstrapping to make results reproducible.

- alpha:

  Numeric in (0, 1). CI uses the `alpha/2` and `1 - alpha/2`
  percentiles. Default 0.05.

- stat:

  Character. Statistic to bootstrap. One of `"proportion"` (mean of a
  0/1 variable), `"mean"`, or `"median"`.

## Value

A data frame with one row per group (or `"Overall"` when `group_col` is
`NULL`) and columns:

- `group`:

  Character. Group label, or `"Overall"` when `group_col` is `NULL`.

- `n`:

  Integer. Number of non-NA observations used.

- `estimate`:

  Numeric. Observed statistic value (proportion, mean, or median
  depending on `stat`).

- `lower_ci`:

  Numeric. Lower percentile CI bound at `alpha/2`.

- `upper_ci`:

  Numeric. Upper percentile CI bound at `1 - alpha/2`.

- `n_boot`:

  Integer. Number of bootstrap replicates used.

## See also

[`mysterycall_acceptance_rate()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_acceptance_rate.md),
[`mysterycall_wait_time_summary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_wait_time_summary.md)
for model-based outcome summaries;
[`mysterycall_compare_waves()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_compare_waves.md)
for cross-wave significance testing.

Other inference:
[`mysterycall_compare_waves()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_compare_waves.md),
[`mysterycall_multiple_comparison_adjust()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multiple_comparison_adjust.md)

## Examples

``` r
set.seed(42)
df <- data.frame(
  insurance = sample(c("Medicaid", "Private"), 200, replace = TRUE),
  accepted  = rbinom(200, 1, 0.6)
)
mysterycall_bootstrap_ci(df, "accepted", group_col = "insurance", seed = 1)
#>      group   n  estimate  lower_ci  upper_ci n_boot
#> 1 Medicaid  89 0.6179775 0.5168539 0.7191011   2000
#> 2  Private 111 0.7387387 0.6576577 0.8198198   2000
```
