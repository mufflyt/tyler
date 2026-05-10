# Summarise appointment wait times

Computes descriptive statistics (n, mean, SD, median, IQR, range) for a
numeric wait-time column and runs an appropriate non-parametric test
when groups are specified: Wilcoxon rank-sum for two groups,
Kruskal-Wallis for three or more.

## Usage

``` r
mysterycall_wait_time_summary(
  data,
  wait_col = "wait_days",
  group_by = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame containing at least the column named by `wait_col`.

- wait_col:

  Name of the numeric column holding days to appointment. Defaults to
  `"wait_days"`.

- group_by:

  Optional character scalar naming a grouping column (e.g.
  `"insurance"`). When `NULL` (default) statistics are computed over the
  full dataset and no test is run.

- conf_level:

  Confidence level used by the Wilcoxon test. Default `0.95`.

## Value

A named list with:

- `summary`:

  Tibble with one row per group (or one row overall): `n`, `n_missing`,
  `mean`, `sd`, `median`, `q1`, `q3`, `min`, `max`.

- `test`:

  `htest` object from
  [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) or
  [`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html),
  or `NULL` when no test was run.

- `p_value`:

  Numeric p-value, or `NA_real_`.

- `test_name`:

  Character label describing the test used.

- `interpretation`:

  One-sentence plain-language summary suitable for pasting into a
  Results section.

## Details

Wait times must already be numeric (days to appointment). If your Phase
2 data records an appointment date rather than a count, compute
`data$wait_days <- as.numeric(data$appt_date - data$call_date)` before
calling this function.

## See also

[`mysterycall_acceptance_rate()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_acceptance_rate.md),
[`mysterycall_clean_phase2()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clean_phase2.md)

Other outcomes:
[`mysterycall_acceptance_rate()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_acceptance_rate.md),
[`mysterycall_irr_plot()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_irr_plot.md),
[`mysterycall_marginal_effects()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_marginal_effects.md),
[`mysterycall_model_metrics()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_model_metrics.md),
[`mysterycall_model_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_model_table.md),
[`mysterycall_plot_distribution()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_distribution.md),
[`mysterycall_plot_effect()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_effect.md),
[`mysterycall_plot_emmeans_full()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_emmeans_full.md),
[`mysterycall_plot_emmeans_interaction()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_emmeans_interaction.md),
[`mysterycall_plot_inclexcl()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_inclexcl.md),
[`mysterycall_plot_residuals()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_residuals.md),
[`mysterycall_plot_sjplot_interaction()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_sjplot_interaction.md),
[`mysterycall_plot_stacked_bar()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_stacked_bar.md),
[`mysterycall_poisson_model()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_poisson_model.md),
[`mysterycall_save_plot()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_save_plot.md),
[`mysterycall_screen_interactions()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_screen_interactions.md),
[`mysterycall_select_best_model()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_select_best_model.md),
[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md)

## Examples

``` r
df <- data.frame(
  wait_days = c(3, 7, 14, 21, 5, 10, 30, 2),
  insurance = c("Medicaid", "Medicaid", "Medicaid", "Medicaid",
                "BCBS", "BCBS", "BCBS", "BCBS")
)
mysterycall_wait_time_summary(df, group_by = "insurance")
#> $summary
#> # A tibble: 2 × 10
#>   insurance     n n_missing  mean    sd median    q1    q3   min   max
#>   <chr>     <int>     <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 BCBS          4         0  11.8 12.6     7.5  4.25  15       2    30
#> 2 Medicaid      4         0  11.2  7.93   10.5  6     15.8     3    21
#> 
#> $test
#> 
#>  Wilcoxon rank sum test with continuity correction
#> 
#> data:  gdata[[1L]] and gdata[[2L]]
#> W = 7, p-value = 0.8852
#> alternative hypothesis: true location shift is not equal to 0
#> 
#> 
#> $p_value
#> [1] 0.8852339
#> 
#> $test_name
#> [1] "Wilcoxon rank-sum (Mann-Whitney U)"
#> 
#> $interpretation
#> [1] "BCBS: median 7.5 days (IQR 4.25-15, n=4); Medicaid: median 10.5 days (IQR 6-15.75, n=4). Wilcoxon rank-sum (Mann-Whitney U): p = 0.885"
#> 
```
