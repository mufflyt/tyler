# Compare a study outcome across waves of a mystery caller study

Calculates per-wave (and optionally per-group) summary statistics and
pairwise tests against a reference wave, using only base R.

## Usage

``` r
mysterycall_compare_waves(
  data,
  wave_col,
  outcome_col,
  group_col = NULL,
  type = c("auto", "proportion", "continuous"),
  ref_wave = NULL
)
```

## Arguments

- data:

  A data frame.

- wave_col:

  Character scalar: column identifying the study wave.

- outcome_col:

  Character scalar: column containing the outcome to compare.

- group_col:

  Character scalar or NULL (default). When provided, results are
  stratified by group x wave.

- type:

  One of `"auto"` (default), `"proportion"`, or `"continuous"`. `"auto"`
  detects based on whether all non-missing outcome values are in {0, 1}.

- ref_wave:

  Scalar or NULL. The reference wave against which other waves are
  compared. When NULL, the first wave (sorted
  alphabetically/numerically) is used.

## Value

A data frame. For proportion outcomes: columns `wave`, (`group`,) `n`,
`n_accepted`, `rate`, `lower_ci`, `upper_ci`, `p_vs_ref`. For continuous
outcomes: columns `wave`, (`group`,) `n`, `mean`, `median`, `sd`, `iqr`,
`p_vs_ref`. The attribute `ref_wave` is set on the returned data frame.

## See also

Other outcomes:
[`mysterycall_acceptance_rate()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_acceptance_rate.md),
[`mysterycall_irr_plot()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_irr_plot.md),
[`mysterycall_marginal_effects()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_marginal_effects.md),
[`mysterycall_model_metrics()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_model_metrics.md),
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
[`mysterycall_wait_time_summary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_wait_time_summary.md),
[`print.mysterycall_poisson_model()`](https://mufflyt.github.io/mysterycall/reference/print.mysterycall_poisson_model.md)

## Examples

``` r
df <- data.frame(
  wave    = c("pre","pre","post","post"),
  outcome = c(1, 0, 1, 1)
)
mysterycall_compare_waves(df, "wave", "outcome")
#>   wave n n_accepted rate   lower_ci  upper_ci  p_vs_ref
#> 1 post 2          2  1.0 0.34238023 1.0000000        NA
#> 2  pre 2          1  0.5 0.09453121 0.9054688 0.2482131
```
