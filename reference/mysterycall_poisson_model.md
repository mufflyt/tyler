# Fit a Poisson GLMER for mystery caller wait-time analysis

Runs a multilevel Poisson regression
([`lme4::glmer`](https://rdrr.io/pkg/lme4/man/glmer.html)) appropriate
for mystery caller studies where the same physician may be called
multiple times (e.g. with different insurance types). The physician
identifier is modelled as a random intercept, which accounts for
correlation within physicians. Fixed-effect results are returned as
incidence rate ratios (IRR) with Wald confidence intervals.

## Usage

``` r
mysterycall_poisson_model(
  data,
  outcome,
  predictors,
  random_intercept,
  conf_level = 0.95,
  nAGQ = 0L,
  offset_col = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing all model columns. Rows with `NA` in any model
  column are dropped before fitting; the count is reported.

- outcome:

  Character scalar naming the count-outcome column (e.g.
  `"business_days_until_appointment"`). Must be non-negative numeric.

- predictors:

  Character vector of fixed-effect predictor column names. Factor and
  character columns are used as-is; their reference level is the first
  level alphabetically (or the first
  [`levels()`](https://rdrr.io/r/base/levels.html) for factors).

- random_intercept:

  Character scalar naming the grouping column for the random intercept
  (e.g. `"name"` for physician). A `(1 | column)` term is added to the
  formula automatically.

- conf_level:

  Confidence level for Wald CIs. Default `0.95`.

- nAGQ:

  Integer passed to
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html). `0`
  (default) uses the fastest approximation; `1` uses the Laplace
  approximation; values `> 1` use adaptive Gauss-Hermite quadrature
  (slower, more accurate for small cluster sizes).

- offset_col:

  Optional character scalar naming a numeric column to use as a
  log-offset (e.g. log(exposure time)). When supplied, the term
  `offset(log(offset_col))` is appended to the fixed-effects formula.

- ...:

  Additional arguments forwarded to
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html).

## Value

A list of class `mysterycall_poisson_model` containing:

- `model`:

  The fitted `glmerMod` object.

- `irr_table`:

  Tibble with one row per fixed-effect term: `term`, `estimate` (log
  scale), `se`, `z_value`, `p_value`, `p_value_fmt` (character), `irr`,
  `ci_lower`, `ci_upper`.

- `random_effects`:

  Data frame from
  [`lme4::VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html)
  describing the random-intercept variance and standard deviation.

- `factor_refs`:

  Named list of reference levels for character/factor predictors.

- `formula`:

  The formula passed to `glmer`.

- `n`:

  Number of complete-case rows used for fitting.

- `n_dropped`:

  Rows excluded due to missing values.

- `n_clusters`:

  Number of unique values of `random_intercept`.

- `overdispersion`:

  Pearson chi-square / residual df. Values substantially above 1 suggest
  overdispersion; consider a negative binomial model.

- `convergence`:

  List with `converged` (logical), `singular` (logical), and `messages`
  (character).

- `aic`, `bic`:

  Model information criteria.

## Interpreting IRRs

An IRR of 1.40 for `insurance_typeMedicaid` means physicians contacted
with Medicaid insurance had, on average, 40% longer wait times than the
reference insurance group. Compute as `exp(estimate)` with Wald CI
`exp(estimate ± z * se)`.

## Overdispersion

Poisson assumes mean = variance. If `overdispersion` is substantially
greater than 1 (a common threshold is 2), the standard errors will be
underestimated. Consider fitting a negative binomial model or using
quasi-Poisson standard errors for inference.

## See also

[`mysterycall_wait_time_summary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_wait_time_summary.md),
[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md),
[`mysterycall_create_formula()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_create_formula.md)

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
[`mysterycall_save_plot()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_save_plot.md),
[`mysterycall_screen_interactions()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_screen_interactions.md),
[`mysterycall_select_best_model()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_select_best_model.md),
[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md),
[`mysterycall_wait_time_summary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_wait_time_summary.md)

## Examples

``` r
set.seed(1978)
df <- data.frame(
  wait_days = rpois(40, lambda = 18),
  insurance = rep(c("Medicaid", "BCBS"), each = 20),
  gender    = sample(c("Male", "Female"), 40, replace = TRUE),
  physician = rep(paste0("Dr_", 1:8), each = 5),
  stringsAsFactors = FALSE
)
result <- mysterycall_poisson_model(
  df,
  outcome          = "wait_days",
  predictors       = c("insurance", "gender"),
  random_intercept = "physician"
)
#> Fitting Poisson GLMER: wait_days ~ insurance + gender + (1 | physician)
#> boundary (singular) fit: see help('isSingular')
#> Warning: Convergence issues detected:
#>   boundary (singular) fit: see help('isSingular')
#> Consider simplifying predictors or using nAGQ = 1.
#> Warning: Singular fit: random-intercept variance is ~0. The physician-level random effect explains little variation.
#> Model fitted: n=40, physicians=8, AIC=230.8, overdispersion=0.90
result$irr_table
#> # A tibble: 3 × 9
#>   term       estimate     se z_value p_value p_value_fmt   irr ci_lower ci_upper
#>   <chr>         <dbl>  <dbl>   <dbl>   <dbl> <chr>       <dbl>    <dbl>    <dbl>
#> 1 (Intercep…   2.88   0.0670  43.0     0     <0.001      17.8    15.6      20.3 
#> 2 insurance…   0.0109 0.0738   0.148   0.883 0.883        1.01    0.875     1.17
#> 3 genderMale   0.0458 0.0744   0.616   0.538 0.538        1.05    0.905     1.21
```
