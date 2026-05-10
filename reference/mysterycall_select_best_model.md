# Compare and rank competing fitted models

Accepts a named list of fitted model objects (e.g. output from repeated
calls to
[`mysterycall_poisson_model()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_poisson_model.md)
with different predictor sets) and returns a ranked summary table.

## Usage

``` r
mysterycall_select_best_model(models, criterion = c("aic", "bic", "lrt"))
```

## Arguments

- models:

  Named list of fitted model objects. Any object with an
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) or
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) method is accepted (glm,
  glmerMod, lm, lmerMod, …).

- criterion:

  One of:

  `"aic"`

  :   Rank by Akaike Information Criterion (default).

  `"bic"`

  :   Rank by Bayesian Information Criterion.

  `"lrt"`

  :   Sequential likelihood-ratio tests between consecutive models.
      Requires `lme4` for mixed models.

## Value

For `"aic"` and `"bic"`: a data frame with columns `model`, `AIC`/`BIC`,
`delta_AIC`/`delta_BIC`, `winner`. For `"lrt"`: a data frame with
columns `comparison`, `Chisq`, `df`, `p_value`.

## See also

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
[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md),
[`mysterycall_wait_time_summary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_wait_time_summary.md)

## Examples

``` r
m1 <- glm(mpg ~ wt, data = mtcars, family = gaussian())
m2 <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
mysterycall_select_best_model(list(base = m1, full = m2))
#>   model      AIC delta_AIC winner
#> 2  full 156.6523   0.00000   TRUE
#> 1  base 166.0294   9.37709  FALSE
```
