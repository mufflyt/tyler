# Stacked bar chart of appointment acceptance by group

Summarises a binary 0/1 outcome column by a grouping variable and draws
a 100-percent stacked bar chart using `ggplot2`. Groups are laid out
horizontally (via
[`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html))
with the "accepted" bar segment always on the right so high-acceptance
groups stand out.

## Usage

``` r
mysterycall_plot_stacked_bar(
  data,
  outcome_col,
  group_col,
  fill_labels = c("Not Accepted", "Accepted"),
  colors = c("#E05C6A", "#4A9F70"),
  title = NULL,
  x_label = NULL,
  y_label = "Proportion of Calls (%)",
  show_n = TRUE,
  order_by_rate = TRUE
)
```

## Arguments

- data:

  A data frame containing at least `outcome_col` and `group_col`.

- outcome_col:

  Character scalar. Name of a column with binary values `0` (not
  accepted) or `1` (accepted). `NA` values are excluded silently.

- group_col:

  Character scalar. Name of the x-axis grouping column (e.g. insurance
  type, specialty).

- fill_labels:

  Character vector of length 2. Display labels for the two bar segments.
  First element = 0-outcome label, second = 1-outcome label. Default
  `c("Not Accepted", "Accepted")`.

- colors:

  Character vector of length \>= 2. Fill colours mapped to `fill_labels`
  in order. Default `c("#E05C6A", "#4A9F70")`.

- title:

  Character scalar. Plot title. `NULL` (default) produces no title.

- x_label:

  Character scalar. X-axis label (the grouping axis after coord_flip).
  `NULL` (default) derives a label from `group_col` by replacing
  underscores with spaces and converting to title case.

- y_label:

  Character scalar. Y-axis label (proportion axis). Default
  `"Proportion of Calls (%)"`.

- show_n:

  Logical. When `TRUE` (default) sample counts are overlaid inside each
  bar segment.

- order_by_rate:

  Logical. When `TRUE` (default) groups are sorted by descending
  acceptance rate so the highest-acceptance group appears at the top of
  the flipped chart.

## Value

A `ggplot` object.

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
[`mysterycall_poisson_model()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_poisson_model.md),
[`mysterycall_save_plot()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_save_plot.md),
[`mysterycall_screen_interactions()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_screen_interactions.md),
[`mysterycall_select_best_model()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_select_best_model.md),
[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md),
[`mysterycall_wait_time_summary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_wait_time_summary.md)

## Examples

``` r
set.seed(42)
df <- data.frame(
  insurance = sample(c("Medicaid", "BCBS", "Medicare"), 120, replace = TRUE),
  accepted  = sample(0:1, 120, replace = TRUE)
)
mysterycall_plot_stacked_bar(df, outcome_col = "accepted",
                              group_col  = "insurance")
```
