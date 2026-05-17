# Format an IRR table for manuscript display

Rounds incidence rate ratios and confidence intervals, combines CI into
a single `"lower-upper"` column (en-dash separator), and formats
p-values. Optionally excludes the intercept row.

## Usage

``` r
mysterycall_format_results_table(x, digits = 2L, include_intercept = FALSE)
```

## Arguments

- x:

  A `mysterycall_poisson_model` result or a data frame with at least
  columns `term`, `irr`, `ci_lower`, `ci_upper`, `p_value`.

- digits:

  Integer. Decimal places for IRR and CI columns. Default `2L`.

- include_intercept:

  Logical. Whether to include the `(Intercept)` row. Default `FALSE`.

## Value

A data frame with columns `Term`, `IRR`, `95% CI`, `p-value`. The data
frame carries an integer attribute `"significant_rows"` with the row
indices where `p_value < 0.05`, allowing downstream formatters to apply
bold styling. Intercept row is excluded by default.

## See also

[`mysterycall_poisson_model()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_poisson_model.md)
which produces the model input;
[`mysterycall_write_results_paragraph()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_results_paragraph.md)
to generate a prose summary.

Other manuscript:
[`mysterycall_methods_paragraph()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_methods_paragraph.md),
[`mysterycall_sample_size_text()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_sample_size_text.md),
[`mysterycall_save_plot()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_save_plot.md),
[`mysterycall_summarize_demographics()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_summarize_demographics.md)

## Examples

``` r
if (FALSE) { # interactive()
tbl <- mysterycall_format_results_table(model_result)
knitr::kable(tbl)
}
```
