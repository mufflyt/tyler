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

A data frame with columns `Term`, `IRR`, `95% CI`, `p-value`. Rows where
`p_value < 0.05` carry attribute `"significant_rows"` (row indices) so
downstream formatters can apply bold styling.

## See also

Other manuscript:
[`mysterycall_methods_paragraph()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_methods_paragraph.md),
[`mysterycall_sample_size_text()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_sample_size_text.md),
[`mysterycall_summarize_demographics()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_summarize_demographics.md),
[`mysterycall_table1_gtsummary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1_gtsummary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- mysterycall_format_results_table(model_result)
knitr::kable(tbl)
} # }
```
