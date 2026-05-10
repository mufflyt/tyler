# Publication-ready Table 1 via gtsummary

Wraps
[`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
with opinionated defaults suitable for mystery-caller study
demographics: no "Unknown" in missing-data reporting (use the `missing`
argument to change), bold labels, an overall column, and optional
p-values for stratified tables.

## Usage

``` r
mysterycall_table1_gtsummary(
  data,
  vars,
  strata_col = NULL,
  label_list = NULL,
  missing = "no",
  ...
)
```

## Arguments

- data:

  A data frame.

- vars:

  Character vector of variable names to include in the table. Must all
  be present in `data`.

- strata_col:

  Optional character scalar naming a stratification column (e.g.
  `"gender"`, `"insurance"`). When supplied, an overall column and
  p-values are added automatically.

- label_list:

  Optional named list mapping variable names to display labels, passed
  to `gtsummary::tbl_summary(label = ...)`.

- missing:

  One of `"no"` (default), `"ifany"`, or `"always"`. Passed directly to
  [`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html).

- ...:

  Additional arguments forwarded to
  [`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html).

## Value

A `gtsummary` `tbl_summary` object. Print it directly or export with
[`gtsummary::as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/reference/as_flex_table.html)
/
[`gtsummary::as_gt()`](https://www.danieldsjoberg.com/gtsummary/reference/as_gt.html).

## See also

Other manuscript:
[`mysterycall_format_results_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_format_results_table.md),
[`mysterycall_methods_paragraph()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_methods_paragraph.md),
[`mysterycall_sample_size_text()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_sample_size_text.md),
[`mysterycall_summarize_demographics()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_summarize_demographics.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  gender   = sample(c("Male","Female"), 100, replace = TRUE),
  setting  = sample(c("Academic","Private"), 100, replace = TRUE),
  age_cat  = sample(c("30-39","40-49","50-59"), 100, replace = TRUE),
  insurance = sample(c("BCBS","Medicaid"), 100, replace = TRUE)
)
tbl <- mysterycall_table1_gtsummary(
  df,
  vars       = c("gender", "setting", "age_cat"),
  strata_col = "insurance"
)
tbl
} # }
```
