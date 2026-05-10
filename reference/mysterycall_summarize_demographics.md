# One-line demographic summary for a study cohort

Builds a compact summary string of the form
`"N = X; Y% female; Z% academic"` from a data frame. Any component whose
source column is omitted is silently excluded.

## Usage

``` r
mysterycall_summarize_demographics(
  data,
  female_col = NULL,
  setting_col = NULL,
  academic_label = "Academic"
)
```

## Arguments

- data:

  A data frame.

- female_col:

  Character scalar naming a column that encodes sex/gender. Recognized
  values (case-insensitive): `"female"`, `"f"` for female; `"male"`,
  `"m"` for male. Logical columns (`TRUE` = female) are also accepted.

- setting_col:

  Character scalar naming the practice-setting column.

- academic_label:

  Character scalar for the academic label used in `setting_col`. Default
  `"Academic"`.

## Value

A single character string.

## See also

Other manuscript:
[`mysterycall_format_results_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_format_results_table.md),
[`mysterycall_methods_paragraph()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_methods_paragraph.md),
[`mysterycall_sample_size_text()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_sample_size_text.md),
[`mysterycall_table1_gtsummary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1_gtsummary.md)

## Examples

``` r
df <- data.frame(
  gender  = c("Female", "Male", "Female", NA),
  setting = c("Academic", "Private Practice", "Academic", "Academic")
)
mysterycall_summarize_demographics(df, female_col = "gender",
                                   setting_col = "setting")
#> [1] "N = 4; 50.0% female; 75.0% academic"
```
