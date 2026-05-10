# Standardise demographic variables for Table 1

A convenience wrapper that applies age imputation, age categorisation,
and gender standardisation in a single call. Columns are added to (not
replacing) the input data frame. Only columns explicitly specified are
processed.

## Usage

``` r
mysterycall_prepare_table1_vars(
  data,
  age_col = NULL,
  grad_year_col = NULL,
  gender_col = NULL,
  setting_col = NULL,
  region_col = NULL,
  ref_year = as.integer(format(Sys.Date(), "%Y"))
)
```

## Arguments

- data:

  A data frame.

- age_col:

  Optional character scalar naming an existing numeric age column. When
  supplied, `age_category` is derived from it.

- grad_year_col:

  Optional character scalar naming a graduation-year column. Used to
  impute age when `age_col` is `NULL`.

- gender_col:

  Optional character scalar naming a gender/sex column. Values are
  standardised to `"Male"`, `"Female"`, or `"Unknown"`.

- setting_col:

  Optional character scalar naming a practice-setting column. Passed
  through as `setting_std`.

- region_col:

  Optional character scalar naming a region column. Passed through as
  `region_std`.

- ref_year:

  Integer reference year for age imputation. Default: current calendar
  year.

## Value

`data` with zero or more additional standardised columns appended:
`age_imputed`, `age_category`, `gender_std`, `setting_std`,
`region_std`.

## See also

Other data management:
[`mysterycall_check_duplicates()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_duplicates.md),
[`mysterycall_check_generalist_presence()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_generalist_presence.md),
[`mysterycall_extract_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_extract_zip5.md),
[`mysterycall_luhn_check()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_luhn_check.md),
[`mysterycall_merge_with_prefix()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_merge_with_prefix.md),
[`mysterycall_stratified_sample()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_stratified_sample.md)

## Examples

``` r
df <- data.frame(
  grad_year = c(1990, 2000, 2010),
  gender    = c("Female", "M", "male")
)
mysterycall_prepare_table1_vars(df, grad_year_col = "grad_year",
                                 gender_col = "gender", ref_year = 2026L)
#>   grad_year gender age_imputed age_category gender_std
#> 1      1990 Female          63        60-69     Female
#> 2      2000      M          53        50-59       Male
#> 3      2010   male          43        40-49       Male
```
