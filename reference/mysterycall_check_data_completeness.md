# Assess completeness for required data columns

Assess completeness for required data columns

## Usage

``` r
mysterycall_check_data_completeness(
  data,
  required = NULL,
  id_cols = NULL,
  thresholds = c(high = 0.9, medium = 0.75)
)
```

## Arguments

- data:

  A data frame to assess.

- required:

  Columns that must be present and non-missing.

- id_cols:

  Optional identifier columns used to compute uniqueness.

- thresholds:

  Named numeric vector with `high` and `medium` breakpoints between 0
  and 1 determining the quality tier.

## Value

A list containing `summary` (tibble of completeness metrics) and
`quality` (overall quality tier).

## See also

Other utilities:
[`mysterycall_assess_data_quality()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_assess_data_quality.md),
[`mysterycall_check_api_response()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_api_response.md),
[`mysterycall_check_dependencies()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_dependencies.md),
[`mysterycall_check_no_data_loss()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_no_data_loss.md),
[`mysterycall_check_no_limits()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_no_limits.md),
[`mysterycall_download_file()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_download_file.md),
[`mysterycall_estimate_resources()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_estimate_resources.md),
[`mysterycall_export_with_backup()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_export_with_backup.md),
[`mysterycall_format_pct()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_format_pct.md),
[`mysterycall_preflight_check()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_preflight_check.md),
[`mysterycall_quality_tier()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_quality_tier.md),
[`mysterycall_remove_constants()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_remove_constants.md),
[`mysterycall_remove_near_zero()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_remove_near_zero.md),
[`mysterycall_resolve_path()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_resolve_path.md),
[`mysterycall_save_quality_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_save_quality_table.md),
[`mysterycall_scan_for_limits()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_scan_for_limits.md),
[`mysterycall_standard_labels()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_standard_labels.md),
[`mysterycall_standard_palette()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_standard_palette.md)

## Examples

``` r
df <- tibble::tibble(id = 1:3, value = c(1, NA, 3))
mysterycall_check_data_completeness(df, required = c("id", "value"))
#> $summary
#> # A tibble: 2 × 3
#>   column completeness missing
#>   <chr>         <dbl>   <dbl>
#> 1 id            1       0    
#> 2 value         0.667   0.333
#> 
#> $quality
#> [1] "medium"
#> 
#> $score
#> [1] 0.8333333
#> 
```
