# Format a Numeric Value as a Percentage

This function formats a numeric value as a percentage with a specified
number of decimal places.

## Usage

``` r
mysterycall_format_pct(x, my_digits = 1)
```

## Arguments

- x:

  A numeric value or vector that you want to format as a percentage.

- my_digits:

  An integer specifying the number of decimal places to include in the
  formatted percentage. The default is 1.

## Value

A character vector representing the formatted percentage(s) with the
specified number of decimal places.

## Details

The function converts a numeric value to a percentage format with the
specified number of decimal places. This is useful for consistent
display of percentage values in reports or visualizations.

## See also

Other utilities:
[`mysterycall_assess_data_quality()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_assess_data_quality.md),
[`mysterycall_check_api_response()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_api_response.md),
[`mysterycall_check_data_completeness()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_data_completeness.md),
[`mysterycall_check_dependencies()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_dependencies.md),
[`mysterycall_check_no_data_loss()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_no_data_loss.md),
[`mysterycall_check_no_limits()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_no_limits.md),
[`mysterycall_download_file()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_download_file.md),
[`mysterycall_estimate_resources()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_estimate_resources.md),
[`mysterycall_export_with_backup()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_export_with_backup.md),
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
# Example 1: Format a single numeric value
result <- mysterycall_format_pct(0.12345)
print(result)  # Output: "12.3%"
#> [1] "12.3%"

# Example 2: Format a vector of numeric values with 2 decimal places
values <- c(0.12345, 0.6789, 0.54321)
formatted_values <- mysterycall_format_pct(values, my_digits = 2)
print(formatted_values)  # Output: "12.35%", "67.89%", "54.32%"
#> [1] "12.35%" "67.89%" "54.32%"

# Example 3: Format a value with no decimal places
no_decimal <- mysterycall_format_pct(0.5, my_digits = 0)
print(no_decimal)  # Output: "50%"
#> [1] "50%"
```
