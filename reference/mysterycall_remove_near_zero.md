# Remove Near-Zero Variance Variables from a Data Frame

This function takes a data frame and returns a new data frame with
near-zero variance variables removed.

## Usage

``` r
mysterycall_remove_near_zero(data_frame, freqCut = 19, uniqueCut = 10)
```

## Arguments

- data_frame:

  A data frame from which near-zero variance variables should be
  removed.

- freqCut:

  The ratio of the most common value to the second most common value.
  Defaults to 19.

- uniqueCut:

  The percentage of distinct values out of the number of total samples.
  Defaults to 10.

## Value

A data frame with near-zero variance variables removed.

## See also

Other utilities:
[`mysterycall_download_file()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_download_file.html),
[`mysterycall_format_pct()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_format_pct.html),
[`mysterycall_remove_constants()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_remove_constants.html),
[`mysterycall_save_quality_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_save_quality_table.html),
[`mysterycall_assess_data_quality()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_assess_data_quality.md),
[`mysterycall_check_api_response()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_api_response.md),
[`mysterycall_check_data_completeness()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_data_completeness.md),
[`mysterycall_check_dependencies()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_dependencies.md),
[`mysterycall_check_no_data_loss()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_no_data_loss.md),
[`mysterycall_check_no_limits()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_no_limits.md),
[`mysterycall_estimate_resources()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_estimate_resources.md),
[`mysterycall_export_with_backup()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_export_with_backup.md),
[`mysterycall_preflight_check()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_preflight_check.md),
[`mysterycall_quality_tier()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_quality_tier.md),
[`mysterycall_resolve_path()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_resolve_path.md),
[`mysterycall_scan_for_limits()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_scan_for_limits.md),
[`mysterycall_standard_labels()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_standard_labels.md),
[`mysterycall_standard_palette()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_standard_palette.md),
[`mysterycall_use_quiet_logging()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_use_quiet_logging.md)

## Examples

``` r
if (FALSE) { # \dontrun{
new_data <- mysterycall_remove_near_zero(data_frame)
} # }
```
