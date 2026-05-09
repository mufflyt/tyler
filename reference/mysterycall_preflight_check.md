# Run comprehensive preflight checks before workflow

Validates all requirements for a successful workflow run including API
keys, data quality, output directories, estimated resources, and
dependencies. Provides a go/no-go decision before starting long-running
operations.

## Usage

``` r
mysterycall_preflight_check(
  input_data,
  output_dir,
  google_maps_api_key = NULL,
  here_api_key = NULL,
  check_apis = TRUE,
  estimate_resources = TRUE,
  interactive = interactive(),
  required_columns = c("first", "last")
)
```

## Arguments

- input_data:

  Path to input data file or data frame

- output_dir:

  Output directory path

- google_maps_api_key:

  Google Maps API key (optional if not geocoding)

- here_api_key:

  HERE API key (optional if not creating isochrones)

- check_apis:

  Whether to validate API keys with test calls (default: TRUE)

- estimate_resources:

  Whether to estimate runtime and memory (default: TRUE)

- interactive:

  Whether to prompt user for confirmation (default: TRUE)

- required_columns:

  Required column names in input data

## Value

Invisible list with check results, or stops with error if checks fail

## See also

Other utilities:
[`mysterycall_download_file()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_download_file.html),
[`mysterycall_format_pct()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_format_pct.html),
[`mysterycall_remove_constants()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_remove_constants.html),
[`mysterycall_remove_near_zero()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_remove_near_zero.html),
[`mysterycall_save_quality_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_save_quality_table.html),
[`mysterycall_assess_data_quality()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_assess_data_quality.md),
[`mysterycall_check_api_response()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_api_response.md),
[`mysterycall_check_data_completeness()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_data_completeness.md),
[`mysterycall_check_dependencies()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_dependencies.md),
[`mysterycall_check_no_data_loss()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_no_data_loss.md),
[`mysterycall_check_no_limits()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_no_limits.md),
[`mysterycall_estimate_resources()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_estimate_resources.md),
[`mysterycall_export_with_backup()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_export_with_backup.md),
[`mysterycall_quality_tier()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_quality_tier.md),
[`mysterycall_resolve_path()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_resolve_path.md),
[`mysterycall_scan_for_limits()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_scan_for_limits.md),
[`mysterycall_standard_labels()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_standard_labels.md),
[`mysterycall_standard_palette()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_standard_palette.md),
[`mysterycall_use_quiet_logging()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_use_quiet_logging.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic preflight check
mysterycall_preflight_check(
  input_data = "physicians.csv",
  output_dir = "output/",
  google_maps_api_key = Sys.getenv("GOOGLE_API_KEY"),
  here_api_key = Sys.getenv("HERE_API_KEY")
)

# Non-interactive (for scripts)
mysterycall_preflight_check(
  input_data = data,
  output_dir = "output/",
  interactive = FALSE
)
} # }
```
