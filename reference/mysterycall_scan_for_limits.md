# Scan code files for artificial limit patterns

Scans R source files for common limiting anti-patterns that violate the
package policy of processing all available data. This is a code audit
tool to catch unintentional data limiting.

## Usage

``` r
mysterycall_scan_for_limits(
  path = "R",
  recursive = TRUE,
  exclude_pattern = NULL
)
```

## Arguments

- path:

  Directory to scan. Defaults to "R/" directory.

- recursive:

  Whether to scan subdirectories. Defaults to TRUE.

- exclude_pattern:

  Optional regex pattern of files to exclude from scan. For example,
  "test-.\*\\.R\$" to exclude test files.

## Value

A data frame of found issues with columns: file, line, pattern, code.
Returns empty data frame if no issues found. Also prints warnings.

## Details

Searches for these anti-patterns:

- `slice_head(n = )`

- `head(data, n)`

- [`sample_n()`](https://dplyr.tidyverse.org/reference/sample_n.html)

- `n_max = `

- `max_records = `

- `LIMIT n` (SQL)

## See also

Other utilities: `%>%()`,
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
[`mysterycall_standard_labels()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_standard_labels.md),
[`mysterycall_standard_palette()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_standard_palette.md)

## Examples

``` r
if (FALSE) { # interactive()
# Scan all R files in package
issues <- mysterycall_scan_for_limits("R/")

# Scan with exclusions
issues <- mysterycall_scan_for_limits("R/", exclude_pattern = "deprecated")
}
```
