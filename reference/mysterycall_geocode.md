# Geocode unique addresses from a file

Reads an input dataset containing an `address` column, geocodes the
unique addresses using the Google Maps API via
[`ggmap::geocode`](https://rdrr.io/pkg/ggmap/man/geocode.html), and
returns the data with additional `latitude` and `longitude` columns.
Optionally writes the result to `output_file_path`.

## Usage

``` r
mysterycall_geocode(
  file_path,
  google_maps_api_key,
  output_file_path = NULL,
  failed_output_path = NULL,
  notify = TRUE,
  quiet = getOption("mysterycall.quiet", FALSE),
  tracker = NULL,
  tracker_step = "Geocoding"
)
```

## Arguments

- file_path:

  Path to a CSV, RDS or XLSX file containing an `address` column.

- google_maps_api_key:

  A valid Google Maps API key.

- output_file_path:

  Optional path to save the geocoded dataset as CSV.

- failed_output_path:

  Optional path that captures rows that failed to geocode after all
  retries. When supplied, a timestamped backup is created before
  overwriting existing results.

- notify:

  Logical. If `TRUE`, play a notification sound when geocoding finishes
  (requires the optional `beepr` package). Defaults to `TRUE`.

- quiet:

  Logical flag controlling log verbosity. Defaults to the package
  quiet-mode option.

- tracker:

  Optional progress tracker created with
  [`mysterycall_progress_tracker()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_tracker.md).
  When supplied, the step named by `tracker_step` is automatically
  started and marked as complete or failed with an appropriate quality
  tier.

- tracker_step:

  Character string describing the step name used when updating
  `tracker`.

## Value

A data frame with latitude and longitude columns added.

## Requirements

Geocoding requires the `ggmap` package (listed in `Suggests`, not
`Imports`) and a Google Maps Platform API key with the **Geocoding API**
enabled. Steps to obtain a key:

1.  Create a project at <https://console.cloud.google.com/>.

2.  Enable the **Geocoding API** under APIs & Services → Library.

3.  Create a key under APIs & Services → Credentials.

4.  Enable **billing** on the project (required even for free-tier
    usage; Google provides a \$200/month credit that covers ~40,000
    geocodes).

Common errors and remedies:

|                       |                                           |
|-----------------------|-------------------------------------------|
| **Error message**     | **Cause / fix**                           |
| `"ggmap is required"` | Run `install.packages("ggmap")`.          |
| `"REQUEST_DENIED"`    | Geocoding API not enabled for the key.    |
| `"OVER_QUERY_LIMIT"`  | Daily free quota (40,000 calls) exceeded. |
| `"INVALID_REQUEST"`   | Malformed address string in the data.     |

An invalid or expired key is only detected at the first geocoding
request, not at function entry. Run
[`mysterycall_preflight_check()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_preflight_check.md)
with `check_apis = TRUE` before a long workflow to catch key problems
early.

## See also

Other geospatial helpers:
[`mysterycall_calculate_overlap()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_calculate_overlap.md),
[`mysterycall_hrr()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_hrr.md)

## Examples

``` r
if (FALSE) { # interactive()
result <- mysterycall_geocode("addresses.csv", "my_api_key")
}
```
