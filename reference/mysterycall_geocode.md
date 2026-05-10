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
  quiet = getOption("tyler.quiet", FALSE),
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
