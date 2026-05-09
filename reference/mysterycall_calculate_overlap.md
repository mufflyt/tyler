# Calculate intersection overlap and save results to shapefiles.

This function calculates the intersection between block groups and
isochrones for a specific drive time and saves the results to a
shapefile. To ensure accurate area-based calculations, both datasets are
temporarily projected to an equal-area CRS before measuring.

## Usage

``` r
mysterycall_calculate_overlap(
  block_groups,
  isochrones_joined,
  drive_time_minutes,
  output_dir,
  crosswalk = NULL,
  notify = TRUE
)
```

## Arguments

- block_groups:

  An sf object representing block groups.

- isochrones_joined:

  An sf object representing isochrones.

- drive_time_minutes:

  The drive time value (in minutes) for which to calculate the
  intersection.

- output_dir:

  The directory where the intersection shapefile will be saved.

- crosswalk:

  Optional function used to translate ACS geographies between 2010 and
  2020 definitions. When supplied, it should accept two arguments: an sf
  object and a named list with `from` and `to` years, returning an sf
  object whose `vintage` column matches the provider `data_year`.

- notify:

  Logical. If `TRUE`, play a notification sound on completion when the
  optional `beepr` package is available. Defaults to `TRUE`.

## Value

Called for its side effect of saving the intersection overlap shapefile
and CSV.

## See also

Other geospatial helpers: `mysterycall_geocode()`, `mysterycall_hrr()`

## Examples

``` r
if (FALSE) { # \dontrun{
mysterycall_calculate_overlap(block_groups, isochrones_joined, 30L, "data/shp/")
} # }
```
