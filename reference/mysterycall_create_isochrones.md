# Memoized function to try a location with isoline calculations

This function calculates isolines for a given location using a
drive-time routing package.

## Usage

``` r
mysterycall_create_isochrones(
  location,
  range,
  posix_time = as.POSIXct("2023-10-20 08:00:00", format = "%Y-%m-%d %H:%M:%S"),
  api_key = Sys.getenv("HERE_API_KEY")
)
```

## Arguments

- location:

  An sf object representing the location for which isolines will be
  calculated.

- range:

  A numeric vector of time ranges in seconds.

- posix_time:

  A POSIXct object representing the date and time of calculation.
  Default is "2023-10-20 08:00:00".

- api_key:

  API key for the drive-time routing service. Defaults to the
  `HERE_API_KEY` environment variable.

## Value

A named list of sf isolines keyed by range in seconds, or a list with an
`error` element if the calculation fails.

## See also

Other mapping:
[`mysterycall_isochrones_for_df()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_isochrones_for_df.md),
[`mysterycall_map_acceptance_rate()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_acceptance_rate.md),
[`mysterycall_map_acog_districts()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_acog_districts.md),
[`mysterycall_map_base()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_base.md),
[`mysterycall_map_block_group()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_block_group.md),
[`mysterycall_map_leaflet()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_leaflet.md),
[`mysterycall_map_physicians()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_physicians.md),
[`mysterycall_plot_density()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_density.md),
[`mysterycall_plot_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_isochrones.md),
[`mysterycall_plot_line()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_line.md),
[`mysterycall_plot_scatter()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_scatter.md)

## Examples

``` r
if (FALSE) { # interactive()
# Set your routing API key in your Renviron file using the following steps:
# 1. Add key to .Renviron
# 2. Reload .Renviron

# Define a sf object for the location
location <- sf::st_point(c(-73.987, 40.757))

# Calculate isolines for the location with a 30-minute, 60-minute, 120-minute, and 180-minute range
isolines <- mysterycall_create_isochrones(location = location, range = c(1800, 3600, 7200, 10800))

# Print the isolines
print(isolines)

# Free the in-memory cache when done with a batch
mysterycall_clear_isochrone_cache()
}
```
