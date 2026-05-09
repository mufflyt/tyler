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

  routing API key. Defaults to the `HERE_API_KEY` environment variable.

## Value

A named list of sf isolines keyed by range in seconds, or a list with an
`error` element if the calculation fails.

## See also

Other mapping:
[`mysterycall_plot_density()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_density.html),
[`mysterycall_plot_isochrones()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_isochrones.html),
[`mysterycall_isochrones_for_df()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_isochrones_for_df.html),
[`mysterycall_plot_line()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_line.html),
[`mysterycall_plot_scatter()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_scatter.html),
[`mysterycall_map_acog_districts()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_acog_districts.html),
[`mysterycall_map_base()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_base.html),
[`mysterycall_map_block_group()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_block_group.html),
[`mysterycall_map_leaflet()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_leaflet.html),
[`mysterycall_map_physicians()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_physicians.html)

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
