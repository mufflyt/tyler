# Create Individual Isochrone Maps and Shapefiles

This function creates individual Leaflet maps and shapefiles for
specified drive times based on isochrone data.

## Usage

``` r
mysterycall_plot_isochrones(isochrones, drive_times, output_dir = NULL)
```

## Arguments

- isochrones:

  An sf object containing isochrone data.

- drive_times:

  A vector of unique drive times (in minutes) for which maps and
  shapefiles will be created.

- output_dir:

  Directory where HTML maps and shapefiles are saved. Defaults to a
  session-specific folder inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

Called for its side effect of writing per-drive-time map HTML files and
shapefiles to disk. Returns `NULL` invisibly.

## See also

Other mapping:
[`mysterycall_clear_isochrone_cache()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clear_isochrone_cache.md),
[`mysterycall_create_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_create_isochrones.md),
[`mysterycall_isochrones_for_df()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_isochrones_for_df.md),
[`mysterycall_map_acceptance_rate()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_acceptance_rate.md),
[`mysterycall_map_acog_districts()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_acog_districts.md),
[`mysterycall_map_base()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_base.md),
[`mysterycall_map_block_group()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_block_group.md),
[`mysterycall_map_leaflet()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_leaflet.md),
[`mysterycall_map_physicians()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_physicians.md),
[`mysterycall_plot_density()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_density.md),
[`mysterycall_plot_line()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_line.md),
[`mysterycall_plot_scatter()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_scatter.md)

## Examples

``` r
if (FALSE) { # interactive()
# Load required libraries
library(sf)
library(leaflet)
library(mysterycall)

# Load isochrone data
isochrones <- readRDS("path_to_isochrones.rds")

# List of unique drive times for which you want to create plots and shapefiles
drive_times <- unique(isochrones$drive_time)

# Create individual isochrone maps and shapefiles
mysterycall_plot_isochrones(isochrones, drive_times)
}
```
