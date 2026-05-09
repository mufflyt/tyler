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
[`mysterycall_plot_density()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_density.html),
[`mysterycall_create_isochrones()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_create_isochrones.html),
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
} # }
```
