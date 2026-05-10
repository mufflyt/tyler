# Create and Save a Leaflet Dot Map of Physicians

This function creates a Leaflet dot map of physicians using their
longitude and latitude coordinates. It also adds ACOG district
boundaries to the map and saves it as an HTML file with an accompanying
PNG screenshot.

## Usage

``` r
mysterycall_map_physicians(
  physician_data,
  jitter_range = 0.05,
  color_palette = "magma",
  popup_var = "name",
  output_dir = NULL
)
```

## Arguments

- physician_data:

  An sf object containing physician data with `"long"` and `"lat"`
  columns.

- jitter_range:

  The range for adding jitter to latitude and longitude coordinates.

- color_palette:

  The color palette for ACOG district colors.

- popup_var:

  The variable to use for popup text.

- output_dir:

  Directory where the HTML map and PNG screenshot are saved. Defaults to
  a session-specific temporary folder.

## Value

Invisibly returns the Leaflet map object.

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
[`mysterycall_plot_density()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_density.md),
[`mysterycall_plot_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_isochrones.md),
[`mysterycall_plot_line()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_line.md),
[`mysterycall_plot_scatter()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_scatter.md)

## Examples

``` r
if (FALSE) { # interactive()
# Load required libraries
library(viridis)
library(leaflet)

# Generate physician data (replace with your own data)
physician_data <- data.frame(
  long = c(-95.363271, -97.743061, -98.493628, -96.900115, -95.369803),
  lat = c(29.763283, 30.267153, 29.424349, 32.779167, 29.751808),
  name = c("Physician 1", "Physician 2", "Physician 3", "Physician 4", "Physician 5"),
  ACOG_District = c("District I", "District II", "District III", "District IV", "District V")
)

# Create and save the dot map
mysterycall_map_physicians(physician_data)
}
```
