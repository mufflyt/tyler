# Function to create and export a map showing block group overlap with isochrones

This function creates a map that displays block groups and their overlap
with isochrones. The map is exported as an HTML file and a PNG image.

## Usage

``` r
mysterycall_map_block_group(
  bg_data,
  isochrones_data,
  output_dir = "figures/"
)
```

## Arguments

- bg_data:

  A SpatialPolygonsDataFrame representing block group data.

- isochrones_data:

  A SpatialPolygonsDataFrame representing isochrone data.

- output_dir:

  Directory path for exporting the map files. Default is "figures/".

## Value

Called for its side effects of saving the block group overlap map as
HTML and PNG inside `output_dir`. Returns `NULL` invisibly.

## See also

Other mapping:
[`mysterycall_plot_density()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_density.html),
[`mysterycall_plot_isochrones()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_isochrones.html),
[`mysterycall_create_isochrones()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_create_isochrones.html),
[`mysterycall_isochrones_for_df()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_isochrones_for_df.html),
[`mysterycall_plot_line()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_line.html),
[`mysterycall_plot_scatter()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_scatter.html),
[`mysterycall_map_acog_districts()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_acog_districts.html),
[`mysterycall_map_base()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_base.html),
[`mysterycall_map_leaflet()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_leaflet.html),
[`mysterycall_map_physicians()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_physicians.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create and export the map with the default output directory
mysterycall_map_block_group(block_groups, isochrones_joined_map)

# Create and export the map with a custom output directory
mysterycall_map_block_group(block_groups, isochrones_joined_map, "custom_output/")
} # }
```
