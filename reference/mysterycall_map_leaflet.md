# Create a Leaflet Base Map

This function creates a Leaflet BASE map with specific configurations,
including the base tile layer, scale bar, default view settings, and
layers control.

## Usage

``` r
mysterycall_map_leaflet()
```

## Value

Invisibly returns the Leaflet map object.

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
[`mysterycall_map_block_group()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_block_group.html),
[`mysterycall_map_physicians()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_physicians.html)

## Examples

``` r
# \donttest{
map <- mysterycall_map_leaflet()
# }
```
