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
[`mysterycall_create_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_create_isochrones.md),
[`mysterycall_isochrones_for_df()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_isochrones_for_df.md),
[`mysterycall_map_acceptance_rate()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_acceptance_rate.md),
[`mysterycall_map_acog_districts()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_acog_districts.md),
[`mysterycall_map_base()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_base.md),
[`mysterycall_map_block_group()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_block_group.md),
[`mysterycall_map_physicians()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_physicians.md),
[`mysterycall_plot_density()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_density.md),
[`mysterycall_plot_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_isochrones.md),
[`mysterycall_plot_line()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_line.md),
[`mysterycall_plot_scatter()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_scatter.md)

## Examples

``` r
# \donttest{
map <- mysterycall_map_leaflet()
# }
```
