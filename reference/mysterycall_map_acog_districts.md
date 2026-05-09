# Create `sf` Polygons for ACOG Districts

This helper reads the packaged ACOG district lookup table and joins it
with state geometries from Natural Earth to construct polygons for each
district. The resulting object can be used to add district boundaries to
Leaflet maps or for further spatial analysis.

## Usage

``` r
mysterycall_map_acog_districts(acog_districts_file = NULL)
```

## Arguments

- acog_districts_file:

  Optional path to a CSV containing the mapping of states to ACOG
  districts. Defaults to the packaged `inst/extdata/ACOG_Districts.csv`.

## Value

An `sf` object with one row per ACOG district and columns describing the
district name, subregion, member states, and their abbreviations.

## See also

Other mapping:
[`mysterycall_plot_density()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_density.html),
[`mysterycall_plot_isochrones()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_isochrones.html),
[`mysterycall_create_isochrones()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_create_isochrones.html),
[`mysterycall_isochrones_for_df()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_isochrones_for_df.html),
[`mysterycall_plot_line()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_line.html),
[`mysterycall_plot_scatter()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_scatter.html),
[`mysterycall_map_base()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_base.html),
[`mysterycall_map_block_group()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_block_group.html),
[`mysterycall_map_leaflet()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_leaflet.html),
[`mysterycall_map_physicians()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_physicians.html)

## Examples

``` r
if (FALSE) { # \dontrun{
mysterycall_map_acog_districts()
mysterycall_map_acog_districts("inst/extdata/ACOG_Districts.csv")
} # }
```
