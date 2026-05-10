# Get isochrones for each point in a dataframe

This function retrieves isochrones for each point in a given dataframe
by looping over the rows and calling the mysterycall_create_isochrones
function for each point.

## Usage

``` r
mysterycall_isochrones_for_df(
  input_file,
  breaks = c(1800, 3600, 7200, 10800),
  api_key = Sys.getenv("HERE_API_KEY"),
  output_dir = NULL,
  save_interval = 240
)
```

## Arguments

- input_file:

  A path to the input file containing points for which isochrones are to
  be retrieved.

- breaks:

  A numeric vector specifying the breaks for categorizing drive times
  (default is c(1800, 3600, 7200, 10800)).

- api_key:

  API key for the drive-time routing service. Defaults to the
  `HERE_API_KEY` environment variable.

- output_dir:

  Directory where intermediate `.rds` results are written. Defaults to a
  session-specific folder beneath
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- save_interval:

  Number of seconds between automatic checkpoint saves. Defaults to 240
  seconds (~4 minutes).

## Value

A dataframe containing the isochrones data with added 'name' column.

## See also

Other mapping:
[`mysterycall_create_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_create_isochrones.md),
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
isochrones_data <- mysterycall_isochrones_for_df("points.csv")
}
```
