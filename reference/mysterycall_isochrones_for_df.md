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

  HERE API key for authenticating isochrone requests. Defaults to the
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
[`mysterycall_plot_density()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_density.html),
[`mysterycall_plot_isochrones()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_plot_isochrones.html),
[`mysterycall_create_isochrones()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_create_isochrones.html),
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
isochrones_data <- mysterycall_isochrones_for_df("points.csv")
} # }
```
