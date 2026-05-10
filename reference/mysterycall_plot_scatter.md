# Create a Scatter Plot for Mystery Caller Studies with Optional Transformations, Jitter, and Custom Labels

This function generates a scatter plot designed for mystery caller
studies, allowing for the visualization of waiting times or similar
outcomes across different categories, such as insurance types. The
function supports transformations on the y-axis, custom jitter, and
colors each category in the x-axis using the `viridis` color palette.
The plot is automatically displayed and saved with a specified
resolution.

## Usage

``` r
mysterycall_plot_scatter(
  plot_data,
  x_var,
  y_var,
  y_transform = "none",
  dpi = 600,
  output_dir = NULL,
  file_prefix = "scatter_plot",
  jitter_width = 0.2,
  jitter_height = 0,
  point_alpha = 0.6,
  x_label = NULL,
  y_label = NULL,
  plot_title = NULL,
  verbose = TRUE
)
```

## Arguments

- plot_data:

  A dataframe containing the data to be plotted. Must contain the
  variables specified in `x_var` and `y_var`.

- x_var:

  A string representing the column name for the x-axis variable. This
  should be a categorical or factor variable (e.g., insurance type).

- y_var:

  A string representing the column name for the y-axis variable. This
  should be a numeric variable (e.g., waiting time in days).

- y_transform:

  A string specifying the transformation for the y-axis: "log" for log
  transformation (log1p), "sqrt" for square root transformation, or
  "none" for no transformation. Default is "none".

- dpi:

  An integer specifying the resolution of the saved plot in dots per
  inch (DPI). Default is 600 for print-ready outputs.

- output_dir:

  A string representing the directory where the plot files will be
  saved. Defaults to a session-specific folder inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- file_prefix:

  A string used as the prefix for the generated plot filenames. The
  filenames will have a timestamp appended to ensure uniqueness. Default
  is "scatter_plot".

- jitter_width:

  A numeric value specifying the width of the jitter along the x-axis.
  Default is 0.2.

- jitter_height:

  A numeric value specifying the height of the jitter along the y-axis.
  Default is 0.

- point_alpha:

  A numeric value specifying the transparency level of the points.
  Default is 0.6.

- x_label:

  A string specifying the label for the x-axis. Default is `NULL` (uses
  x_var).

- y_label:

  A string specifying the label for the y-axis. Default is `NULL` (uses
  y_var or transformed version).

- plot_title:

  A string specifying the title of the plot. Default is `NULL` (no
  title).

- verbose:

  A boolean indicating whether to print messages about the saved plot
  locations. Default is TRUE.

## Value

Invisibly returns the generated ggplot object.

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
[`mysterycall_plot_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_isochrones.md),
[`mysterycall_plot_line()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_line.md)

## Examples

``` r
# \donttest{
example_data <- data.frame(
  insurance = rep(c("Medicaid", "Commercial", "Medicare"), each = 3),
  business_days_until_appointment = c(1.5, 2.0, 2.8, 1.9, 2.4, 2.6, 2.1, 2.7, 3.0)
)

mysterycall_plot_scatter(
  plot_data = example_data,
  x_var = "insurance",
  y_var = "business_days_until_appointment",
  y_transform = "none",
  dpi = 50,
  output_dir = tempdir(),
  file_prefix = "demo_scatter",
  x_label = "Insurance",
  y_label = "Waiting Times in Days",
  plot_title = "Example Scatter Plot",
  verbose = FALSE
)
# }
```
