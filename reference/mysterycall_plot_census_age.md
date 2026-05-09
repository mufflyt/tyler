# Plot the distribution of female age groups

Plot the distribution of female age groups

## Usage

``` r
mysterycall_plot_census_age(
  census_df,
  group_var = NULL,
  output_dir = NULL,
  file_prefix = "census_age_distribution",
  dpi = 600,
  verbose = TRUE
)
```

## Arguments

- census_df:

  A data frame produced by
  [`mysterycall_get_census_data()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_get_census_data.html)
  (or another table containing equivalent columns).

- group_var:

  Optional single column name used to facet the distribution. When
  supplied, stacked bars are produced for each value of `group_var`. Set
  to `NULL` (the default) to visualize the aggregate distribution across
  all rows in `census_df`.

- output_dir:

  Directory where image files should be written. Defaults to a
  session-specific directory from
  [`mysterycall_tempdir()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_tempdir.md).

- file_prefix:

  Prefix used when writing plot files. Defaults to
  "census_age_distribution".

- dpi:

  Resolution used when saving the plots (defaults to 600 DPI).

- verbose:

  When `TRUE`, prints the output file paths after saving.

## Value

Invisibly returns the generated ggplot object.

## See also

Other census:
[`mysterycall_get_census_data()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_get_census_data.html),
[`mysterycall_summarize_census()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_summarize_census.html)

## Examples

``` r
if (FALSE) { # \dontrun{
  mysterycall_plot_census_age(census_example, group_var = "statefp", verbose = FALSE)
} # }
```
