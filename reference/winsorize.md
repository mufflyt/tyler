# Winsorize extreme values

Clips values to specified quantile bounds. Useful for access-score maps
where a handful of rural tracts with extreme values (0% or 100%
coverage) dominate the color scale and obscure variation in the interior
of the distribution.

## Usage

``` r
winsorize(x, lower = 0.005, upper = 0.995, na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector to winsorize.

- lower:

  Numeric. Lower quantile bound (default 0.005 = 0.5th percentile).

- upper:

  Numeric. Upper quantile bound (default 0.995 = 99.5th percentile).

- na.rm:

  Logical. Remove NAs before computing quantiles (default `TRUE`).

## Value

Numeric vector, same length as `x`.

## See also

[`truncate_for_viz()`](https://mufflyt.github.io/mysterycall/reference/truncate_for_viz.md)

Other green-journal-spatial:
[`compose_map_density()`](https://mufflyt.github.io/mysterycall/reference/compose_map_density.md),
[`crs_albers_conus()`](https://mufflyt.github.io/mysterycall/reference/crs_albers_conus.md),
[`truncate_for_viz()`](https://mufflyt.github.io/mysterycall/reference/truncate_for_viz.md)

## Examples

``` r
x <- c(0, 5, 10, 50, 90, 95, 100)
winsorize(x, lower = 0.1, upper = 0.9)
#> [1]  3  5 10 50 90 95 97
```
