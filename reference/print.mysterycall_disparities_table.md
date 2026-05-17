# Print a mysterycall_disparities_table

Prints a formatted table of disparity metrics with group sizes,
acceptance rates, Wilson confidence intervals, absolute risk
differences, relative risks, and p-values versus the reference group.
Column headers are labelled using the `ci_method` and `alpha` attributes
stored on the object.

## Usage

``` r
# S3 method for class 'mysterycall_disparities_table'
print(x, ...)
```

## Arguments

- x:

  A `mysterycall_disparities_table` object returned by
  [`mysterycall_disparities_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_disparities_table.md).

- ...:

  Ignored.

## Value

Invisibly returns `x`.

## See also

[`mysterycall_disparities_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_disparities_table.md)

Other table helpers:
[`mysterycall_disparities_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_disparities_table.md),
[`mysterycall_format_pct()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_format_pct.md),
[`mysterycall_max_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_max_table.md),
[`mysterycall_min_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_min_table.md),
[`mysterycall_model_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_model_table.md),
[`mysterycall_table_percentages()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_percentages.md),
[`mysterycall_table_proportion()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_proportion.md)
