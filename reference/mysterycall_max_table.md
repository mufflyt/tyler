# Calculate the Maximum Value(s) and Corresponding Level(s) of a Factor Variable

This function returns the level(s) corresponding to the maximum value(s)
of a factor variable.

## Usage

``` r
mysterycall_max_table(InVec, mult = FALSE)
```

## Arguments

- InVec:

  Input vector, expected to be a factor variable or convertible to a
  factor.

- mult:

  Logical value indicating whether to return multiple maximum values or
  just the first one. Default is FALSE.

## Value

If `mult` is FALSE, returns the level corresponding to the maximum value
of the factor variable. If `mult` is TRUE, returns a character vector
containing all the levels with the maximum value.

## See also

Other table:
[`mysterycall_write_arsenal_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_write_arsenal_table.html),
[`mysterycall_min_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_min_table.html),
[`mysterycall_table_percentages()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_table_percentages.html),
[`mysterycall_table_proportion()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_table_proportion.html),
[`mysterycall_table_overall()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_table_overall.html)

## Examples

``` r
vec <- factor(c("A", "B", "A", "C", "B", "B"))
mysterycall_max_table(vec) # Returns "B"
#> [1] "B"
mysterycall_max_table(vec, mult = TRUE) # Returns "B"
#> [1] "B"
```
