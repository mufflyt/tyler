# Calculate the Minimum Value(s) and Corresponding Level(s) of a Factor Variable

This function returns the level(s) corresponding to the minimum value(s)
of a factor variable.

## Usage

``` r
mysterycall_min_table(InVec, mult = FALSE)
```

## Arguments

- InVec:

  Input vector, expected to be a factor variable or convertible to a
  factor.

- mult:

  Logical value indicating whether to return multiple minimum values or
  just the first one. Default is FALSE.

## Value

If `mult` is FALSE, returns the level corresponding to the minimum value
of the factor variable. If `mult` is TRUE, returns a character vector
containing all the levels with the minimum value.

## See also

Other table:
[`mysterycall_max_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_max_table.md),
[`mysterycall_table_overall()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_overall.md),
[`mysterycall_table_percentages()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_percentages.md),
[`mysterycall_table_proportion()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_proportion.md),
[`mysterycall_write_arsenal_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_arsenal_table.md)

## Examples

``` r
vec <- factor(c("A", "B", "A", "C", "B", "B"))
mysterycall_min_table(vec) # Returns "C"
#> [1] "C"
mysterycall_min_table(vec, mult = TRUE) # Returns "C"
#> [1] "C"
```
