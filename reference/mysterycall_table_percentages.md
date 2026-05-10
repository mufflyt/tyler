# Calculate the Percentage of the Most Common Value in a Categorical Variable

This function calculates the percentage of the most common value in a
specified categorical variable from a data frame.

## Usage

``` r
mysterycall_table_percentages(data_frame, variable)
```

## Arguments

- data_frame:

  A data frame containing the categorical variable.

- variable:

  A character string representing the name of the categorical variable
  within `data_frame`.

## Value

A data frame containing the most common value and its count, along with
the percentage of the total count that it represents.

## Details

The function converts the variable name to a character string, then
counts the occurrences of each unique value in the specified column. It
calculates the percentage each value represents of the total and returns
the most common value with its count and percentage.

## See also

Other table:
[`mysterycall_max_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_max_table.md),
[`mysterycall_min_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_min_table.md),
[`mysterycall_table_overall()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_overall.md),
[`mysterycall_table_proportion()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_proportion.md),
[`mysterycall_write_arsenal_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_arsenal_table.md)

## Examples

``` r
# Example 1: Basic usage with a simple dataset
data_frame <- data.frame(category = c("A", "B", "A", "C", "A", "B", "B", "A"))
result <- mysterycall_table_percentages(data_frame, "category")
print(result)
#>   category n percent
#> 1        A 4    50.0
#> 2        B 3    37.5
#> 3        C 1    12.5

# Example 2: Using a dataset with multiple most common values
df_tie <- data.frame(category = c("A", "B", "A", "B", "C", "C", "C", "A", "B"))
result <- mysterycall_table_percentages(df_tie, "category")
print(result)
#>   category n percent
#> 1        A 3    33.3
#> 2        B 3    33.3
#> 3        C 3    33.3

# Example 3: Handling a dataset with missing values
df_na <- data.frame(category = c("A", NA, "A", "C", "A", "B", "B", NA))
result <- mysterycall_table_percentages(df_na, "category")
print(result)
#>   category n percent
#> 1        A 3    37.5
#> 2        B 2    25.0
#> 3     <NA> 2    25.0
#> 4        C 1    12.5
```
