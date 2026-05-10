# Calculate the Proportion of Each Level in a Categorical Variable

This function calculates the proportion of each level in a specified
categorical variable within a data frame. It returns a data frame with
the counts and percentages of each level.

## Usage

``` r
mysterycall_table_proportion(data, variable_name)
```

## Arguments

- data:

  A data frame containing the categorical variable.

- variable_name:

  The name of the categorical variable for which proportions are
  calculated, passed as an unquoted expression.

## Value

A data frame with two columns: `n` (the count of each level) and
`percent` (the percentage of the total count represented by each level).

## Details

The function counts the occurrences of each unique value in the
specified variable and calculates the percentage each value represents
of the total count. The percentages are rounded to two decimal places.

## See also

Other table:
[`mysterycall_max_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_max_table.md),
[`mysterycall_min_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_min_table.md),
[`mysterycall_table_overall()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_overall.md),
[`mysterycall_table_percentages()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_percentages.md),
[`mysterycall_write_arsenal_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_arsenal_table.md)

## Examples

``` r
# Example 1: Basic usage with a simple dataset
data <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female"))
result <- mysterycall_table_proportion(data, gender)
print(result)
#>   gender n percent
#> 1 Female 3      50
#> 2   Male 3      50

# Example 2: Handling a dataset with missing values
df_na <- data.frame(gender = c("Male", NA, "Female", "Female", "Male", "Female", NA))
result <- mysterycall_table_proportion(df_na, gender)
print(result)
#>   gender n percent
#> 1 Female 3   42.86
#> 2   Male 2   28.57
#> 3   <NA> 2   28.57

# Example 3: Using a variable with multiple levels
df_multi <- data.frame(grade = c("A", "B", "A", "C", "B", "A", "C", "B"))
result <- mysterycall_table_proportion(df_multi, grade)
print(result)
#>   grade n percent
#> 1     A 3    37.5
#> 2     B 3    37.5
#> 3     C 2    25.0
```
