# Calculate and Summarize Physician Age

This function calculates the median age, as well as the 25th and 75th
percentiles (Interquartile Range, IQR) of a specified age column in a
data frame. It returns a sentence summarizing these statistics.

## Usage

``` r
mysterycall_physician_age(data, age_column)
```

## Arguments

- data:

  A data frame containing the age data.

- age_column:

  A character string representing the name of the column in `data` that
  contains the age data.

## Value

A character string summarizing the median age and IQR of the specified
age column in the dataset.

## Details

The function calculates the median, 25th percentile (Q1), and 75th
percentile (Q3) of the age data, rounding the results to two decimal
places for the median and one decimal place for the percentiles. It then
constructs a summary sentence describing these statistics.

## See also

Other summary:
[`mysterycall_not_contacted_states()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_not_contacted_states.md)

## Examples

``` r
# Example 1: Basic usage with a small dataset
data <- data.frame(age = c(30, 40, 50, 60, 35, 45, 55, 65))
summary_sentence <- mysterycall_physician_age(data, "age")
print(summary_sentence)
#> [1] "The median age of the dataset was 47.5 (IQR 25th percentile 38.8 to 75th percentile 56.2)."

# Example 2: Handling missing data
df_with_na <- data.frame(age = c(30, 40, NA, 60, 35, NA, 55, 65))
summary_sentence <- mysterycall_physician_age(df_with_na, "age")
print(summary_sentence)
#> [1] "The median age of the dataset was 47.5 (IQR 25th percentile 36.2 to 75th percentile 58.8)."

# Example 3: Different age distribution
df_large <- data.frame(age = c(rep(30, 70), rep(40, 30), rep(50, 20), rep(60, 10)))
summary_sentence <- mysterycall_physician_age(df_large, "age")
print(summary_sentence)
#> [1] "The median age of the dataset was 30 (IQR 25th percentile 30 to 75th percentile 40)."
```
