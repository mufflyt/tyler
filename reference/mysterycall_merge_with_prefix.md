# Merge two data frames with source-prefixed column names

Renames all non-key columns in `x` with `prefix_x` and all non-key
columns in `y` with `prefix_y`, then performs a left join on `by`. This
keeps columns traceable (e.g. `findent_state` vs `abohns_state`) when
two data sources are combined.

## Usage

``` r
mysterycall_merge_with_prefix(
  x,
  y,
  by,
  prefix_x = "x_",
  prefix_y = "y_",
  join_type = c("left", "inner", "full", "right")
)
```

## Arguments

- x:

  A data frame (left table).

- y:

  A data frame (right table).

- by:

  Character vector of column names to join on. These columns are NOT
  prefixed.

- prefix_x:

  Character scalar prefix applied to non-key columns of `x`. Default
  `"x_"`.

- prefix_y:

  Character scalar prefix applied to non-key columns of `y`. Default
  `"y_"`.

- join_type:

  One of `"left"` (default), `"inner"`, `"full"`, or `"right"`.

## Value

A data frame with key columns first, followed by prefixed columns from
`x`, followed by prefixed columns from `y`.

## See also

Other data management:
[`mysterycall_check_duplicates()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_duplicates.md),
[`mysterycall_check_generalist_presence()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_check_generalist_presence.md),
[`mysterycall_extract_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_extract_zip5.md),
[`mysterycall_luhn_check()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_luhn_check.md),
[`mysterycall_prepare_table1_vars()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_prepare_table1_vars.md),
[`mysterycall_stratified_sample()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_stratified_sample.md)

## Examples

``` r
registry <- data.frame(npi = c("A","B"), state = c("CO","TX"),
                       specialty = c("ENT","ENT"), stringsAsFactors = FALSE)
abohns    <- data.frame(npi = c("A","C"), cert = c("Neurotology",NA),
                        stringsAsFactors = FALSE)
mysterycall_merge_with_prefix(registry, abohns, by = "npi",
                               prefix_x = "npi_", prefix_y = "abohns_")
#>   npi npi_state npi_specialty abohns_cert
#> 1   A        CO           ENT Neurotology
#> 2   B        TX           ENT        <NA>
```
