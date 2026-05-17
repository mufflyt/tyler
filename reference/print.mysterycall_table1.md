# Print a mysterycall_table1 object

Prints a formatted Table 1 with column sample sizes in the header and
the underlying tibble displayed via
[`tibble::print.tbl_df()`](https://tibble.tidyverse.org/reference/formatting.html).

## Usage

``` r
# S3 method for class 'mysterycall_table1'
print(x, ...)
```

## Arguments

- x:

  A `mysterycall_table1` object returned by
  [`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md).

- ...:

  Additional arguments passed to
  [`tibble::print.tbl_df()`](https://tibble.tidyverse.org/reference/formatting.html).

## Value

Invisibly returns `x`.

## See also

[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md)
which produces this object;
[`mysterycall_table1_gtsummary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1_gtsummary.md)
for a publication-ready `gtsummary` alternative.

Other table:
[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md),
[`mysterycall_table1_gtsummary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1_gtsummary.md),
[`mysterycall_table_overall()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_overall.md),
[`mysterycall_write_arsenal_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_arsenal_table.md),
[`mysterycall_write_table_pdf()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_table_pdf.md)
