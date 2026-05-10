# Generate an overall summary table

Generate an overall table summarizing the demographics of the Table 1.

## Usage

``` r
mysterycall_table_overall(
  input_file_path,
  output_directory,
  title = "Overall Table Summary",
  selected_columns = NULL,
  label_translations = NULL
)
```

## Arguments

- input_file_path:

  The path to an RDS data file.

- output_directory:

  The directory where the output table file will be saved.

- title:

  The title for the overall table summary (default is "Overall Table
  Summary").

- selected_columns:

  Optional character vector of column names to include. When `NULL`
  (default) all columns are used.

- label_translations:

  Optional named list mapping column names to display labels, passed to
  [`arsenal::summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.html).

## Value

Invisibly returns the path to the generated PDF file.

## See also

[`mysterycall_write_table_pdf()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_table_pdf.md)

Other table:
[`mysterycall_disparities_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_disparities_table.md),
[`mysterycall_format_pct()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_format_pct.md),
[`mysterycall_max_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_max_table.md),
[`mysterycall_min_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_min_table.md),
[`mysterycall_model_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_model_table.md),
[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md),
[`mysterycall_table1_gtsummary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1_gtsummary.md),
[`mysterycall_table_percentages()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_percentages.md),
[`mysterycall_table_proportion()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_proportion.md),
[`mysterycall_write_arsenal_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_arsenal_table.md),
[`mysterycall_write_table_pdf()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_table_pdf.md),
[`print.mysterycall_disparities_table()`](https://mufflyt.github.io/mysterycall/reference/print.mysterycall_disparities_table.md),
[`print.mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/print.mysterycall_table1.md)

## Examples

``` r
if (FALSE) { # interactive()
# Generate the overall table
mysterycall_table_overall("data/Table1.rds", "output_tables")
}
```
