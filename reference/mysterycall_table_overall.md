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

  Optional vector of selected columns to include in the table.

- label_translations:

  Optional named list for label translations.

## Value

Path to the generated PDF file

## See also

Other table:
[`mysterycall_write_arsenal_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_write_arsenal_table.html),
[`mysterycall_max_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_max_table.html),
[`mysterycall_min_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_min_table.html),
[`mysterycall_table_percentages()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_table_percentages.html),
[`mysterycall_table_proportion()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_table_proportion.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate the overall table
mysterycall_table_overall("data/Table1.rds", "output_tables")
} # }
```
