# Writes an Arsenal table object to a Word document.

Writes an Arsenal table object to a Word document.

## Usage

``` r
mysterycall_write_arsenal_table(object, filename, output_dir = NULL)
```

## Arguments

- object:

  An object to be written to Word, typically an Arsenal table.

- filename:

  The filename (without extension) for the Word document.

- output_dir:

  Directory where the Word document should be written. Defaults to a
  session-specific folder inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

Called for its side effect of saving a Word document to `word_path`.
Returns `NULL` invisibly.

## See also

Other table:
[`mysterycall_max_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_max_table.html),
[`mysterycall_min_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_min_table.html),
[`mysterycall_table_percentages()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_table_percentages.html),
[`mysterycall_table_proportion()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_table_proportion.html),
[`mysterycall_table_overall()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_table_overall.html)

## Examples

``` r
if (FALSE) { # \dontrun{
mysterycall_write_arsenal_table(my_table, "output_table")
} # }
```
