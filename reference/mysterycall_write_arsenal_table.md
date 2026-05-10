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
[`mysterycall_max_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_max_table.md),
[`mysterycall_min_table()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_min_table.md),
[`mysterycall_table_overall()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_overall.md),
[`mysterycall_table_percentages()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_percentages.md),
[`mysterycall_table_proportion()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_proportion.md)

## Examples

``` r
if (FALSE) { # interactive()
mysterycall_write_arsenal_table(my_table, "output_table")
}
```
