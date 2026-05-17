# Writes an Arsenal summary table to a Word document

Writes an Arsenal summary table to a Word document

## Usage

``` r
mysterycall_write_arsenal_table(object, filename, output_dir = NULL)
```

## Arguments

- object:

  A data frame (e.g. the output of
  [`arsenal::summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.html))
  to write. Must be a data frame – the function will error otherwise.

- filename:

  The filename without extension for the Word document.

- output_dir:

  Directory where the Word document should be written. Defaults to a
  session-specific folder inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

Invisibly returns `NULL`. Called for the side effect of writing
`<filename>.docx` to `output_dir`.

## See also

[`mysterycall_write_table_pdf()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_table_pdf.md),
[`mysterycall_table_overall()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_overall.md)

Other table:
[`mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1.md),
[`mysterycall_table1_gtsummary()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table1_gtsummary.md),
[`mysterycall_table_overall()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_table_overall.md),
[`mysterycall_write_table_pdf()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_write_table_pdf.md),
[`print.mysterycall_table1()`](https://mufflyt.github.io/mysterycall/reference/print.mysterycall_table1.md)

## Examples

``` r
if (FALSE) { # interactive()
mysterycall_write_arsenal_table(my_table, "output_table")
}
```
