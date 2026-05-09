# Write an Arsenal table to a PDF file

Utility wrapper around
[`arsenal::write2pdf`](https://mayoverse.github.io/arsenal/reference/write2specific.html)
to save a table object as a PDF document.

## Usage

``` r
mysterycall_write_table_pdf(object, filename)
```

## Arguments

- object:

  An `arsenal` table object (or summary thereof) to write.

- filename:

  Path to the output PDF file. A `.pdf` extension is appended
  automatically if not already present.

## Value

Invisibly returns the file path.

## Examples

``` r
if (FALSE) { # \dontrun{
mysterycall_write_table_pdf(overall_summary, "table.pdf")
} # }
```
