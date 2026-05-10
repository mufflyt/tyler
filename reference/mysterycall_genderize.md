# Genderize Physicians Data

This function reads a CSV file containing physician data, genderizes the
first names, and joins the gender information back to the original data.
It then saves the result to a new CSV file with a timestamp.

## Usage

``` r
mysterycall_genderize(
  data_or_path,
  output_dir = NULL,
  output_format = c("csv", "parquet")
)
```

## Arguments

- data_or_path:

  A data frame containing physician data, or a path to a roster file.
  CSV and Parquet formats are supported when a path is supplied.

- output_dir:

  The directory where the output file will be saved. Default is a
  session-specific folder inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- output_format:

  Output format for the saved roster. Either "csv" or "parquet" with
  "csv" as the default to preserve backwards compatibility.

## Value

A data frame with genderized information joined to the original data.

The function queries the [Genderize.io](https://genderize.io) API for
first name gender predictions. First names are deduplicated before
querying the service to minimize repeated requests.

## See also

Other gender:
[`mysterycall_most_common_gender()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_most_common_gender.md)

## Examples

``` r
if (FALSE) { # interactive()
result <- mysterycall_genderize("sample.csv")
result <- mysterycall_genderize(my_dataframe)  # also accepts a data frame directly
}
```
