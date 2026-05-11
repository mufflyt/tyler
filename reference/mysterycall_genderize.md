# Infer physician gender from first names via Genderize.io

Queries the [Genderize.io](https://genderize.io) API for first-name
gender predictions and joins the results back to the input roster. First
names are deduplicated before querying to minimize API calls. The
enriched roster is written to `output_dir` with a timestamp in the
filename.

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

  A data frame with at least a `first_name` column, or a file path
  (character scalar) to a CSV or Parquet roster.

- output_dir:

  Character scalar. Directory where the genderized roster is saved.
  Defaults to a session-specific subfolder of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- output_format:

  `"csv"` (default) or `"parquet"`. Format for the saved output file.

## Value

A data frame matching the input rows with additional columns from
Genderize.io: typically `gender`, `probability`, and `count`.

## Details

Requires a `first_name` column in the input data. HTTP errors from the
API are raised as errors immediately (no silent fallback).

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
