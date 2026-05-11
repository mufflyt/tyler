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

## Gender coding

Genderize.io returns `"male"`, `"female"`, or `NULL` for ambiguous or
unknown names. The API does not currently return non-binary values.
`NULL` responses are stored as `NA` in the raw `gender` output column.
If the API ever adds non-binary values they will be stored as-is in
`gender` but will be recoded to `"Unknown"` by
[`mysterycall_prepare_table1_vars()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_prepare_table1_vars.md).

## API response schema

Each Genderize.io JSON entry is expected to contain exactly four fields:
`name`, `gender`, `probability`, and `count`. Any additional fields
returned by the API are silently dropped. If the API renames or removes
one of these four fields, the corresponding output column will contain
`NA` without an explicit error or warning.

## Output file timestamps

Output filenames include a timestamp from
[`Sys.time()`](https://rdrr.io/r/base/Sys.time.html), which uses the
**local system timezone** (not UTC). Files produced on systems in
different timezones will reflect different local times for the same
wall-clock moment.

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
