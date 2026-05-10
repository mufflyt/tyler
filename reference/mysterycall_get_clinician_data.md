# Retrieve Clinician Data

This function retrieves clinician data for each valid NPI in the input
data frame.

## Usage

``` r
mysterycall_get_clinician_data(input_data)
```

## Arguments

- input_data:

  Either a data frame containing NPI numbers or a path to a CSV file.

## Value

A tibble with clinician data for the provided NPIs.

## See also

Other npi:
[`mysterycall_search_and_process_npi()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_search_and_process_npi.md),
[`mysterycall_search_taxonomy()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_search_taxonomy.md)

## Examples

``` r
if (FALSE) { # interactive()
clinician_df <- mysterycall_get_clinician_data("clinicians.csv")
}
```
