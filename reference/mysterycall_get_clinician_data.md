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
[`scrape_physicians_data_with_tor()`](https://mufflyt.github.io/mysterycall/reference/scrape_physicians_data_with_tor.md),
[`mysterycall_search_and_process_npi()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_and_process_npi.html),
[`mysterycall_search_taxonomy()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_taxonomy.html)

## Examples

``` r
if (FALSE) { # \dontrun{
clinician_df <- mysterycall_get_clinician_data("clinicians.csv")
} # }
```
