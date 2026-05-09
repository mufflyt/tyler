# Validate and Remove Invalid NPI Numbers

This function reads a CSV file containing NPI numbers, validates their
format using the npi package, and removes rows with missing or invalid
NPIs.

## Usage

``` r
mysterycall_validate_npi(input_data)
```

## Arguments

- input_data:

  Either a data frame containing NPI numbers or a path to a CSV file.

## Value

A data frame containing valid NPI numbers.
