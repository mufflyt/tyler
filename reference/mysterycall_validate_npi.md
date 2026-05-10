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

A data frame containing valid NPI numbers, with invalid or missing NPI
rows removed.

## Contract

**Inputs:**

- `input_data` must contain an `npi` column (character or numeric).

- NPIs are validated via the Luhn checksum algorithm (NPI standard).

**Guarantees:**

- Output rows \\\subseteq\\ input rows — rows are only removed, never
  added.

- Every removed row had either `NA`, a non-10-digit string, or a failed
  Luhn checksum.

- Output NPI column is always character type.

**Disaster Prevention:** Prevents silent retention of placeholder NPIs
(e.g., all-zeros or test values such as "1234567890") that would match
real providers in downstream joins and inflate match rates.

## Performance

O(n) with a small constant; Luhn validation is a pure arithmetic check.
Expect \< 0.1 s for 10,000 rows.

## Called By

- [`mysterycall_run_workflow()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_run_workflow.md)

- [`mysterycall_search_and_process_npi()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_search_and_process_npi.md)

## Examples

``` r
if (FALSE) { # interactive()
df <- data.frame(npi = c("1234567893", "0000000000", NA_character_))
mysterycall_validate_npi(df)
}
```
