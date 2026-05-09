# Clean and process Phase 2 data

This function reads data from a file or data frame, cleans column names,
and applies renaming based on specified criteria to facilitate data
analysis. The function logs each step of the process, including data
loading, column cleaning, and renaming for transparency.

## Usage

``` r
mysterycall_clean_phase2(
  data_or_path,
  required_strings,
  standard_names,
  output_directory = NULL,
  output_format = c("csv", "parquet")
)
```

## Arguments

- data_or_path:

  Path to the data file or a data frame.

- required_strings:

  Vector of substrings for which to search in column names.

- standard_names:

  Vector of new names to apply to the matched columns.

- output_directory:

  Directory where the cleaned dataset should be written. Defaults to a
  session-specific folder inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) when not provided.

- output_format:

  File format to use when persisting the cleaned dataset. Supported
  values are "csv" (default) and "parquet".

## Value

A data frame with processed data.

## See also

Other workflow:
[`mysterycall_clean_phase1()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase1.html),
[`mysterycall_rename_columns()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_rename_columns.html),
[`mysterycall_run_workflow()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow.html),
[`mysterycall_run_workflow_logged()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow_logged.html),
[`mysterycall_split_and_save()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_split_and_save.html),
[`mysterycall_print_dashboard()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_print_dashboard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming an input path to a CSV file
input_path <- "path_to_your_data.csv"
required_strings <- c("physician_information", "able_to_contact_office")
standard_names <- c("physician_info", "contact_office")
cleaned_data <- mysterycall_clean_phase2(input_path, required_strings, standard_names)
} # }

# Directly using a data frame
df <- data.frame(
  doc_info = 1:5,
  contact_data = 6:10
)
required_strings <- c("doc_info", "contact_data")
standard_names <- c("doctor_info", "patient_contact_info")
cleaned_df <- mysterycall_clean_phase2(df, required_strings, standard_names)
#> Loaded Phase 2 data from provided data frame with 5 row(s) and 2 column(s).
#> Converted column names to snake_case format.
#> --- Starting to search and rename columns based on target substrings ---
#> Matched 1 column(s) by exact match for 'doc_info': doc_info
#> Renamed 'doc_info' to 'doctor_info'.
#> 
#> Matched 1 column(s) by exact match for 'contact_data': contact_data
#> Renamed 'contact_data' to 'patient_contact_info'.
#> 
#> --- Column renaming complete. Final column set: doctor_info, patient_contact_info ---
#> Summary of applied renames:
#>        pattern renamed_from           renamed_to
#> 1     doc_info     doc_info          doctor_info
#> 2 contact_data contact_data patient_contact_info
#> Standardised Phase 2 column names based on required patterns.
#> Proceeding with additional data processing steps...
#> Cleaned Phase 2 data (5 row(s), 2 column(s)) saved to: /tmp/RtmpgjpAog/mysterycall/phase2/cleaned_phase_2_data_2026-05-09_04-56-59.csv
print(cleaned_df)
#>   doctor_info patient_contact_info
#> 1           1                    6
#> 2           2                    7
#> 3           3                    8
#> 4           4                    9
#> 5           5                   10
```
