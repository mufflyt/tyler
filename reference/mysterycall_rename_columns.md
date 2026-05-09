# Rename columns based on substring matches

This function searches through column names in a data frame for
specified substrings and renames the first matching column to a new name
provided by the user. It provides detailed logs for each operation,
including the columns found and any renaming actions taken. If multiple
columns match a substring, only the first is renamed, and a warning is
issued.

## Usage

``` r
mysterycall_rename_columns(data, target_strings, new_names)
```

## Arguments

- data:

  A data frame whose columns need renaming.

- target_strings:

  A vector of substrings to search for within column names.

- new_names:

  A vector of new names corresponding to the target strings.

## Value

A data frame with renamed columns.

## See also

Other workflow:
[`mysterycall_clean_phase1()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase1.html),
[`mysterycall_clean_phase2()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase2.html),
[`mysterycall_run_workflow()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow.html),
[`mysterycall_run_workflow_logged()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow_logged.html),
[`mysterycall_split_and_save()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_split_and_save.html),
[`mysterycall_print_dashboard()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_print_dashboard.md)

## Examples

``` r
df <- data.frame(
  doctor_info = 1:5,
  patient_contact_data = 6:10
)
# Renaming 'doctor_info' to 'physician_info'
df <- mysterycall_rename_columns(df,
                                  target_strings = c("doctor"),
                                  new_names = c("physician_info"))
#> --- Starting to search and rename columns based on target substrings ---
#> Matched 1 column(s) by substring match for 'doctor': doctor_info
#> Renamed 'doctor_info' to 'physician_info'.
#> 
#> --- Column renaming complete. Final column set: physician_info, patient_contact_data ---
print(df)
#>   physician_info patient_contact_data
#> 1              1                    6
#> 2              2                    7
#> 3              3                    8
#> 4              4                    9
#> 5              5                   10

# More complex example with multiple renamings
df <- data.frame(
  doc_information = 1:5,
  patient_contact = 6:10,
  doctor_notes = 11:15
)
# Renaming 'doc_information' to 'doctor_info' and 'doctor_notes' to 'notes'
df <- mysterycall_rename_columns(df,
                                  target_strings = c("doc_information", "doctor_notes"),
                                  new_names = c("doctor_info", "notes"))
#> --- Starting to search and rename columns based on target substrings ---
#> Matched 1 column(s) by exact match for 'doc_information': doc_information
#> Renamed 'doc_information' to 'doctor_info'.
#> 
#> Matched 1 column(s) by exact match for 'doctor_notes': doctor_notes
#> Renamed 'doctor_notes' to 'notes'.
#> 
#> --- Column renaming complete. Final column set: doctor_info, patient_contact, notes ---
print(df)
#>   doctor_info patient_contact notes
#> 1           1               6    11
#> 2           2               7    12
#> 3           3               8    13
#> 4           4               9    14
#> 5           5              10    15
```
