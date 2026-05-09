# Run the mystery caller workflow with structured logging

A thin wrapper around
[`mysterycall_run_workflow()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow.html)
that initialises the `mysterycall` logging infrastructure before the run
and tears it down afterward. All substantive workflow logic lives in the
underlying function; any bug fixed there is automatically inherited
here.

## Usage

``` r
mysterycall_run_workflow_logged(
  taxonomy_terms = NULL,
  name_data = NULL,
  phase1_data,
  lab_assistant_names,
  output_directory,
  phase2_data,
  phase2_output_directory = output_directory,
  quality_check_path,
  phase1_output_directory = output_directory,
  split_insurance_order = c("Medicaid", "Blue Cross/Blue Shield"),
  phase2_required_strings = c("physician_information", "able_to_contact_office",
    "are_we_including", "reason_for_exclusions", "appointment_date",
    "number_of_transfers", "call_time", "hold_time", "notes", "person_completing",
    "state", "npi", "name"),
  phase2_standard_names = c("physician_info", "contact_office", "included_in_study",
    "exclusion_reasons", "appt_date", "transfer_count", "call_duration", "hold_duration",
    "notes", "completed_by", "state", "npi", "name"),
  npi_search_args = list(),
  all_states = NULL,
  npi_progress_observer = NULL,
  log_file = NULL,
  skip_preflight = FALSE
)
```

## Arguments

- taxonomy_terms:

  Character vector of taxonomy descriptions to pass to
  [`mysterycall_search_taxonomy()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_taxonomy.html).
  Set to `NULL` to skip taxonomy-based searches.

- name_data:

  Optional data frame containing `first` and `last` columns to use with
  [`mysterycall_search_and_process_npi()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_and_process_npi.html).
  Provide `NULL` to skip name-based searches.

- phase1_data:

  Data frame holding Phase 1 calling roster information to pass to
  [`mysterycall_clean_phase1()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase1.html).

- lab_assistant_names:

  Character vector of caller names used when splitting the cleaned
  roster via
  [`mysterycall_split_and_save()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_split_and_save.html).
  Must contain at least two entries.

- output_directory:

  Directory where
  [`mysterycall_split_and_save()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_split_and_save.html)
  should write the complete and per-caller workbooks.

- phase2_data:

  Data frame or file path consumed by
  [`mysterycall_clean_phase2()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase2.html).

- phase2_output_directory:

  Directory where Phase 2 exports should be written. Defaults to
  `output_directory`.

- quality_check_path:

  File path where
  [`mysterycall_save_quality_table()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_save_quality_table.html)
  should write the quality check CSV.

- phase1_output_directory:

  Directory where
  [`mysterycall_clean_phase1()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase1.html)
  should write the cleaned Phase 1 CSV. Defaults to `output_directory`.

- split_insurance_order:

  Ordering passed to
  [`mysterycall_split_and_save()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_split_and_save.html)'s
  `insurance_order` argument. Defaults to
  `c("Medicaid", "Blue Cross/Blue Shield")`.

- phase2_required_strings:

  Character vector of substrings used when standardizing Phase 2 column
  names via
  [`mysterycall_clean_phase2()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase2.html).

- phase2_standard_names:

  Replacement names corresponding to `phase2_required_strings`.

- npi_search_args:

  Named list of additional arguments forwarded to
  [`mysterycall_search_and_process_npi()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_and_process_npi.html).

- all_states:

  Optional character vector of all states to supply to
  [`mysterycall_not_contacted_states()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_not_contacted_states.html).

- npi_progress_observer:

  Optional callback that receives progress updates from
  [`mysterycall_search_and_process_npi()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_search_and_process_npi.html).
  It is invoked with the same payload as the `progress_callback`
  argument for that function.

- log_file:

  Optional path to write a plain-text log file. When `NULL` (default), a
  timestamped file is created inside `output_directory`.

- skip_preflight:

  Logical. When `TRUE`, skip the preflight validation step. Defaults to
  `FALSE`.

## Value

The same list returned by
[`mysterycall_run_workflow()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow.html).

## See also

Other workflow:
[`mysterycall_clean_phase1()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase1.html),
[`mysterycall_clean_phase2()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase2.html),
[`mysterycall_rename_columns()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_rename_columns.html),
[`mysterycall_run_workflow()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow.html),
[`mysterycall_split_and_save()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_split_and_save.html),
[`mysterycall_print_dashboard()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_print_dashboard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- mysterycall_run_workflow_logged(
  phase1_data = phase1,
  phase2_data = phase2,
  lab_assistant_names = c("Alice", "Bob"),
  output_directory = "output/",
  quality_check_path = "output/qc.csv",
  log_file = "output/run.log"
)
} # }
```
