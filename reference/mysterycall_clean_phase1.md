# Clean Phase 1 Results Data

This function reads the Phase 1 results data file, performs various
cleaning and transformation operations, and prepares the data for
further analysis. It ensures all required fields are present and formats
column names. Missing NPI numbers are handled by generating a unique
`random_id`.

## Usage

``` r
mysterycall_clean_phase1(
  phase1_data,
  output_directory = tempdir(),
  verbose = TRUE,
  notify = TRUE,
  duplicate_rows = TRUE,
  id_seed = NULL,
  output_format = c("csv", "parquet")
)
```

## Arguments

- phase1_data:

  A data frame containing the Phase 1 results data. Ensure that it
  includes columns like 'for_redcap', 'id', 'names', 'practice_name',
  'phone_number', 'state_name', and optionally 'npi'. If 'npi' is
  missing or any of its values are NA, a `random_id` is generated as a
  fallback. If an 'id' column exists, it will be preserved as
  'original_id'.

- output_directory:

  Directory where the cleaned Phase 1 data should be written. Defaults
  to [`tempdir()`](https://rdrr.io/r/base/tempfile.html). An audit trail
  JSON file will also be saved here.

- verbose:

  Logical. If `TRUE`, progress messages are printed while cleaning.
  Defaults to `TRUE`.

- notify:

  Logical. If `TRUE`, play a notification sound on completion when the
  optional `beepr` package is available. Defaults to `TRUE`.

- duplicate_rows:

  Logical. If `TRUE`, each row in `phase1_data` is duplicated to retain
  the previous behavior that paired insurance entries for each
  physician. Set to `FALSE` to keep the original number of rows. A
  `processing_flag_is_duplicate` column tracks which rows are
  duplicates.

- id_seed:

  Optional integer seed used when generating fallback random IDs so runs
  can be reproduced without permanently mutating the global RNG state.

- output_format:

  File format to use when writing the cleaned dataset. Supported options
  are "csv" (default) and "parquet".

## Value

Invisibly returns the cleaned data frame with the following attributes:

- `audit_trail`: List containing processing metadata (timestamps, row
  counts, flags)

- `processing_timestamp`: Time when processing completed

- `function_version`: Version number of the function (2.0)

- `output_path`: Path to the saved CSV/Parquet file

- `audit_trail_path`: Path to the saved audit trail JSON file

The returned data frame includes processing flag columns:

- `processing_flag_empty_name`: TRUE if name was empty/whitespace

- `processing_flag_generated_id`: TRUE if random ID was generated (no
  NPI)

- `processing_flag_is_duplicate`: TRUE for duplicated rows (when
  duplicate_rows=TRUE)

- `processing_flag_no_last_name`: TRUE if last name could not be
  extracted

## Details

**New in Version 2.0: Data Provenance and Audit Trail**

This function now includes comprehensive data provenance tracking:

- Preserves original `id` and `npi` columns as `original_id` and
  `original_npi`

- Adds processing flags (e.g., `processing_flag_empty_name`,
  `processing_flag_generated_id`)

- Handles edge cases like whitespace-only strings safely

- Saves audit trail JSON file with processing metadata

- Attaches audit trail and quality metrics as data frame attributes

## See also

Other workflow:
[`mysterycall_clean_phase2()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase2.html),
[`mysterycall_rename_columns()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_rename_columns.html),
[`mysterycall_run_workflow()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow.html),
[`mysterycall_run_workflow_logged()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow_logged.html),
[`mysterycall_split_and_save()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_split_and_save.html),
[`mysterycall_print_dashboard()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_print_dashboard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(mysterycall)
file_path <- "/path/to/your/input/file.xls"
phase1_data <- readxl::read_excel(file_path)  # Assuming use of readxl for Excel files
mysterycall_clean_phase1(phase1_data)
} # }
```
