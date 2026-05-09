# Split data into multiple parts and save each part as separate Excel files

This function splits the data based on provided lab assistant names and
saves each part as a separate Excel file. It allows the arrangement of
calls by insurance type to prioritize Medicaid in the first two days and
Blue Cross/Blue Shield in the last two days.

## Usage

``` r
mysterycall_split_and_save(
  data_or_path,
  output_directory,
  lab_assistant_names,
  seed = 1978,
  complete_file_prefix = "complete_non_split_version_",
  split_file_prefix = "",
  recursive_create = TRUE,
  insurance_order = c("Medicaid", "Blue Cross/Blue Shield")
)
```

## Arguments

- data_or_path:

  Either a dataframe containing the input data or a path to the input
  data file (RDS, CSV, Parquet, or XLS/XLSX).

- output_directory:

  Directory where output Excel files will be saved.

- lab_assistant_names:

  Vector of lab assistant names to name the output files.

- seed:

  Seed value for randomization (default is 1978).

- complete_file_prefix:

  Prefix for the complete output file name (default is
  "complete_non_split_version\_").

- split_file_prefix:

  Prefix for each split output file name (default is empty).

- recursive_create:

  Logical indicating if directories should be created recursively
  (default is TRUE).

- insurance_order:

  Vector of insurance types ordered by priority for call scheduling
  (default is c("Medicaid", "Blue Cross/Blue Shield")).

## Value

Invisible list of file paths to the created Excel files

## See also

Other workflow:
[`mysterycall_clean_phase1()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase1.html),
[`mysterycall_clean_phase2()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_clean_phase2.html),
[`mysterycall_rename_columns()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_rename_columns.html),
[`mysterycall_run_workflow()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow.html),
[`mysterycall_run_workflow_logged()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_run_workflow_logged.html),
[`mysterycall_print_dashboard()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_print_dashboard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
input_data <- readr::read_csv("/path/to/your/input/file.csv")
output_directory <- "/path/to/your/output/directory"
lab_assistant_names <- c("Label1", "Label2", "Label3")
insurance_order <- c("Medicaid", "Blue Cross/Blue Shield")
mysterycall_split_and_save(
  data_or_path = input_data,
  output_directory = output_directory,
  lab_assistant_names = lab_assistant_names,
  insurance_order = insurance_order
)
} # }
```
