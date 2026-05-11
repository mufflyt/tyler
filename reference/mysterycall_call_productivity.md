# Compute per-caller productivity metrics

Summarises call volume, scheduling rates, and time metrics for each
caller in a mystery caller dataset.

## Usage

``` r
mysterycall_call_productivity(
  data,
  caller_col,
  date_col = NULL,
  outcome_col = NULL,
  hold_time_col = NULL,
  call_time_col = NULL
)
```

## Arguments

- data:

  A data frame with at least a caller column.

- caller_col:

  Character scalar: column identifying each caller.

- date_col:

  Character scalar or NULL. Column containing call dates. Accepts
  `Date`, `POSIXct`, or character (coerced via
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html)).

- outcome_col:

  Character scalar or NULL. Binary (0/1) column indicating whether a
  call resulted in an accepted appointment.

- hold_time_col:

  Character scalar or NULL. Hold time in seconds (numeric) or "MM:SS"
  format (character).

- call_time_col:

  Character scalar or NULL. Total call time in seconds (numeric) or
  "MM:SS" format (character).

## Value

A data frame with one row per caller, sorted by `n_calls` descending.
Columns:

- caller:

  Caller identifier.

- n_calls:

  Total calls.

- n_days:

  Unique call dates (NA if `date_col` is NULL).

- calls_per_day:

  n_calls / n_days (NA if `date_col` is NULL).

- n_accepted:

  Sum of `outcome_col` (NA if not provided).

- acceptance_rate:

  Formatted "XX.X%" (NA if `outcome_col` NULL).

- mean_hold_sec:

  Mean hold time in seconds (NA if not provided).

- mean_call_sec:

  Mean call time in seconds (NA if not provided).

The attribute `total_calls_all` is set on the returned data frame.

## See also

Other workflow:
[`mysterycall_clean_phase1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clean_phase1.md),
[`mysterycall_clean_phase2()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clean_phase2.md),
[`mysterycall_print_dashboard()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_print_dashboard.md),
[`mysterycall_rename_columns()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_rename_columns.md),
[`mysterycall_run_workflow()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_run_workflow.md),
[`mysterycall_run_workflow_logged()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_run_workflow_logged.md),
[`mysterycall_split_and_save()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_split_and_save.md),
[`mysterycall_verify_artifact()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_verify_artifact.md)

## Examples

``` r
df <- data.frame(
  caller  = c("Alice","Alice","Bob"),
  outcome = c(1, 0, 1)
)
mysterycall_call_productivity(df, "caller", outcome_col = "outcome")
#>   caller n_calls n_days calls_per_day n_accepted acceptance_rate mean_hold_sec
#> 1  Alice       2     NA            NA          1           50.0%            NA
#> 2    Bob       1     NA            NA          1          100.0%            NA
#>   mean_call_sec
#> 1            NA
#> 2            NA
```
