# Compute per-caller productivity metrics

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

  Formatted "XX.X\\ mean_hold_secMean hold time in seconds (NA if not
  provided). mean_call_secMean call time in seconds (NA if not
  provided).

Summarises call volume, scheduling rates, and time metrics for each
caller in a mystery caller dataset. df \<- data.frame( caller =
c("Alice","Alice","Bob"), outcome = c(1, 0, 1) )
mysterycall_call_productivity(df, "caller", outcome_col = "outcome")
