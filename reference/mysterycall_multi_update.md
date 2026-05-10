# Update current step in multi-progress tracker

Increments the inner progress bar for the current step.

## Usage

``` r
mysterycall_multi_update(tracker, amount = 1, status = NULL)
```

## Arguments

- tracker:

  Multi-progress tracker object from
  [`mysterycall_multi_progress()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_progress.md).

- amount:

  Amount to increment (default: 1).

- status:

  Optional status message to display.

## Value

Invisible NULL.

## Examples

``` r
tr <- mysterycall_multi_progress(c("Geocode", "Validate"))
mysterycall_multi_step(tr, 1, total = 5)
#> 
#> ── Step 1/2: Geocode ──
#> 
#> Starting: Geocode (5 items)
for (i in seq_len(5)) mysterycall_multi_update(tr)
#>   Progress: 1/5 (20%) - ETA: 0.0s
#>   Progress: 2/5 (40%) - ETA: 0.0s
#>   Progress: 3/5 (60%) - ETA: 0.0s
#>   Progress: 4/5 (80%) - ETA: 0.0s
#>   Progress: 5/5 (100%) - ETA: 0.0s
```
