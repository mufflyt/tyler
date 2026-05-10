# Update progress bar

Increments the progress bar by a specified amount and optionally updates
the status message.

## Usage

``` r
mysterycall_progress_update(pb, amount = 1, status = NULL, set = NULL)
```

## Arguments

- pb:

  Progress bar object from
  [`mysterycall_progress_bar()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_bar.md).

- amount:

  Amount to increment (default: 1).

- status:

  Optional status message to display.

- set:

  Set to a specific absolute value instead of incrementing.

## Value

Invisible NULL.

## Examples

``` r
pb <- mysterycall_progress_bar("Processing", total = 10)
#> Starting: Processing (10 items)
for (i in seq_len(10)) mysterycall_progress_update(pb)
#>   Progress: 1/10 (10%) - ETA: 0.0s
#>   Progress: 2/10 (20%) - ETA: 0.0s
#>   Progress: 3/10 (30%) - ETA: 0.0s
#>   Progress: 4/10 (40%) - ETA: 0.0s
#>   Progress: 5/10 (50%) - ETA: 0.0s
#>   Progress: 6/10 (60%) - ETA: 0.0s
#>   Progress: 7/10 (70%) - ETA: 0.0s
#>   Progress: 8/10 (80%) - ETA: 0.0s
#>   Progress: 9/10 (90%) - ETA: 0.0s
#>   Progress: 10/10 (100%) - ETA: 0.0s
mysterycall_progress_done(pb)
#>   ✓ Processing complete
```
