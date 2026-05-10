# Complete progress bar

Marks a progress bar as complete with an optional summary message.

## Usage

``` r
mysterycall_progress_done(pb, result = NULL, status = "done")
```

## Arguments

- pb:

  Progress bar object from
  [`mysterycall_progress_bar()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_bar.md).

- result:

  Optional result message to display on completion.

- status:

  Final status label (default: `"done"`).

## Value

Invisible NULL.

## Examples

``` r
pb <- mysterycall_progress_bar("Processing", total = 5)
#> Starting: Processing (5 items)
for (i in seq_len(5)) mysterycall_progress_update(pb)
#>   Progress: 1/5 (20%) - ETA: 0.0s
#>   Progress: 2/5 (40%) - ETA: 0.0s
#>   Progress: 3/5 (60%) - ETA: 0.0s
#>   Progress: 4/5 (80%) - ETA: 0.0s
#>   Progress: 5/5 (100%) - ETA: 0.0s
mysterycall_progress_done(pb, result = "5 items processed")
#>   ✓ 5 items processed
```
