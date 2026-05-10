# Create a multi-step progress tracker

Creates a progress tracker for workflows with multiple major steps.
Shows overall progress plus current step progress.

## Usage

``` r
mysterycall_multi_progress(steps, show_overall = TRUE)
```

## Arguments

- steps:

  Character vector of step names

- show_overall:

  Whether to show overall progress bar (default: TRUE)

## Value

Multi-progress tracker object

## See also

Other progress-bars:
[`mysterycall_multi_complete()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_complete.md),
[`mysterycall_multi_done()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_done.md),
[`mysterycall_multi_step()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_step.md),
[`mysterycall_multi_update()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_update.md),
[`mysterycall_progress_bar()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_bar.md),
[`mysterycall_progress_done()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_done.md),
[`mysterycall_progress_fail()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_fail.md),
[`mysterycall_progress_map()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_map.md),
[`mysterycall_progress_update()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_update.md),
[`mysterycall_spinner_start()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_spinner_start.md),
[`mysterycall_spinner_stop()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_spinner_stop.md)

## Examples

``` r
# \donttest{
tracker <- mysterycall_multi_progress(c("Load Data", "Process", "Save"))

# Step 1
mysterycall_multi_step(tracker, 1, total = 100)
#> 
#> ── Step 1/3: Load Data ──
#> 
#> Starting: Load Data (100 items)
for (i in 1:100) {
  mysterycall_multi_update(tracker)
}
#>   Progress: 10/100 (10%) - ETA: 0.0s
#>   Progress: 20/100 (20%) - ETA: 0.0s
#>   Progress: 30/100 (30%) - ETA: 0.0s
#>   Progress: 40/100 (40%) - ETA: 0.0s
#>   Progress: 50/100 (50%) - ETA: 0.0s
#>   Progress: 60/100 (60%) - ETA: 0.0s
#>   Progress: 70/100 (70%) - ETA: 0.0s
#>   Progress: 80/100 (80%) - ETA: 0.0s
#>   Progress: 90/100 (90%) - ETA: 0.0s
#>   Progress: 100/100 (100%) - ETA: 0.0s
mysterycall_multi_complete(tracker)
#>   ✓ Load Data complete

# Step 2
mysterycall_multi_step(tracker, 2, total = 50)
#> ── Step 2/3: Process ──
#> 
#> Starting: Process (50 items)
for (i in 1:50) {
  mysterycall_multi_update(tracker)
}
#>   Progress: 5/50 (10%) - ETA: 0.0s
#>   Progress: 10/50 (20%) - ETA: 0.0s
#>   Progress: 15/50 (30%) - ETA: 0.0s
#>   Progress: 20/50 (40%) - ETA: 0.0s
#>   Progress: 25/50 (50%) - ETA: 0.0s
#>   Progress: 30/50 (60%) - ETA: 0.0s
#>   Progress: 35/50 (70%) - ETA: 0.0s
#>   Progress: 40/50 (80%) - ETA: 0.0s
#>   Progress: 45/50 (90%) - ETA: 0.0s
#>   Progress: 50/50 (100%) - ETA: 0.0s
mysterycall_multi_complete(tracker)
#>   ✓ Process complete

mysterycall_multi_done(tracker)
#> ✔ All steps complete!
# }
```
