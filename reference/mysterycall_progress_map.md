# Create a progress bar for batch processing

Convenience wrapper for processing items in batches with automatic
progress updates and ETA calculations.

## Usage

``` r
mysterycall_progress_map(
  items,
  fn,
  name = "Processing items",
  batch_size = NULL,
  parallel = FALSE
)
```

## Arguments

- items:

  Vector of items to process

- fn:

  Function to apply to each item

- name:

  Name of operation (default: "Processing items")

- batch_size:

  Show progress every N items (default: 1, or max(1, length(items) /
  100))

- parallel:

  Whether processing is parallel (affects ETA calculation)

## Value

List of results from fn

## See also

Other progress-bars:
[`mysterycall_multi_complete()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_complete.md),
[`mysterycall_multi_done()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_done.md),
[`mysterycall_multi_progress()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_progress.md),
[`mysterycall_multi_step()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_step.md),
[`mysterycall_multi_update()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_update.md),
[`mysterycall_progress_bar()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_bar.md),
[`mysterycall_progress_done()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_done.md),
[`mysterycall_progress_fail()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_fail.md),
[`mysterycall_progress_update()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_progress_update.md),
[`mysterycall_spinner_start()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_spinner_start.md),
[`mysterycall_spinner_stop()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_spinner_stop.md)

## Examples

``` r
# \donttest{
# Process items with progress bar
results <- mysterycall_progress_map(
  items = 1:100,
  fn = function(x) { Sys.sleep(0.1); x^2 },
  name = "Computing squares"
)
#> Starting: Computing squares (100 items)
#>   Progress: 10/100 (10%) - ETA: 9.0s
#>   Progress: 20/100 (20%) - ETA: 8.0s
#>   Progress: 30/100 (30%) - ETA: 7.0s
#>   Progress: 40/100 (40%) - ETA: 6.0s
#>   Progress: 50/100 (50%) - ETA: 5.0s
#>   Progress: 60/100 (60%) - ETA: 4.0s
#>   Progress: 70/100 (70%) - ETA: 3.0s
#>   Progress: 80/100 (80%) - ETA: 2.0s
#>   Progress: 90/100 (90%) - ETA: 1.0s
#>   Progress: 100/100 (100%) - ETA: 0.0s
#>   ✓ Processed 100 items

# With custom batch size
results <- mysterycall_progress_map(
  items = 1:50,
  fn = function(x) x^2,
  name = "Computing squares",
  batch_size = 10  # Update every 10 items
)
#> Starting: Computing squares (50 items)
#>   Progress: 10/50 (20%) - ETA: 0.0s
#>   Progress: 20/50 (40%) - ETA: 0.0s
#>   Progress: 30/50 (60%) - ETA: 0.0s
#>   Progress: 40/50 (80%) - ETA: 0.0s
#>   Progress: 50/50 (100%) - ETA: 0.0s
#>   ✓ Processed 50 items
# }
```
