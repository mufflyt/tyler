# Start a step in multi-progress tracker

Advances the multi-progress tracker to the given step and creates an
inner progress bar for that step's items.

## Usage

``` r
mysterycall_multi_step(tracker, step_num, total, detail = NULL)
```

## Arguments

- tracker:

  Multi-progress tracker object from
  [`mysterycall_multi_progress()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_multi_progress.md).

- step_num:

  Step number (1-based).

- total:

  Total number of items to process in this step.

- detail:

  Optional detail message shown beneath the step header.

## Value

Invisible NULL.

## Examples

``` r
tr <- mysterycall_multi_progress(c("Geocode", "Validate"))
mysterycall_multi_step(tr, 1, total = 10)
#> 
#> ── Step 1/2: Geocode ──
#> 
#> Starting: Geocode (10 items)
```
