# Track multi-stage workflow progress

Helpers that record per-step status, emit periodic progress updates, and
surface quality tiers for long-running workflows.

## Usage

``` r
mysterycall_progress_tracker(steps, update_every = 300, quiet = getOption("tyler.quiet", FALSE))

mysterycall_progress_start(tracker, step, note = NULL)

mysterycall_progress_finish(tracker, step, score = NULL, quality = NULL, note = NULL)

mysterycall_progress_fail(tracker, step, reason = NULL)

mysterycall_progress_update(tracker, force = FALSE)

mysterycall_progress_summary(tracker)
```

## Arguments

- steps:

  Character vector naming each workflow stage.

- update_every:

  Number of seconds between automatic update messages.

- quiet:

  Logical flag that suppresses log output when `TRUE`.

- tracker:

  Object created by
  [`mysterycall_progress_tracker()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_tracker.html).

- step:

  Step name to update.

- note:

  Optional free-text annotation stored alongside the step.

- score:

  Numeric score between 0 and 1 used to compute a quality tier.

- quality:

  Explicit quality tier that overrides `score` when provided.

- reason:

  Explanation recorded for failed steps.

- force:

  Logical flag forcing an immediate update regardless of elapsed time.

## Value

[`mysterycall_progress_tracker()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_tracker.html)
returns an object of class `mysterycall_progress_tracker`. The other
helpers return the tracker (invisibly) for fluent chaining, except
[`mysterycall_progress_summary()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_summary.html),
which returns a tibble describing each step.

## Details

The tracker records start and finish times, keeps a method-by-method
breakdown, and estimates completion times based on observed throughput.
Use
[`mysterycall_progress_update()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_tracker.html)
within long loops to emit five-minute heartbeat messages, while
[`mysterycall_progress_finish()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_finish.html)
handles quality tier calculation using
[`mysterycall_quality_tier()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_quality_tier.md).

## Examples

``` r
tracker <- mysterycall_progress_tracker(c("Geocode", "Validate"), update_every = 10)
mysterycall_progress_start(tracker, "Geocode")
#> [04:45:17] Started Geocode
#> [04:45:17] Progress: 0/2 steps complete (0.0%)
mysterycall_progress_finish(tracker, "Geocode", score = 0.92)
#> [04:45:17] Completed Geocode (high)
#> [04:45:17] Progress: 1/2 steps complete (50.0%) - ETA 04:45:17
mysterycall_progress_summary(tracker)
#> # A tibble: 2 × 6
#>   step     status    started_at          finished_at         quality note 
#>   <chr>    <fct>     <dttm>              <dttm>              <chr>   <chr>
#> 1 Geocode  completed 2026-05-09 04:45:17 2026-05-09 04:45:17 high    NA   
#> 2 Validate pending   NA                  NA                  NA      NA   
```
