# Mark a step as completed

Mark a step as completed

## Usage

``` r
mysterycall_progress_finish(
  tracker,
  step,
  score = NULL,
  quality = NULL,
  note = NULL
)
```

## Arguments

- tracker:

  Object created by
  [`mysterycall_progress_tracker()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_tracker.html).

- step:

  Step name.

- score:

  Optional numeric score between 0 and 1 used to derive a quality tier.

- quality:

  Optional explicit quality tier. Overrides `score` when provided.

- note:

  Optional note to store for the step.

## See also

Other logging utilities:
[`mysterycall_progress_tracker()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_tracker.html),
[`mysterycall_progress_fail()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_fail.html),
[`mysterycall_progress_start()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_start.html),
[`mysterycall_progress_summary()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_summary.html),
[`mysterycall_progress_update()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_tracker.html),
[`mysterycall_log_cache_hit()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_cache_hit.md),
[`mysterycall_log_error()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_error.md),
[`mysterycall_log_info()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_info.md),
[`mysterycall_log_progress()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_progress.md),
[`mysterycall_log_save()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_save.md),
[`mysterycall_log_step()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_step.md),
[`mysterycall_log_step_complete()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_step_complete.md),
[`mysterycall_log_success()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_success.md),
[`mysterycall_log_warning()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_log_warning.md),
[`mysterycall_workflow_start()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_workflow_start.md)
