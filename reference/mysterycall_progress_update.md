# Emit a manual progress update

Emit a manual progress update

## Usage

``` r
mysterycall_progress_update(tracker, force = FALSE)
```

## Arguments

- tracker:

  Object created by
  [`mysterycall_progress_tracker()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_tracker.html).

- force:

  Logical flag indicating whether the update should be emitted even if
  the configured interval has not elapsed.

## See also

Other logging utilities:
[`mysterycall_progress_tracker()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_tracker.html),
[`mysterycall_progress_fail()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_fail.html),
[`mysterycall_progress_finish()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_finish.html),
[`mysterycall_progress_start()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_start.html),
[`mysterycall_progress_summary()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_progress_summary.html),
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
