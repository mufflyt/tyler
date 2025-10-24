#' Progress tracker for long-running workflows
#'
#' Provides reusable helpers for emitting periodic progress updates across
#' multi-stage workflows such as the end-to-end mystery caller pipeline. The
#' tracker keeps a method-by-method breakdown, surfaces estimated completion
#' times, and exposes convenience helpers for recording failures.
#'
#' @name progress_tracker
NULL

#' Create a progress tracker instance
#'
#' @param steps Character vector naming each discrete method or stage.
#' @param update_every Number of seconds between automatic status updates. The
#'   default (300 seconds) produces five-minute summaries.
#' @param quiet Logical flag controlling console output. When `TRUE`, suppresses
#'   status messages.
#'
#' @return An object of class `tyler_progress_tracker`.
#' @importFrom tibble tibble
#' @export
#' @examples
#' tracker <- progress_tracker(c("Geocode", "Validate", "Export"))
#' progress_tracker_start(tracker, "Geocode")
#' Sys.sleep(1)
#' progress_tracker_finish(tracker, "Geocode", score = 0.95)
progress_tracker <- function(steps, update_every = 300, quiet = getOption("tyler.quiet", FALSE)) {
  if (!is.character(steps) || !length(steps)) {
    stop("`steps` must be a non-empty character vector.", call. = FALSE)
  }
  steps <- unique(steps)

  env <- new.env(parent = emptyenv())
  env$records <- tibble::tibble(
    step = steps,
    status = factor(rep("pending", length(steps)), levels = c("pending", "in_progress", "completed", "failed")),
    started_at = as.POSIXct(NA, tz = "UTC"),
    finished_at = as.POSIXct(NA, tz = "UTC"),
    quality = NA_character_,
    note = NA_character_
  )
  env$start_time <- Sys.time()
  env$last_update <- env$start_time
  env$quiet <- quiet
  env$update_every <- update_every

  structure(list(env = env), class = "tyler_progress_tracker")
}

# Internal helper to retrieve the record index for a step
.tracker_index <- function(tracker, step) {
  if (!inherits(tracker, "tyler_progress_tracker")) {
    stop("`tracker` must be created with progress_tracker().", call. = FALSE)
  }
  env <- tracker$env
  idx <- match(step, env$records$step)
  if (is.na(idx)) {
    stop(sprintf("Unknown step '%s'.", step), call. = FALSE)
  }
  idx
}

.tracker_log <- function(tracker, message_text, force = FALSE) {
  env <- tracker$env
  if (!isTRUE(env$quiet) || isTRUE(force)) {
    message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), message_text))
  }
}

.tracker_emit_update <- function(tracker, force = FALSE) {
  env <- tracker$env
  elapsed <- as.numeric(difftime(Sys.time(), env$last_update, units = "secs"))
  if (!force && elapsed < env$update_every) {
    return(invisible(NULL))
  }

  records <- env$records
  completed <- sum(records$status == "completed")
  total <- nrow(records)
  progress <- if (total) completed / total else 0
  elapsed_total <- as.numeric(difftime(Sys.time(), env$start_time, units = "secs"))
  eta <- NA_character_
  if (completed > 0 && completed < total) {
    avg_duration <- elapsed_total / completed
    remaining <- total - completed
    eta_time <- Sys.time() + avg_duration * remaining
    eta <- format(eta_time, "%H:%M:%S")
  }

  msg <- sprintf(
    "Progress: %d/%d steps complete (%.1f%%)%s",
    completed,
    total,
    progress * 100,
    if (!is.na(eta)) sprintf(" – ETA %s", eta) else ""
  )
  .tracker_log(tracker, msg)
  env$last_update <- Sys.time()
  invisible(NULL)
}

#' Mark a step as started
#'
#' @param tracker Object created by [progress_tracker()].
#' @param step Step name.
#' @param note Optional note stored alongside the step.
#'
#' @export
progress_tracker_start <- function(tracker, step, note = NULL) {
  idx <- .tracker_index(tracker, step)
  env <- tracker$env
  env$records$status[idx] <- "in_progress"
  env$records$started_at[idx] <- Sys.time()
  if (!is.null(note)) {
    env$records$note[idx] <- note
  }
  .tracker_log(tracker, sprintf("Started %s", step))
  .tracker_emit_update(tracker, force = TRUE)
  invisible(tracker)
}

#' Mark a step as completed
#'
#' @param tracker Object created by [progress_tracker()].
#' @param step Step name.
#' @param score Optional numeric score between 0 and 1 used to derive a
#'   quality tier.
#' @param quality Optional explicit quality tier. Overrides `score` when
#'   provided.
#' @param note Optional note to store for the step.
#'
#' @export
progress_tracker_finish <- function(tracker, step, score = NULL, quality = NULL, note = NULL) {
  idx <- .tracker_index(tracker, step)
  env <- tracker$env
  env$records$status[idx] <- "completed"
  env$records$finished_at[idx] <- Sys.time()
  if (!is.null(note)) {
    env$records$note[idx] <- note
  }
  resolved_quality <- if (!is.null(quality)) quality else tyler_quality_tier(score)
  env$records$quality[idx] <- resolved_quality
  .tracker_log(tracker, sprintf("Completed %s (%s)", step, if (!is.null(resolved_quality)) resolved_quality else "no tier"))
  .tracker_emit_update(tracker, force = TRUE)
  invisible(tracker)
}

#' Mark a step as failed
#'
#' @param tracker Object created by [progress_tracker()].
#' @param step Step name.
#' @param reason Optional string describing why the step failed.
#'
#' @export
progress_tracker_fail <- function(tracker, step, reason = NULL) {
  idx <- .tracker_index(tracker, step)
  env <- tracker$env
  env$records$status[idx] <- "failed"
  env$records$finished_at[idx] <- Sys.time()
  env$records$note[idx] <- reason
  .tracker_log(tracker, sprintf("Failed %s%s", step, if (!is.null(reason)) paste0(": ", reason) else ""), force = TRUE)
  .tracker_emit_update(tracker, force = TRUE)
  invisible(tracker)
}

#' Emit a manual progress update
#'
#' @param tracker Object created by [progress_tracker()].
#'
#' @param force Logical flag indicating whether the update should be emitted
#'   even if the configured interval has not elapsed.
#'
#' @export
progress_tracker_update <- function(tracker, force = FALSE) {
  .tracker_emit_update(tracker, force = force)
  invisible(tracker)
}

#' Return a tibble describing step-by-step progress
#'
#' @param tracker Object created by [progress_tracker()].
#'
#' @return Tibble with per-step status, timestamps, and quality tiers.
#' @importFrom tibble as_tibble
#' @export
progress_tracker_summary <- function(tracker) {
  env <- tracker$env
  tibble::as_tibble(env$records)
}
