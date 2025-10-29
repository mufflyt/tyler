#' Comprehensive Logging Utilities for Tyler Package
#'
#' Provides plain-language logging with progress tracking, timing, and
#' user-friendly output for long-running workflows.
#'
#' @name logging-utils
NULL

# Global environment for tracking workflow state
.tyler_workflow <- new.env(parent = emptyenv())

#' Initialize workflow tracking
#'
#' @param workflow_name Name of the workflow (e.g., "Mystery Caller Workflow")
#' @param total_steps Total number of major steps
#' @param log_file Optional path to log file for persistent logging
#' @return Invisible NULL
#' @export
tyler_workflow_start <- function(workflow_name, total_steps = NULL, log_file = NULL) {
  .tyler_workflow$name <- workflow_name
  .tyler_workflow$start_time <- Sys.time()
  .tyler_workflow$total_steps <- total_steps
  .tyler_workflow$current_step <- 0
  .tyler_workflow$log_file <- log_file
  .tyler_workflow$step_results <- list()

  # Header
  message(strrep("=", 60))
  message(sprintf("  %s", workflow_name))
  message(sprintf("  Started: %s", format(.tyler_workflow$start_time, "%Y-%m-%d %H:%M:%S")))
  if (!is.null(total_steps)) {
    message(sprintf("  Total Steps: %d", total_steps))
  }
  message(strrep("=", 60))
  message("")

  # Write to log file if specified
  if (!is.null(log_file)) {
    dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
    writeLines(c(
      strrep("=", 60),
      sprintf("Workflow: %s", workflow_name),
      sprintf("Started: %s", format(.tyler_workflow$start_time, "%Y-%m-%d %H:%M:%S")),
      strrep("=", 60),
      ""
    ), log_file)
  }

  invisible(NULL)
}

#' Log a step start
#'
#' @param step_name Name of the step
#' @param detail Optional additional details
#' @param n_items Number of items to process (for progress tracking)
#' @return Invisible NULL
#' @export
tyler_log_step <- function(step_name, detail = NULL, n_items = NULL) {
  if (exists("current_step", envir = .tyler_workflow)) {
    .tyler_workflow$current_step <- .tyler_workflow$current_step + 1
  } else {
    .tyler_workflow$current_step <- 1
  }

  step_num <- .tyler_workflow$current_step
  total_steps <- .tyler_workflow$total_steps

  # Format step header
  if (!is.null(total_steps)) {
    header <- sprintf("\u25B6 Step %d/%d: %s", step_num, total_steps, step_name)
  } else {
    header <- sprintf("\u25B6 Step %d: %s", step_num, step_name)
  }

  message(header)

  if (!is.null(detail)) {
    message(sprintf("  %s", detail))
  }

  if (!is.null(n_items)) {
    message(sprintf("  Processing %s item(s)...", format(n_items, big.mark = ",")))
  }

  # Store step start time
  .tyler_workflow$step_results[[as.character(step_num)]] <- list(
    name = step_name,
    start_time = Sys.time(),
    n_items = n_items
  )

  tyler_log_to_file(header)
  if (!is.null(detail)) tyler_log_to_file(sprintf("  %s", detail))

  invisible(NULL)
}

#' Log informational message
#'
#' @param msg Message to log
#' @param indent Whether to indent (default TRUE)
#' @return Invisible NULL
#' @export
tyler_log_info <- function(msg, indent = TRUE) {
  formatted <- if (indent) sprintf("  \u2139 %s", msg) else sprintf("\u2139 %s", msg)
  message(formatted)
  tyler_log_to_file(formatted)
  invisible(NULL)
}

#' Log success message
#'
#' @param msg Message to log
#' @param details Optional list of details (name-value pairs)
#' @param indent Whether to indent (default TRUE)
#' @return Invisible NULL
#' @export
tyler_log_success <- function(msg, details = NULL, indent = TRUE) {
  formatted <- if (indent) sprintf("  \u2713 %s", msg) else sprintf("\u2713 %s", msg)
  message(formatted)
  tyler_log_to_file(formatted)

  if (!is.null(details)) {
    for (name in names(details)) {
      detail_line <- sprintf("    - %s: %s", name, details[[name]])
      message(detail_line)
      tyler_log_to_file(detail_line)
    }
  }

  invisible(NULL)
}

#' Log warning message
#'
#' @param msg Warning message
#' @param fix Optional suggested fix
#' @param indent Whether to indent (default TRUE)
#' @return Invisible NULL
#' @export
tyler_log_warning <- function(msg, fix = NULL, indent = TRUE) {
  formatted <- if (indent) sprintf("  \u26A0 WARNING: %s", msg) else sprintf("\u26A0 WARNING: %s", msg)
  message(formatted)
  tyler_log_to_file(formatted)

  if (!is.null(fix)) {
    fix_line <- sprintf("    Fix: %s", fix)
    message(fix_line)
    tyler_log_to_file(fix_line)
  }

  invisible(NULL)
}

#' Log error message with context
#'
#' @param msg Error message
#' @param cause Root cause of error
#' @param fix Suggested fix
#' @param indent Whether to indent (default TRUE)
#' @return Invisible NULL
#' @export
tyler_log_error <- function(msg, cause = NULL, fix = NULL, indent = TRUE) {
  formatted <- if (indent) sprintf("  \u2717 ERROR: %s", msg) else sprintf("\u2717 ERROR: %s", msg)
  message(formatted)
  tyler_log_to_file(formatted)

  if (!is.null(cause)) {
    cause_line <- sprintf("    Cause: %s", cause)
    message(cause_line)
    tyler_log_to_file(cause_line)
  }

  if (!is.null(fix)) {
    fix_line <- sprintf("    Fix: %s", fix)
    message(fix_line)
    tyler_log_to_file(fix_line)
  }

  invisible(NULL)
}

#' Log progress for batch operations
#'
#' @param current Current item number
#' @param total Total items
#' @param status Optional status message
#' @param show_percent Whether to show percentage (default TRUE)
#' @return Invisible NULL
#' @export
tyler_log_progress <- function(current, total, status = NULL, show_percent = TRUE) {
  pct <- round(current / total * 100, 1)

  if (show_percent) {
    if (!is.null(status)) {
      msg <- sprintf("  \u25B8 Progress: %d/%d (%.1f%%) - %s", current, total, pct, status)
    } else {
      msg <- sprintf("  \u25B8 Progress: %d/%d (%.1f%%)", current, total, pct)
    }
  } else {
    if (!is.null(status)) {
      msg <- sprintf("  \u25B8 Progress: %d/%d - %s", current, total, status)
    } else {
      msg <- sprintf("  \u25B8 Progress: %d/%d", current, total)
    }
  }

  message(msg)
  tyler_log_to_file(msg)
  invisible(NULL)
}

#' Log cache hit
#'
#' @param what What was loaded from cache
#' @param n_items Number of cached items
#' @return Invisible NULL
#' @export
tyler_log_cache_hit <- function(what, n_items) {
  msg <- sprintf("  \u21BB Loaded %s from cache (%s item(s))",
                 what,
                 format(n_items, big.mark = ","))
  message(msg)
  tyler_log_to_file(msg)
  invisible(NULL)
}

#' Log file save
#'
#' @param path File path
#' @param n_rows Number of rows (optional)
#' @return Invisible NULL
#' @export
tyler_log_save <- function(path, n_rows = NULL) {
  if (!is.null(n_rows)) {
    msg <- sprintf("  \u1F4BE Saved to: %s (%s rows)",
                   path,
                   format(n_rows, big.mark = ","))
  } else {
    msg <- sprintf("  \u1F4BE Saved to: %s", path)
  }
  message(msg)
  tyler_log_to_file(msg)
  invisible(NULL)
}

#' Complete current step with timing
#'
#' @param success_rate Optional success rate (0-1)
#' @param n_success Number of successful items
#' @param n_total Total items attempted
#' @return Invisible NULL
#' @export
tyler_log_step_complete <- function(success_rate = NULL, n_success = NULL, n_total = NULL) {
  step_num <- .tyler_workflow$current_step
  step_info <- .tyler_workflow$step_results[[as.character(step_num)]]

  if (!is.null(step_info)) {
    duration <- as.numeric(difftime(Sys.time(), step_info$start_time, units = "secs"))
    duration_str <- tyler_format_duration(duration)

    if (!is.null(success_rate)) {
      pct <- round(success_rate * 100, 1)
      msg <- sprintf("  \u2713 Step complete: %.1f%% success in %s", pct, duration_str)
    } else if (!is.null(n_success) && !is.null(n_total)) {
      pct <- round(n_success / n_total * 100, 1)
      msg <- sprintf("  \u2713 Step complete: %d/%d (%.1f%%) in %s",
                     n_success, n_total, pct, duration_str)
    } else {
      msg <- sprintf("  \u2713 Step complete in %s", duration_str)
    }

    message(msg)
    message("")  # Blank line after step
    tyler_log_to_file(msg)
    tyler_log_to_file("")

    # Store completion info
    step_info$end_time <- Sys.time()
    step_info$duration <- duration
    step_info$success_rate <- success_rate
    step_info$n_success <- n_success
    step_info$n_total <- n_total
    .tyler_workflow$step_results[[as.character(step_num)]] <- step_info
  }

  invisible(NULL)
}

#' End workflow and print summary
#'
#' @param final_n Number of final output rows
#' @param input_n Number of input rows
#' @return Invisible NULL
#' @export
tyler_workflow_end <- function(final_n = NULL, input_n = NULL) {
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, .tyler_workflow$start_time, units = "secs"))
  duration_str <- tyler_format_duration(duration)

  message("")
  message(strrep("=", 60))
  message(sprintf("  %s - COMPLETE", .tyler_workflow$name))
  message(strrep("=", 60))
  message("")

  if (!is.null(final_n) && !is.null(input_n)) {
    pct <- round(final_n / input_n * 100, 1)
    message(sprintf("  Input:  %s records", format(input_n, big.mark = ",")))
    message(sprintf("  Output: %s records (%.1f%%)", format(final_n, big.mark = ","), pct))
    message("")
  }

  # Print step summary
  if (length(.tyler_workflow$step_results) > 0) {
    message("  Step Summary:")
    for (step_num in seq_along(.tyler_workflow$step_results)) {
      step <- .tyler_workflow$step_results[[step_num]]
      if (!is.null(step$duration)) {
        step_dur <- tyler_format_duration(step$duration)
        if (!is.null(step$success_rate)) {
          pct <- round(step$success_rate * 100, 1)
          message(sprintf("    %d. %s: %.1f%% success in %s",
                         step_num, step$name, pct, step_dur))
        } else if (!is.null(step$n_success) && !is.null(step$n_total)) {
          pct <- round(step$n_success / step$n_total * 100, 1)
          message(sprintf("    %d. %s: %d/%d (%.1f%%) in %s",
                         step_num, step$name, step$n_success, step$n_total, pct, step_dur))
        } else {
          message(sprintf("    %d. %s: %s", step_num, step$name, step_dur))
        }
      }
    }
    message("")
  }

  message(sprintf("  Total Duration: %s", duration_str))
  message(sprintf("  Completed: %s", format(end_time, "%Y-%m-%d %H:%M:%S")))
  message("")
  message(strrep("=", 60))

  # Write to log file
  if (!is.null(.tyler_workflow$log_file)) {
    log_lines <- c(
      "",
      strrep("=", 60),
      "WORKFLOW COMPLETE",
      strrep("=", 60),
      sprintf("Total Duration: %s", duration_str),
      sprintf("Completed: %s", format(end_time, "%Y-%m-%d %H:%M:%S")),
      strrep("=", 60)
    )
    write(log_lines, file = .tyler_workflow$log_file, append = TRUE)
  }

  invisible(NULL)
}

#' Format duration in human-readable form
#'
#' @param seconds Duration in seconds
#' @return Formatted string (e.g., "2h 34m 15s")
#' @keywords internal
tyler_format_duration <- function(seconds) {
  if (seconds < 60) {
    return(sprintf("%.1fs", seconds))
  } else if (seconds < 3600) {
    mins <- floor(seconds / 60)
    secs <- round(seconds %% 60)
    return(sprintf("%dm %ds", mins, secs))
  } else {
    hours <- floor(seconds / 3600)
    mins <- floor((seconds %% 3600) / 60)
    secs <- round(seconds %% 60)
    return(sprintf("%dh %dm %ds", hours, mins, secs))
  }
}

#' Write to log file if configured
#'
#' @param msg Message to write
#' @keywords internal
tyler_log_to_file <- function(msg) {
  if (!is.null(.tyler_workflow$log_file)) {
    # Remove ANSI codes and special characters for file logging
    clean_msg <- gsub("\u2713|\u2717|\u26A0|\u2139|\u25B6|\u25B8|\u21BB|\u1F4BE", "", msg)
    write(clean_msg, file = .tyler_workflow$log_file, append = TRUE)
  }
  invisible(NULL)
}

#' Create a simple progress callback for batch operations
#'
#' @param total Total number of items
#' @param label Label for progress messages
#' @return A function that updates progress
#' @export
#'
#' @examples
#' \dontrun{
#' progress <- tyler_progress_callback(100, "Processing")
#' for (i in 1:100) {
#'   # do work
#'   progress(i)
#' }
#' }
tyler_progress_callback <- function(total, label = "Processing") {
  last_reported <- 0
  start_time <- Sys.time()

  function(current) {
    # Report every 10% or on last item
    pct <- current / total
    should_report <- (pct - last_reported >= 0.1) || (current == total)

    if (should_report) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- current / elapsed
      remaining <- (total - current) / rate

      tyler_log_progress(
        current,
        total,
        sprintf("%s | ETA: %s", label, tyler_format_duration(remaining))
      )

      last_reported <<- pct
    }
  }
}
