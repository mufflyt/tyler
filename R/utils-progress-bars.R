#' Beautiful Progress Bars for Tyler Package
#'
#' Provides animated progress bars with ETA calculations using the cli package.
#' Integrates seamlessly with the logging framework for a modern CLI experience.
#'
#' @name progress-bars
NULL

#' Create a beautiful progress bar
#'
#' Creates an animated progress bar with ETA, percentage, and custom formatting.
#' Uses the cli package for cross-platform compatibility and beautiful output.
#'
#' @param name Name/description of the operation
#' @param total Total number of items to process
#' @param format Custom format string (uses cli::cli_progress_bar format)
#' @param clear Whether to clear the progress bar when done (default: FALSE)
#' @param show_after Show progress bar after this many seconds (default: 0)
#' @param force Whether to force progress bar even if not in terminal (default: FALSE)
#'
#' @return Progress bar ID (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple progress bar
#' pb_id <- tyler_progress_bar("Processing", total = 100)
#' for (i in 1:100) {
#'   Sys.sleep(0.1)
#'   tyler_progress_update(pb_id)
#' }
#' tyler_progress_done(pb_id)
#'
#' # With custom message
#' pb_id <- tyler_progress_bar("Geocoding addresses", total = 500)
#' for (i in 1:500) {
#'   # do work
#'   tyler_progress_update(pb_id, status = sprintf("Address %d", i))
#' }
#' tyler_progress_done(pb_id, result = "All addresses geocoded!")
#' }
tyler_progress_bar <- function(name,
                                total,
                                format = NULL,
                                clear = FALSE,
                                show_after = 0,
                                force = FALSE) {

  # Check if cli is available
  if (!requireNamespace("cli", quietly = TRUE)) {
    # Fallback to simple message-based progress
    message(sprintf("Starting: %s (%d items)", name, total))
    return(structure(
      list(name = name, total = total, current = 0, use_cli = FALSE),
      class = "tyler_progress"
    ))
  }

  # Check if we should show progress bar
  show_progress <- force || (interactive() && cli::is_ansi_tty())

  if (!show_progress) {
    # Fallback to message-based progress
    message(sprintf("Starting: %s (%d items)", name, total))
    return(structure(
      list(name = name, total = total, current = 0, use_cli = FALSE),
      class = "tyler_progress"
    ))
  }

  # Default format string
  if (is.null(format)) {
    format <- paste0(
      "{cli::pb_spin} {name} ",
      "{cli::pb_bar} ",
      "{cli::pb_current}/{cli::pb_total} ",
      "({cli::pb_percent}) ",
      "ETA: {cli::pb_eta}"
    )
  }

  # Create progress bar using cli
  pb_id <- cli::cli_progress_bar(
    name = name,
    total = total,
    format = format,
    clear = clear,
    show_after = show_after,
    .auto_close = FALSE
  )

  return(structure(
    list(
      id = pb_id,
      name = name,
      total = total,
      current = 0,
      use_cli = TRUE,
      start_time = Sys.time()
    ),
    class = "tyler_progress"
  ))
}


#' Update progress bar
#'
#' Increments the progress bar by specified amount and optionally updates status.
#'
#' @param pb Progress bar object from tyler_progress_bar()
#' @param amount Amount to increment (default: 1)
#' @param status Optional status message to display
#' @param set Set to specific value instead of incrementing
#'
#' @return Invisible NULL
#' @export
tyler_progress_update <- function(pb, amount = 1, status = NULL, set = NULL) {
  if (!inherits(pb, "tyler_progress")) {
    return(invisible(NULL))
  }

  if (pb$use_cli) {
    # Update with cli
    if (!is.null(set)) {
      cli::cli_progress_update(id = pb$id, set = set, status = status)
      pb$current <- set
    } else {
      cli::cli_progress_update(id = pb$id, inc = amount, status = status)
      pb$current <- pb$current + amount
    }
  } else {
    # Fallback: message-based progress (show every 10%)
    if (!is.null(set)) {
      pb$current <- set
    } else {
      pb$current <- pb$current + amount
    }

    pct <- round(pb$current / pb$total * 100)
    prev_pct <- round((pb$current - amount) / pb$total * 100)

    # Report every 10%
    if ((pct %/% 10) > (prev_pct %/% 10) || pb$current == pb$total) {
      elapsed <- as.numeric(difftime(Sys.time(), pb$start_time, units = "secs"))
      rate <- pb$current / elapsed
      remaining_secs <- (pb$total - pb$current) / rate
      eta_str <- tyler_format_duration(remaining_secs)

      message(sprintf("  Progress: %d/%d (%d%%) - ETA: %s",
                     pb$current, pb$total, pct, eta_str))
    }
  }

  invisible(NULL)
}


#' Complete progress bar
#'
#' Marks progress bar as complete with optional success message.
#'
#' @param pb Progress bar object from tyler_progress_bar()
#' @param result Optional result message to display
#' @param status Final status (default: "done")
#'
#' @return Invisible NULL
#' @export
tyler_progress_done <- function(pb, result = NULL, status = "done") {
  if (!inherits(pb, "tyler_progress")) {
    return(invisible(NULL))
  }

  if (pb$use_cli) {
    cli::cli_progress_done(id = pb$id, result = result, .auto_close = TRUE)
  } else {
    # Fallback: completion message
    if (!is.null(result)) {
      message(sprintf("  \u2713 %s", result))
    } else {
      message(sprintf("  \u2713 %s complete", pb$name))
    }
  }

  invisible(NULL)
}


#' Fail progress bar
#'
#' Marks progress bar as failed with error message.
#'
#' @param pb Progress bar object from tyler_progress_bar()
#' @param msg Error message
#'
#' @return Invisible NULL
#' @export
tyler_progress_fail <- function(pb, msg = NULL) {
  if (!inherits(pb, "tyler_progress")) {
    return(invisible(NULL))
  }

  if (pb$use_cli) {
    if (!is.null(msg)) {
      cli::cli_progress_done(id = pb$id, result = "failed", .auto_close = TRUE)
      cli::cli_alert_danger(msg)
    } else {
      cli::cli_progress_done(id = pb$id, result = "failed", .auto_close = TRUE)
    }
  } else {
    # Fallback: error message
    if (!is.null(msg)) {
      message(sprintf("  \u2717 %s failed: %s", pb$name, msg))
    } else {
      message(sprintf("  \u2717 %s failed", pb$name))
    }
  }

  invisible(NULL)
}


#' Create a multi-step progress tracker
#'
#' Creates a progress tracker for workflows with multiple major steps.
#' Shows overall progress plus current step progress.
#'
#' @param steps Character vector of step names
#' @param show_overall Whether to show overall progress bar (default: TRUE)
#'
#' @return Multi-progress tracker object
#' @export
#'
#' @examples
#' \dontrun{
#' tracker <- tyler_multi_progress(c("Load Data", "Process", "Save"))
#'
#' # Step 1
#' tyler_multi_step(tracker, 1, total = 100)
#' for (i in 1:100) {
#'   tyler_multi_update(tracker)
#' }
#' tyler_multi_complete(tracker)
#'
#' # Step 2
#' tyler_multi_step(tracker, 2, total = 50)
#' for (i in 1:50) {
#'   tyler_multi_update(tracker)
#' }
#' tyler_multi_complete(tracker)
#'
#' tyler_multi_done(tracker)
#' }
tyler_multi_progress <- function(steps, show_overall = TRUE) {
  structure(
    list(
      steps = steps,
      total_steps = length(steps),
      current_step = 0,
      show_overall = show_overall,
      overall_pb = NULL,
      step_pb = NULL,
      use_cli = requireNamespace("cli", quietly = TRUE)
    ),
    class = "tyler_multi_progress"
  )
}


#' Start a step in multi-progress tracker
#'
#' @param tracker Multi-progress tracker object
#' @param step_num Step number (1-based)
#' @param total Total items in this step
#' @param detail Optional detail message
#'
#' @return Invisible NULL
#' @export
tyler_multi_step <- function(tracker, step_num, total, detail = NULL) {
  if (!inherits(tracker, "tyler_multi_progress")) {
    return(invisible(NULL))
  }

  tracker$current_step <- step_num
  step_name <- tracker$steps[step_num]

  # Show step header
  if (tracker$use_cli) {
    cli::cli_h2(sprintf("Step %d/%d: %s", step_num, tracker$total_steps, step_name))
    if (!is.null(detail)) {
      cli::cli_text(detail)
    }
  } else {
    message(sprintf("\n\u25B6 Step %d/%d: %s", step_num, tracker$total_steps, step_name))
    if (!is.null(detail)) {
      message(sprintf("  %s", detail))
    }
  }

  # Create step progress bar
  tracker$step_pb <- tyler_progress_bar(
    name = step_name,
    total = total,
    clear = FALSE
  )

  invisible(NULL)
}


#' Update current step in multi-progress tracker
#'
#' @param tracker Multi-progress tracker object
#' @param amount Amount to increment (default: 1)
#' @param status Optional status message
#'
#' @return Invisible NULL
#' @export
tyler_multi_update <- function(tracker, amount = 1, status = NULL) {
  if (!inherits(tracker, "tyler_multi_progress")) {
    return(invisible(NULL))
  }

  if (!is.null(tracker$step_pb)) {
    tyler_progress_update(tracker$step_pb, amount = amount, status = status)
  }

  invisible(NULL)
}


#' Complete current step in multi-progress tracker
#'
#' @param tracker Multi-progress tracker object
#' @param result Optional result message
#'
#' @return Invisible NULL
#' @export
tyler_multi_complete <- function(tracker, result = NULL) {
  if (!inherits(tracker, "tyler_multi_progress")) {
    return(invisible(NULL))
  }

  if (!is.null(tracker$step_pb)) {
    tyler_progress_done(tracker$step_pb, result = result)
    tracker$step_pb <- NULL
  }

  invisible(NULL)
}


#' Complete multi-step tracker
#'
#' @param tracker Multi-progress tracker object
#'
#' @return Invisible NULL
#' @export
tyler_multi_done <- function(tracker) {
  if (!inherits(tracker, "tyler_multi_progress")) {
    return(invisible(NULL))
  }

  if (tracker$use_cli) {
    cli::cli_alert_success("All steps complete!")
  } else {
    message("\n  \u2713 All steps complete!")
  }

  invisible(NULL)
}


#' Create a progress bar for batch processing
#'
#' Convenience wrapper for processing items in batches with automatic progress
#' updates and ETA calculations.
#'
#' @param items Vector of items to process
#' @param fn Function to apply to each item
#' @param name Name of operation (default: "Processing items")
#' @param batch_size Show progress every N items (default: 1, or max(1, length(items) / 100))
#' @param parallel Whether processing is parallel (affects ETA calculation)
#'
#' @return List of results from fn
#' @export
#'
#' @examples
#' \dontrun{
#' # Process items with progress bar
#' results <- tyler_progress_map(
#'   items = 1:100,
#'   fn = function(x) { Sys.sleep(0.1); x^2 },
#'   name = "Computing squares"
#' )
#'
#' # With custom batch size
#' results <- tyler_progress_map(
#'   items = addresses,
#'   fn = geocode_address,
#'   name = "Geocoding addresses",
#'   batch_size = 10  # Update every 10 items
#' )
#' }
tyler_progress_map <- function(items,
                                fn,
                                name = "Processing items",
                                batch_size = NULL,
                                parallel = FALSE) {

  n <- length(items)

  # Auto-calculate batch size if not provided
  if (is.null(batch_size)) {
    batch_size <- max(1, floor(n / 100))  # Update ~100 times
  }

  # Create progress bar
  pb <- tyler_progress_bar(name = name, total = n)

  # Process items
  results <- vector("list", length = n)
  for (i in seq_along(items)) {
    results[[i]] <- fn(items[[i]])

    # Update progress bar
    if (i %% batch_size == 0 || i == n) {
      tyler_progress_update(pb, amount = batch_size, set = i)
    }
  }

  # Complete
  tyler_progress_done(pb, result = sprintf("Processed %d items", n))

  return(results)
}


#' Show a spinner for indeterminate operations
#'
#' Shows an animated spinner for operations where total is unknown.
#'
#' @param name Operation name
#' @param msg Optional message to display
#'
#' @return Spinner ID
#' @export
tyler_spinner_start <- function(name, msg = NULL) {
  if (requireNamespace("cli", quietly = TRUE) && cli::is_ansi_tty()) {
    if (is.null(msg)) {
      msg <- sprintf("Working on: %s", name)
    }
    id <- cli::cli_progress_bar(
      name = name,
      total = NA,
      format = paste0("{cli::pb_spin} ", msg)
    )
    return(id)
  } else {
    message(sprintf("  \u21BB %s...", name))
    return(NULL)
  }
}


#' Stop a spinner
#'
#' @param id Spinner ID from tyler_spinner_start()
#' @param result Result message
#'
#' @return Invisible NULL
#' @export
tyler_spinner_stop <- function(id, result = "done") {
  if (!is.null(id) && requireNamespace("cli", quietly = TRUE)) {
    cli::cli_progress_done(id = id, result = result)
  }
  invisible(NULL)
}
