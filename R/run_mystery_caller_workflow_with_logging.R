#' Run Mystery Caller Workflow with Comprehensive Logging
#'
#' This is an enhanced version of the mystery caller workflow that provides
#' comprehensive, plain-language logging throughout the entire process. Use this
#' to see detailed progress, timing, and success rates for each step.
#'
#' @param input_data Path to input CSV or data frame with physician data
#' @param output_dir Directory for output files
#' @param google_maps_api_key Google Maps API key for geocoding
#' @param here_api_key HERE API key for isochrones
#' @param drive_time_minutes Drive time breaks (default: c(30, 60, 120, 180))
#' @param census_year Census data year (default: 2020)
#' @param log_file Optional path to save log file (default: auto-generated)
#' @return Final results data frame
#'
#' @examples
#' \dontrun{
#' # Run with comprehensive logging
#' results <- run_mystery_caller_workflow_with_logging(
#'   input_data = "physicians.csv",
#'   output_dir = "output/",
#'   google_maps_api_key = Sys.getenv("GOOGLE_API_KEY"),
#'   here_api_key = Sys.getenv("HERE_API_KEY")
#' )
#'
#' # The console will show beautiful progress like:
#' # ============================================================
#' #   Mystery Caller Workflow
#' #   Started: 2025-10-29 14:30:00
#' #   Total Steps: 5
#' # ============================================================
#' #
#' # ▶ Step 1/5: Searching NPI Registry
#' #   Processing 1,234 item(s)...
#' #   ✓ NPI search complete: 1,174/1,234 (95.1%)
#' #   ✓ Step complete: 95.1% success in 23m 15s
#' #
#' # ▶ Step 2/5: Geocoding Addresses
#' #   Found 1174 total address records, 1152 unique
#' #   ✓ Geocoding complete: 1,152/1,152 succeeded (100.0%)
#' #   ✓ Step complete in 12m 30s
#' # ...
#' }
#'
#' @export
run_mystery_caller_workflow_with_logging <- function(input_data,
                                                      output_dir,
                                                      google_maps_api_key,
                                                      here_api_key,
                                                      drive_time_minutes = c(30, 60, 120, 180),
                                                      census_year = 2020,
                                                      log_file = NULL) {

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Auto-generate log file path if not provided
  if (is.null(log_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path(output_dir, sprintf("workflow_log_%s.txt", timestamp))
  }

  # Initialize workflow tracking
  tyler_workflow_start(
    workflow_name = "Mystery Caller Workflow",
    total_steps = 5,
    log_file = log_file
  )

  # Load input data
  if (is.character(input_data)) {
    data <- tyler_read_table(input_data)
    input_n <- nrow(data)
  } else {
    data <- input_data
    input_n <- nrow(data)
  }

  tyler_log_info(sprintf("Loaded %s input records", format(input_n, big.mark = ",")))
  tyler_log_info(sprintf("Output directory: %s", output_dir))
  tyler_log_info(sprintf("Log file: %s", log_file))
  message("")

  # ==================== STEP 1: NPI Search ====================
  tyler_log_step(
    "Searching NPI Registry",
    detail = "Looking up physician NPIs by name",
    n_items = input_n
  )

  tryCatch({
    npi_results <- search_and_process_npi(
      data,
      dest_dir = output_dir,
      accumulate_path = file.path(output_dir, "npi_accumulated.csv"),
      resume = TRUE,
      notify = FALSE
    )

    success_rate_npi <- nrow(npi_results) / input_n
    tyler_log_step_complete(
      n_success = nrow(npi_results),
      n_total = input_n
    )

    data <- npi_results

  }, error = function(e) {
    tyler_log_error(
      "NPI search failed",
      cause = e$message,
      fix = "Check API connectivity and input data format"
    )
    tyler_workflow_end()
    stop(e)
  })

  # ==================== STEP 2: Geocoding ====================
  tyler_log_step(
    "Geocoding Addresses",
    detail = "Converting addresses to latitude/longitude coordinates",
    n_items = nrow(data)
  )

  tryCatch({
    geocoded_data <- geocode_unique_addresses(
      file_path = file.path(output_dir, "npi_accumulated.csv"),
      google_maps_api_key = google_maps_api_key,
      output_file_path = file.path(output_dir, "geocoded_physicians.csv"),
      failed_output_path = file.path(output_dir, "geocoding_failures.csv"),
      notify = FALSE,
      quiet = FALSE
    )

    tyler_log_step_complete()
    data <- geocoded_data

  }, error = function(e) {
    tyler_log_error(
      "Geocoding failed",
      cause = e$message,
      fix = "Check Google Maps API key and address format"
    )
    tyler_workflow_end()
    stop(e)
  })

  # ==================== STEP 3: Generate Isochrones ====================
  tyler_log_step(
    "Generating Isochrones",
    detail = sprintf("Creating drive-time polygons: %s minutes",
                    paste(drive_time_minutes, collapse = ", ")),
    n_items = nrow(data)
  )

  tryCatch({
    isochrone_data <- create_isochrones_for_dataframe(
      dataframe = data,
      api_key = here_api_key,
      range_in_seconds = drive_time_minutes * 60,
      output_dir = file.path(output_dir, "isochrones"),
      notify = FALSE
    )

    tyler_log_step_complete()
    data <- isochrone_data

  }, error = function(e) {
    tyler_log_error(
      "Isochrone generation failed",
      cause = e$message,
      fix = "Check HERE API key and coordinate validity"
    )
    tyler_workflow_end()
    stop(e)
  })

  # ==================== STEP 4: Calculate Census Overlap ====================
  for (drive_time in drive_time_minutes) {
    tyler_log_step(
      sprintf("Calculating Census Overlap (%d min)", drive_time),
      detail = "Computing demographic statistics for service areas"
    )

    tryCatch({
      overlap_result <- calculate_intersection_overlap_and_save(
        isochrones = data,
        drive_time_minutes = drive_time,
        provider_year = census_year,
        acs_year = census_year,
        output_dir = output_dir
      )

      tyler_log_step_complete()

    }, error = function(e) {
      tyler_log_error(
        sprintf("Census overlap calculation failed for %d minutes", drive_time),
        cause = e$message,
        fix = "Check census data availability and coordinate system alignment"
      )
      tyler_workflow_end()
      stop(e)
    })
  }

  # ==================== STEP 5: Finalize Results ====================
  tyler_log_step(
    "Finalizing Results",
    detail = "Cleaning and exporting final dataset"
  )

  final_output <- file.path(output_dir, "mystery_caller_final_results.csv")
  tyler_write_table(data, final_output)
  tyler_log_save(final_output, n_rows = nrow(data))
  tyler_log_step_complete()

  # End workflow with summary
  tyler_workflow_end(
    final_n = nrow(data),
    input_n = input_n
  )

  message("")
  tyler_log_success("✨ Workflow complete! All results saved to output directory.", indent = FALSE)
  message("")

  invisible(data)
}


#' Print a formatted summary dashboard
#'
#' @param results List containing workflow results
#' @export
tyler_print_dashboard <- function(results) {
  message("")
  message("\u256D", strrep("\u2500", 58), "\u256E")
  message("\u2502", "   Mystery Caller Workflow Summary", strrep(" ", 23), "\u2502")
  message("\u2570", strrep("\u2500", 58), "\u256F")
  message("")

  if (!is.null(results$input_n) && !is.null(results$final_n)) {
    pct <- round(results$final_n / results$input_n * 100, 1)
    message(sprintf("  Input:  %s physicians", format(results$input_n, big.mark = ",")))
    message(sprintf("  Output: %s complete records (%.1f%%)",
                   format(results$final_n, big.mark = ","), pct))
    message("")
  }

  if (!is.null(results$steps)) {
    message("  \U0001F4CA Step Results:")
    for (step in results$steps) {
      icon <- if (step$success) "\u2713" else "\u2717"
      message(sprintf("    %s %s: %s", icon, step$name, step$result))
    }
    message("")
  }

  if (!is.null(results$duration)) {
    message(sprintf("  \U0001F551 Duration: %s", results$duration))
  }

  if (!is.null(results$output_dir)) {
    message(sprintf("  \U0001F4BE Output: %s", results$output_dir))
  }

  message("")
  message(strrep("\u2500", 60))
  message("")
}
