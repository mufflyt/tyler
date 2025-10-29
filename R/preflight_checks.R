#' Preflight Checks for Tyler Workflows
#'
#' Pre-flight validation system that checks all requirements before starting
#' long-running workflows. Catches errors early, estimates resources, and
#' gives users confidence before committing hours to processing.
#'
#' @name preflight-checks
NULL

#' Run comprehensive preflight checks before workflow
#'
#' Validates all requirements for a successful workflow run including API keys,
#' data quality, output directories, estimated resources, and dependencies.
#' Provides a go/no-go decision before starting long-running operations.
#'
#' @param input_data Path to input data file or data frame
#' @param output_dir Output directory path
#' @param google_maps_api_key Google Maps API key (optional if not geocoding)
#' @param here_api_key HERE API key (optional if not creating isochrones)
#' @param check_apis Whether to validate API keys with test calls (default: TRUE)
#' @param estimate_resources Whether to estimate runtime and memory (default: TRUE)
#' @param interactive Whether to prompt user for confirmation (default: TRUE)
#' @param required_columns Required column names in input data
#'
#' @return Invisible list with check results, or stops with error if checks fail
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic preflight check
#' tyler_preflight_check(
#'   input_data = "physicians.csv",
#'   output_dir = "output/",
#'   google_maps_api_key = Sys.getenv("GOOGLE_API_KEY"),
#'   here_api_key = Sys.getenv("HERE_API_KEY")
#' )
#'
#' # Non-interactive (for scripts)
#' tyler_preflight_check(
#'   input_data = data,
#'   output_dir = "output/",
#'   interactive = FALSE
#' )
#' }
tyler_preflight_check <- function(input_data,
                                   output_dir,
                                   google_maps_api_key = NULL,
                                   here_api_key = NULL,
                                   check_apis = TRUE,
                                   estimate_resources = TRUE,
                                   interactive = interactive(),
                                   required_columns = c("first", "last")) {

  message("")
  message("\u256D", strrep("\u2500", 58), "\u256E")
  message("\u2502", "   Tyler Package - Preflight Check", strrep(" ", 23), "\u2502")
  message("\u2570", strrep("\u2500", 58), "\u256F")
  message("")

  checks <- list(
    data = FALSE,
    output_dir = FALSE,
    api_keys = FALSE,
    data_quality = FALSE,
    resources = FALSE
  )

  errors <- character()
  warnings <- character()

  # ==================== Check 1: Input Data ====================
  message("\U0001F4CA Checking input data...")

  data_check <- tryCatch({
    if (is.character(input_data)) {
      if (!file.exists(input_data)) {
        errors <- c(errors, sprintf("Input file not found: %s", input_data))
        list(success = FALSE, n_rows = 0, data = NULL)
      } else {
        data <- tyler_read_table(input_data)
        checks$data <- TRUE
        message(sprintf("  \u2713 Input file found: %s (%s rows)",
                       basename(input_data),
                       format(nrow(data), big.mark = ",")))
        list(success = TRUE, n_rows = nrow(data), data = data)
      }
    } else if (is.data.frame(input_data)) {
      checks$data <- TRUE
      message(sprintf("  \u2713 Input data frame validated (%s rows)",
                     format(nrow(input_data), big.mark = ",")))
      list(success = TRUE, n_rows = nrow(input_data), data = input_data)
    } else {
      errors <- c(errors, "Input must be a file path or data frame")
      list(success = FALSE, n_rows = 0, data = NULL)
    }
  }, error = function(e) {
    errors <- c(errors, sprintf("Failed to load input data: %s", e$message))
    list(success = FALSE, n_rows = 0, data = NULL)
  })

  # ==================== Check 2: Required Columns ====================
  if (data_check$success && !is.null(data_check$data)) {
    missing_cols <- setdiff(required_columns, names(data_check$data))
    if (length(missing_cols) > 0) {
      errors <- c(errors, sprintf(
        "Input data missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ))
    } else {
      message(sprintf("  \u2713 All required columns present: %s",
                     paste(required_columns, collapse = ", ")))
    }
  }

  # ==================== Check 3: Output Directory ====================
  message("")
  message("\U0001F4BE Checking output directory...")

  if (!dir.exists(output_dir)) {
    tryCatch({
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      checks$output_dir <- TRUE
      message(sprintf("  \u2713 Created output directory: %s", output_dir))
    }, error = function(e) {
      errors <- c(errors, sprintf("Cannot create output directory: %s", e$message))
    })
  } else {
    # Check if writable
    test_file <- file.path(output_dir, ".tyler_write_test")
    can_write <- tryCatch({
      writeLines("test", test_file)
      file.remove(test_file)
      TRUE
    }, error = function(e) FALSE)

    if (can_write) {
      checks$output_dir <- TRUE
      message(sprintf("  \u2713 Output directory writable: %s", output_dir))
    } else {
      errors <- c(errors, sprintf("Output directory not writable: %s", output_dir))
    }
  }

  # ==================== Check 4: API Keys ====================
  message("")
  message("\U0001F511 Checking API keys...")

  if (!is.null(google_maps_api_key) && nzchar(google_maps_api_key)) {
    if (check_apis) {
      google_check <- tyler_validate_google_api(google_maps_api_key)
      if (google_check$valid) {
        message("  \u2713 Google Maps API key valid")
      } else {
        errors <- c(errors, sprintf("Google Maps API key invalid: %s", google_check$error))
      }
    } else {
      message("  \u2713 Google Maps API key provided (not tested)")
    }
  } else {
    warnings <- c(warnings, "No Google Maps API key provided (geocoding will fail)")
  }

  if (!is.null(here_api_key) && nzchar(here_api_key)) {
    if (check_apis) {
      here_check <- tyler_validate_here_api(here_api_key)
      if (here_check$valid) {
        message("  \u2713 HERE API key valid")
      } else {
        errors <- c(errors, sprintf("HERE API key invalid: %s", here_check$error))
      }
    } else {
      message("  \u2713 HERE API key provided (not tested)")
    }
  } else {
    warnings <- c(warnings, "No HERE API key provided (isochrones will fail)")
  }

  checks$api_keys <- length(errors) == 0

  # ==================== Check 5: Data Quality ====================
  message("")
  message("\U0001F50D Checking data quality...")

  if (data_check$success && !is.null(data_check$data)) {
    quality_report <- tyler_assess_data_quality(
      data_check$data,
      required_columns = required_columns
    )

    checks$data_quality <- quality_report$score >= 0.70

    message(sprintf("  Data Quality Score: %.1f%% %s",
                   quality_report$score * 100,
                   if (quality_report$score >= 0.80) "\u2713" else "\u26A0"))

    if (quality_report$score < 0.70) {
      errors <- c(errors, "Data quality too low (< 70%). Review and clean data first.")
    } else if (quality_report$score < 0.80) {
      warnings <- c(warnings, sprintf(
        "Data quality acceptable but not ideal (%.1f%%). Consider cleaning data.",
        quality_report$score * 100
      ))
    }

    # Report specific issues
    for (issue in quality_report$issues) {
      if (issue$severity == "error") {
        message(sprintf("    \u2717 %s", issue$message))
      } else {
        message(sprintf("    \u26A0 %s", issue$message))
      }
    }
  }

  # ==================== Check 6: Resource Estimation ====================
  if (estimate_resources && data_check$success) {
    message("")
    message("\U0001F4CA Estimating resources...")

    estimates <- tyler_estimate_resources(data_check$n_rows)

    message(sprintf("  \u23F1 Estimated runtime: %s (for %s records)",
                   estimates$runtime_str,
                   format(data_check$n_rows, big.mark = ",")))
    message(sprintf("  \U0001F4BE Estimated memory: %s",
                   estimates$memory_str))

    if (estimates$memory_gb > 8) {
      warnings <- c(warnings, sprintf(
        "High memory usage estimated (%.1f GB). Ensure sufficient RAM available.",
        estimates$memory_gb
      ))
    }

    if (estimates$runtime_hours > 2) {
      warnings <- c(warnings, sprintf(
        "Long runtime estimated (%.1f hours). Consider running overnight or in batches.",
        estimates$runtime_hours
      ))
    }

    checks$resources <- TRUE
  }

  # ==================== Check 7: Dependencies ====================
  message("")
  message("\U0001F4E6 Checking dependencies...")

  required_packages <- c("dplyr", "tidyr", "sf", "ggmap", "npi")
  missing_packages <- character()

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }

  if (length(missing_packages) > 0) {
    errors <- c(errors, sprintf(
      "Missing required packages: %s. Install with: install.packages(c(%s))",
      paste(missing_packages, collapse = ", "),
      paste(sprintf("'%s'", missing_packages), collapse = ", ")
    ))
  } else {
    message(sprintf("  \u2713 All required packages installed (%d checked)",
                   length(required_packages)))
  }

  # ==================== Summary ====================
  message("")
  message(strrep("\u2500", 60))

  all_passed <- length(errors) == 0
  has_warnings <- length(warnings) > 0

  if (all_passed && !has_warnings) {
    message("")
    message("  \u2705 All preflight checks PASSED")
    message("")
  } else if (all_passed && has_warnings) {
    message("")
    message("  \u26A0 Preflight checks passed with WARNINGS:")
    for (w in warnings) {
      message(sprintf("    - %s", w))
    }
    message("")
  } else {
    message("")
    message("  \u274C Preflight checks FAILED:")
    for (e in errors) {
      message(sprintf("    - %s", e))
    }
    if (has_warnings) {
      message("")
      message("  Warnings:")
      for (w in warnings) {
        message(sprintf("    - %s", w))
      }
    }
    message("")
    message(strrep("\u2500", 60))
    stop("Preflight checks failed. Fix errors above before proceeding.", call. = FALSE)
  }

  # ==================== User Confirmation ====================
  if (interactive && all_passed) {
    message(strrep("\u2500", 60))
    message("")
    if (data_check$success) {
      message(sprintf("  Ready to process %s records",
                     format(data_check$n_rows, big.mark = ",")))
      if (estimate_resources) {
        message(sprintf("  Estimated time: %s", estimates$runtime_str))
        message(sprintf("  Estimated memory: %s", estimates$memory_str))
      }
    }
    message("")

    response <- readline(prompt = "  \u25B6 Proceed with workflow? [Y/n]: ")

    if (tolower(trimws(response)) %in% c("n", "no")) {
      message("")
      message("  \u26A0 Workflow cancelled by user")
      message("")
      stop("User cancelled workflow", call. = FALSE)
    }

    message("")
    message("  \u2705 Starting workflow...")
    message("")
  }

  message(strrep("\u2500", 60))
  message("")

  invisible(list(
    passed = all_passed,
    checks = checks,
    errors = errors,
    warnings = warnings,
    data = data_check$data,
    n_rows = data_check$n_rows,
    estimates = if (estimate_resources) estimates else NULL
  ))
}


#' Validate Google Maps API key
#'
#' @param api_key Google Maps API key
#' @return List with valid (logical) and error (character) fields
#' @keywords internal
tyler_validate_google_api <- function(api_key) {
  tryCatch({
    # Try a simple geocoding request
    ggmap::register_google(key = api_key)
    result <- ggmap::geocode("1600 Amphitheatre Parkway, Mountain View, CA", key = api_key)

    if (is.data.frame(result) && nrow(result) == 1) {
      list(valid = TRUE, error = NULL)
    } else {
      list(valid = FALSE, error = "API returned unexpected result")
    }
  }, error = function(e) {
    list(valid = FALSE, error = e$message)
  })
}


#' Validate HERE API key
#'
#' @param api_key HERE API key
#' @return List with valid (logical) and error (character) fields
#' @keywords internal
tyler_validate_here_api <- function(api_key) {
  tryCatch({
    # Try a simple isochrone request
    url <- sprintf(
      "https://isoline.router.hereapi.com/v8/isolines?transportMode=car&origin=40.7128,-74.0060&range[type]=time&range[values]=300&apiKey=%s",
      api_key
    )

    response <- httr::GET(url, httr::timeout(10))

    if (httr::status_code(response) == 200) {
      list(valid = TRUE, error = NULL)
    } else if (httr::status_code(response) == 401) {
      list(valid = FALSE, error = "API key unauthorized (401)")
    } else if (httr::status_code(response) == 403) {
      list(valid = FALSE, error = "API key forbidden (403)")
    } else {
      list(valid = FALSE, error = sprintf("API returned status %d", httr::status_code(response)))
    }
  }, error = function(e) {
    list(valid = FALSE, error = e$message)
  })
}


#' Assess data quality
#'
#' @param data Data frame to assess
#' @param required_columns Required column names
#' @return List with score (0-1) and issues
#' @keywords internal
tyler_assess_data_quality <- function(data, required_columns = c("first", "last")) {
  issues <- list()
  penalties <- 0
  max_penalties <- 10

  # Check for missing values in required columns
  for (col in required_columns) {
    if (col %in% names(data)) {
      na_count <- sum(is.na(data[[col]]))
      na_pct <- na_count / nrow(data)

      if (na_pct > 0.5) {
        issues <- c(issues, list(list(
          severity = "error",
          message = sprintf("Column '%s' has %.1f%% missing values", col, na_pct * 100)
        )))
        penalties <- penalties + 3
      } else if (na_pct > 0.2) {
        issues <- c(issues, list(list(
          severity = "warning",
          message = sprintf("Column '%s' has %.1f%% missing values", col, na_pct * 100)
        )))
        penalties <- penalties + 1
      }
    }
  }

  # Check for duplicate rows
  dup_count <- sum(duplicated(data))
  dup_pct <- dup_count / nrow(data)

  if (dup_pct > 0.1) {
    issues <- c(issues, list(list(
      severity = "warning",
      message = sprintf("%.1f%% duplicate rows detected", dup_pct * 100)
    )))
    penalties <- penalties + 1
  }

  # Check for data type consistency
  for (col in required_columns) {
    if (col %in% names(data)) {
      if (!is.character(data[[col]]) && !is.factor(data[[col]])) {
        issues <- c(issues, list(list(
          severity = "warning",
          message = sprintf("Column '%s' should be character/text type", col)
        )))
        penalties <- penalties + 0.5
      }
    }
  }

  score <- max(0, 1 - (penalties / max_penalties))

  list(
    score = score,
    issues = issues,
    penalties = penalties
  )
}


#' Estimate workflow resources
#'
#' @param n_rows Number of input rows
#' @return List with runtime and memory estimates
#' @keywords internal
tyler_estimate_resources <- function(n_rows) {
  # Rough estimates based on typical performance
  # These should be calibrated with real-world data

  # Time estimates (seconds per row for each major operation)
  time_per_row_npi <- 1.5  # NPI search
  time_per_row_geocode <- 0.5  # Geocoding (cached)
  time_per_row_isochrone <- 2.5  # Isochrone generation

  total_seconds <- n_rows * (time_per_row_npi + time_per_row_geocode + time_per_row_isochrone)
  runtime_hours <- total_seconds / 3600

  # Memory estimates (MB per row)
  memory_per_row <- 2  # Rough estimate for full workflow
  memory_mb <- n_rows * memory_per_row + 500  # +500 MB base
  memory_gb <- memory_mb / 1024

  # Format strings
  if (runtime_hours < 1) {
    runtime_minutes <- total_seconds / 60
    runtime_str <- sprintf("%.0f minutes", runtime_minutes)
  } else {
    hours <- floor(runtime_hours)
    minutes <- round((runtime_hours - hours) * 60)
    runtime_str <- sprintf("%dh %dm", hours, minutes)
  }

  if (memory_gb < 1) {
    memory_str <- sprintf("%.0f MB", memory_mb)
  } else {
    memory_str <- sprintf("%.1f GB", memory_gb)
  }

  list(
    runtime_seconds = total_seconds,
    runtime_hours = runtime_hours,
    runtime_str = runtime_str,
    memory_mb = memory_mb,
    memory_gb = memory_gb,
    memory_str = memory_str
  )
}
