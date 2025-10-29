#' Validate no artificial data limits in workflow
#'
#' Checks for common anti-patterns that limit data processing. This function
#' helps enforce the package policy of processing ALL available data without
#' artificial limits.
#'
#' @param data Input dataframe to check
#' @param context Description of where data came from, used in warning messages
#' @param min_expected Minimum number of rows expected (optional). If provided
#'   and actual rows are less, generates a warning.
#' @param max_expected Maximum number of rows expected (optional). If provided
#'   and actual rows exceed this, generates a warning.
#'
#' @return Invisible TRUE if passes all checks
#'
#' @examples
#' \dontrun{
#' data <- read_csv("providers.csv")
#' tyler_check_no_limits(data, "provider input")
#'
#' npi_results <- search_and_process_npi(data)
#' tyler_check_no_limits(npi_results, "NPI search results", min_expected = 100)
#' }
#'
#' @export
tyler_check_no_limits <- function(data,
                                   context = "dataset",
                                   min_expected = NULL,
                                   max_expected = NULL) {
  if (!is.data.frame(data)) {
    stop(sprintf("'%s' must be a data frame, got: %s", context, class(data)[1]))
  }

  n <- nrow(data)

  # Check for suspiciously round numbers that suggest artificial limits
  suspicious_counts <- c(5, 10, 25, 50, 100, 500, 1000, 5000, 10000, 50000, 100000)

  if (n %in% suspicious_counts) {
    warning(sprintf(
      paste(
        "Dataset '%s' has exactly %d rows - SUSPICIOUS!",
        "This may indicate artificial limiting via:",
        "  - head(%d)",
        "  - slice_head(n = %d)",
        "  - sample_n(%d)",
        "  - SQL LIMIT %d",
        "Verify this is the complete dataset, not a sample."
      ),
      context, n, n, n, n, n
    ), call. = FALSE)
  }

  # Check if data is an exact power of 10
  if (n > 0 && n %% 1000 == 0 && n <= 100000) {
    message(sprintf(
      "NOTE: Dataset '%s' has %d rows (exact multiple of 1000). If this is unexpected, check for max_records or n_max parameters.",
      context, n
    ))
  }

  # Check minimum expected rows
  if (!is.null(min_expected) && n < min_expected) {
    warning(sprintf(
      "Dataset '%s' has only %d rows but expected at least %d. Possible data loss or incomplete processing.",
      context, n, min_expected
    ), call. = FALSE)
  }

  # Check maximum expected rows
  if (!is.null(max_expected) && n > max_expected) {
    warning(sprintf(
      "Dataset '%s' has %d rows but expected at most %d. This may indicate duplicates or incorrect filtering.",
      context, n, max_expected
    ), call. = FALSE)
  }

  # Check for exactly 0 rows (silent failure indicator)
  if (n == 0) {
    warning(sprintf(
      "Dataset '%s' has ZERO rows! This likely indicates a silent failure. Check error logs.",
      context
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Scan code files for artificial limit patterns
#'
#' Scans R source files for common limiting anti-patterns that violate the
#' package policy of processing all available data. This is a code audit tool
#' to catch unintentional data limiting.
#'
#' @param path Directory to scan. Defaults to "R/" directory.
#' @param recursive Whether to scan subdirectories. Defaults to TRUE.
#' @param exclude_pattern Optional regex pattern of files to exclude from scan.
#'   For example, "test-.*\\\\.R$" to exclude test files.
#'
#' @return A data frame of found issues with columns: file, line, pattern, code.
#'   Returns empty data frame if no issues found. Also prints warnings.
#'
#' @details
#' Searches for these anti-patterns:
#' \itemize{
#'   \item \code{slice_head(n = )}
#'   \item \code{head(data, n)}
#'   \item \code{sample_n()}
#'   \item \code{n_max = }
#'   \item \code{max_records = }
#'   \item \code{LIMIT n} (SQL)
#' }
#'
#' @examples
#' \dontrun{
#' # Scan all R files in package
#' issues <- tyler_scan_for_limits("R/")
#'
#' # Scan with exclusions
#' issues <- tyler_scan_for_limits("R/", exclude_pattern = "deprecated")
#' }
#'
#' @export
tyler_scan_for_limits <- function(path = "R",
                                   recursive = TRUE,
                                   exclude_pattern = NULL) {
  if (!dir.exists(path)) {
    stop(sprintf("Directory '%s' does not exist", path))
  }

  # Anti-patterns to search for
  # Each pattern has: regex, description, severity
  patterns <- list(
    list(
      regex = "slice_head\\s*\\(\\s*n\\s*=\\s*[0-9]+",
      desc = "slice_head(n = N) - artificial row limit",
      severity = "CRITICAL"
    ),
    list(
      regex = "head\\s*\\([^,)]+,\\s*[0-9]+\\s*\\)",
      desc = "head(data, N) - artificial row limit",
      severity = "CRITICAL"
    ),
    list(
      regex = "sample_n\\s*\\([^,)]+,\\s*[0-9]+",
      desc = "sample_n(data, N) - sampling instead of full data",
      severity = "CRITICAL"
    ),
    list(
      regex = "n_max\\s*=\\s*[0-9]+",
      desc = "n_max = N - artificial read limit",
      severity = "HIGH"
    ),
    list(
      regex = "max_records\\s*=\\s*[0-9]+",
      desc = "max_records = N - artificial limit parameter",
      severity = "HIGH"
    ),
    list(
      regex = "LIMIT\\s+[0-9]+",
      desc = "SQL LIMIT N - database query limit",
      severity = "HIGH"
    ),
    list(
      regex = "\\[1:[0-9]+\\]",
      desc = "[1:N] subsetting - potential artificial limit",
      severity = "MEDIUM"
    ),
    list(
      regex = "top_n\\s*\\([^,)]+,\\s*[0-9]+",
      desc = "top_n(data, N) - selecting top N rows only",
      severity = "HIGH"
    )
  )

  # Get all R files
  files <- list.files(path,
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = recursive
  )

  # Apply exclusion pattern if provided
  if (!is.null(exclude_pattern)) {
    files <- files[!grepl(exclude_pattern, files)]
  }

  # Initialize results
  issues <- data.frame(
    file = character(),
    line = integer(),
    severity = character(),
    pattern = character(),
    code = character(),
    stringsAsFactors = FALSE
  )

  # Scan each file
  for (file in files) {
    # Skip if file is empty or can't be read
    if (!file.exists(file)) next

    tryCatch(
      {
        lines <- readLines(file, warn = FALSE)

        for (i in seq_along(lines)) {
          # Skip comments
          line_trimmed <- trimws(lines[i])
          if (grepl("^#", line_trimmed)) next

          # Check each pattern
          for (p in patterns) {
            if (grepl(p$regex, lines[i], ignore.case = FALSE)) {
              issues <- rbind(issues, data.frame(
                file = basename(file),
                line = i,
                severity = p$severity,
                pattern = p$desc,
                code = trimws(lines[i]),
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      },
      error = function(e) {
        warning(sprintf("Could not read file '%s': %s", file, e$message))
      }
    )
  }

  # Report findings
  if (nrow(issues) > 0) {
    # Sort by severity
    severity_order <- c("CRITICAL", "HIGH", "MEDIUM", "LOW")
    issues$severity <- factor(issues$severity, levels = severity_order)
    issues <- issues[order(issues$severity, issues$file, issues$line), ]

    warning(sprintf(
      "\n\n❌ Found %d potential artificial limits in code:\n\nCRITICAL: %d | HIGH: %d | MEDIUM: %d\n\nReview each occurrence carefully!",
      nrow(issues),
      sum(issues$severity == "CRITICAL"),
      sum(issues$severity == "HIGH"),
      sum(issues$severity == "MEDIUM")
    ), call. = FALSE)

    print(issues, row.names = FALSE)
  } else {
    message(sprintf("✓ No artificial data limits found in %d files", length(files)))
  }

  invisible(issues)
}


#' Validate API response row count matches expectation
#'
#' Helper to catch silent API failures that return empty or partial results.
#' Use after any API call where you expect a specific number of results.
#'
#' @param result API response data frame
#' @param expected Expected number of rows (e.g., number of queries sent)
#' @param api_name Name of API for error messages
#' @param tolerance Acceptable difference between expected and actual.
#'   Defaults to 0 (exact match required).
#'
#' @return Invisible TRUE if within tolerance, errors otherwise
#'
#' @examples
#' \dontrun{
#' # Expect exactly 100 geocoding results
#' coords <- geocode(addresses)
#' tyler_check_api_response(coords, expected = 100, api_name = "Google Geocoding")
#'
#' # Allow up to 5% missing
#' coords <- geocode(addresses)
#' tyler_check_api_response(coords, expected = 100, api_name = "Google Geocoding",
#'                         tolerance = 5)
#' }
#'
#' @export
tyler_check_api_response <- function(result,
                                     expected,
                                     api_name = "API",
                                     tolerance = 0) {
  if (!is.data.frame(result)) {
    stop(sprintf("%s returned non-dataframe result: %s", api_name, class(result)[1]))
  }

  actual <- nrow(result)
  diff <- abs(actual - expected)
  pct_diff <- if (expected > 0) diff / expected * 100 else 0

  if (diff > tolerance) {
    stop(sprintf(
      "%s response count mismatch:\n  Expected: %d rows\n  Actual: %d rows\n  Difference: %d (%.1f%%)\n  This indicates API failure or partial response.",
      api_name, expected, actual, diff, pct_diff
    ))
  }

  if (diff > 0 && diff <= tolerance) {
    message(sprintf(
      "%s: %d/%d rows returned (%.1f%% success)",
      api_name, actual, expected, actual / expected * 100
    ))
  }

  invisible(TRUE)
}


#' Validate no data loss between pipeline steps
#'
#' Ensures row counts don't unexpectedly decrease between operations.
#' Use at key checkpoints in data pipelines to catch silent filtering or joins
#' that lose data.
#'
#' @param before Row count before operation (or data frame)
#' @param after Row count after operation (or data frame)
#' @param operation Description of operation for error messages
#' @param expected_change Expected change in rows. Defaults to 0 (no change).
#'   Use positive for operations that add rows (e.g., joins), negative for
#'   operations that should remove rows (e.g., deduplication).
#' @param tolerance Maximum acceptable unexpected loss. Defaults to 0.
#'
#' @return Invisible TRUE if within expected change ± tolerance
#'
#' @examples
#' \dontrun{
#' # Expect no data loss in cleaning
#' before <- nrow(raw_data)
#' clean_data <- clean_phase_1_results(raw_data)
#' tyler_check_no_data_loss(before, clean_data, "Phase 1 cleaning")
#'
#' # Expect deduplication to remove ~10 rows, allow ±5
#' before <- nrow(data)
#' dedup_data <- deduplicate(data)
#' tyler_check_no_data_loss(before, dedup_data, "Deduplication",
#'                         expected_change = -10, tolerance = 5)
#' }
#'
#' @export
tyler_check_no_data_loss <- function(before,
                                     after,
                                     operation = "operation",
                                     expected_change = 0,
                                     tolerance = 0) {
  # Extract row counts if data frames provided
  n_before <- if (is.data.frame(before)) nrow(before) else as.integer(before)
  n_after <- if (is.data.frame(after)) nrow(after) else as.integer(after)

  actual_change <- n_after - n_before
  unexpected_change <- actual_change - expected_change

  if (abs(unexpected_change) > tolerance) {
    if (unexpected_change < 0) {
      stop(sprintf(
        "DATA LOSS detected in '%s':\n  Before: %d rows\n  After: %d rows\n  Lost: %d rows (%.1f%%)\n  Expected change: %+d\n  Tolerance: ±%d\n\nThis indicates silent filtering, failed joins, or data corruption.",
        operation, n_before, n_after, abs(actual_change),
        abs(actual_change) / n_before * 100,
        expected_change, tolerance
      ))
    } else {
      warning(sprintf(
        "UNEXPECTED ROW INCREASE in '%s':\n  Before: %d rows\n  After: %d rows\n  Added: %d rows (%.1f%%)\n  Expected change: %+d\n\nThis may indicate row duplication or incorrect joins.",
        operation, n_before, n_after, actual_change,
        actual_change / n_before * 100,
        expected_change
      ), call. = FALSE)
    }
  }

  invisible(TRUE)
}
