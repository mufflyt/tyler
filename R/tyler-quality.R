#' Convert numeric scores to qualitative tiers
#'
#' @param score Numeric value between 0 and 1.
#' @param thresholds Named numeric vector with `high` and `medium` entries.
#'
#' @return A quality tier of `"high"`, `"medium"`, or `"low"` (or `NA` when
#'   the score is missing).
#' @family utilities
#' @export
tyler_quality_tier <- function(score, thresholds = c(high = 0.9, medium = 0.75)) {
  if (is.null(score) || is.na(score)) {
    return(NA_character_)
  }
  if (!is.numeric(score) || any(score < 0 | score > 1)) {
    stop("`score` must be between 0 and 1.", call. = FALSE)
  }
  if (!all(c("high", "medium") %in% names(thresholds))) {
    stop("`thresholds` must be a named vector with 'high' and 'medium' entries.", call. = FALSE)
  }

  high_cutoff <- thresholds[["high"]]
  medium_cutoff <- thresholds[["medium"]]

  if (score >= high_cutoff) {
    "high"
  } else if (score >= medium_cutoff) {
    "medium"
  } else {
    "low"
  }
}

#' Assess completeness for required data columns
#'
#' @param data A data frame to assess.
#' @param required Columns that must be present and non-missing.
#' @param id_cols Optional identifier columns used to compute uniqueness.
#' @param thresholds Named numeric vector with `high` and `medium` breakpoints
#'   between 0 and 1 determining the quality tier.
#'
#' @return A list containing `summary` (tibble of completeness metrics) and
#'   `quality` (overall quality tier).
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom stats complete.cases
#' @family utilities
#' @export
#' @examples
#' df <- tibble::tibble(id = 1:3, value = c(1, NA, 3))
#' tyler_check_data_completeness(df, required = c("id", "value"))
tyler_check_data_completeness <- function(data, required = NULL, id_cols = NULL, thresholds = c(high = 0.9, medium = 0.75)) {
  validate_dataframe(data, name = "data", allow_zero_rows = FALSE)
  validate_required_columns(data, required, name = "data")

  if (!is.null(id_cols)) {
    validate_required_columns(data, id_cols, name = "data")
  }

  total_rows <- nrow(data)
  completeness <- vapply(required, function(col) {
    mean(stats::complete.cases(data[[col]]))
  }, numeric(1))

  summary <- tibble::tibble(
    column = required,
    completeness = completeness,
    missing = 1 - completeness
  )

  if (!is.null(id_cols) && length(id_cols)) {
    unique_rows <- nrow(unique(data[id_cols]))
    summary <- dplyr::bind_rows(
      summary,
      tibble::tibble(
        column = paste(id_cols, collapse = "+"),
        completeness = unique_rows / total_rows,
        missing = 1 - (unique_rows / total_rows)
      )
    )
  }

  overall_score <- mean(summary$completeness, na.rm = TRUE)
  quality <- tyler_quality_tier(overall_score, thresholds = thresholds)

  list(summary = summary, quality = quality, score = overall_score)
}

#' Toggle quiet logging for helper functions
#'
#' @param quiet Logical flag. When `TRUE`, suppress messages emitted by
#'   [tyler_log_info()].
#'
#' @return The previous quiet value (invisibly).
#' @family utilities
#' @export
tyler_use_quiet_logging <- function(quiet = TRUE) {
  old <- getOption("tyler.quiet", FALSE)
  options(tyler.quiet = quiet)
  invisible(old)
}

#' Create a compact data transparency report
#'
#' Builds a single, auditable summary table showing completeness for key
#' columns, duplicate row rate, and (when available) audit metadata attached by
#' cleaning workflows such as [clean_phase_1_results()].
#'
#' @param data A data frame to profile.
#' @param key_columns Character vector of columns to include in completeness
#'   checks. Missing columns are retained in the report with `NA` metrics.
#' @param id_cols Optional identifier columns used to compute duplicate rate.
#'
#' @return A list with:
#' \itemize{
#'   \item `column_metrics`: tibble with per-column completeness.
#'   \item `dataset_metrics`: tibble with dataset-level transparency metrics.
#'   \item `audit_trail`: audit metadata attribute from `data`, if present.
#' }
#' @family utilities
#' @export
tyler_data_transparency_report <- function(data,
                                           key_columns = c("names", "npi", "phone_number", "state_name"),
                                           id_cols = c("random_id", "doctor_id", "id")) {
  validate_dataframe(data, name = "data", allow_zero_rows = FALSE)

  present_key_cols <- intersect(key_columns, names(data))
  missing_key_cols <- setdiff(key_columns, names(data))

  metrics_present <- if (length(present_key_cols) > 0) {
    tibble::tibble(
      column = present_key_cols,
      present = TRUE,
      non_missing = vapply(present_key_cols, function(col) {
        sum(!is.na(data[[col]]) & as.character(data[[col]]) != "")
      }, numeric(1)),
      total_rows = nrow(data)
    )
  } else {
    tibble::tibble(
      column = character(0),
      present = logical(0),
      non_missing = numeric(0),
      total_rows = numeric(0)
    )
  }

  metrics_missing <- tibble::tibble(
    column = missing_key_cols,
    present = FALSE,
    non_missing = NA_real_,
    total_rows = nrow(data)
  )

  column_metrics <- dplyr::bind_rows(metrics_present, metrics_missing)
  column_metrics$completeness <- column_metrics$non_missing / column_metrics$total_rows
  column_metrics$quality <- vapply(column_metrics$completeness, tyler_quality_tier, character(1))

  id_col <- id_cols[id_cols %in% names(data)][1]
  duplicate_rate <- NA_real_
  if (!is.na(id_col)) {
    duplicate_rate <- mean(duplicated(data[[id_col]]))
  }

  audit_trail <- attr(data, "audit_trail")

  dataset_metrics <- tibble::tibble(
    metric = c("rows", "columns", "duplicate_rate", "has_audit_trail"),
    value = c(
      as.character(nrow(data)),
      as.character(ncol(data)),
      ifelse(is.na(duplicate_rate), NA_character_, sprintf("%.4f", duplicate_rate)),
      as.character(!is.null(audit_trail))
    )
  )

  list(
    column_metrics = column_metrics,
    dataset_metrics = dataset_metrics,
    audit_trail = audit_trail
  )
}
