#' Utility helpers for project scaffolding and reporting
#'
#' These functions provide common scaffolding pieces used across
#' field work and analysis workflows. They intentionally live in a
#' single module so that end users can discover them easily when they
#' need to bootstrap a new project or audit environment.
#'
#' @name utility_library
NULL

# -------------------------------------------------------------------------
# Dependency management ---------------------------------------------------

#' Check for required R package dependencies
#'
#' @param packages Character vector of package names to verify.
#' @param install Logical; when `TRUE`, attempt to install missing packages
#'   using [install.packages()]. Defaults to `FALSE`.
#' @param repos Character vector of repositories to forward to
#'   [install.packages()] when `install = TRUE`.
#' @param quietly Logical flag controlling console output. When `TRUE`,
#'   suppresses status messages.
#'
#' @return A tibble summarising the dependency status for each package.
#' @importFrom tibble as_tibble
#' @importFrom utils installed.packages install.packages
#' @export
#' @examples
#' \donttest{
#' tyler_check_dependencies(c("dplyr", "sf"), install = FALSE)
#' }
tyler_check_dependencies <- function(packages, install = FALSE, repos = getOption("repos"), quietly = FALSE) {
  validate_dataframe(data.frame(pkg = packages), name = "packages", allow_zero_rows = FALSE)
  if (!is.character(packages)) {
    stop("`packages` must be a character vector of package names.", call. = FALSE)
  }

  packages <- unique(packages[nzchar(packages)])
  if (!length(packages)) {
    stop("No packages supplied to `tyler_check_dependencies()`.", call. = FALSE)
  }

  installed <- utils::installed.packages()[, c("Package", "Version"), drop = FALSE]
  installed_names <- installed[, "Package"]

  results <- lapply(packages, function(pkg) {
    is_installed <- pkg %in% installed_names
    version <- if (is_installed) installed[installed_names == pkg, "Version"] else NA_character_
    install_command <- sprintf("install.packages(\"%s\")", pkg)
    list(
      package = pkg,
      installed = is_installed,
      version = version,
      install_command = install_command
    )
  })

  summary <- tibble::as_tibble(do.call(rbind, results))

  missing_pkgs <- summary$package[!summary$installed]
  if (length(missing_pkgs)) {
    message_fn <- if (isTRUE(quietly)) identity else message
    message_fn(sprintf(
      "Missing %d package(s): %s",
      length(missing_pkgs),
      paste(missing_pkgs, collapse = ", ")
    ))

    if (isTRUE(install)) {
      for (pkg in missing_pkgs) {
        message_fn(sprintf("Installing %s via install.packages().", pkg))
        utils::install.packages(pkg, repos = repos)
      }
    }
  }

  summary
}

# -------------------------------------------------------------------------
# Path resolution ---------------------------------------------------------

#' Resolve project-relative paths from standard aliases
#'
#' @param ... Additional path components appended to the resolved base.
#' @param type Optional alias describing the base path. Supported values are
#'   `"data"`, `"raw-data"`, `"tables"`, `"figures"`, `"docs"`, and `"cache"`.
#'   When `NULL`, `...` are treated as relative to `base_dir`.
#' @param base_dir Base directory to resolve paths from. Defaults to the value
#'   stored in `getOption("tyler.base_dir")` or the current working directory.
#' @param create Logical. When `TRUE`, ensure the resolved parent directory
#'   exists.
#'
#' @return A normalised path as a character string.
#' @export
#' @examples
#' tyler_resolve_path("output.csv", type = "tables", create = TRUE)
tyler_resolve_path <- function(..., type = NULL, base_dir = getOption("tyler.base_dir", getwd()), create = FALSE) {
  if (!dir.exists(base_dir)) {
    stop(sprintf("Base directory '%s' does not exist.", base_dir), call. = FALSE)
  }

  aliases <- c(
    data = "data",
    `raw-data` = "data-raw",
    tables = "tables",
    figures = file.path("tables", "figures"),
    docs = "docs",
    cache = tyler_tempdir("cache")
  )

  base <- base_dir
  if (!is.null(type)) {
    if (!type %in% names(aliases)) {
      stop(sprintf("Unknown path alias '%s'.", type), call. = FALSE)
    }
    base <- file.path(base, aliases[[type]])
  }

  path <- file.path(base, ...)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (isTRUE(create)) {
    dir_to_create <- if (dir.exists(path) || grepl("\\.[^/]+$", path)) dirname(path) else path
    if (!dir.exists(dir_to_create)) {
      dir.create(dir_to_create, recursive = TRUE, showWarnings = FALSE)
    }
  }

  path
}

# -------------------------------------------------------------------------
# Data completeness -------------------------------------------------------

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
#' @export
#' @examples
#' df <- tibble::tibble(id = 1:3, value = c(1, NA, 3))
#' tyler_check_data_completeness(df, required = c("id", "value"))
tyler_check_data_completeness <- function(data, required = NULL, id_cols = NULL, thresholds = c(high = 0.9, medium = 0.75)) {
  validate_dataframe(data, name = "data")
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

  overall_score <- mean(summary$completeness)
  quality <- tyler_quality_tier(overall_score, thresholds = thresholds)

  list(summary = summary, quality = quality, score = overall_score)
}

# -------------------------------------------------------------------------
# Safe exports ------------------------------------------------------------

#' Write tabular or graphical outputs with timestamped backups
#'
#' @param x Object to export. Data frames are written as CSV files by default
#'   while `ggplot` objects are saved with [ggplot2::ggsave()]. Other objects are
#'   serialised via [saveRDS()].
#' @param path Destination file path.
#' @param backup Logical flag; when `TRUE` and `path` already exists, create a
#'   timestamped copy before overwriting.
#' @param quiet Logical flag controlling log output.
#'
#' @return The resolved path (invisible).
#' @importFrom ggplot2 ggsave
#' @importFrom readr write_csv
#' @importFrom utils write.table
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' tyler_export_with_backup(mtcars, tmp)
tyler_export_with_backup <- function(x, path, backup = TRUE, quiet = getOption("tyler.quiet", FALSE)) {
  if (missing(path) || !nzchar(path)) {
    stop("`path` must be a non-empty string.", call. = FALSE)
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (isTRUE(backup) && file.exists(path)) {
    timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
    backup_path <- sprintf("%s.%s.bak", path, timestamp)
    file.copy(path, backup_path, overwrite = TRUE)
    if (!quiet) {
      message(sprintf("Existing file backed up to %s", backup_path))
    }
  }

  ext <- tolower(tools::file_ext(path))
  if (inherits(x, "ggplot") && nzchar(ext)) {
    ggplot2::ggsave(filename = path, plot = x)
  } else if (is.data.frame(x) && identical(ext, "csv")) {
    readr::write_csv(x, path)
  } else if (identical(ext, "rds")) {
    saveRDS(x, path)
  } else if (identical(ext, "rda")) {
    save(list = deparse(substitute(x)), file = path)
  } else if (is.data.frame(x) && nzchar(ext)) {
    utils::write.table(x, file = path, sep = ",", row.names = FALSE)
  } else {
    saveRDS(x, if (nzchar(ext)) path else sprintf("%s.rds", path))
  }

  invisible(path)
}

# -------------------------------------------------------------------------
# Quiet logging -----------------------------------------------------------

#' Toggle quiet logging for helper functions
#'
#' @param quiet Logical flag. When `TRUE`, suppress messages emitted by
#'   `tyler_log_info()`.
#'
#' @return The previous quiet value (invisibly).
#' @export
tyler_use_quiet_logging <- function(quiet = TRUE) {
  old <- getOption("tyler.quiet", FALSE)
  options(tyler.quiet = quiet)
  invisible(old)
}

#' Emit a timestamped log message when quiet mode is disabled
#'
#' @param message_text Character string to print.
#' @param quiet Optional override for quiet behaviour.
#'
#' @export
tyler_log_info <- function(message_text, quiet = getOption("tyler.quiet", FALSE)) {
  if (!isTRUE(quiet)) {
    message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), message_text))
  }
  invisible(NULL)
}

# -------------------------------------------------------------------------
# Standard labels and palettes -------------------------------------------

#' Retrieve the standard label dictionary used in `tyler`
#'
#' @return A named list of canonical labels for common columns.
#' @export
tyler_standard_labels <- function() {
  list(
    npi = "National Provider Identifier",
    state = "State",
    city = "City",
    call_outcome = "Call Outcome",
    quality = "Quality Tier",
    call_time = "Call Duration (minutes)",
    hold_time = "Hold Duration (minutes)",
    eta = "Estimated Completion"
  )
}

#' Retrieve a standard colour palette
#'
#' @param name Palette identifier. Supported values are `"primary"`,
#'   `"sequential"`, and `"diverging"`.
#'
#' @return A character vector of hex colours.
#' @export
tyler_standard_palette <- function(name = c("primary", "sequential", "diverging")) {
  name <- match.arg(name)
  palettes <- list(
    primary = c("#0B3C5D", "#328CC1", "#D9B310", "#1D2731"),
    sequential = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5"),
    diverging = c("#b30000", "#fdbf6f", "#1b7837")
  )
  palettes[[name]]
}

# -------------------------------------------------------------------------
# Quality tier helper -----------------------------------------------------

#' Convert numeric scores to qualitative tiers
#'
#' @param score Numeric value between 0 and 1.
#' @param thresholds Named numeric vector with `high` and `medium` entries.
#'
#' @return A quality tier of `"high"`, `"medium"`, or `"low"` (or `NA` when the
#'   score is missing).
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
