#' Internal helpers for reading and writing roster files
#'
#' These helpers standardise how the package reads and writes tabular
#' datasets so that functions can transparently support both CSV and Parquet
#' storage without duplicating logic.
#'
#' @keywords internal
NULL

# nocov start

tyler_normalize_file_format <- function(format = NULL, path = NULL, default = "csv") {
  choices <- c("csv", "parquet")
  if (!is.null(format)) {
    return(match.arg(format, choices))
  }
  if (!is.null(path)) {
    ext <- tolower(tools::file_ext(path))
    if (nzchar(ext) && ext %in% choices) {
      return(ext)
    }
  }
  default
}

tyler_require_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "The 'arrow' package is required to read or write Parquet files. ",
      "Install it with install.packages('arrow').",
      call. = FALSE
    )
  }
}

tyler_read_table <- function(path, format = NULL, ...) {
  fmt <- tyler_normalize_file_format(format, path = path)
  if (identical(fmt, "csv")) {
    readr::read_csv(path, show_col_types = FALSE, ...)
  } else {
    tyler_require_arrow()
    arrow::read_parquet(path, as_data_frame = TRUE)
  }
}

tyler_write_table <- function(data, path, format = NULL, append = FALSE, col_names = TRUE, ...) {
  fmt <- tyler_normalize_file_format(format, path = path)
  if (identical(fmt, "csv")) {
    readr::write_csv(data, path, append = append, col_names = col_names, ...)
  } else {
    tyler_require_arrow()
    if (inherits(data, "grouped_df")) {
      data <- dplyr::ungroup(data)
    }
    if (append && file.exists(path)) {
      existing <- arrow::read_parquet(path, as_data_frame = TRUE)
      data <- dplyr::bind_rows(existing, data)
    }
    arrow::write_parquet(data, sink = path, ...)
  }
  invisible(path)
}

# nocov end
